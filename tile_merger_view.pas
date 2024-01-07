unit tile_merger_view;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, FileUtil, FPimage,
  tile_merger_core;

type

  ETileRangeError = class(Exception)
  public
    constructor Create(value:double);
  end;

  TTileFormat = (tfPNG, tfJPG, tfBMP);

  TTile = class
    FPicture:TPicture;
    FIndirect:Boolean;
    FTileTop:Int64;
    FTileLeft:Int64;
    FTileWidth:Int64;
    FTileHeight:Int64;
    FImageFormat:TTileFormat;
  protected
    function GetTileRight:Int64;
    function GetTileBottom:Int64;
    function GetTileRect:TRect;
    function GetCanvas:TCanvas;
  public
    procedure SetTileRange(ALeft,ATop,AWidth,AHeight:Int64);
    property TileTop:Int64 read FTileTop;
    property TileLeft:Int64 read FTileLeft;
    property TileWidth:Int64 read FTileWidth;
    property TileHeight:Int64 read FTileHeight;
    property TileRight:Int64 read GetTileRight;
    property TileBottom:Int64 read GetTileBottom;
    property TileRect:TRect read GetTileRect;
    property Canvas:TCanvas read GetCanvas;
  public
    constructor CreateFromFile(const AFileName:String;AFormat:TTileFormat);
    constructor CreateFromTiles(tiles:TList);
    destructor Destroy; override;
  public
    procedure Update(tile:TTile);
    class procedure GetWorldRange(tiles:TList;out vLeft,vTop,vRight,vBottom:Int64);
  end;

    TTileViewer = class(TCustomControl)
  private
    FTileList:TList;
    FTileLevel:Byte;
    FCanvasTop:Int64;
    FCanvasLeft:Int64;
    FCanvasWidth:Int64;
    FCanvasHeight:Int64;
    FMouseCursor:TPoint;
    FMovementCursor:TPoint;
    FMovementEnabled:Boolean;
    FMovementCenter:TInt64Point;
  private
    FShowGrid:Boolean;
    FShowInfo:Boolean;
    FStopDrawing:Boolean;
  public
    property ShowGrid:Boolean read FShowGrid write FShowGrid;
    property ShowInfo:Boolean read FShowInfo write FShowInfo;
    property StopDrawing:Boolean read FStopDrawing write FStopDrawing;
  protected
    function GetCanvasRight:Int64;
    function GetCanvasBottom:Int64;
    procedure SetCanvasRight(value:Int64);
    procedure SetCanvasBottom(value:Int64);
  public
    property CanvasTop:Int64 read FCanvasTop write FCanvasTop;
    property CanvasLeft:Int64 read FCanvasLeft write FCanvasLeft;
    property CanvasWidth:Int64 read FCanvasWidth write FCanvasWidth;
    property CanvasHeight:Int64 read FCanvasHeight write FCanvasHeight;
    property CanvasRight:Int64 read GetCanvasRight write SetCanvasRight;
    property CanvasBottom:Int64 read GetCanvasBottom write SetCanvasBottom;
  protected
    procedure MouseDown(Button:TMouseButton;Shift:TShiftState;X,Y:Integer);override;
    procedure MouseUp(Button:TMouseButton;Shift:TShiftState;X,Y:Integer);override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure ViewResize(Sender:TObject);
  protected
    function TileVisible(ATile:TTile):Boolean;
    function TileToCanvasRect(ATile:TTile):TRect;
    function CanvasCenter:TInt64Point;
    procedure GetCanvasRange(out vLeft,vTop,vRight,vBottom:Int64);
    function CursorPoint(X,Y:Integer):TWebMercator;
    procedure MoveCanvas(X,Y:Int64);
    procedure PanToPoint(APoint:TInt64Point);
    procedure Zoom(AOrigin:TWebMercator;AScale:Double);
    procedure ProportionCorrection;
    procedure PaintInfo;
    procedure PaintStop;
    procedure Paint; override;
  public
    procedure Clear;
    procedure Refresh;
    procedure ZoomToWorld;
    procedure LoadFromWMTS(WmtsPath:String;Level:Byte;AFormat:TTileFormat);
    procedure SaveToGeoTiff(FilenameWithoutExt:String);
  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
  end;

implementation

procedure view_proportion_correction(vw,vh:integer;var ct,cl,cw,ch:int64);
var vp,cp:double;
    delta:int64;
begin
  if cw*ch =0 then exit;
  vp := vw / vh;
  cp := cw / ch;
  if vp > cp then begin
    delta := round(ch*vp) - cw;
    cl    := cl - delta div 2;
    cw    := cw + delta;
  end else begin
    delta := round(cw/vp) - ch;
    ct    := ct - delta div 2;
    ch    := ch + delta;
  end;
end;

{ ETileRangeError }
constructor ETileRangeError.Create(value:double);
begin
  inherited Create(Format('瓦片范围计算错误：%f',[value]));
end;

{ TTile }

function TTile.GetTileRight:Int64;
begin
  result:=FTileLeft+FTileWidth;
end;

function TTile.GetTileBottom:Int64;
begin
  result:=FTileTop+FTileHeight;
end;

function TTile.GetTileRect:TRect;
begin
  result:=Classes.Rect(round(TileLeft),round(TileTop),round(TileRight),round(TileBottom));
end;

function TTile.GetCanvas:TCanvas;
begin
  case FImageFormat of
    tfPNG:result:=FPicture.PNG.Canvas;
    tfJPG:result:=FPicture.Jpeg.Canvas;
    tfBMP:result:=FPicture.Bitmap.Canvas;
  end;
end;

procedure TTile.SetTileRange(ALeft,ATop,AWidth,AHeight:Int64);
begin
  FTileTop:=ATop;
  FTileLeft:=ALeft;
  FTileWidth:=AWidth;
  FTileHeight:=AHeight;
end;

constructor TTile.CreateFromFile(const AFileName:String;AFormat:TTileFormat);
begin
  inherited Create;
  FIndirect:=false;
  FPicture:=TPicture.Create;
  case AFormat of
    tfPNG:FPicture.PNG.LoadFromFile(AFileName);
    tfBMP:FPicture.Bitmap.LoadFromFile(AFileName);
    tfJPG:FPicture.Jpeg.LoadFromFile(AFileName);
  end;
  FImageFormat:=AFormat;
end;

constructor TTile.CreateFromTiles(tiles:TList);
var len,idx:integer;
    l,t,r,b:int64;
    tmpTile:TTile;
begin
  inherited Create;
  FIndirect:=false;
  FPicture:=TPicture.Create;
  len:=tiles.Count;
  if len<1 then raise Exception.Create('TTile.CreateFromTiles need at least one tile in tiles argument.');
  tmpTile:=TTile(tiles[0]);
  FImageFormat:=tmpTile.FImageFormat;
  FTileTop:=tmpTile.FTileTop;
  FTileLeft:=tmpTile.FTileLeft;
  FTileWidth:=tmpTile.FTileWidth;
  FTileHeight:=tmpTile.FTileHeight;
  TTile.GetWorldRange(tiles,l,t,r,b);
  SetTileRange(l,t,r-l,b-t);
  case FImageFormat of
    tfBMP:FPicture.Bitmap.SetSize(r-l,b-t);
    tfJPG:FPicture.Jpeg.SetSize(r-l,b-t);
    tfPNG:FPicture.PNG.SetSize(r-l,b-t);
  end;
  idx:=0;
  while idx<len do begin
    tmpTile:=TTile(tiles[idx]);
    Update(tmpTile);
    inc(idx);
  end;
end;

destructor TTile.Destroy;
begin
  FPicture.Free;
  inherited Destroy;
end;

procedure TTile.Update(tile:TTile);
var csrc,cdst:TCanvas;
    rsrc,rdst:TRect;
begin
  case Self.FImageFormat of
    tfBMP:cdst:=Self.FPicture.Bitmap.Canvas;
    tfJPG:cdst:=Self.FPicture.Jpeg.Canvas;
    tfPNG:cdst:=Self.FPicture.PNG.Canvas;
  end;
  case tile.FImageFormat of
    tfBMP:csrc:=tile.FPicture.Bitmap.Canvas;
    tfJPG:csrc:=tile.FPicture.Jpeg.Canvas;
    tfPNG:csrc:=tile.FPicture.PNG.Canvas;
  end;
  rdst:=tile.TileRect;
  rsrc:=Classes.Rect(0,0,tile.FPicture.Width,tile.FPicture.Height);
  rdst.Offset(-TileLeft,-TileTop);
  cdst.CopyRect(rdst,csrc,rsrc);
end;

class procedure TTile.GetWorldRange(tiles:TList;out vLeft,vTop,vRight,vBottom:Int64);
var len,t,l,r,b:int64;
    index:integer;
    tmpTile:TTile;
begin
  len:=tiles.Count;
  if len<1 then exit;
  tmpTile:=TTile(tiles[0]);
  t:=tmpTile.TileTop;
  l:=tmpTile.TileLeft;
  r:=tmpTile.TileRight;
  b:=tmpTile.TileBottom;
  index:=1;
  while index<len do begin
    tmpTile:=TTile(tiles[index]);
    if tmpTile.TileTop<t then t:=tmpTile.TileTop;
    if tmpTile.TileLeft<l then l:=tmpTile.TileLeft;
    if tmpTile.TileRight>r then r:=tmpTile.TileRight;
    if tmpTile.TileBottom>b then b:=tmpTile.TileBottom;
    inc(index);
  end;
  vTop:=t;
  vLeft:=l;
  vRight:=r;
  vBottom:=b;
end;

{ TTileViewer }

function TTileViewer.GetCanvasRight:Int64;
begin
  result:=FCanvasLeft+FCanvasWidth;
end;

function TTileViewer.GetCanvasBottom:Int64;
begin
  result:=FCanvasTop+FCanvasHeight;
end;

procedure TTileViewer.SetCanvasRight(value:Int64);
begin
  if value<FCanvasLeft then raise ETileRangeError.Create(value);
  FCanvasWidth:=value-FCanvasLeft;
end;

procedure TTileViewer.SetCanvasBottom(value:Int64);
begin
  if value<FCanvasTop then raise ETileRangeError.Create(value);
  FCanvasHeight:=value-FCanvasTop;
end;

procedure TTileViewer.MouseDown(Button:TMouseButton;Shift:TShiftState;X,Y:Integer);
begin
  if Button<>mbMiddle then exit;
  FMovementEnabled:=true;
  FMovementCursor:=Classes.Point(X,Y);
  FMovementCenter:=CanvasCenter;
end;

procedure TTileViewer.MouseUp(Button:TMouseButton;Shift:TShiftState;X,Y:Integer);
begin
  if Button=mbMiddle then begin
    FMovementEnabled:=false;
    Paint;
  end;
end;

procedure TTileViewer.MouseMove(Shift: TShiftState; X, Y: Integer);
var vec:TInt64Point;
begin
  if FMovementEnabled then begin
    vec.x:=(FMovementCursor.X-X)*FCanvasWidth div Width;
    vec.y:=(FMovementCursor.Y-Y)*FCanvasHeight div Height;
    PanToPoint(FMovementCenter+vec);
    Paint;
  end;
  if ShowInfo then begin
    FMouseCursor.x:=X;
    FMouseCursor.y:=Y;
    PaintInfo;
  end;
end;

procedure TTileViewer.MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if WheelDelta>0 then Zoom(CursorPoint(MousePos.X,MousePos.Y),0.8)
  else Zoom(CursorPoint(MousePos.X,MousePos.Y),1.25);
  Paint;
end;

procedure TTileViewer.ViewResize(Sender:TObject);
begin
  Paint;
end;

function TTileViewer.TileVisible(ATile:TTile):Boolean;
begin
  result:=false;
  if ATile.TileLeft > CanvasRight then exit;
  if ATile.TileRight < CanvasLeft then exit;
  if ATile.TileTop > CanvasBottom then exit;
  if ATile.TileRight < CanvasLeft then exit;
  result:=true;
end;

function TTileViewer.TileToCanvasRect(ATile:TTile):TRect;
var dx1,dx2,dy1,dy2,rx,ry:double;
begin
  rx:=Width/CanvasWidth;
  ry:=Height/CanvasHeight;
  dx1:=ATile.TileLeft-CanvasLeft;
  dy1:=ATile.TileTop-CanvasTop;
  dx2:=ATile.TileRight-CanvasLeft;
  dy2:=ATile.TileBottom-CanvasTop;
  dx1:=dx1*rx;
  dx2:=dx2*rx;
  dy1:=dy1*ry;
  dy2:=dy2*ry;
  result:=Classes.Rect(round(dx1),round(dy1),round(dx2),round(dy2));
end;

function TTileViewer.CanvasCenter:TInt64Point;
begin
  result.x:=FCanvasLeft+FCanvasWidth div 2;
  result.y:=FCanvasTop+FCanvasHeight div 2;
end;

procedure TTileViewer.GetCanvasRange(out vLeft,vTop,vRight,vBottom:Int64);
var t,l,r,b:int64;
begin
  TTile.GetWorldRange(FTileList,l,t,r,b);
  vTop:=t;
  vLeft:=l;
  vRight:=r;
  vBottom:=b;
end;

function TTileViewer.CursorPoint(X,Y:Integer):TWebMercator;
begin
  result.level:=FTileLevel;
  result.coord.x:=FCanvasLeft+FCanvasWidth*X div Width;
  result.coord.y:=FCanvasTop+FCanvasHeight*Y div Height;
end;

procedure TTileViewer.MoveCanvas(X,Y:Int64);
begin
  FCanvasLeft:=FCanvasLeft+X;
  FCanvasTop:=FCanvasTop+Y;
end;

procedure TTileViewer.PanToPoint(APoint:TInt64Point);
var dx,dy:int64;
    center:TInt64Point;
begin
  center:=CanvasCenter;
  dx:=APoint.x-center.x;
  dy:=APoint.y-center.y;
  FCanvasLeft:=FCanvasLeft+dx;
  FCanvasTop:=FCanvasTop+dy;
end;

procedure TTileViewer.Zoom(AOrigin:TWebMercator;AScale:Double);
var dl,dr,dt,db:int64;
begin
  if (AScale<0.01) or (AScale>100) then raise ETileRangeError.Create(AScale);
  dt:=AOrigin.coord.y-CanvasTop;
  dl:=AOrigin.coord.x-CanvasLeft;
  dr:=CanvasRight-AOrigin.coord.x;
  db:=CanvasBottom-AOrigin.coord.y;
  dt:=round(dt*AScale);
  dl:=round(dl*AScale);
  dr:=round(dr*AScale);
  db:=round(db*AScale);
  FCanvasTop:=AOrigin.coord.y-dt;
  FCanvasLeft:=AOrigin.coord.x-dl;
  FCanvasWidth:=dl+dr;
  FCanvasHeight:=dt+db;
end;

procedure TTileViewer.ProportionCorrection;
var tt,ll,ww,hh:int64;
begin
  tt:=CanvasTop;
  ll:=CanvasLeft;
  ww:=CanvasWidth;
  hh:=CanvasHeight;
  view_proportion_correction(Width,Height,tt,ll,ww,hh);
  CanvasTop:=tt;
  CanvasLeft:=ll;
  CanvasWidth:=ww;
  CanvasHeight:=hh;
end;

procedure TTileViewer.PaintInfo;
var wmct:TWebMercator;
    wmct_xy,wmct_lt,wmct_rb:TDoublePoint;
    ltlg:TLatLong;
    prompt_cursor,prompt_view,wmct_cursor,wmct_view:string;
    text_height,text_top,pw_cursor,pw_view,sw_cursor,sw_view,pl_view,sl_view:integer;
begin
  if ShowInfo then begin
    wmct:=CursorPoint(FMouseCursor.X,FMouseCursor.Y);
    wmct_xy:=WebmercatorToXY(wmct);
    wmct_lt:=WebmercatorToXY(WebMercator(CanvasLeft,CanvasTop,FTileLevel));
    wmct_rb:=WebmercatorToXY(WebMercator(CanvasRight,CanvasBottom,FTileLevel));
    ltlg:=WebmercatorToLatlong(wmct);
    Canvas.Pen.Color:=clNone;
    Canvas.Brush.Color:=clWhite;
    Canvas.Brush.Style:=bsSolid;
    prompt_cursor:=Format(' cx=%d  cy=%d  tx=%d  ty=%d',[wmct.coord.x,wmct.coord.y,wmct.coord.x div cell_pixel_width,wmct.coord.y div cell_pixel_width]);
    prompt_view:=Format(' cl=%d  cr=%d  ct=%d  cb=%d',[CanvasLeft,CanvasRight,CanvasTop,CanvasBottom]);
    wmct_cursor:=Format(' X=%f  Y=%f  lng=%3.6f  lat=%2.6f',[wmct_xy.x,wmct_xy.y,ltlg.x,ltlg.y]);
    wmct_view:=Format(' l=%f  r=%f  t=%f  b=%f',[wmct_lt.x,wmct_rb.x,wmct_lt.y,wmct_rb.y]);
    text_height:=Canvas.TextHeight(prompt_cursor);
    text_top:=Height-text_height;
    pw_cursor:=Canvas.TextWidth(prompt_cursor);
    pw_view:=Canvas.TextWidth(prompt_view);
    sw_cursor:=Canvas.TextWidth(wmct_cursor);
    sw_view:=Canvas.TextWidth(wmct_view);
    pl_view:=Width-pw_view-10;
    sl_view:=Width-sw_view-10;
    Canvas.Rectangle(0,0,Width,text_height);
    Canvas.Rectangle(0,text_top,Width,Height);
    Canvas.TextOut(0,0,prompt_cursor);
    Canvas.TextOut(0,text_top,wmct_cursor);
    if pl_view>pw_cursor then Canvas.TextOut(pl_view,0,prompt_view);
    if sl_view>sw_cursor then Canvas.TextOut(sl_view,text_top,wmct_view);
  end;
end;

procedure TTileViewer.PaintStop;
const prompt = '[暂停绘制]';
var th,tw:integer;
begin
  tw:=Canvas.TextWidth(prompt);
  th:=Canvas.TextHeight(prompt);
  Canvas.TextOut((Width-tw) div 2,(Height-th) div 2,prompt);
end;

procedure TTileViewer.Paint;
var index:integer;
    tile:TTile;
    SrcRect,DstRect:TRect;
begin
  Canvas.Brush.Color:=clWhite;
  Canvas.Brush.Style:=bsSolid;
  Canvas.Clear;
  ProportionCorrection;
  SrcRect:=Classes.Rect(0,0,cell_pixel_width,cell_pixel_width);
  if FStopDrawing and not FShowGrid then begin
    PaintStop;
    exit;
  end;
  index:=0;
  while index<FTileList.Count do begin
    tile:=TTile(FTileList.Items[index]);
    if TileVisible(tile) then begin
      DstRect:=TileToCanvasRect(tile);
      if not FStopDrawing then Canvas.CopyRect(DstRect,tile.Canvas,SrcRect);
      if FShowGrid then begin
        Canvas.Pen.Color:=clRed;
        Canvas.Pen.Style:=psSolid;
        Canvas.Pen.Width:=1;
        Canvas.Brush.Color:=clNone;
        Canvas.Brush.Style:=bsClear;
        Canvas.Rectangle(DstRect);
      end;
    end;
    inc(index);
  end;
  PaintInfo;
end;

procedure TTileViewer.Clear;
begin
  while FTileList.Count>0 do begin
    TTile(FTileList.Items[0]).Free;
    FTileList.Delete(0);
  end;
end;

procedure TTileViewer.Refresh;
begin
  Paint;
end;

procedure TTileViewer.ZoomToWorld;
var l,t,r,b,w,h:int64;
begin
  GetCanvasRange(l,t,r,b);
  w:=r-l;
  h:=b-t;
  view_proportion_correction(Width,Height,t,l,w,h);
  FCanvasTop:=t;
  FCanvasLeft:=l;
  FCanvasWidth:=w;
  FCanvasHeight:=h;
  Paint;
end;

procedure TTileViewer.LoadFromWMTS(WmtsPath:String;Level:Byte;AFormat:TTileFormat);
var files:TStringList;
    rootpath,filename:string;
    str_x,str_y:string;
    rootpath_length:integer;
    cell_x,cell_y:integer;
    x1,x2,y1,y2:int64;
    tmpTile:TTile;
begin
  FTileLevel:=Level;
  rootpath:=WmtsPath+'/'+IntToStr(Level);
  rootpath_length:=length(rootpath);
  try
    files:=TStringList.Create;
    FindAllFiles(files,rootpath,'*.png',true,faAnyFile);
    for filename in files do begin
      str_x:=filename;
      System.Delete(str_x,1,rootpath_length+1);
      str_y:=ExtractFileNameWithoutExt(ExtractFileName(str_x));
      str_x:=ExtractFilePath(str_x);
      System.delete(str_x,length(str_x),1);
      cell_x:=StrToInt(str_x);
      cell_y:=StrToInt(str_y);
      x1:=cell_x*cell_pixel_width;
      y1:=cell_y*cell_pixel_width;
      x2:=x1+cell_pixel_width;
      y2:=y1+cell_pixel_width;
      tmpTile:=TTile.CreateFromFile(filename,AFormat);
      tmpTile.SetTileRange(x1,y1,x2-x1,y2-y1);
      FTileList.Add(tmpTile);
    end;
  finally
    files.Free;
  end;
end;

procedure TTileViewer.SaveToGeoTiff(FilenameWithoutExt:String);
var StopDrawingState:boolean;
    l,t,r,b:int64;
    w,h:integer;
    tmpTfwFile:TStringList;
    tmpTile:TTile;
    lt,rb:TDoublePoint;
begin
  if FTileList.Count<1 then exit;
  StopDrawingState:=StopDrawing;
  StopDrawing:=true;
  GetCanvasRange(l,t,r,b);
  w:=trunc(r-l);
  h:=trunc(b-t);
  tmpTile:=TTile.CreateFromTiles(FTileList);
  tmpTFWFile:=TStringList.Create;
  try
    tmpTile.FPicture.SaveToFile(FilenameWithoutExt+'.tif','tif');  ;
    //GeoTiff里的exif信息要专门去写
    lt:=WebmercatorToXY(WebMercator(l,t,FTileLevel));
    rb:=WebmercatorToXY(WebMercator(r,b,FTileLevel));
    tmpTFWFile.Add(FloatToStr((rb.x-lt.x)/w));  // A = (MaxX - MinX) / vpwidth
    tmpTFWFile.Add('0.0');                      // B = 0
    tmpTFWFile.Add('0.0');                      // C = 0
    tmpTFWFile.Add(FloatToStr(-(lt.y-rb.y)/h)); // D = (-MinY + MaxY) / vpheight
    tmpTFWFile.Add(FloatToStr(lt.x));           // E = MinX
    tmpTFWFile.Add(FloatToStr(lt.y));           // F = MinY
    tmpTFWFile.SaveToFile(FilenameWithoutExt+'.tfw');
  finally
    StopDrawing:=StopDrawingState;
    tmpTfwFile.Free;
    tmpTile.Free;
  end;
end;

constructor TTileViewer.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FTileList:=TList.Create;
  FMovementEnabled:=false;
  FShowGrid:=false;
  FShowInfo:=false;
  FStopDrawing:=false;
  OnMouseWheel:=@MouseWheel;
  OnResize:=@ViewResize;
end;

destructor TTileViewer.Destroy;
begin
  Clear;
  inherited Destroy;
end;



end.

