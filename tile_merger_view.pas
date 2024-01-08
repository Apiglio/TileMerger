unit tile_merger_view;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, FileUtil, FPimage,
  tile_merger_core;

const ogc_ppi = 90.71446714322;
      ogc_mm_per_pixel = 0.28;

type

  ETileRangeError = class(Exception)
  public
    constructor Create(value:double);
  end;

  TTileFormat = (tfPNG, tfJPG, tfBMP);

  TTile = class
    FPicture:TPicture;
    FIndirect:Boolean;
    FLeftTop:TDoublePoint;
    FScaleX:Double;
    FScaleY:Double;
    FPixelWidth:Int64;
    FPixelHeight:Int64;
    FImageFormat:TTileFormat;
  protected
    function GetRightBottom:TDoublePoint;
    function GetTileTop:Double;
    function GetTileLeft:Double;
    function GetTileRight:Double;
    function GetTileBottom:Double;
    function GetTileWidth:Double;
    function GetTileHeight:Double;
    function GetCanvas:TCanvas;
  public
    procedure SetTileRange(ALeft,ATop,ARight,ABottom:Double);
    property LeftTop:TDoublePoint read FLeftTop;
    property RightBottom:TDoublePoint read GetRightBottom;
    property TileTop:Double read GetTileTop;
    property TileLeft:Double read GetTileLeft;
    property TileWidth:Double read GetTileWidth;
    property TileHeight:Double read GetTileHeight;
    property TileRight:Double read GetTileRight;
    property TileBottom:Double read GetTileBottom;
    property Width:Int64 read FPixelWidth;
    property Height:Int64 read FPixelHeight;
    property Canvas:TCanvas read GetCanvas;
  public
    function GetCanvasPoint(wmct_xy:TDoublePoint):TPoint;
    function GetMercatorXY(canvas_point:TPoint):TDoublePoint;
  public
    constructor CreateFromFile(const AFileName:String;AFormat:TTileFormat);
    constructor CreateFromTiles(tiles:TList);
    destructor Destroy; override;
  public
    procedure Update(tile:TTile);
    class procedure GetWorldRange(tiles:TList;out vLeft,vTop,vRight,vBottom:Double);
  end;

  TTileViewer = class(TCustomControl)
  private
    //只通过以下两个参数和width、height定位画幅，其他均通过Get/Set获得
    FLeftTop:TDoublePoint;
    FScaleX:Double;
    FScaleY:Double;
  private
    FTileList:TList;
    FTileLevel:Byte;
    FMouseCursor:TPoint;
    FMovementCursor:TPoint;
    FMovementEnabled:Boolean;
    FMovementCenter:TDoublePoint;
  private
    FShowGrid:Boolean;
    FShowInfo:Boolean;
    FStopDrawing:Boolean;
  public
    property ShowGrid:Boolean read FShowGrid write FShowGrid;
    property ShowInfo:Boolean read FShowInfo write FShowInfo;
    property StopDrawing:Boolean read FStopDrawing write FStopDrawing;
  protected
    function GetRightBottom:TDoublePoint;
    function GetCanvasTop:Double;
    function GetCanvasLeft:Double;
    function GetCanvasRight:Double;
    function GetCanvasBottom:Double;
    function GetCanvasWidth:Double;
    function GetCanvasHeight:Double;
    procedure SetCanvasTop(value:Double);
    procedure SetCanvasLeft(value:Double);
    procedure SetCanvasRight(value:Double);
    procedure SetCanvasBottom(value:Double);
    procedure SetCanvasWidth(value:Double);
    procedure SetCanvasHeight(value:Double);
  public
    property LeftTop:TDoublePoint read FLeftTop;
    property RightBottom:TDoublePoint read GetRightBottom;
    property CanvasTop:Double read GetCanvasTop write SetCanvasTop;
    property CanvasLeft:Double read GetCanvasLeft write SetCanvasLeft;
    property CanvasRight:Double read GetCanvasRight write SetCanvasRight;
    property CanvasBottom:Double read GetCanvasBottom write SetCanvasBottom;
    property CanvasWidth:Double read GetCanvasWidth write SetCanvasWidth;
    property CanvasHeight:Double read GetCanvasHeight write SetCanvasHeight;
    property ScaleX:Double read FScaleX write FScaleX;
    property ScaleY:Double read FScaleY write FScaleY;
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
    function CanvasCenter:TDoublePoint;
    procedure GetCanvasRange(out vLeft,vTop,vRight,vBottom:Double);
    function CursorPoint(X,Y:Integer):TDoublePoint;
    procedure PanToPoint(APoint:TDoublePoint);
    procedure Zoom(AOrigin:TDoublePoint;AScale:Double);
    procedure ProportionCorrection;
    procedure PaintInfo;
    procedure PaintStop;
    procedure PaintTile(ATile:TTile);
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

procedure view_proportion_correction(vw,vh:integer;var ct,cl,cw,ch:double);
var vp,cp:double;
    delta:double;
begin
  if cw*ch =0 then exit;
  vp := vw / vh;
  cp := cw / ch;
  if vp > cp then begin
    delta := ch*vp - cw;
    cl    := cl - delta/2;
    cw    := cw + delta;
  end else begin
    delta := cw/vp - ch;
    ct    := ct - delta/2;
    ch    := ch + delta;
  end;
end;

{ ETileRangeError }
constructor ETileRangeError.Create(value:double);
begin
  inherited Create(Format('瓦片范围计算错误：%f',[value]));
end;

{ TTile }

function TTile.GetRightBottom:TDoublePoint;
begin
  result.x:=FLeftTop.x+FScaleX*Width*ogc_mm_per_pixel;
  result.y:=FLeftTop.y-FScaleY*Height*ogc_mm_per_pixel;
end;

function TTile.GetTileTop:Double;
begin
  result:=FLeftTop.y;
end;

function TTile.GetTileLeft:Double;
begin
  result:=FLeftTop.x;
end;

function TTile.GetTileRight:Double;
begin
  result:=FLeftTop.x+FScaleX*ogc_mm_per_pixel*FPixelWidth;
end;

function TTile.GetTileBottom:Double;
begin
  result:=FLeftTop.y-FScaleY*ogc_mm_per_pixel*FPixelHeight;
end;

function TTile.GetTileWidth:Double;
begin
  result:=FScaleX*ogc_mm_per_pixel*FPixelWidth;
end;

function TTile.GetTileHeight:Double;
begin
  result:=FScaleY*ogc_mm_per_pixel*FPixelHeight;
end;

function TTile.GetCanvas:TCanvas;
begin
  case FImageFormat of
    tfPNG:result:=FPicture.PNG.Canvas;
    tfJPG:result:=FPicture.Jpeg.Canvas;
    tfBMP:result:=FPicture.Bitmap.Canvas;
  end;
end;

procedure TTile.SetTileRange(ALeft,ATop,ARight,ABottom:Double);
begin
  FLeftTop.x:=ALeft;
  FLeftTop.y:=ATop;
  FScaleX:=(ARight-ALeft)/ogc_mm_per_pixel/FPixelWidth;
  FScaleY:=(ATop-ABottom)/ogc_mm_per_pixel/FPixelHeight;
end;

function TTile.GetCanvasPoint(wmct_xy:TDoublePoint):TPoint;
begin
  result.x:=+round((wmct_xy.x-FLeftTop.x)/FScaleX/ogc_mm_per_pixel);
  result.y:=-round((wmct_xy.y-FLeftTop.y)/FScaleY/ogc_mm_per_pixel);
end;

function TTile.GetMercatorXY(canvas_point:TPoint):TDoublePoint;
begin
  result.x:=FLeftTop.x+FScaleX*canvas_point.x*ogc_mm_per_pixel;
  result.y:=FLeftTop.y-FScaleY*canvas_point.y*ogc_mm_per_pixel;
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
  FPixelWidth:=FPicture.Width;
  FPixelHeight:=FPicture.Height;
  FImageFormat:=AFormat;
end;

constructor TTile.CreateFromTiles(tiles:TList);
var len,idx:integer;
    l,t,r,b:double;
    tmpTile:TTile;
begin
  inherited Create;
  FIndirect:=false;
  FPicture:=TPicture.Create;
  len:=tiles.Count;
  if len<1 then raise Exception.Create('TTile.CreateFromTiles need at least one tile in tiles argument.');
  tmpTile:=TTile(tiles[0]);
  FPixelWidth:=tmpTile.FPixelWidth;
  FPixelHeight:=tmpTile.FPixelHeight;
  TTile.GetWorldRange(tiles,l,t,r,b);
  SetTileRange(l,t,r,b);
  FImageFormat:=tmpTile.FImageFormat;
  //还原像素长度比例同时相应增加像素数量
  FPixelWidth:=round(tmpTile.FPixelWidth*FScaleX/tmpTile.FScaleX);
  FPixelHeight:=round(tmpTile.FPixelHeight*FScaleY/tmpTile.FScaleY);
  FScaleX:=tmpTile.FScaleX;
  FScaleY:=tmpTile.FScaleY;
  case FImageFormat of
    tfBMP:FPicture.Bitmap.SetSize(FPixelWidth,FPixelHeight);
    tfJPG:FPicture.Jpeg.SetSize(FPixelWidth,FPixelHeight);
    tfPNG:FPicture.PNG.SetSize(FPixelWidth,FPixelHeight);
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
    lt,rb:TPoint;
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
  lt:=GetCanvasPoint(tile.LeftTop);
  rb:=GetCanvasPoint(tile.RightBottom);
  rdst:=Classes.Rect(lt.x,lt.y,rb.x,rb.y);
  rsrc:=Classes.Rect(0,0,tile.Width,tile.Height);
  cdst.CopyRect(rdst,csrc,rsrc);
end;

class procedure TTile.GetWorldRange(tiles:TList;out vLeft,vTop,vRight,vBottom:Double);
var len:int64;
    t,l,r,b:double;
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
    if tmpTile.TileTop>t then t:=tmpTile.TileTop;
    if tmpTile.TileLeft<l then l:=tmpTile.TileLeft;
    if tmpTile.TileRight>r then r:=tmpTile.TileRight;
    if tmpTile.TileBottom<b then b:=tmpTile.TileBottom;
    inc(index);
  end;
  vTop:=t;
  vLeft:=l;
  vRight:=r;
  vBottom:=b;
end;

{ TTileViewer }

function TTileViewer.GetRightBottom:TDoublePoint;
begin
  result.x:=FLeftTop.x+FScaleX*Width*ogc_mm_per_pixel;
  result.y:=FLeftTop.y-FScaleY*Height*ogc_mm_per_pixel;
end;

function TTileViewer.GetCanvasTop:Double;
begin
  result:=FLeftTop.y;
end;

function TTileViewer.GetCanvasLeft:Double;
begin
  result:=FLeftTop.x;
end;

function TTileViewer.GetCanvasRight:Double;
begin
  result:=FLeftTop.x+Width*FScaleX*ogc_mm_per_pixel;
end;

function TTileViewer.GetCanvasBottom:Double;
begin
  result:=FLeftTop.y-Height*FScaleY*ogc_mm_per_pixel;
end;

function TTileViewer.GetCanvasWidth:Double;
begin
  result:=Width*FScaleX*ogc_mm_per_pixel;
end;

function TTileViewer.GetCanvasHeight:Double;
begin
  result:=Height*FScaleY*ogc_mm_per_pixel;
end;

procedure TTileViewer.SetCanvasTop(value:Double);
begin
  FLeftTop.y:=value;
end;

procedure TTileViewer.SetCanvasLeft(value:Double);
begin
  FLeftTop.x:=value;
end;

procedure TTileViewer.SetCanvasRight(value:Double);
begin
  if value<=FLeftTop.x then raise ETileRangeError.Create(value);
  FScaleX:=(value-FLeftTop.x)/Width/ogc_mm_per_pixel;
end;

procedure TTileViewer.SetCanvasBottom(value:Double);
begin
  if value>=FLeftTop.y then raise ETileRangeError.Create(value);
  FScaleY:=(FLeftTop.y-value)/Height/ogc_mm_per_pixel;
end;

procedure TTileViewer.SetCanvasWidth(value:Double);
begin
  FScaleX:=value/Width/ogc_mm_per_pixel;
end;

procedure TTileViewer.SetCanvasHeight(value:Double);
begin
  FScaleY:=value/Height/ogc_mm_per_pixel;
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
var vec:TDoublePoint;
begin
  if FMovementEnabled then begin
    vec.x:=+(FMovementCursor.X-X)*FScaleX*ogc_mm_per_pixel;
    vec.y:=-(FMovementCursor.Y-Y)*FScaleY*ogc_mm_per_pixel;
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
  if ATile.TileTop < CanvasBottom then exit;
  if ATile.TileBottom > CanvasTop then exit;
  result:=true;
end;

function TTileViewer.TileToCanvasRect(ATile:TTile):TRect;
var x1,x2,y1,y2:integer;
begin
  x1:=round((ATile.LeftTop.x-Self.LeftTop.x)/FScaleX/ogc_mm_per_pixel);
  y1:=round((ATile.LeftTop.y-Self.LeftTop.y)/FScaleY/ogc_mm_per_pixel);
  x2:=round((ATile.RightBottom.x-Self.LeftTop.x)/FScaleX/ogc_mm_per_pixel);
  y2:=round((ATile.RightBottom.y-Self.LeftTop.y)/FScaleY/ogc_mm_per_pixel);
  result:=Classes.Rect(x1,-y1,x2,-y2);
end;

function TTileViewer.CanvasCenter:TDoublePoint;
begin
  result.x:=FLeftTop.x+CanvasWidth/2;
  result.y:=FLeftTop.y-CanvasHeight/2;
end;

procedure TTileViewer.GetCanvasRange(out vLeft,vTop,vRight,vBottom:Double);
begin
  TTile.GetWorldRange(FTileList,vLeft,vTop,vRight,vBottom);
end;

function TTileViewer.CursorPoint(X,Y:Integer):TDoublePoint;
begin
  result.x:=FLeftTop.x+CanvasWidth*X/Width;
  result.y:=FLeftTop.y-CanvasHeight*Y/Height;
end;

procedure TTileViewer.PanToPoint(APoint:TDoublePoint);
var offset:TDoublePoint;
begin
  offset.x:=CanvasWidth/2;
  offset.y:=-CanvasHeight/2;
  FLeftTop:=APoint-offset;
end;

procedure TTileViewer.Zoom(AOrigin:TDoublePoint;AScale:Double);
var offset:TDoublePoint;
begin
  if (AScale<0.01) or (AScale>100) then raise ETileRangeError.Create(AScale);
  offset:=AOrigin-FLeftTop;
  offset.x:=offset.x*AScale;
  offset.y:=offset.y*AScale;
  FLeftTop.x:=AOrigin.x-offset.x;
  FLeftTop.y:=AOrigin.y-offset.y;
  FScaleX:=FScaleX*AScale;
  FScaleY:=FScaleY*AScale;
end;

procedure TTileViewer.ProportionCorrection;
var tt,ll,ww,hh:double;
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
    wmct_xy:=CursorPoint(FMouseCursor.X,FMouseCursor.Y);
    wmct_lt:=LeftTop;
    wmct_rb:=RightBottom;
    ltlg:=WebmercatorXYToLatlong(wmct_xy);

    Canvas.Pen.Color:=clNone;
    Canvas.Brush.Color:=clWhite;
    Canvas.Brush.Style:=bsSolid;
    prompt_cursor:=Format(' cx=%d  cy=%d',[FMouseCursor.X,FMouseCursor.Y]);
    prompt_view:='N/A';//Format(' cl=%f  cr=%f  ct=%f  cb=%f',[CanvasLeft,CanvasRight,CanvasTop,CanvasBottom]);
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

procedure TTileViewer.PaintTile(ATile:TTile);
var SrcRect,DstRect:TRect;
    TextHeight:Integer;
begin
  SrcRect:=Classes.Rect(0,0,ATile.FPixelWidth,ATile.FPixelHeight);
  if TileVisible(ATile) then begin
    DstRect:=TileToCanvasRect(ATile);
    if not FStopDrawing then Canvas.CopyRect(DstRect,ATile.Canvas,SrcRect);
    if FShowGrid then begin
      Canvas.Pen.Color:=clRed;
      Canvas.Pen.Style:=psSolid;
      Canvas.Pen.Width:=1;
      Canvas.Brush.Color:=clNone;
      Canvas.Brush.Style:=bsClear;
      Canvas.Rectangle(DstRect);
    end;
    TextHeight:=Canvas.TextHeight('0');
    if FShowInfo and (DstRect.Top<=TextHeight) or (DstRect.Bottom>=Height-TextHeight) then PaintInfo;
  end;
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
  if FStopDrawing and not FShowGrid then begin
    PaintStop;
    exit;
  end;
  index:=0;
  while index<FTileList.Count do begin
    tile:=TTile(FTileList.Items[index]);
    SrcRect:=Classes.Rect(0,0,tile.Width,tile.Height);
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
var l,t,r,b,w,h:double;
begin
  GetCanvasRange(l,t,r,b);
  w:=r-l;
  h:=t-b;
  view_proportion_correction(Width,Height,t,l,w,h);
  CanvasTop:=t;
  CanvasLeft:=l;
  CanvasWidth:=w;
  CanvasHeight:=h;
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
    wmxy_lt,wmxy_rb:TDoublePoint;
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
      tmpTile:=TTile.CreateFromFile(filename,AFormat);
      x1:=cell_x*tmpTile.FPixelWidth;
      y1:=cell_y*tmpTile.FPixelHeight;
      x2:=x1+tmpTile.FPixelWidth;
      y2:=y1+tmpTile.FPixelHeight;
      wmxy_lt:=WebmercatorToXY(WebMercator(x1,y1,Level));
      wmxy_rb:=WebmercatorToXY(WebMercator(x2,y2,Level));
      tmpTile.SetTileRange(wmxy_lt.x,wmxy_lt.y,wmxy_rb.x,wmxy_rb.y);
      FTileList.Add(tmpTile);
    end;
  finally
    files.Free;
  end;
end;

procedure TTileViewer.SaveToGeoTiff(FilenameWithoutExt:String);
var StopDrawingState:boolean;
    l,t,r,b:double;
    tmpTfwFile:TStringList;
    tmpTile:TTile;
    lt,rb:TDoublePoint;
begin
  if FTileList.Count<1 then exit;
  StopDrawingState:=StopDrawing;
  StopDrawing:=true;
  GetCanvasRange(l,t,r,b);
  tmpTile:=TTile.CreateFromTiles(FTileList);
  tmpTFWFile:=TStringList.Create;
  try
    tmpTile.FPicture.SaveToFile(FilenameWithoutExt+'.tif','tif');
    //GeoTiff里的exif信息要专门去写
    lt:=tmpTile.LeftTop;
    rb:=tmpTile.RightBottom;
    // A = (RB.x - LT.x) / vpwidth
    // B = 0
    // C = 0
    // D = (RB.y - LT.y) / vpheight
    // E = MinX
    // F = MinY
    tmpTFWFile.Add(FloatToStr((rb.x-lt.x)/tmpTile.Width));
    tmpTFWFile.Add('0.0');
    tmpTFWFile.Add('0.0');
    tmpTFWFile.Add(FloatToStr((rb.y-lt.y)/tmpTile.Height));
    tmpTFWFile.Add(FloatToStr(lt.x));
    tmpTFWFile.Add(FloatToStr(lt.y));
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

