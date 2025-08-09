unit tile_merger_view;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef UNIX}
  cthreads,
  {$endif}
  Classes, SysUtils, Controls, Graphics, FileUtil, FPimage, FPWriteTIFF,
  fphttpclient, openssl, URIParser,
  tile_merger_core, tile_merger_wmts_client;

const ogc_ppi = 90.71446714322;
      ogc_mm_per_pixel = 0.00028;

type

  ETileRangeError = class(Exception)
  public
    constructor Create(value:double);
  end;

  TTileFormat = (tfPNG, tfJPG, tfBMP);

  TTile = class
    FPicture:TPicture;
    FEnabled:Boolean;
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
    constructor CreateFromLayer(ATileViewer:TObject;ALayer:TWMTS_Layer;ATileMatrix:TWMTS_TileMatrix;ARow,ACol:Integer);
    constructor CreateFromTiles(tiles:TList);
    destructor Destroy; override;
  public
    procedure Update(tile:TTile);
    class procedure GetWorldRange(tiles:TList;out vLeft,vTop,vRight,vBottom:Double);
  private
    FOnReady:TNotifyEvent;
    FTileViewer:TObject;
  public
    property OnReady:TNotifyEvent read FOnReady write FOnReady;
  end;

  TOnlineTile = class(TTile)
  private
    FCachePath:String; //从TileViewPool中抄
  protected
    function GetCacheFileName:string;
  public
    Layer:TWMTS_Layer;
    TileMatrix:TWMTS_TileMatrix;
    Row,Col:Integer;
  public
    constructor CreateFromLayer(ATileViewer:TObject;ALayer:TWMTS_Layer;ATileMatrix:TWMTS_TileMatrix;ARow,ACol:Integer); reintroduce;
    property CacheFileName:string read GetCacheFileName;
  end;

  TTileViewerPool = class
  private
    FTileList:TList;   // 实际上是OnlineTile列表
    FCachePath:String; // 网络下载的瓦片暂存位置
    PTileViewer:TObject;
  private
    function FetchTile(aLayer:TWMTS_Layer;aTileMatrix:TWMTS_TileMatrix;aRow,aCol:integer):TOnlineTile;
  public
    function GetTile(aLayer:TWMTS_Layer;aTileMatrix:TWMTS_TileMatrix;aRow,aCol:integer):TOnlineTile;
    procedure Clear;
    property CachePath:String read FCachePath write FCachePath;
  public
    constructor Create(AOwner:TObject);
    destructor Destroy; override;
  end;

  TFetchTileResult = (ftrError, ftrCache, ftrWmtsFail, ftrSaved, ftrConflict);
  //获取瓦片的结果：Error在任务开始前出错，Cache从缓存中加载，WmtsFail下载失败，
  //                Saved下载成功并缓存，Conflict下载成功但缓存失败

  TFetchTileThread = class(TThread)
  private
    PTile:TOnlineTile;
    FUrl:String;
    FForceFetch:Boolean; //无论是否有缓存均下载瓦片并缓存（需要考虑线程竞争）
    FFetchResult:TFetchTileResult;
    FStartTime:TDateTime;
  public
    procedure CheckURI (Sender: TObject; const ASrc: String; var ADest: String);
    procedure FetchInit;
    procedure FetchDone;
    procedure Execute; override;
  public
    constructor Create(aTile:TOnlineTile;aUrl:String;aForceFetch:Boolean);
  end;

  TTileViewer = class(TCustomControl)
  private
    //只通过以下两个参数和width、height定位画幅，其他均通过Get/Set获得
    FLeftTop:TDoublePoint;
    FScaleX:Double;
    FScaleY:Double;
  private
    FTilePool:TTileViewerPool;   //用于预览显示
    FExportPool:TTileViewerPool; //用于图片导出
    FCurrentLayer:TWMTS_Layer;
    FCurrentTileMatrixSet:TWMTS_TileMatrixSet;
    PBestTileMatrix:TWMTS_TileMatrix; //当前显示的最佳层级，在缩放时重新计算。如果没有选择TMS则始终为nil
    FMouseCursor:TPoint;
    FMovementCursor:TPoint;
    FMovementEnabled:Boolean;
    FMovementCenter:TDoublePoint;
  private
    FShowGrid:Boolean;
    FShowInfo:Boolean;
    FShowScale:Boolean;
    FStopDrawing:Boolean;
    FAutoFetchTile:Boolean;
    FForceFetchTile:Boolean;
  protected
    procedure SetAutoFetchTile(value:boolean);
    procedure SetForceFetchTile(value:boolean);
    function GetCurrentService:TWMTS_Service;
    procedure SetCurrentLayer(value:TWMTS_Layer);
    procedure SetCurrentTileMatrixSet(value:TWMTS_TileMatrixSet);
    function GetCachePath:string;
  public
    property CurrentService:TWMTS_Service read GetCurrentService;
    property CurrentLayer:TWMTS_Layer read FCurrentLayer write SetCurrentLayer;
    property CurrentTileMatrixSet:TWMTS_TileMatrixSet read FCurrentTileMatrixSet write SetCurrentTileMatrixSet;
    property TilePool:TTileViewerPool read FTilePool;
    property ShowGrid:Boolean read FShowGrid write FShowGrid;
    property ShowInfo:Boolean read FShowInfo write FShowInfo;
    property ShowScale:Boolean read FShowScale write FShowScale;
    property StopDrawing:Boolean read FStopDrawing write FStopDrawing;
    property AutoFetchTile:Boolean read FAutoFetchTile write SetAutoFetchTile;
    property ForceFetchTile:Boolean read FForceFetchTile write SetForceFetchTile;
    property CachePath:string read GetCachePath;
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
    function LocatePoint(CoordX,CoordY:Double):TPoint;
    procedure PanToPoint(APoint:TDoublePoint);
    procedure Zoom(AOrigin:TDoublePoint;AScale:Double);
    procedure ZoomTo(AScale:Double);
    procedure ProportionCorrection;
    procedure PaintScale;
    procedure PaintInfo;
    procedure PaintStop;
    procedure PaintTile(ATile:TTile);
    procedure Paint; override;
  private
    FOnLayerChange:TNotifyEvent;
    FOnTileMatrixSetChange:TNotifyEvent;
  public
    property OnLayerChange:TNotifyEvent read FOnLayerChange write FOnLayerChange;
    property OnTileMatrixSetChange:TNotifyEvent read FOnTileMatrixSetChange write FOnTileMatrixSetChange;
  public
    procedure Clear; virtual;
    procedure Refresh;
    procedure ZoomToWorld; virtual;
    procedure SaveToGeoTiff(FilenameWithoutExt:String);
    procedure ShowTiles(AScale:Double=0); //从服务器加载瓦片数据，根据给定比例尺确定层级，比例尺小于等于0时根据地图比例尺自动选择最合适的层级
  public
    procedure TileThreadTerminate(Sender:TObject);
  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
  end;

implementation
uses debugline;

function fetch_tile_result_to_str(fetchresult:TFetchTileResult):string;
begin
  case fetchresult of
    ftrError:     result:='Error';
    ftrCache:     result:='Cache';
    ftrWmtsFail:  result:='WmtsFail';
    ftrSaved:     result:='Saved';
    ftrConflict:  result:='Conflict';
  end;
end;

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
  FOnReady:=nil;
  FIndirect:=false;
  FEnabled:=true;
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

constructor TTile.CreateFromLayer(ATileViewer:TObject;ALayer:TWMTS_Layer;ATileMatrix:TWMTS_TileMatrix;ARow,ACol:Integer);
var tmpTileViewer:TTileViewer;
begin
  inherited Create;
  tmpTileViewer:=ATileViewer as TTileViewer;
  FTileViewer:=tmpTileViewer;
  FOnReady:=@tmpTileViewer.TileThreadTerminate;
  FIndirect:=false;
  FEnabled:=false;//加载线程结束后修改为true
  FPixelWidth:=ATileMatrix.Width;
  FPixelHeight:=ATileMatrix.Height;
  FLeftTop.x:=ATileMatrix.LeftTop.x+ACol*(ATileMatrix.Scale*ogc_mm_per_pixel*ATileMatrix.Width);
  FLeftTop.y:=ATileMatrix.LeftTop.y-ARow*(ATileMatrix.Scale*ogc_mm_per_pixel*ATileMatrix.Height);
  FScaleX:=ATileMatrix.Scale;
  FScaleY:=ATileMatrix.Scale;
  FPicture:=TPicture.Create;
  case lowercase(ALayer.Format) of
    'image/png':begin
      FPicture.PNG.SetSize(FPixelWidth,FPixelHeight);
      FImageFormat:=tfPNG;
    end;
    'image/jpeg':begin
      FPicture.Jpeg.SetSize(FPixelWidth,FPixelHeight);
      FImageFormat:=tfJPG;
    end;
    else raise Exception.Create('不支持的图像格式：'+ALayer.Format);
  end;
end;

constructor TTile.CreateFromTiles(tiles:TList);
var len,idx:integer;
    l,t,r,b:double;
    tmpTile:TTile;
begin
  inherited Create;
  FOnReady:=nil;
  FIndirect:=false;
  FEnabled:=true;
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

{ TFetchTileThread }

//Fixed by @wittbo on Lazarus Forum,
//Source: https://forum.lazarus.freepascal.org/index.php/topic,43553.msg335901.html#msg335901
procedure TFetchTileThread.CheckURI (Sender: TObject; const ASrc: String; var ADest: String);
var newURI     : TURI;
    OriginalURI: TURI;
begin
   newURI := ParseURI (ADest, False);
   if (newURI.Host = '') then begin                         // NewURI does not contain protocol or host
      OriginalURI          := ParseURI (ASrc, False);       // use the original URI...
      OriginalURI.Path     := newURI.Path;                  // ... with the new subpage (path)...
      OriginalURI.Document := newURI.Document;              // ... and the new document info...
      ADest                := EncodeURI (OriginalURI)       // ... and return the complete redirected URI
   end
end;

procedure TFetchTileThread.FetchInit;
begin
  {
  with PTile do
    Form_Debug.AddMessage('['+DateTimeToStr(Now)+']  '+Format('L:%s X:%u Y:%u', [TileMatrix.Identifier, Col, Row]));
  Form_Debug.AddMessage(FUrl);
  }
  FStartTime:=Now;
end;

procedure TFetchTileThread.FetchDone;
begin
  with PTile do
    Form_Debug.AddMessage(
      '['+DateTimeToStr(Now)+']  '+fetch_tile_result_to_str(FFetchResult)+'  '
      +Format('T:%5.0fms L:%s X:%u Y:%u', [1000*86400*(Now-FStartTime),TileMatrix.Identifier, Col, Row])
    );
  PTile.FEnabled:=true;
  if PTile.OnReady<>nil then PTile.OnReady(PTile);
end;

procedure TFetchTileThread.Execute;
var content:TMemoryStream;
begin
  FFetchResult:=ftrError;
  Synchronize(@FetchInit);
  //如果缓存文件能找到就不访问瓦片服务器
  if not FForceFetch then
    if FileExists(PTile.CacheFileName) then
      begin
        PTile.FPicture.LoadFromFile(PTile.CacheFileName);
        FFetchResult:=ftrCache;
        Synchronize(@FetchDone);
        exit;
      end;
  //找不到再开始走访问流程
  content:=TMemoryStream.Create;
  try try
    with TFPHTTPClient.Create(nil) do try try
      AllowRedirect:=true;
      OnRedirect:=@CheckURI;
      AddHeader('User-Agent','ArcGIS Client Using WinInet');
      FFetchResult:=ftrWmtsFail;
      Get(FUrl,content);
    except {silent 404} end;
    finally
      Free;
    end;
    if content.Size=0 then exit;
    FFetchResult:=ftrConflict;
    content.Position:=0;
    PTile.FPicture.LoadFromStream(content);
    ForceDirectories(ExtractFileDir(PTile.CacheFileName));
    PTile.FPicture.SaveToFile(PTile.CacheFileName);
    FFetchResult:=ftrSaved;
    Synchronize(@FetchDone);
  except {silent unable-to-open} end;
  finally
    content.Free;
  end;
end;

constructor TFetchTileThread.Create(aTile:TOnlineTile;aUrl:String;aForceFetch:Boolean);
begin
  inherited Create(true);
  FUrl:=aUrl;
  PTile:=aTile;
  FForceFetch:=aForceFetch;
  FreeOnTerminate:=true;
end;


{ TOnlineTile }

function TOnlineTile.GetCacheFileName:string;
begin
  result:=FCachePath;
  result:=result + DirectorySeparator + TileMatrix.Identifier;
  result:=result + DirectorySeparator + IntToStr(Col);
  result:=result + DirectorySeparator + IntToStr(Row);
  result:=result + DirectorySeparator + TWMTS_Service(Layer.Service).Title;
  result:=result + '#' + Layer.Title;
  result:=result + '.' + Layer.TileExtent;
end;

constructor TOnlineTile.CreateFromLayer(ATileViewer:TObject;ALayer:TWMTS_Layer;ATileMatrix:TWMTS_TileMatrix;ARow,ACol:Integer);
begin
  inherited CreateFromLayer(ATileViewer,ALayer,ATileMatrix,ARow,ACol);
  Layer:=ALayer;
  TileMatrix:=ATileMatrix;
  Col:=ACol;
  Row:=ARow;
  FCachePath:='TilesCache';
end;

{ TTileViewerPool }

function TTileViewerPool.FetchTile(aLayer:TWMTS_Layer;aTileMatrix:TWMTS_TileMatrix;aRow,aCol:integer):TOnlineTile;
var thread:TFetchTileThread;
begin
  result:=TOnlineTile.CreateFromLayer(PTileViewer,aLayer,aTileMatrix,aRow,aCol);
  result.FCachePath:=Self.FCachePath;
  thread:=TFetchTileThread.Create(result,aLayer.URL(aTileMatrix,aRow,aCol),TTileViewer(PTileViewer).FForceFetchTile);
  thread.Start;
end;

function TTileViewerPool.GetTile(aLayer:TWMTS_Layer;aTileMatrix:TWMTS_TileMatrix;aRow,aCol:integer):TOnlineTile;
label NEXT;
var idx,len:integer;
    tmpTile:TOnlineTile;
begin
  //想办法改一种访问更快的散列表
  aRow:=(aRow+aTileMatrix.RowCount) mod aTileMatrix.RowCount;
  aCol:=(aCol+aTileMatrix.ColumnCount) mod aTileMatrix.ColumnCount;
  len:=FTileList.Count;
  idx:=0;
  while idx<len do begin
    tmpTile:=TOnlineTile(FTileList[idx]);
    if aLayer<>tmpTile.Layer then goto NEXT;
    if aTileMatrix<>tmpTile.TileMatrix then goto NEXT;
    if aRow<>tmpTile.Row then goto NEXT;
    if aCol<>tmpTile.Col then goto NEXT;
    break;
    NEXT:
    inc(idx);
  end;
  if idx=len then begin
    result:=FetchTile(aLayer,aTileMatrix,aRow,aCol);
    FTileList.Add(result);
  end else begin
    if TTileViewer(PTileViewer).FForceFetchTile then begin
      TOnlineTile(FTileList[idx]).Free;
      result:=FetchTile(aLayer,aTileMatrix,aRow,aCol);
      FTileList[idx]:=result;
    end else
      result:=TOnlineTile(FTileList[idx]);
  end;
end;

procedure TTileViewerPool.Clear;
begin
  while FTileList.Count>0 do begin
    TOnlineTile(FTileList[0]).Free;
    FTileList.Delete(0);
  end;
end;

constructor TTileViewerPool.Create(AOwner:TObject);
begin
  inherited Create;
  FTileList:=TList.Create;
  FCachePath:='TilesCache';
  PTileViewer:=AOwner;
end;

destructor TTileViewerPool.Destroy;
begin
  Clear;
  FTileList.Free;
  inherited Destroy;
end;



{ TTileViewer }

procedure TTileViewer.SetAutoFetchTile(value:boolean);
begin
  FAutoFetchTile:=value;
  if value then ShowTiles;
end;

procedure TTileViewer.SetForceFetchTile(value:boolean);
begin
  FForceFetchTile:=value;
  if value then ShowTiles;
end;

function TTileViewer.GetCurrentService:TWMTS_Service;
begin
  result:=nil;
  if FCurrentLayer=nil then exit;
  result:=TWMTS_Layer(FCurrentLayer).Service as TWMTS_Service;
end;

procedure TTileViewer.SetCurrentLayer(value:TWMTS_Layer);
begin
  FCurrentLayer:=value;
  if FOnLayerChange<>nil then FOnLayerChange(Self);
  if FAutoFetchTile then begin
    FTilePool.Clear;
    ShowTiles;
  end;
end;

procedure TTileViewer.SetCurrentTileMatrixSet(value:TWMTS_TileMatrixSet);
begin
  FCurrentTileMatrixSet:=value;
  PBestTileMatrix:=value.BestFitTileMatrix(FScaleX);
  if FOnTileMatrixSetChange<>nil then FOnTileMatrixSetChange(Self);
end;

function TTileViewer.GetCachePath:string;
begin
  result:=FTilePool.CachePath;
end;

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
  ProportionCorrection;
  if FAutoFetchTile then ShowTiles;
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
  TTile.GetWorldRange(FTilePool.FTileList,vLeft,vTop,vRight,vBottom);
end;

function TTileViewer.CursorPoint(X,Y:Integer):TDoublePoint;
begin
  result.x:=FLeftTop.x+CanvasWidth*X/Width;
  result.y:=FLeftTop.y-CanvasHeight*Y/Height;
end;

function TTileViewer.LocatePoint(CoordX,CoordY:Double):TPoint;
begin
  result.X:=round(Width*(CoordX-FLeftTop.x)/CanvasWidth);
  result.Y:=round(Height*(FLeftTop.y-CoordY)/CanvasHeight);
end;

procedure TTileViewer.PanToPoint(APoint:TDoublePoint);
var offset:TDoublePoint;
begin
  offset.x:=CanvasWidth/2;
  offset.y:=-CanvasHeight/2;
  FLeftTop:=APoint-offset;
  if FAutoFetchTile then ShowTiles;
end;

procedure TTileViewer.Zoom(AOrigin:TDoublePoint;AScale:Double);
var offset:TDoublePoint;
begin
  if (AScale<1e-9) or (AScale>1e9) then raise ETileRangeError.Create(AScale);
  offset:=AOrigin-FLeftTop;
  offset.x:=offset.x*AScale;
  offset.y:=offset.y*AScale;
  FLeftTop.x:=AOrigin.x-offset.x;
  FLeftTop.y:=AOrigin.y-offset.y;
  FScaleX:=FScaleX*AScale;
  FScaleY:=FScaleY*AScale;
  if FCurrentTileMatrixSet<>nil then PBestTileMatrix:=FCurrentTileMatrixSet.BestFitTileMatrix(FScaleX);
  if FAutoFetchTile then ShowTiles;
end;

procedure TTileViewer.ZoomTo(AScale:Double);
begin
  Zoom(CanvasCenter,AScale/FScaleX);
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

function BestScaleDistance(max_scale:double):double;
var level:integer;
    base,base2,base5:double;
begin
  // result <= max_scale and result =  M*10^N ( M in {1,2,5}, N in interger )
  level := trunc(ln(max_scale)/ln(10));
  base  := exp(level*ln(10));
  base2 := base*2;
  base5 := base*5;
  if max_scale>=base5 then result:=base5
  else if max_scale>=base2 then result:=base2
  else result:=base;
end;

function BestMeterUnit(len:double):string;
begin
  if len >= 1000 then result:=Format('%.0fkm',[len/1000])
  else result:=Format('%.0fm',[len]);
end;

procedure TTileViewer.PaintScale;
const _scale_offset_ = 8;
var sp1,sp2,sp_tmp:TPoint;
    wm1,wm2,wm_tmp:TDoublePoint;
    scale_factor:double;
    cw,ch,rw,rh:double;
    tw,th:integer;
    scale_text:string;
begin
  if not FShowScale then exit;
  if Height < _scale_offset_*5 then exit;
  if Width  < _scale_offset_*5 then exit;
  sp1.x := _scale_offset_;
  sp1.y := _scale_offset_;
  sp2.x := Width  - _scale_offset_;
  sp2.y := Height - _scale_offset_;
  if FShowInfo then begin
    sp1.y := sp1.y + Canvas.TextHeight('H');
    sp2.y := sp2.y - Canvas.TextHeight('H');
  end;
  wm1   := CursorPoint(sp1.x, sp1.y);
  wm2   := CursorPoint(sp2.x, sp2.y);
  cw    := wm2.x - wm1.x;
  ch    := wm1.y - wm2.y;
  if (wm1.y>=webmercator_min_y) and (wm1.y<=webmercator_max_y) then begin
    //drawing left-top scale
    scale_factor:=WebmercatorToScaleFactor(wm1);
    rw:=BestScaleDistance(cw/scale_factor/6);
    rh:=BestScaleDistance(ch/scale_factor/5);
    wm_tmp:=wm1;
    wm_tmp.x:=wm_tmp.x+rw*scale_factor;
    Canvas.Pen.Color := clWhite;
    Canvas.Pen.Width := 3;
    Canvas.Pen.Style := psSolid;
    sp_tmp:=LocatePoint(wm_tmp.x, wm_tmp.y);
    Canvas.Line(sp1, sp_tmp);
    Canvas.Line(sp1, sp1+Classes.Point(0,+3));
    Canvas.Line(sp_tmp, sp_tmp+Classes.Point(0,+3));
    Canvas.Pen.Color := clBlack;
    Canvas.Pen.Width := 1;
    Canvas.Line(sp1, LocatePoint(wm_tmp.x, wm_tmp.y));
    Canvas.Line(sp1, sp1+Classes.Point(0,+3));
    Canvas.Line(sp_tmp, sp_tmp+Classes.Point(0,+3));

    Canvas.Brush.Style := bsClear;
    Canvas.Font.Color  := clWhite;
    //outline, dirty but work
    scale_text:=BestMeterUnit(rw);
    th:=Canvas.TextHeight('10m') div 2;
    Canvas.TextOut(sp1.X - 1, sp1.y + th - 1, scale_text);
    Canvas.TextOut(sp1.X - 1, sp1.y + th + 1, scale_text);
    Canvas.TextOut(sp1.X + 1, sp1.y + th - 1, scale_text);
    Canvas.TextOut(sp1.X + 1, sp1.y + th + 1, scale_text);
    Canvas.Font.Color  := clBlack;
    Canvas.TextOut(sp1.X,     sp1.y + th,     scale_text);
  end;
  if (wm2.y>=webmercator_min_y) and (wm2.y<=webmercator_max_y) then begin
    //drawing right-bottom scale
    scale_factor:=WebmercatorToScaleFactor(wm2);
    rw:=BestScaleDistance(cw/scale_factor/6);
    rh:=BestScaleDistance(ch/scale_factor/5);
    wm_tmp:=wm2;
    wm_tmp.x:=wm_tmp.x-rw*scale_factor;
    Canvas.Pen.Color := clWhite;
    Canvas.Pen.Width := 3;
    Canvas.Pen.Style := psSolid;
    sp_tmp:=LocatePoint(wm_tmp.x, wm_tmp.y);
    Canvas.Line(sp2, sp_tmp);
    Canvas.Line(sp2, sp2+Classes.Point(0,-3));
    Canvas.Line(sp_tmp, sp_tmp+Classes.Point(0,-3));
    Canvas.Pen.Color := clBlack;
    Canvas.Pen.Width := 1;
    Canvas.Line(sp2, LocatePoint(wm_tmp.x, wm_tmp.y));
    Canvas.Line(sp2, sp2+Classes.Point(0,-3));
    Canvas.Line(sp_tmp, sp_tmp+Classes.Point(0,-3));

    Canvas.Brush.Style := bsClear;
    Canvas.Font.Color  := clWhite;
    //outline, dirty but work
    scale_text:=BestMeterUnit(rw);
    th:=3*Canvas.TextHeight('10m') div 2;
    tw:=Canvas.TextWidth(scale_text);
    Canvas.TextOut(sp2.X - tw - 1, sp2.y - th - 1, scale_text);
    Canvas.TextOut(sp2.X - tw - 1, sp2.y - th + 1, scale_text);
    Canvas.TextOut(sp2.X - tw + 1, sp2.y - th - 1, scale_text);
    Canvas.TextOut(sp2.X - tw + 1, sp2.y - th + 1, scale_text);
    Canvas.Font.Color  := clBlack;
    Canvas.TextOut(sp2.X - tw,     sp2.y - th,     scale_text);
  end;
end;

procedure TTileViewer.PaintInfo;
var wmct:TWebMercator;
    wmct_xy,wmct_lt,wmct_rb:TDoublePoint;
    ltlg:TLatLong;
    prompt_cursor,prompt_view,wmct_cursor,wmct_view,BestTM_Name:string;
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
    //prompt_cursor:=Format('  lyr=%s  tm=%s',[CurrentLayer.Title, CurrentTileMatrixSet.Identifier]);
    prompt_view:=Format(' scale_x=%f  scale_y=%f',[FScaleX,FScaleY]);
    if PBestTileMatrix<>nil then prompt_view:=Format(' level=%s %s',[PBestTileMatrix.Identifier,prompt_view]);
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
    bestTM:TWMTS_TileMatrix;
begin
  Canvas.Brush.Color:=clWhite;
  Canvas.Brush.Style:=bsSolid;
  Canvas.Clear;
  ProportionCorrection;
  if FCurrentTileMatrixSet<>nil then
    bestTM:=FCurrentTileMatrixSet.BestFitTileMatrix(FScaleX)
  else
    bestTM:=nil;
  if FStopDrawing and not FShowGrid then begin
    PaintStop;
    exit;
  end;
  index:=0;
  while index<FTilePool.FTileList.Count do begin
    tile:=TTile(FTilePool.FTileList.Items[index]);
    //自动预览时限制非最佳层级瓦片的显示，这个逻辑还是不太清晰
    if FAutoFetchTile then
      if bestTM<>nil then
        if tile.FScaleX<>bestTM.Scale then
          begin
            inc(index);
            continue;
          end;
    SrcRect:=Classes.Rect(0,0,tile.Width,tile.Height);
    if TileVisible(tile) then begin
      DstRect:=TileToCanvasRect(tile);
      if not FStopDrawing and tile.FEnabled then Canvas.CopyRect(DstRect,tile.Canvas,SrcRect);
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
  PaintScale;
end;

procedure TTileViewer.Clear;
begin
  while FTilePool.FTileList.Count>0 do begin
    TOnlineTile(FTilePool.FTileList.Items[0]).Free;
    FTilePool.FTileList.Delete(0);
  end;
end;

procedure TTileViewer.Refresh;
begin
  Paint;
end;

procedure TTileViewer.ZoomToWorld;
var origin:TDoublePoint;
begin
  origin.x:=0;
  origin.y:=0;
  PanToPoint(origin);
  ZoomTo(2.6e8);
end;

procedure TTileViewer.SaveToGeoTiff(FilenameWithoutExt:String);
var StopDrawingState:boolean;
    l,t,r,b:double;
    tmpTfwFile:TStringList;
    //tmpTifFile:TFPCustomImage;
    //tmpTiffWriter:TFPWriterTiff;
    tmpTile:TTile;
    lt,rb:TDoublePoint;
begin
  if FTilePool.FTileList.Count<1 then exit;
  StopDrawingState:=StopDrawing;
  StopDrawing:=true;
  GetCanvasRange(l,t,r,b);
  tmpTile:=TTile.CreateFromTiles(FTilePool.FTileList);
  tmpTFWFile:=TStringList.Create;
  //tmpTifFile:=TFPCustomImage.create(tmpTile.Width,tmpTile.Height);
  //tmpTiffWriter:=TFPWriterTiff.Create;
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
    //tmpTifFile.Free;
    //tmpTiffWriter.Free;
    tmpTile.Free;
  end;
end;

procedure TTileViewer.ShowTiles(AScale:Double=0);
var c1,c2,r1,r2,col,row:integer;
    bestTM:TWMTS_TileMatrix;
begin
  //TilePool.Clear; //不清空了，所有瓦片都留在池内
  if AScale<=0 then
      bestTM:=CurrentTileMatrixSet.BestFitTileMatrix(FScaleX)
  else
      bestTM:=CurrentTileMatrixSet.BestFitTileMatrix(AScale);
  c1:=trunc((LeftTop.x-bestTM.LeftTop.x) / bestTM.Scale/ogc_mm_per_pixel/bestTM.Width);
  c2:=trunc((RightBottom.x-bestTM.LeftTop.x) / bestTM.Scale/ogc_mm_per_pixel/bestTM.Width);
  r1:=trunc((bestTM.LeftTop.y-LeftTop.y) / bestTM.Scale/ogc_mm_per_pixel/bestTM.Height);
  r2:=trunc((bestTM.LeftTop.y-RightBottom.y) / bestTM.Scale/ogc_mm_per_pixel/bestTM.Height);
  for col:=c1 to c2 do begin
    for row:= r1 to r2 do begin
      TilePool.GetTile(CurrentLayer,bestTM,row,col);
    end;
  end;
end;

procedure TTileViewer.TileThreadTerminate(Sender:TObject);
var tmpTile:TOnlineTile;
begin
  tmpTile:=Sender as TOnlineTile;
  if tmpTile.TileMatrix<>PBestTileMatrix then exit;
  PaintTile(tmpTile);
  PaintScale;
end;

constructor TTileViewer.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FTilePool:=TTileViewerPool.Create(Self);
  FExportPool:=TTileViewerPool.Create(Self);
  FCurrentTileMatrixSet:=nil;
  FCurrentLayer:=nil;
  FCurrentTileMatrixSet:=nil;
  FMovementEnabled:=false;
  FShowGrid:=false;
  FShowInfo:=false;
  FShowScale:=true;//暂时强制显示
  FStopDrawing:=false;
  AutoFetchTile:=false;
  //很随意的一个起始范围
  FLeftTop.x:=13244000;
  FLeftTop.y:=3034000;
  FScaleX:=320000;
  FScaleY:=320000;
  OnMouseWheel:=@MouseWheel;
  OnResize:=@ViewResize;
end;

destructor TTileViewer.Destroy;
begin
  Clear;
  FTilePool.Free;
  FExportPool.Free;
  inherited Destroy;
end;



end.

