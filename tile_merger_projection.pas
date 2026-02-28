unit tile_merger_projection;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils;

const
  default_ogc_mm_per_pixel = 0.00028;
  equator_circumference = 2*20037508.3427892;
  semi_equator_circumference = 20037508.3427892;
  earth_radius = 6378137.0;

type
  TGeoCoord = Double;
  TGeoPoint = record
    case Byte of
      0:(x,y:TGeoCoord);
      1:(lng,lat:TGeoCoord);
  end;
  TGeoRectangle = record
    LeftTop:TGeoPoint;
    RightBottom:TGeoPoint;
  private
    function GetWidth:TGeoCoord;
    function GetHeight:TGeoCoord;
    procedure SetWidth(Value:TGeoCoord);
    procedure SetHeight(Value:TGeoCoord);
  public
    property Width: TGeoCoord read GetWidth write SetWidth;
    property Height: TGeoCoord read GetHeight write SetHeight;
  end;

  TTileIndex = record
    case Byte of
      0:(x,y:Integer);
      1:(col,row:Integer);
  end;

  //全部使用投影坐标米单位，即默认XY
  //CRS坐标统一使用DecodeCoordinate转换为XY
  //decode: CRS  -->  XY
  //encode: XY   -->  CRS
  TProjection = class
  private
    FMetterPerPixel:TGeoCoord;
  public
    function LatlongToXY(latlong:TGeoPoint):TGeoPoint; virtual; abstract;
    function XYToLatlong(coordxy:TGeoPoint):TGeoPoint; virtual; abstract;
    function DecodeCoordinate(raw:TGeoPoint):TGeoPoint; virtual;
    function EncodeCoordinate(coordxy:TGeoPoint):TGeoPoint; virtual;
    function GetWMTSTileRect(TopLeftCorner:TGeoPoint;ScaleDenominator:TGeoCoord;
      TileWidth,TileHeight,MatrixCol,MatrixRow:Integer):TGeoRectangle; virtual;
    function GetWMTSTileIndex(TopLeftCorner:TGeoPoint;ScaleDenominator:TGeoCoord;
      TileWidth,TileHeight:Integer;Point:TGeoPoint):TTileIndex; virtual;
    function GetWMTSTileIndexNormalized(TopLeftCorner:TGeoPoint;ScaleDenominator:TGeoCoord;
      TileWidth,TileHeight:Integer;Point:TGeoPoint):TTileIndex; virtual;
    function ScaleFactorByLatlong(latlong: TGeoPoint):Double; virtual;
    function ScaleFactorByXY(coordxy: TGeoPoint):Double; virtual;
    function NormalizeXY(coordxy:TGeoPoint; out normalizedxy:TGeoPoint):boolean; virtual; //无法转为标准坐标时x和y均为nan
    class function CreateProjectionByText(urn_text:string):TProjection;
    constructor Create;
    property MetterPerPixel:TGeoCoord read FMetterPerPixel write FMetterPerPixel;
  end;

  TEquirectangular = class(TProjection)
    function LatlongToXY(latlong:TGeoPoint):TGeoPoint; override;
    function XYToLatlong(coordxy:TGeoPoint):TGeoPoint; override;
    function ScaleFactorByLatlong(latlong: TGeoPoint): Double; override;
    function ScaleFactorByXY(coordxy: TGeoPoint): Double; override;
  end;
  EPSG_4087 = TEquirectangular;

  TWebMercator_AuxiliarySphere = class(TProjection)
    function LatlongToXY(latlong:TGeoPoint):TGeoPoint; override;
    function XYToLatlong(coordxy:TGeoPoint):TGeoPoint; override;
    function ScaleFactorByLatlong(latlong: TGeoPoint):Double; override;
    function ScaleFactorByXY(coordxy: TGeoPoint):Double; override;
  end;
  EPSG_3857 = TWebMercator_AuxiliarySphere;

  TGoogle_WebMercator = class(TWebMercator_AuxiliarySphere)
    function LatlongToXY(latlong:TGeoPoint):TGeoPoint; override;
    function XYToLatlong(coordxy:TGeoPoint):TGeoPoint; override;
    function DecodeCoordinate(raw:TGeoPoint):TGeoPoint; override;
    function EncodeCoordinate(coordxy:TGeoPoint):TGeoPoint; override;
  end;
  EPSG_900913 = TGoogle_WebMercator;

  TCGCS = class(TEquirectangular)
    function LatlongToXY(latlong:TGeoPoint):TGeoPoint; override;
    function XYToLatlong(coordxy:TGeoPoint):TGeoPoint; override;
    function DecodeCoordinate(raw:TGeoPoint):TGeoPoint; override;
    function EncodeCoordinate(coordxy:TGeoPoint):TGeoPoint; override;
  end;
  EPSG_4490 = TCGCS;

  operator +(ina,inb:TGeoPoint):TGeoPoint;
  operator -(ina,inb:TGeoPoint):TGeoPoint;
  operator =(ina,inb:TGeoPoint):Boolean;
  operator mod(ina,inb:TGeoCoord):TGeoCoord;

  function GCJ02_To_WGS84(const P:TGeoPoint):TGeoPoint;

implementation
uses math;

operator +(ina,inb:TGeoPoint):TGeoPoint;
begin
  result.x:=ina.x+inb.x;
  result.y:=ina.y+inb.y;
end;

operator -(ina,inb:TGeoPoint):TGeoPoint;
begin
  result.x:=ina.x-inb.x;
  result.y:=ina.y-inb.y;
end;

operator =(ina,inb:TGeoPoint):Boolean;
begin
  result:=(ina.x=inb.x) and (ina.y=inb.y);
end;

operator mod(ina, inb: TGeoCoord): TGeoCoord;
begin
  if inb=0 then raise Exception.Create('rem by zero')
  else result:=ina-inb*trunc(ina/inb);
end;

{ TGeoRectangle }

function TGeoRectangle.GetWidth:TGeoCoord;
begin
  Result := RightBottom.x - LeftTop.x;
end;

function TGeoRectangle.GetHeight:TGeoCoord;
begin
  Result := LeftTop.y - RightBottom.y;
end;

procedure TGeoRectangle.SetWidth(Value:TGeoCoord);
begin
  RightBottom.x := LeftTop.x + Value;
end;

procedure TGeoRectangle.SetHeight(Value:TGeoCoord);
begin
  RightBottom.y := LeftTop.y - Value;
end;


{TProjection}

function TProjection.DecodeCoordinate(raw:TGeoPoint):TGeoPoint;
begin
  result := raw;
end;

function TProjection.EncodeCoordinate(coordxy:TGeoPoint):TGeoPoint;
begin
  result := coordxy;
end;

class function TProjection.CreateProjectionByText(urn_text:string):TProjection;
var urn:string;
begin
  urn:=urn_text+':';
  if pos(':3857:',urn)>0 then result:=EPSG_3857.Create
  else if pos(':900913:',urn)>0 then result:=EPSG_900913.Create
  else if pos(':4087:',urn)>0 then result:=EPSG_4087.Create
  else if pos(':4490:',urn)>0 then result:=EPSG_4490.Create
  else if pos(':4326:',urn)>0 then result:=EPSG_4490.Create
  else raise Exception.Create('Unknown CRS Text');
end;

constructor TProjection.Create;
begin
  inherited Create;
  FMetterPerPixel:=default_ogc_mm_per_pixel;
end;

function TProjection.GetWMTSTileRect(
  TopLeftCorner: TGeoPoint; ScaleDenominator: TGeoCoord;
  TileWidth, TileHeight, MatrixCol, MatrixRow: Integer): TGeoRectangle;
var
  pixelSize, tileSpanX, tileSpanY: TGeoCoord;
begin
  pixelSize := ScaleDenominator * FMetterPerPixel;
  tileSpanX := TileWidth  * pixelSize;
  tileSpanY := TileHeight * pixelSize;

  Result.LeftTop.x     := TopLeftCorner.x + MatrixCol * tileSpanX;
  Result.LeftTop.y     := TopLeftCorner.y - MatrixRow * tileSpanY;
  Result.RightBottom.x := Result.LeftTop.x + tileSpanX;
  Result.RightBottom.y := Result.LeftTop.y - tileSpanY;
end;

function TProjection.GetWMTSTileIndex(
  TopLeftCorner: TGeoPoint; ScaleDenominator: TGeoCoord;
  TileWidth, TileHeight: Integer; Point: TGeoPoint): TTileIndex;
var
  pixelSize, tileSpanX, tileSpanY: TGeoCoord;
begin
  pixelSize := ScaleDenominator * FMetterPerPixel;
  tileSpanX := TileWidth  * pixelSize;
  tileSpanY := TileHeight * pixelSize;

  Result.col := Floor((Point.x - TopLeftCorner.x) / tileSpanX);
  Result.row := Floor((TopLeftCorner.y - Point.y) / tileSpanY);
end;

function TProjection.GetWMTSTileIndexNormalized(
  TopLeftCorner: TGeoPoint; ScaleDenominator: TGeoCoord;
  TileWidth, TileHeight: Integer; Point: TGeoPoint): TTileIndex;
var
  pixelSize, tileSpanX, tileSpanY: TGeoCoord;
  normPoint: TGeoPoint;
begin
  pixelSize := ScaleDenominator * FMetterPerPixel;
  tileSpanX := TileWidth  * pixelSize;
  tileSpanY := TileHeight * pixelSize;

  if not NormalizeXY(Point, normPoint) then begin
    Result.col := -High(Integer);
    Result.row := -High(Integer);
  end else begin
    Result.col := Floor((normPoint.x - TopLeftCorner.x) / tileSpanX);
    Result.row := Floor((TopLeftCorner.y - normPoint.y) / tileSpanY);
  end;
end;

function TProjection.ScaleFactorByLatlong(latlong: TGeoPoint):Double;
begin
  result:=1.0; //默认的投影不计算长度形变
end;

function TProjection.ScaleFactorByXY(coordxy: TGeoPoint):Double;
begin
  result:=1.0; //默认的投影不计算长度形变
end;

function TProjection.NormalizeXY(coordxy:TGeoPoint; out normalizedxy:TGeoPoint):boolean;
const semi_ec = semi_equator_circumference;
      ec      = equator_circumference;
begin
  result:=true;
  normalizedxy:=coordxy;
  if normalizedxy.x >= +semi_ec then normalizedxy.x := (normalizedxy.x+semi_ec) mod ec - semi_ec;
  if normalizedxy.x <= -semi_ec then normalizedxy.x := (normalizedxy.x-semi_ec) mod ec + semi_ec;
  if normalizedxy.y >= +semi_ec then result := false;
  if normalizedxy.y <= -semi_ec then result := false;
end;


{ TEquirectangular }

function TEquirectangular.LatlongToXY(latlong: TGeoPoint): TGeoPoint;
begin
  Result.x := earth_radius*DegToRad(latlong.lng);
  Result.y := earth_radius*DegToRad(latlong.lat);
end;

function TEquirectangular.XYToLatlong(coordxy: TGeoPoint): TGeoPoint;
begin
  Result.lng := RadToDeg(coordxy.x/earth_radius);
  Result.lat := RadToDeg(coordxy.y/earth_radius);
end;

function TEquirectangular.ScaleFactorByLatlong(latlong: TGeoPoint): Double;
begin
  result := 1.0/cos(DegToRad(latlong.lat));
end;

function TEquirectangular.ScaleFactorByXY(coordxy: TGeoPoint): Double;
var latlong:TGeoPoint;
begin
  latlong:=XYToLatlong(coordxy);
  result := ScaleFactorByLatlong(latlong);
end;

{ TWebMercator_AuxiliarySphere }

function TWebMercator_AuxiliarySphere.LatlongToXY(latlong: TGeoPoint): TGeoPoint;
var rx,ry:TGeoCoord;
begin
  rx  := (latlong.lng+180)/360;
  ry  := (arcsinh(tan(latlong.lat*pi/180.0))+pi)/pi/2;
  result.x := (rx-0.5)*equator_circumference;
  result.y := (ry-0.5)*equator_circumference;
end;

function TWebMercator_AuxiliarySphere.XYToLatlong(coordxy: TGeoPoint): TGeoPoint;
var rx,ry:TGeoCoord;
begin
  rx := coordxy.x/equator_circumference+0.5;
  ry := 0.5-coordxy.y/(-equator_circumference);
  result.x := 360*rx-180;
  result.y := 180*arctan(sinh(2*pi*ry-pi))/pi;
end;

function TWebMercator_AuxiliarySphere.ScaleFactorByLatlong(latlong: TGeoPoint):Double;
var coordxy:TGeoPoint;
begin
  coordxy:=LatlongToXY(latlong);
  result:=cosh(coordxy.y / earth_radius);
end;

function TWebMercator_AuxiliarySphere.ScaleFactorByXY(coordxy: TGeoPoint):Double;
begin
  result:=cosh(coordxy.y / earth_radius);
end;


{ TGoogle_WebMercator }

function TGoogle_WebMercator.LatlongToXY(latlong: TGeoPoint): TGeoPoint;
var
  rx, ry: TGeoCoord;
begin
  rx := (latlong.lng + 180) / 360;
  ry := (arcsinh(tan(latlong.lat * Pi / 180.0)) + Pi) / Pi / 2;
  Result.x := (rx - 0.5) * equator_circumference;
  Result.y := (ry - 0.5) * equator_circumference;
end;

function TGoogle_WebMercator.XYToLatlong(coordxy: TGeoPoint): TGeoPoint;
var
  rx, ry: TGeoCoord;
begin
  rx := coordxy.x / equator_circumference + 0.5;
  ry := coordxy.y / equator_circumference + 0.5;  // <-- notice no negative
  Result.x := 360 * rx - 180;
  Result.y := 180 * arctan(sinh(2 * Pi * ry - Pi)) / Pi;
end;

function TGoogle_WebMercator.DecodeCoordinate(raw:TGeoPoint):TGeoPoint;
begin
  //x is lat and y is lng
  Result.x := raw.lat;
  Result.y := raw.lng;
end;

function TGoogle_WebMercator.EncodeCoordinate(coordxy:TGeoPoint):TGeoPoint;
begin
  //!!! tfw is not "x is lat and y is lng"
  Result.lng := coordxy.x;
  Result.lat := coordxy.y;
end;


{ TCGCS }

function TCGCS.LatlongToXY(latlong:TGeoPoint):TGeoPoint;
begin
  Result.x := DegToRad(latlong.lng) * earth_radius;
  Result.y := DegToRad(latlong.lat) * earth_radius;
end;

function TCGCS.XYToLatlong(coordxy:TGeoPoint):TGeoPoint;
begin
  Result.lng := RadToDeg(coordxy.x / earth_radius);
  Result.lat := RadToDeg(coordxy.y / earth_radius);
end;

function TCGCS.DecodeCoordinate(raw:TGeoPoint):TGeoPoint;
begin
  //x is lat and y is lng
  Result.x := DegToRad(raw.lat) * earth_radius;
  Result.y := DegToRad(raw.lng) * earth_radius;
end;

function TCGCS.EncodeCoordinate(coordxy:TGeoPoint):TGeoPoint;
begin
  //!!! tfw is not "x is lat and y is lng"
  Result.lng := RadToDeg(coordxy.x / earth_radius);
  Result.lat := RadToDeg(coordxy.y / earth_radius);
end;

function GCJ02_To_WGS84(const P:TGeoPoint):TGeoPoint;
const a  = 6378245.0;                 // Krasovsky 1940
      ee = 0.00669342162296594323;    // 偏心率平方
      pi = 3.14159265358979323846;
var   lng, lat: Double;
      dLat, dLng: Double;
      radLat, magic, sqrtMagic: Double;

  function OutOfChina(const lng,lat:Double):Boolean;inline;
  begin
    Result:=(lng<72.004) or (lng>137.8347) or (lat<0.8293) or (lat>55.8271);
  end;

  function TransformLat(x, y: Double): Double; inline;
  begin
    Result:=-100.0+2.0*x+3.0*y+0.2*y*y+0.1*x*y+0.2*Sqrt(Abs(x));
    Result:=Result
     +(20.0*Sin(6.0*x*pi)+20.0*Sin(2.0*x*pi)) * 2.0/3.0
     +(20.0*Sin(y*pi)+40.0*Sin(y/3.0*pi)) * 2.0/3.0
     +(160.0*Sin(y/12.0*pi)+320.0*Sin(y*pi/30.0)) * 2.0/3.0;
  end;

  function TransformLng(x,y:Double):Double;inline;
  begin
    Result:=300.0+x+2.0*y+0.1*x*x+0.1*x*y+0.1*Sqrt(Abs(x));
    Result:=Result
     +(20.0*Sin(6.0*x*pi)+20.0*Sin(2.0*x*pi)) * 2.0/3.0
     +(20.0*Sin(x*pi)+40.0*Sin(x/3.0*pi)) * 2.0/3.0
     +(150.0*Sin(x/12.0*pi)+300.0*Sin(x/30.0*pi)) * 2.0/3.0;
  end;

begin
  lng:=P.lng;
  lat:=P.lat;
  if OutOfChina(lng, lat) then begin result:=P;exit end;
  dLat:=TransformLat(lng-105.0, lat-35.0);
  dLng:=TransformLng(lng-105.0, lat-35.0);

  radLat:=lat/180.0*pi;
  magic:=Sin(radLat);
  magic:=1-ee*magic*magic;
  sqrtMagic:=Sqrt(magic);

  dLat:=(dLat*180.0)/((a*(1-ee))/(magic*sqrtMagic)*pi);
  dLng:=(dLng*180.0)/(a/sqrtMagic*Cos(radLat)*pi);

  // ---- GCJ02 反推 WGS84（核心一步）----
  Result.lng:=lng-dLng;
  Result.lat:=lat-dLat;
end;


end.

