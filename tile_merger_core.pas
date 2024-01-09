unit tile_merger_core;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math;

const cell_pixel_width = 256;
      webmercator_ec = 2*20037508.3427892;  //equator circumference
      webmercator_ms = 5.590822640285016E8; //WMTS L0 ScaleDenominator

type
  TDoublePoint = record
    x,y:double;
  end;
  TInt64Point = record
    x,y:int64;
  end;
  TLatLong = TDoublePoint;      //经纬度
  TWebMercator = record
    coord:TInt64Point;          //像素坐标
    level:byte;                 //层级
  end;

function WebMercator(X,Y:Int64;Level:Byte):TWebMercator;
function LatLong(Lng,Lat:Double):TLatLong;
function LatlongToWebmercator(latlong:TLatLong;level:integer):TWebMercator;
function LatlongToWebmercatorXY(latlong:TLatLong):TDoublePoint;
function WebmercatorToXY(wmct:TWebMercator):TDoublePoint;
function WebmercatorToLatlong(wmct:TWebMercator):TLatLong;
function WebmercatorXYToLatlong(wmct_xy:TDoublePoint):TLatLong;
operator +(ina,inb:TDoublePoint):TDoublePoint;
operator -(ina,inb:TDoublePoint):TDoublePoint;
operator +(ina,inb:TInt64Point):TInt64Point;
operator -(ina,inb:TInt64Point):TInt64Point;


implementation

function pow(base,ex:double):double;inline;
begin
  result:=exp(ex*ln(base));
end;
function tan(angle:double):double;inline;
begin
  result:=sin(angle)/cos(angle);
end;

function WebMercator(X,Y:Int64;Level:Byte):TWebMercator;
begin
  result.coord.x:=X;
  result.coord.y:=Y;
  result.level:=Level;
end;

function LatLong(Lng,Lat:Double):TLatLong;
begin
  result.x:=Lng;
  result.y:=Lat;
end;

(*
tileX=int( (lng+180)/360*2^z)
tileY=(1- arsinh(tan(lat*π/180))/π) *2^(z-1)
*)
function LatlongToWebmercator(latlong:TLatLong;level:integer):TWebMercator;
var lat,lng:double;
    rx,ry:int64;
begin
  lng:=latlong.x;
  lat:=latlong.y;
  rx:=round((((lng+180)/360)*pow(2,level))*cell_pixel_width);
  ry:=round(((1-arcsinh(tan(lat*pi/180))/pi) * pow(2,level-1))*cell_pixel_width);
  result.coord.x:=rx;
  result.coord.y:=ry;
  result.level:=level;
end;

function LatlongToWebmercatorXY(latlong:TLatLong):TDoublePoint;
var lat,lng,rx,ry:double;
begin
  lng:=latlong.x;
  lat:=latlong.y;
  rx:=(lng+180)/360;
  ry:=(arcsinh(tan(lat*pi/180.0))+pi)/pi/2;
  result.x:=(rx-0.5)*webmercator_ec;
  result.y:=(ry-0.5)*webmercator_ec;
end;

function WebmercatorToXY(wmct:TWebMercator):TDoublePoint;
begin
  result.x:=(wmct.coord.x/pow(2,wmct.level)/cell_pixel_width-0.5)*webmercator_ec;
  result.y:=-(wmct.coord.y/pow(2,wmct.level)/cell_pixel_width-0.5)*webmercator_ec;
end;

(*
lng=tileX/2^z * 360-180
lat=arctan(sinh(π*(1-2*tileY/2^z)))*180/π
*)
function WebmercatorToLatlong(wmct:TWebMercator):TLatLong;
var lat,lng,rx,ry:double;
begin
  rx:=wmct.coord.x/cell_pixel_width;
  ry:=wmct.coord.y/cell_pixel_width;
  lng:=rx/pow(2,wmct.level)*360-180;
  lat:=arctan(sinh(pi*(1-2*ry/pow(2,wmct.level))))*180/pi;
  result.x:=lng;
  result.y:=lat;
end;

function WebmercatorXYToLatlong(wmct_xy:TDoublePoint):TLatLong;
var rx,ry:double;
begin
  rx:=wmct_xy.x/webmercator_ec+0.5;
  ry:=0.5-wmct_xy.y/(-webmercator_ec);
  result.x:=360*rx-180;
  result.y:=180*arctan(sinh(2*pi*ry-pi))/pi;
end;

operator +(ina,inb:TDoublePoint):TDoublePoint;
begin
  result.x:=ina.x+inb.x;
  result.y:=ina.y+inb.y;
end;

operator -(ina,inb:TDoublePoint):TDoublePoint;
begin
  result.x:=ina.x-inb.x;
  result.y:=ina.y-inb.y;
end;

operator +(ina,inb:TInt64Point):TInt64Point;
begin
  result.x:=ina.x+inb.x;
  result.y:=ina.y+inb.y;
end;

operator -(ina,inb:TInt64Point):TInt64Point;
begin
  result.x:=ina.x-inb.x;
  result.y:=ina.y-inb.y;
end;

{
# tfw file
# x' = Ax + Cy + E
# y' = Bx + Dy + F

web mercator equator circumference = 2*20037508.3427892
}


end.

