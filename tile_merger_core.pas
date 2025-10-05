unit tile_merger_core;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, tile_merger_projection;

type
  TDoublePoint = TGeoPoint;
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

