unit tile_merger_core;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, tile_merger_projection;
{
type
  TDoublePoint = TGeoPoint;

operator +(ina,inb:TDoublePoint):TDoublePoint;
operator -(ina,inb:TDoublePoint):TDoublePoint;
}

implementation
{
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
}
{
# tfw file
# x' = Ax + Cy + E
# y' = Bx + Dy + F

web mercator equator circumference = 2*20037508.3427892
}


end.

