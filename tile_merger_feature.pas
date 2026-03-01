unit tile_merger_feature;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, tile_merger_projection;

type

  EAGeoError = class(Exception)
  end;

  EAGeoCoordinateDepthError = class(EAGeoError)
    constructor Create;
  end;
  EAGeoFeaturesCountError = class(EAGeoError)
    constructor Create;
  end;

  TAGeoFeature = class
  private
    FCoordinateSize:Integer;
    FCoordinateDepth:Integer;
    FCoordinates:PDouble;
    FLabelSize:Integer;
    FLabelText:PChar;
  protected
    function GetLabelText:string;
    procedure SetLabelText(value:string);
  public
    function WKT:string;virtual;abstract;
    constructor Create;
    destructor Destroy; override;
    property LabelText:String read GetLabelText write SetLabelText;
  end;

  TAGeoPointGeometry = class(TAGeoFeature)
  protected
    function GetX:Double;
    function GetY:Double;
    function GetZ:Double;
    function GetM:Double;
    procedure SetX(value:Double);
    procedure SetY(value:Double);
    procedure SetZ(value:Double);
    procedure SetM(value:Double);
  public
    function WKT:string;virtual;
    constructor Create(ACoordinateDepth:Integer);
    destructor Destroy; override;
    property X:Double read GetX write SetX;
    property Y:Double read GetY write SetY;
    property Z:Double read GetZ write SetZ;
    property M:Double read GetM write SetM;

  end;

  TAGeoFeatures = class
    FFeatureList:TList;
  public
    function AddFeature(AFeature:TAGeoFeature):Integer;
    function RemoveFeature(Index:Integer):boolean;
    procedure Clear;
    procedure SaveToCSV(filename:string);
    constructor Create;
    destructor Destroy; override;
  protected
    function GetItem(Index:Integer):TAGeoFeature;
    function GetItemCount:Integer;
  public
    property Items[index:Integer]:TAGeoFeature read GetItem; default;
    property Count:Integer read GetItemCount;
  end;


  function CreatePoint2D(xPos,yPos:TGeoCoord):TAGeoPointGeometry;

implementation


{ EAGeoCoordinateDepthError }
constructor EAGeoCoordinateDepthError.Create;
begin
  inherited Create('坐标深度错误');
end;

{ EAGeoFeaturesCountError }
constructor EAGeoFeaturesCountError.Create;
begin
  inherited Create('要素数量错误');
end;


{ TAGeoFeature }

function TAGeoFeature.GetLabelText:string;
begin
  if FLabelSize>0 then result:=FLabelText;
end;

procedure TAGeoFeature.SetLabelText(value:string);
begin
  if FLabelSize>0 then begin
    FreeMem(FLabelText, FLabelSize+1);
    FLabelText:=nil;
    FLabelSize:=0;
  end;
  if length(value)=0 then exit;
  FLabelSize:=length(value);
  FLabelText:=GetMem(FLabelSize+1);
  move(value[1], FLabelText^, FLabelSize);
  (FLabelText+FLabelSize)^:=chr(0);
end;

constructor TAGeoFeature.Create;
begin
  inherited Create;
  //几何定义部分完全由子类实现
  FLabelSize:=0;
  FLabelText:=nil;
end;

destructor TAGeoFeature.Destroy;
begin
  if FLabelSize>0 then FreeMem(FLabelText, FLabelSize+1);
  inherited Destroy;
end;


{ TAGeoPointGeometry }

function TAGeoPointGeometry.GetX:Double;
begin
  result:=FCoordinates^;
end;

function TAGeoPointGeometry.GetY:Double;
begin
  result:=(FCoordinates+1)^;
end;

function TAGeoPointGeometry.GetZ:Double;
begin
  if FCoordinateDepth<3 then raise EAGeoCoordinateDepthError.Create;
  result:=(FCoordinates+2)^;
end;

function TAGeoPointGeometry.GetM:Double;
begin
  if FCoordinateDepth<4 then raise EAGeoCoordinateDepthError.Create;
  result:=(FCoordinates+3)^;
end;

procedure TAGeoPointGeometry.SetX(value:Double);
begin
  FCoordinates^:=value;
end;

procedure TAGeoPointGeometry.SetY(value:Double);
begin
  (FCoordinates+1)^:=value;
end;

procedure TAGeoPointGeometry.SetZ(value:Double);
begin
  if FCoordinateDepth<3 then raise EAGeoCoordinateDepthError.Create;
  (FCoordinates+2)^:=value;
end;

procedure TAGeoPointGeometry.SetM(value:Double);
begin
  if FCoordinateDepth<4 then raise EAGeoCoordinateDepthError.Create;
  (FCoordinates+3)^:=value;
end;

function TAGeoPointGeometry.WKT:string;
begin
  case FCoordinateDepth of
    2:result:=Format('Point (%f %f)',[X,Y]);
    3:result:=Format('Point Z (%f %f %f)',[X,Y,Z]);
    4:result:=Format('Point ZM (%f %f %f %f)',[X,Y,Z,M]);
    else raise EAGeoCoordinateDepthError.Create;
  end;
end;

constructor TAGeoPointGeometry.Create(ACoordinateDepth:Integer);
begin
  if (ACoordinateDepth<2) or (ACoordinateDepth>4) then raise EAGeoCoordinateDepthError.Create;
  inherited Create;
  FCoordinateDepth:=ACoordinateDepth;
  FCoordinateSize:=1;
  FCoordinates:=GetMem(FCoordinateSize*FCoordinateDepth*sizeof(Double));
end;

destructor TAGeoPointGeometry.Destroy;
begin
  FreeMem(FCoordinates, FCoordinateSize*FCoordinateDepth*sizeof(Double));
  inherited Destroy;
end;


{ TAGeoFeatures }

function TAGeoFeatures.AddFeature(AFeature:TAGeoFeature):Integer;
begin
  result:=FFeatureList.Add(AFeature);
end;

function TAGeoFeatures.RemoveFeature(Index:Integer):boolean;
begin
  if Index<0 then Index:=FFeatureList.Count+Index
  else if Index>=FFeatureList.Count then raise EAGeoFeaturesCountError.Create;
  TAGeoFeature(FFeatureList.Items[Index]).Free;
  FFeatureList.Delete(Index);
end;

procedure TAGeoFeatures.Clear;
var idx:Integer;
begin
  for idx:=FFeatureList.Count - 1 downto 0 do begin
    TAGeoFeature(FFeatureList.Items[idx]).Free;
  end;
  FFeatureList.Clear;
end;

procedure TAGeoFeatures.SaveToCSV(filename:string);
var idx,len:integer;
    lines:TStringList;
    tmpFT:TAGeoFeature;
begin
  len:=FFeatureList.Count;
  lines:=TStringList.Create;
  try
    for idx:=0 to len-1 do begin
      tmpFT:=TAGeoFeature(FFeatureList.Items[idx]);
      case TAGeoFeatures(tmpFT).ClassName of
        'TAGeoPointGeometry':begin
          with TAGeoPointGeometry(tmpFT) do lines.Add(Format('%d,%3.8f,%3.8f,%s',[idx,X,Y,LabelText]));
        end;
        //其他几何类型
      end;
    end;
    lines.SaveToFile(filename);
  finally
    lines.Free;
  end;
end;

constructor TAGeoFeatures.Create;
begin
  inherited Create;
  FFeatureList:=TList.Create;
end;

destructor TAGeoFeatures.Destroy;
begin
  Clear;
  FFeatureList.Free;
  inherited Destroy;
end;

function TAGeoFeatures.GetItem(Index:Integer):TAGeoFeature;
begin
  result:=TAGeoFeature(FFeatureList.Items[Index]);
end;

function TAGeoFeatures.GetItemCount:Integer;
begin
  result:=FFeatureList.Count;
end;


function CreatePoint2D(xPos,yPos:TGeoCoord):TAGeoPointGeometry;
begin
  result:=TAGeoPointGeometry.Create(2);
  result.X:=xPos;
  result.Y:=yPos;
end;

end.

