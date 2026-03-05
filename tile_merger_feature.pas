unit tile_merger_feature;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, tile_merger_projection;

type

  EAGeoError = class(Exception)
  end;

  EAGeoCoordinateDepthError = class(EAGeoError)
    constructor Create;
  end;
  EAGeoFeaturesCountError = class(EAGeoError)
    constructor Create;
  end;
  EAGeoFeaturesTypeError = class(EAGeoError)
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
    function WKT:string; virtual; abstract;
    function CSV:string; virtual; abstract;
    function GeoJSON:TJSONData; virtual; abstract;
    function EsriJSON:TJSONData; virtual; abstract;
    class function EsriGeoCode:string; virtual; abstract;
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
    function WKT:string; override;
    function CSV:string; override;
    function GeoJSON:TJSONData; override;
    function EsriJSON:TJSONData; override;
    class function EsriGeoCode:string; override;
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
    procedure SaveToGeoJSON(filename:string);
    procedure SaveToEsriJSON(filename:string);
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

{ EAGeoFeaturesTypeError }
constructor EAGeoFeaturesTypeError.Create;
begin
  inherited Create('要素类型错误');
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

function TAGeoPointGeometry.CSV:string;
const delimiter = ', ';
var idx:integer;
begin
  result:=GetLabelText;
  for idx:=0 to FCoordinateDepth-1 do begin
    result:=result+delimiter+FloatToStr((FCoordinates+idx)^);
  end;
end;

function TAGeoPointGeometry.GeoJSON:TJSONData;
var resultObject, geom, prop: TJSONObject;
    coords: TJSONArray;
begin
  resultObject:=TJSONObject.Create;
  resultObject.Strings['type']:='Feature';
  geom:=TJSONObject.Create;
  geom.Strings['type']:='Point';
  coords:=TJSONArray.Create;
  coords.Add(GetX);
  coords.Add(GetY);
  geom.Arrays['coordinates']:=coords;
  resultObject.Objects['geometry']:=geom;
  prop:=TJSONObject.Create;
  prop.Strings['name']:=GetLabelText;
  resultObject.Objects['properties']:=prop;
  result:=resultObject;
end;

function TAGeoPointGeometry.EsriJSON:TJSONData;
var resultObject, geom, attr: TJSONObject;
begin
  resultObject:=TJSONObject.Create;
  attr:=TJSONObject.Create;
  attr.Integers['FID']:=0; //在Features导出时额外修改
  attr.Strings['name']:=GetLabelText;
  resultObject.Objects['attributes']:=attr;
  geom:=TJSONObject.Create;
  geom.Floats['x']:=GetX;
  geom.Floats['y']:=GetY;
  resultObject.Objects['geometry']:=geom;
  result:=resultObject;
end;

class function TAGeoPointGeometry.EsriGeoCode:string;
begin
  result:='esriGeometryPoint';
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
  result:=false;
  if Index<0 then Index:=FFeatureList.Count+Index
  else if Index>=FFeatureList.Count then raise EAGeoFeaturesCountError.Create;
  TAGeoFeature(FFeatureList.Items[Index]).Free;
  FFeatureList.Delete(Index);
  result:=true;
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
    //判断要素类型
    tmpFT:=TAGeoFeature(FFeatureList.Items[0]);
    case TAGeoFeatures(tmpFT).ClassName of
      'TAGeoPointGeometry': lines.Add('name, lng, lat');
      else raise EAGeoFeaturesTypeError.Create;
    end;
    //创建文件内容
    for idx:=0 to len-1 do begin
      tmpFT:=TAGeoFeature(FFeatureList.Items[idx]);
      lines.Add(tmpFT.CSV);
    end;
    lines.SaveToFile(filename);
  finally
    lines.Free;
  end;
end;

//备用：如果有不支持指数形式浮点数时使用
{
function FmtJSON(json_data:TJSONData):string;
var idx,len:integer;
    pArray:TJSONArray;
    pObject:TJSONObject;
begin
  //TFormatOption就以后再说吧
  result:='';
  case json_data.ClassName of
    'TJSONFloatNumber':result:=FormatFloat('0.###############',json_data.AsFloat);
    'TJSONArray':begin
      pArray:=TJSONArray(json_data);
      result:='[';
      len:=pArray.Count;
      case len of
        0:;
        1:result:=result+FmtJSON(pArray.Items[0]);
        else begin
          for idx:=0 to len-2 do begin
            result:=result+FmtJSON(pArray.Items[idx])+',';
          end;
          result:=result+FmtJSON(pArray.Items[len-1]);
        end;
      end;
      result:=result+']';
    end;
    'TJSONObject':begin
      pObject:=TJSONObject(json_data);
      result:='{';
      len:=pObject.Count;
      case len of
        0:;
        1:result:=result+'"'+StringToJSONString(pObject.Names[0])+'":'+FmtJSON(pObject.Items[0]);
        else begin
          for idx:=0 to len-2 do begin
            result:=result+'"'+StringToJSONString(pObject.Names[idx])+'":'+FmtJSON(pObject.Items[idx])+',';
          end;
          result:=result+'"'+StringToJSONString(pObject.Names[len-1])+'":'+FmtJSON(pObject.Items[len-1]);
        end;
      end;
      result:=result+'}';
    end;
    else result:=json_data.AsJSON;
  end;
end;
}

procedure TAGeoFeatures.SaveToGeoJSON(filename:string);
var geojson:TJSONObject;
    features:TJSONArray;
    idx,len:integer;
    geojsonfile:TStringList;
begin
  geojson:=TJSONObject.Create;
  geojsonfile:=TStringList.Create;
  try
    geojson.Strings['type']:='FeatureCollection';
      features:=TJSONArray.Create;
      len:=FFeatureList.Count;
      for idx:=0 to len-1 do features.Add(TAGeoFeature(FFeatureList.Items[idx]).GeoJSON);
    geojson.Arrays['features']:=features;
    //geojsonfile.Text:=FmtJSON(geojson);
    geojsonfile.Text:=geojson.FormatJSON();
    geojsonfile.SaveToFile(filename);
  finally
    geojson.Free;
    geojsonfile.Free;
  end;
end;

procedure TAGeoFeatures.SaveToEsriJSON(filename:string);
var geojson, al, sr, fd, fea:TJSONObject;
    features, fs:TJSONArray;
    idx,len:integer;
    geojsonfile:TStringList;
    esriJsonGeoCode:string;
begin
  if FFeatureList.Count=0 then exit;
  esriJsonGeoCode:=TAGeoFeature(FFeatureList.Items[0]).EsriGeoCode;
  geojson:=TJSONObject.Create;
  geojsonfile:=TStringList.Create;
  try
    geojson.Strings['displayFieldName']:='';
      al:=TJSONObject.Create;
      al.Strings['FID']:='FID';
      al.Strings['name']:='name';
    geojson.Objects['fieldAliases']:=al;
    geojson.Strings['geometryType']:=esriJsonGeoCode;
      sr:=TJSONObject.Create;
      sr.Integers['wkid']:=32651;        //默认WGS-84
      sr.Integers['lastestWkid']:=32651; //默认WGS-84
    geojson.Objects['spatialReference']:=sr;
      fs:=TJSONArray.Create;
        fd:=TJSONObject.Create;
        fd.Strings['name']:='FID';
        fd.Strings['type']:='esriFieldTypeOID';
        fd.Strings['alias']:='FID';
      fs.Add(fd);
        fd:=TJSONObject.Create;
        fd.Strings['name']:='name';
        fd.Strings['type']:='esriFieldTypeString';
        fd.Strings['alias']:='name';
        fd.Integers['length']:=50;
      fs.Add(fd);
    geojson.Arrays['fields']:=fs;
      features:=TJSONArray.Create;
      len:=FFeatureList.Count;
      for idx:=0 to len-1 do begin
        fea:=TAGeoFeature(FFeatureList.Items[idx]).EsriJSON as TJSONObject;
        fea.Integers['FID']:=idx;
        features.Add(fea);
      end;
    geojson.Arrays['features']:=features;
    //geojsonfile.Text:=FmtJSON(geojson);
    geojsonfile.Text:=geojson.FormatJSON();
    geojsonfile.SaveToFile(filename);
  finally
    geojson.Free;
    geojsonfile.Free;
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

