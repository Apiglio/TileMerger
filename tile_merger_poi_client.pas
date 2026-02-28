unit tile_merger_poi_client;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, tile_merger_feature;

type

  TFetchPOIResult = (fprError, fprFail, fprFmtError, fprSuccess);

  TPOISearchTask = class;

  TPOISearchThread = class(TThread)
  private
    FOwner:TPOISearchTask;
    FFetchResult:TFetchPOIResult;
    FUrl:string;
    FStartTime:TDateTime;
  public
    procedure CheckURI(Sender:TObject; const ASrc:String; var ADest:String);
    procedure FetchInit;
    procedure FetchDone;
    procedure Execute; override;
  public
    constructor Create(aOwner:TPOISearchTask; aUrl:string);
  end;

  TPOISearchTaskProgressEvent = procedure (Position, Max:Integer) of object;

  TPOISearchTask = class
  private
    FSearchEntries:TStringList;
    FUrlTemplate:string;
    PFeatures:TAGeoFeatures;
    FPosition:Integer;
    FOnProgress:TPOISearchTaskProgressEvent;
  protected
    function GetTaskSize:Integer;
  public
    function GetSearchURL(search_key:string):string;
    constructor Create(aStrings:TStrings; aUrlTemplate:string; aFeatures:TAGeoFeatures);
    destructor Destroy; override;
    property Position:Integer read FPosition write FPosition;
    property TaskSize:Integer read GetTaskSize;
    property OnProgress:TPOISearchTaskProgressEvent read FOnProgress write FOnProgress;
  end;

implementation
uses tile_merger_projection, URIParser, fpjson, fphttpclient, debugline;


{ TPOISearchThread }

procedure TPOISearchThread.CheckURI (Sender: TObject; const ASrc: String; var ADest: String);
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

procedure TPOISearchThread.FetchInit;
begin
  FFetchResult:=fprError;
  FStartTime:=Now;
  Form_Debug.AddMessage('[POI搜索开始]'+FUrl);
end;

procedure TPOISearchThread.FetchDone;
begin
  with FOwner do begin
    Position:=Position-1;
    if FOnProgress<>nil then FOnProgress(Position, TaskSize);
  end;
  Form_Debug.AddMessage('[POI搜索结束]'+FUrl);
end;

procedure TPOISearchThread.Execute;
const poi_ua  = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/140.0.0.0 Safari/537.36';
var SearchEntry:string;
    content:TMemoryStream;
    jData,res,loc:TJSONData;
    poi:TJSONObject;
    tmpFT:TAGeoPointGeometry;
    tmpGP:TGeoPoint;
    httpclient:TFPHTTPClient;

begin
  httpclient:=TFPHTTPClient.Create(nil);
  content:=TMemoryStream.Create;
  try
    FFetchResult:=fprFail;
    httpclient.KeepConnection:=false;
    httpclient.AllowRedirect:=true;
    httpclient.OnRedirect:=@CheckURI;
    httpclient.AddHeader('User-Agent', poi_ua);
    try
      content.Clear;
      httpclient.Get(FUrl, content);
    except
      //on E:Exception do begin
      //  ShowMessage(Format('Error %s: %s',[E.ClassName, E.Message]));
      //end;
    end;
    if httpclient.ResponseStatusCode<>200 then exit;
    FFetchResult:=fprFmtError;
    if content.Size=0 then exit;

    content.Position:=0;
    jData:=nil;
    jData:=GetJSON(content);
    res:=jData.FindPath('result');
    if res.JSONType<>jtObject then exit;
    poi:=TJSONObject(res);
    //poi内部不作异常检测了
    loc:=poi.Find('location');
    tmpFT:=TAGeoPointGeometry.Create(2);
    tmpGP.x:=loc.FindPath('lng').AsFloat;
    tmpGP.y:=loc.FindPath('lat').AsFloat;
    tmpGP:=GCJ02_To_WGS84(tmpGP);
    tmpFT.X:=tmpGP.x;
    tmpFT.Y:=tmpGP.y;
    tmpFT.LabelText:=SearchEntry;
    FFetchResult:=fprSuccess;
  finally
    httpclient.Free;
    content.Free;
  end;

end;

constructor TPOISearchThread.Create(aOwner:TPOISearchTask; aUrl:string);
begin
  inherited Create(false);
  FUrl:=aUrl;
end;


{ TPOISearchTask }

function TPOISearchTask.GetTaskSize:Integer;
begin
  result:=FSearchEntries.Count;
end;

function TPOISearchTask.GetSearchURL(search_key:string):string;
begin
  result:=Format(FUrlTemplate,[search_key]);
end;

constructor TPOISearchTask.Create(aStrings:TStrings; aUrlTemplate:string; aFeatures:TAGeoFeatures);
var idx:integer;
begin
  inherited Create;
  FSearchEntries:=TStringList.Create;
  FSearchEntries.Sorted:=true;
  FSearchEntries.Assign(aStrings);
  FUrlTemplate:=aUrlTemplate;
  PFeatures:=aFeatures;

  FPosition:=FSearchEntries.Count;
  for idx:=FSearchEntries.Count-1 downto 0 do begin
    FSearchEntries.Objects[idx]:=TPOISearchThread.Create(Self, GetSearchURL(FSearchEntries.Strings[idx]));
  end;

end;

destructor TPOISearchTask.Destroy;
var idx:integer;
    thd:TPOISearchThread;
begin
  for idx:=FSearchEntries.Count-1 downto 0 do begin
    thd:=TPOISearchThread(FSearchEntries.Objects[idx]);
    if thd<>nil then thd.Terminate;
  end;
  FSearchEntries.Clear;
  FSearchEntries.Free;
  inherited Destroy;
end;


end.

