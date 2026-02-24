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
    FStartTime:TDateTime;
  public
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


{ TPOISearchThread }

procedure TPOISearchThread.FetchInit;
begin
  FStartTime:=Now;
end;

procedure TPOISearchThread.FetchDone;
begin
  with FOwner do begin
    dec(Position);
    if FOnProgress<>nil then FOnProgress(Position, TaskSize);
  end;
end;

procedure TPOISearchThread.Execute;
begin

end;

constructor TPOISearchThread.Create(aOwner:TPOISearchTask; aUrl:string);
begin
  inherited Create(false);
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

