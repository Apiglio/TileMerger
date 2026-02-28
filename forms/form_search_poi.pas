unit form_search_poi;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, URIParser;

type

  { TForm_PoiServer }

  TForm_PoiServer = class(TForm)
    Button_Search: TButton;
    Edit_CityParam: TEdit;
    Memo_PoiSearchEntries: TMemo;
    procedure Button_SearchClick(Sender: TObject);
  private

  public
    procedure CheckURI(Sender:TObject;const ASrc:String;var ADest:String);
  end;

var
  Form_PoiServer: TForm_PoiServer;

implementation
uses tile_merger_main, tile_merger_wmts_client, tile_merger_feature, tile_merger_projection, fphttpclient, fpjson, tile_merger_poi_client;

{$R *.lfm}







{ TForm_PoiServer }

//Fixed by @wittbo on Lazarus Forum,
//Source: https://forum.lazarus.freepascal.org/index.php/topic,43553.msg335901.html#msg335901
procedure TForm_PoiServer.CheckURI (Sender: TObject; const ASrc: String; var ADest: String);
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

procedure TForm_PoiServer.Button_SearchClick(Sender: TObject);
const //poi_url = 'http://api.map.baidu.com/place/v3/region';
      poi_url = 'http://api.map.baidu.com/geocoding/v3/';
      poi_ua  = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/140.0.0.0 Safari/537.36';
var SearchEntry:string;
    SearchCity:string;
    GetURL:String;
    content:TMemoryStream;
    jData,res,loc:TJSONData;
    search_result:TJSONArray;
    poi:TJSONObject;
    tmpFT:TAGeoPointGeometry;
    tmpGP:TGeoPoint;
    httpclient:TFPHTTPClient;
    idx,len:integer;

begin

  WMTS_Client.FeatureLayers[0].Features.Clear;
  TPOISearchTask.Create(
    Memo_PoiSearchEntries.Lines,
    poi_url+'?address=%s&output=json&ret_coordtype=gcj02ll&ak=F3JisKBLlCzkVsF74ZIUmtJLYvHWjD8L',
    WMTS_Client.FeatureLayers[0].Features
  );
  exit;


  SearchCity:=Edit_CityParam.Text;
  //临时的搜索方法，临时的要素图层
  WMTS_Client.FeatureLayers[0].Features.Clear;
  httpclient:=TFPHTTPClient.Create(nil);
  content:=TMemoryStream.Create;
  try
    httpclient.KeepConnection:=false;
    httpclient.AllowRedirect:=true;
    httpclient.OnRedirect:=@CheckURI;
    httpclient.AddHeader('User-Agent', poi_ua);
    for SearchEntry in Memo_PoiSearchEntries.Lines do begin
      //GetURL:=poi_url+'?query='+EncodeURLElement(SearchEntry)+'&region='+EncodeURLElement(SearchCity)+'&ret_coordtype=gcj02ll&ak=F3JisKBLlCzkVsF74ZIUmtJLYvHWjD8L';
      GetURL:=poi_url+'?address='+EncodeURLElement(SearchCity+SearchEntry)+'&output=json&ret_coordtype=gcj02ll&ak=F3JisKBLlCzkVsF74ZIUmtJLYvHWjD8L';
      try
        content.Clear;
        httpclient.Get(GetURL, content);
      except
        //on E:Exception do begin
        //  ShowMessage(Format('Error %s: %s',[E.ClassName, E.Message]));
        //end;
      end;
      if httpclient.ResponseStatusCode<>200 then continue;
      if content.Size=0 then continue;

      content.Position:=0;
      jData:=nil;
      jData:=GetJSON(content);
      //res:=jData.FindPath('results');
      res:=jData.FindPath('result');
      //if (res<>nil) and (res is TJSONArray) then begin
      //  search_result:=TJSONArray(res);
      //  len:=search_result.Count;
      //  for idx:=0 to len-1 do begin
          //if search_result[idx].JSONType<>jtObject then continue;
          //poi:=TJSONObject(search_result[idx]);
          if res.JSONType<>jtObject then continue;
          poi:=TJSONObject(res);
          //poi内部不作异常检测了
          loc:=poi.Find('location');
          tmpFT:=TAGeoPointGeometry.Create(2);
          tmpGP.x:=loc.FindPath('lng').AsFloat;
          tmpGP.y:=loc.FindPath('lat').AsFloat;
          tmpGP:=GCJ02_To_WGS84(tmpGP);
          tmpFT.X:=tmpGP.x;
          tmpFT.Y:=tmpGP.y;
          //tmpFT.LabelText:=Format('%s [%s,%d]',[poi.Find('name').AsString, SearchEntry, idx]);
          tmpFT.LabelText:=SearchEntry;
          WMTS_Client.FeatureLayers[0].Features.AddFeature(tmpFT);
      //  end;
      //end;
    end;
    ShowMessage(Format('共找到%d个POI',[WMTS_Client.FeatureLayers[0].Features.Count]));
    WMTS_Client.FeatureLayers[0].Features.SaveToCSV('POI_Search.csv');
  finally
    httpclient.Free;
    content.Free;
  end;

end;

end.

