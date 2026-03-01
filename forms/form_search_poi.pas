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
    procedure POISearchTaskProgress(Pos, Max:Integer);
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

procedure TForm_PoiServer.POISearchTaskProgress(Pos, Max:Integer);
begin
  if Pos=Max then FormTileMerger.UpdateFeatureListEntry;
end;

procedure TForm_PoiServer.Button_SearchClick(Sender: TObject);
const poi_url = 'http://api.map.baidu.com/geocoding/v3/';
begin
  WMTS_Client.FeatureLayers[0].Features.Clear;
  TPOISearchTask.Create(
    Memo_PoiSearchEntries.Lines,
    poi_url+'?address='+Edit_CityParam.Text+'%s&output=json&ret_coordtype=gcj02ll&ak=F3JisKBLlCzkVsF74ZIUmtJLYvHWjD8L',
    WMTS_Client.FeatureLayers[0].Features,
    @POISearchTaskProgress
  );
end;

end.

