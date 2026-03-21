program TileMerger;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazcontrols, tile_merger_main, tile_merger_wmts_client, debugline,
  exporttiff, tile_merger_tiff, tile_merger_projection, tile_merger_feature,
  form_search_poi, tile_merger_poi_client, form_options, form_view_location;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TFormTileMerger, FormTileMerger);
  Application.CreateForm(TForm_Debug, Form_Debug);
  Application.CreateForm(TForm_ExportTiff, Form_ExportTiff);
  Application.CreateForm(TForm_PoiServer, Form_PoiServer);
  Application.CreateForm(TFormOptions, FormOptions);
  Application.CreateForm(TFormViewLocation, FormViewLocation);
  Application.Run;
end.

