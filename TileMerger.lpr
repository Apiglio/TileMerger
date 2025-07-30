program TileMerger;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazcontrols, tile_merger_main, tile_merger_wmts_client, debugline,
  exporttiff
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TFormTileMerger, FormTileMerger);
  Application.CreateForm(TForm_Debug, Form_Debug);
  Application.CreateForm(TForm_ExportTiff, Form_ExportTiff);
  Application.Run;
end.

