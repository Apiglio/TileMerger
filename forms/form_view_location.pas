unit form_view_location;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFormViewLocation }

  TFormViewLocation = class(TForm)
    Button_Locate: TButton;
    ComboBox_Lng: TComboBox;
    ComboBox_Lat: TComboBox;
    Edit_Lng: TEdit;
    Edit_Lat: TEdit;
    Edit_LngDegree: TEdit;
    Edit_LatDegree: TEdit;
    Edit_LngMinute: TEdit;
    Edit_LatMinute: TEdit;
    Edit_LngSecond: TEdit;
    Edit_LatSecond: TEdit;
    Label_LngDegreeUnit: TLabel;
    Label_LatDegreeUnit: TLabel;
    Label_LatEqualution: TLabel;
    Label_LngMinuteUnit: TLabel;
    Label_LatMinuteUnit: TLabel;
    Label_LngSecondUnit: TLabel;
    Label_LatSecondUnit: TLabel;
    Label_LngTitle: TLabel;
    Label_LngEqualution: TLabel;
    Label_LatTitle: TLabel;
    procedure Button_LocateClick(Sender: TObject);
    procedure DecimalToDMS(Sender:TObject);
    procedure DMSToDecimal(Sender:TObject);
    procedure FormActivate(Sender: TObject);
  private

  public

  end;


  TEditHelper = class helper for TEdit
    procedure SetTextSilent(const AValue: string);
  end;
  TComboBoxHelper = class helper for TComboBox
    procedure SetItemIndexSilent(const AValue: Integer);
  end;

var
  FormViewLocation: TFormViewLocation;

implementation
uses tile_merger_main, tile_merger_projection;

{$R *.lfm}


{ TFormViewLocation }

procedure TFormViewLocation.DecimalToDMS(Sender:TObject);
var lat,lng:double;
    degree,minute:word;
    second:double;
    negative:boolean;
begin
  try
    lat:=StrToFloat(Edit_Lat.Caption);
    negative:=lat<0;
    lat:=abs(lat);
    degree:=Trunc(lat);
    minute:=Trunc((lat-degree)*60);
    second:=(lat-degree-minute/60)*3600;
    Edit_LatDegree.SetTextSilent(IntToStr(degree));
    Edit_LatMinute.SetTextSilent(IntToStr(minute));
    Edit_LatSecond.SetTextSilent(FloatToStr(second));
    if negative then ComboBox_Lat.SetItemIndexSilent(1) else ComboBox_Lat.SetItemIndexSilent(0);
  except
  end;
  try
    lng:=StrToFloat(Edit_Lng.Caption);
    negative:=lng<0;
    lng:=abs(lng);
    degree:=Trunc(lng);
    minute:=Trunc((lng-degree)*60);
    second:=(lng-degree-minute/60)*3600;
    Edit_LngDegree.SetTextSilent(IntToStr(degree));
    Edit_LngMinute.SetTextSilent(IntToStr(minute));
    Edit_LngSecond.SetTextSilent(FloatToStr(second));
    if negative then ComboBox_Lng.SetItemIndexSilent(1) else ComboBox_Lng.SetItemIndexSilent(0);
  except
  end;
end;

procedure TFormViewLocation.Button_LocateClick(Sender: TObject);
var ll,xy:TGeoPoint;
    prj:TProjection;
begin
  ll.lat:=StrToFloat(Edit_Lat.Caption);
  ll.lng:=StrToFloat(Edit_Lng.Caption);
  prj:=FormTileMerger.TileViewer.CurrentTileMatrixSet.Projection;
  xy:=prj.LatlongToXY(ll);
  FormTileMerger.TileViewer.PanToPoint(xy);
  FormTileMerger.TileViewer.Refresh;
end;

procedure TFormViewLocation.DMSToDecimal(Sender:TObject);
var lat,lng:double;
    degree,minute:word;
    second:double;
begin
  try
    second:=StrToFloat(Edit_LatSecond.Caption);
    minute:=StrToInt(Edit_LatMinute.Caption);
    degree:=StrToInt(Edit_LatDegree.Caption);
    lat:=degree+minute/60+second/3600;
    if ComboBox_Lat.ItemIndex=1 then lat:=-lat;
    Edit_Lat.SetTextSilent(FloatToStr(lat));
  except
  end;
  try
    second:=StrToFloat(Edit_LngSecond.Caption);
    minute:=StrToInt(Edit_LngMinute.Caption);
    degree:=StrToInt(Edit_LngDegree.Caption);
    lng:=degree+minute/60+second/3600;
    if ComboBox_Lng.ItemIndex=1 then lng:=-lng;
    Edit_Lng.SetTextSilent(FloatToStr(lng));
  except
  end;
end;

procedure TFormViewLocation.FormActivate(Sender: TObject);
var xy,ll:TGeoPoint;
    prj:TProjection;
begin
  prj:=FormTileMerger.TileViewer.CurrentTileMatrixSet.Projection;
  xy:=FormTileMerger.TileViewer.Centroid;
  ll:=prj.XYToLatlong(xy);
  Edit_Lat.Caption:=FloatToStr(ll.lat);
  Edit_Lng.Caption:=FloatToStr(ll.lng);
end;


{ TEditHelper }

procedure TEditHelper.SetTextSilent(const AValue: string);
var OldEvent: TNotifyEvent;
begin
  OldEvent := Self.OnChange;
  Self.OnChange := nil;
  try
    Self.Text := AValue;
  finally
    Self.OnChange := OldEvent;
  end;
end;


{ TComboBoxHelper }

procedure TComboBoxHelper.SetItemIndexSilent(const AValue: Integer);
var OldEvent: TNotifyEvent;
begin
  OldEvent := Self.OnChange;
  Self.OnChange := nil;
  try
    Self.ItemIndex := AValue;
  finally
    Self.OnChange := OldEvent;
  end;
end;

end.

