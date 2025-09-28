unit tile_merger_tiff;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils;

const
    EPSG_3857 = 'PCS Name = WGS_1984_Web_Mercator_Auxiliary_Sphere|GCS Name = GCS_WGS_1984|Datum = D_WGS_1984|Ellipsoid = WGS_1984|Primem = Greenwich||ESRI PE String = PROJCS["WGS_1984_Web_Mercator_Auxiliary_Sphere",GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]],PROJECTION["Mercator_Auxiliary_Sphere"],PARAMETER["False_Easting",0.0],PARAMETER["False_Northing",0.0],PARAMETER["Central_Meridian",0.0],PARAMETER["Standard_Parallel_1",0.0],PARAMETER["Auxiliary_Sphere_Type",0.0],UNIT["Meter",1.0]]|';

    TIFF_TYPE_BYTE      = $0001;
    TIFF_TYPE_ASCII     = $0002;
    TIFF_TYPE_SHORT     = $0003;
    TIFF_TYPE_LONG      = $0004;
    TIFF_TYPE_RETIONAL  = $0005;
    TIFF_TYPE_SBYTE     = $0006;
    TIFF_TYPE_UNDEFINED = $0007;
    TIFF_TYPE_SSHORT    = $0008;
    TIFF_TYPE_SLONG     = $0009;
    TIFF_TYPE_SRETIONAL = $000A;
    TIFF_TYPE_FLOAT     = $000B;
    TIFF_TYPE_DOUBLE    = $000C;

    TIFF_Tag_ImageWidth      = $0100;
    TIFF_Tag_ImageLength     = $0101;
    TIFF_Tag_BitsPerSample   = $0102;
    TIFF_Tag_Compression     = $0103;
    TIFF_Tag_PhotometricInterpretation  = $0106;
    TIFF_Tag_StripOffsets    = $0111;
    TIFF_Tag_Orientation     = $0112;
    TIFF_Tag_SamplesPerPixel = $0115;
    TIFF_Tag_StripByteCounts = $0117;
    TIFF_Tag_XResolution     = $011A;
    TIFF_Tag_YResolution     = $011B;
    TIFF_Tag_PlanarConfiguration        = $011C;
    TIFF_Tag_ResolutionUnit  = $0128;
    TIFF_Tag_Software        = $0131;
    TIFF_Tag_ExtraSamples    = $0152;

    TIFF_Tag_ModelTiepointTag           = $8482;
    TIFF_Tag_ModelPixelScaleTag         = $830E;
    TIFF_Tag_ModelTransformationTag     = $8480;

    TIFF_Tag_GeoKeyDirectoryTag         = $87AF;
    TIFF_Tag_GeoDoubleParamsTag         = $87B0;
    TIFF_Tag_GeoAsciiParamsTag          = $87B1;

    TIFF_Tag_GDAL_METADATA              = $A480;


    //GeoTIFF Configuration GeoKeys
    GT_Conf_GTModelTypeGeoKey           = $0400;
    GT_Conf_GTRasterTypeGeoKey          = $0401;
    GT_Conf_GTCitationGeoKey            = $0402;
    GT_Conf_GeographicTypeGeoKey        = $0800;
    GT_Conf_GeogCitationGeoKey          = $0801;
    GT_Conf_GeogGeodeticDatumGeoKey     = $0802;
    GT_Conf_GeogPrimeMeridianGeoKey     = $0803;
    GT_Conf_GeogLinearUnitsGeoKey       = $0804;
    GT_Conf_GeogLinearUnitSizeGeoKey    = $0805;
    GT_Conf_GeogAngularUnitsGeoKey      = $0806;
    GT_Conf_GeogAngularUnitSizeGeoKey   = $0807;
    GT_Conf_GeogEllipsoidGeoKey         = $0808;
    GT_Conf_GeogSemiMajorAxisGeoKey     = $0809;
    GT_Conf_GeogSemiMinorAxisGeoKey     = $080A;
    GT_Conf_GeogInvFlatteningGeoKey     = $080B;
    GT_Conf_GeogAzimuthUnitsGeoKey      = $080C;
    GT_Conf_GeogPrimeMeridianLongGeoKey = $080D;
    GT_Conf_ProjectedCSTypeGeoKey       = $0C00;
    GT_Conf_PCSCitationGeoKey           = $0C01;
    GT_Conf_ProjectionGeoKey            = $0C02;
    GT_Conf_ProjCoordTransGeoKey        = $0C03;
    GT_Conf_ProjLinearUnitsGeoKey       = $0C04;



implementation

end.

