using System;
using System.CodeDom.Compiler;
using System.ComponentModel;
using System.Configuration;
using System.Diagnostics;
using System.Drawing;
using System.Runtime.CompilerServices;
using System.Windows.Forms;

namespace Occult.Properties
{
	[CompilerGenerated]
	[GeneratedCode("Microsoft.VisualStudio.Editors.SettingsDesigner.SettingsSingleFileGenerator", "17.14.0.0")]
	internal sealed class Settings : ApplicationSettingsBase
	{
		private static Settings defaultInstance = (Settings)(object)SettingsBase.Synchronized((SettingsBase)(object)new Settings());

		public static Settings Default => defaultInstance;

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public string Site_Longitude_dd_d
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("Site_Longitude_dd_d");
			}
			set
			{
				((SettingsBase)this).set_Item("Site_Longitude_dd_d", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public string Site_Latitude_dd_d
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("Site_Latitude_dd_d");
			}
			set
			{
				((SettingsBase)this).set_Item("Site_Latitude_dd_d", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public string Site_TimeZone_Hrs
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("Site_TimeZone_Hrs");
			}
			set
			{
				((SettingsBase)this).set_Item("Site_TimeZone_Hrs", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public string Site_Altitude
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("Site_Altitude");
			}
			set
			{
				((SettingsBase)this).set_Item("Site_Altitude", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string Site_Name
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("Site_Name");
			}
			set
			{
				((SettingsBase)this).set_Item("Site_Name", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("astorb.dat.gz")]
		public string ASTORB_file
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("ASTORB_file");
			}
			set
			{
				((SettingsBase)this).set_Item("ASTORB_file", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("https://ftp.lowell.edu/pub/elgb/")]
		public string ASTORB_Server
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("ASTORB_Server");
			}
			set
			{
				((SettingsBase)this).set_Item("ASTORB_Server", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("MPCORB.DAT.gz")]
		public string MPCOrb_file
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("MPCOrb_file");
			}
			set
			{
				((SettingsBase)this).set_Item("MPCOrb_file", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("https://www.minorplanetcenter.net/iau/MPCORB/")]
		public string MPCOrb_Server
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("MPCOrb_Server");
			}
			set
			{
				((SettingsBase)this).set_Item("MPCOrb_Server", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("Soft00Cmt.txt")]
		public string Comet_file
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("Comet_file");
			}
			set
			{
				((SettingsBase)this).set_Item("Comet_file", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("https://www.minorplanetcenter.net/iau/Ephemerides/Comets/")]
		public string Comet_Server
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("Comet_Server");
			}
			set
			{
				((SettingsBase)this).set_Item("Comet_Server", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("http://www.asteroidoccultation.com")]
		public string Future_Server
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("Future_Server");
			}
			set
			{
				((SettingsBase)this).set_Item("Future_Server", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("FutureAll.zip")]
		public string FutureAll_File_XML
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("FutureAll_File_XML");
			}
			set
			{
				((SettingsBase)this).set_Item("FutureAll_File_XML", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("http://www.asteroidoccultation.com")]
		public string FutureAll_Server
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("FutureAll_Server");
			}
			set
			{
				((SettingsBase)this).set_Item("FutureAll_Server", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string FTP_AnonymousPassword
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("FTP_AnonymousPassword");
			}
			set
			{
				((SettingsBase)this).set_Item("FTP_AnonymousPassword", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("Normal")]
		public FormWindowState SolarEclipseLocalMapWindow
		{
			get
			{
				//IL_000b: Unknown result type (might be due to invalid IL or missing references)
				return (FormWindowState)((SettingsBase)this).get_Item("SolarEclipseLocalMapWindow");
			}
			set
			{
				//IL_0006: Unknown result type (might be due to invalid IL or missing references)
				((SettingsBase)this).set_Item("SolarEclipseLocalMapWindow", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string AsteroidSearchUnnumbered
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("AsteroidSearchUnnumbered");
			}
			set
			{
				((SettingsBase)this).set_Item("AsteroidSearchUnnumbered", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("3")]
		public int AsteroidMultiSigma
		{
			get
			{
				return (int)((SettingsBase)this).get_Item("AsteroidMultiSigma");
			}
			set
			{
				((SettingsBase)this).set_Item("AsteroidMultiSigma", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool AsteroidMultiMiles
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsteroidMultiMiles");
			}
			set
			{
				((SettingsBase)this).set_Item("AsteroidMultiMiles", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string UCAC2_Path
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("UCAC2_Path");
			}
			set
			{
				((SettingsBase)this).set_Item("UCAC2_Path", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string FilePathGoogleEarthKML
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("FilePathGoogleEarthKML");
			}
			set
			{
				((SettingsBase)this).set_Item("FilePathGoogleEarthKML", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string OccelmntLastFileName
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("OccelmntLastFileName");
			}
			set
			{
				((SettingsBase)this).set_Item("OccelmntLastFileName", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public int OccelmntLastIndex
		{
			get
			{
				return (int)((SettingsBase)this).get_Item("OccelmntLastIndex");
			}
			set
			{
				((SettingsBase)this).set_Item("OccelmntLastIndex", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0.2")]
		public decimal AsteroidSearchDefaultUncertainty
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("AsteroidSearchDefaultUncertainty");
			}
			set
			{
				((SettingsBase)this).set_Item("AsteroidSearchDefaultUncertainty", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool AstorbNumberedAsteroidsOnly
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AstorbNumberedAsteroidsOnly");
			}
			set
			{
				((SettingsBase)this).set_Item("AstorbNumberedAsteroidsOnly", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool AstorbLimitUnNumbered
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AstorbLimitUnNumbered");
			}
			set
			{
				((SettingsBase)this).set_Item("AstorbLimitUnNumbered", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("9.0")]
		public decimal PrePointFaintStar
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("PrePointFaintStar");
			}
			set
			{
				((SettingsBase)this).set_Item("PrePointFaintStar", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("5")]
		public decimal PrePointFaintWidth
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("PrePointFaintWidth");
			}
			set
			{
				((SettingsBase)this).set_Item("PrePointFaintWidth", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("6.0")]
		public decimal PrePointBrightStar
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("PrePointBrightStar");
			}
			set
			{
				((SettingsBase)this).set_Item("PrePointBrightStar", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("60")]
		public decimal PrePointBrightWidth
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("PrePointBrightWidth");
			}
			set
			{
				((SettingsBase)this).set_Item("PrePointBrightWidth", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("6")]
		public decimal PrePointLeadTime
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("PrePointLeadTime");
			}
			set
			{
				((SettingsBase)this).set_Item("PrePointLeadTime", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("Normal")]
		public FormWindowState AsteroidPlotPathWindow
		{
			get
			{
				//IL_000b: Unknown result type (might be due to invalid IL or missing references)
				return (FormWindowState)((SettingsBase)this).get_Item("AsteroidPlotPathWindow");
			}
			set
			{
				//IL_0006: Unknown result type (might be due to invalid IL or missing references)
				((SettingsBase)this).set_Item("AsteroidPlotPathWindow", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("Normal")]
		public FormWindowState AsteroidPlotMultiPathWindow
		{
			get
			{
				//IL_000b: Unknown result type (might be due to invalid IL or missing references)
				return (FormWindowState)((SettingsBase)this).get_Item("AsteroidPlotMultiPathWindow");
			}
			set
			{
				//IL_0006: Unknown result type (might be due to invalid IL or missing references)
				((SettingsBase)this).set_Item("AsteroidPlotMultiPathWindow", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("Normal")]
		public FormWindowState SolarEclipseWorldMapWindow
		{
			get
			{
				//IL_000b: Unknown result type (might be due to invalid IL or missing references)
				return (FormWindowState)((SettingsBase)this).get_Item("SolarEclipseWorldMapWindow");
			}
			set
			{
				//IL_0006: Unknown result type (might be due to invalid IL or missing references)
				((SettingsBase)this).set_Item("SolarEclipseWorldMapWindow", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("Normal")]
		public FormWindowState PlanetViewerWindow
		{
			get
			{
				//IL_000b: Unknown result type (might be due to invalid IL or missing references)
				return (FormWindowState)((SettingsBase)this).get_Item("PlanetViewerWindow");
			}
			set
			{
				//IL_0006: Unknown result type (might be due to invalid IL or missing references)
				((SettingsBase)this).set_Item("PlanetViewerWindow", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public decimal MapWest1
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("MapWest1");
			}
			set
			{
				((SettingsBase)this).set_Item("MapWest1", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public decimal MapWest2
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("MapWest2");
			}
			set
			{
				((SettingsBase)this).set_Item("MapWest2", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public decimal MapWest3
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("MapWest3");
			}
			set
			{
				((SettingsBase)this).set_Item("MapWest3", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public decimal MapWest4
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("MapWest4");
			}
			set
			{
				((SettingsBase)this).set_Item("MapWest4", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public decimal MapEast1
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("MapEast1");
			}
			set
			{
				((SettingsBase)this).set_Item("MapEast1", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public decimal MapEast2
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("MapEast2");
			}
			set
			{
				((SettingsBase)this).set_Item("MapEast2", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public decimal MapEast3
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("MapEast3");
			}
			set
			{
				((SettingsBase)this).set_Item("MapEast3", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public decimal MapEast4
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("MapEast4");
			}
			set
			{
				((SettingsBase)this).set_Item("MapEast4", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public decimal MapNorth1
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("MapNorth1");
			}
			set
			{
				((SettingsBase)this).set_Item("MapNorth1", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public decimal MapNorth2
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("MapNorth2");
			}
			set
			{
				((SettingsBase)this).set_Item("MapNorth2", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public decimal MapNorth3
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("MapNorth3");
			}
			set
			{
				((SettingsBase)this).set_Item("MapNorth3", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public decimal MapNorth4
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("MapNorth4");
			}
			set
			{
				((SettingsBase)this).set_Item("MapNorth4", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public decimal MapSouth1
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("MapSouth1");
			}
			set
			{
				((SettingsBase)this).set_Item("MapSouth1", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public decimal MapSouth2
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("MapSouth2");
			}
			set
			{
				((SettingsBase)this).set_Item("MapSouth2", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public decimal MapSouth3
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("MapSouth3");
			}
			set
			{
				((SettingsBase)this).set_Item("MapSouth3", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public decimal MapSouth4
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("MapSouth4");
			}
			set
			{
				((SettingsBase)this).set_Item("MapSouth4", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool Skip5thLine
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("Skip5thLine");
			}
			set
			{
				((SettingsBase)this).set_Item("Skip5thLine", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("2")]
		public decimal AsteroidPathStepSize
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("AsteroidPathStepSize");
			}
			set
			{
				((SettingsBase)this).set_Item("AsteroidPathStepSize", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("2")]
		public decimal SolarEclipsePathStepSize
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("SolarEclipsePathStepSize");
			}
			set
			{
				((SettingsBase)this).set_Item("SolarEclipsePathStepSize", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public int MultiLocationSelectedIndex
		{
			get
			{
				return (int)((SettingsBase)this).get_Item("MultiLocationSelectedIndex");
			}
			set
			{
				((SettingsBase)this).set_Item("MultiLocationSelectedIndex", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("100")]
		public decimal CometDuration
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("CometDuration");
			}
			set
			{
				((SettingsBase)this).set_Item("CometDuration", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("5")]
		public decimal CometInterval
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("CometInterval");
			}
			set
			{
				((SettingsBase)this).set_Item("CometInterval", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("5")]
		public decimal EphemerisInterval
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("EphemerisInterval");
			}
			set
			{
				((SettingsBase)this).set_Item("EphemerisInterval", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool AsteroidExportEnabled
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsteroidExportEnabled");
			}
			set
			{
				((SettingsBase)this).set_Item("AsteroidExportEnabled", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("4")]
		public int StarChartSizeIndex
		{
			get
			{
				return (int)((SettingsBase)this).get_Item("StarChartSizeIndex");
			}
			set
			{
				((SettingsBase)this).set_Item("StarChartSizeIndex", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool BWFlag
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("BWFlag");
			}
			set
			{
				((SettingsBase)this).set_Item("BWFlag", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string FilePathGoogleMaps
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("FilePathGoogleMaps");
			}
			set
			{
				((SettingsBase)this).set_Item("FilePathGoogleMaps", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string FilePathMapsMIF
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("FilePathMapsMIF");
			}
			set
			{
				((SettingsBase)this).set_Item("FilePathMapsMIF", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("7")]
		public decimal DateRangeInAsteroidDisplaySelectionCriterion
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("DateRangeInAsteroidDisplaySelectionCriterion");
			}
			set
			{
				((SettingsBase)this).set_Item("DateRangeInAsteroidDisplaySelectionCriterion", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("ftp://cdsarc.cds.unistra.fr/pub/cats/B/astorb/")]
		public string ASTORB_Mirror
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("ASTORB_Mirror");
			}
			set
			{
				((SettingsBase)this).set_Item("ASTORB_Mirror", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("astorb.dat.gz")]
		public string ASTORB_Mirror_file
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("ASTORB_Mirror_file");
			}
			set
			{
				((SettingsBase)this).set_Item("ASTORB_Mirror_file", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("ftp://mpcorb.klet.org/")]
		public string MPCORB_Mirror_Server
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("MPCORB_Mirror_Server");
			}
			set
			{
				((SettingsBase)this).set_Item("MPCORB_Mirror_Server", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool AsteroidSearchExcludeStars
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsteroidSearchExcludeStars");
			}
			set
			{
				((SettingsBase)this).set_Item("AsteroidSearchExcludeStars", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("13.0")]
		public decimal AsteroidSearchFaintStarLimit
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("AsteroidSearchFaintStarLimit");
			}
			set
			{
				((SettingsBase)this).set_Item("AsteroidSearchFaintStarLimit", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("15")]
		public decimal AsteroidSearchMinDiameter
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("AsteroidSearchMinDiameter");
			}
			set
			{
				((SettingsBase)this).set_Item("AsteroidSearchMinDiameter", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool PlanetSearchIncludeMainMoons
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("PlanetSearchIncludeMainMoons");
			}
			set
			{
				((SettingsBase)this).set_Item("PlanetSearchIncludeMainMoons", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool PlanetSearchIncludeSmallMoons
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("PlanetSearchIncludeSmallMoons");
			}
			set
			{
				((SettingsBase)this).set_Item("PlanetSearchIncludeSmallMoons", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool AsteroidPathSunUp
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsteroidPathSunUp");
			}
			set
			{
				((SettingsBase)this).set_Item("AsteroidPathSunUp", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool AsteroidalsAutoPredictFutureOutputs
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsteroidalsAutoPredictFutureOutputs");
			}
			set
			{
				((SettingsBase)this).set_Item("AsteroidalsAutoPredictFutureOutputs", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool StarChartBWFlag
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("StarChartBWFlag");
			}
			set
			{
				((SettingsBase)this).set_Item("StarChartBWFlag", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool AutoAsteroidStarChart
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AutoAsteroidStarChart");
			}
			set
			{
				((SettingsBase)this).set_Item("AutoAsteroidStarChart", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool AutoAsteroidPathCoords
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AutoAsteroidPathCoords");
			}
			set
			{
				((SettingsBase)this).set_Item("AutoAsteroidPathCoords", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool AutoAsteroidMultisite
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AutoAsteroidMultisite");
			}
			set
			{
				((SettingsBase)this).set_Item("AutoAsteroidMultisite", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool AutoAsteroidWorld
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AutoAsteroidWorld");
			}
			set
			{
				((SettingsBase)this).set_Item("AutoAsteroidWorld", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool AutoAsteroidPrePoint
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AutoAsteroidPrePoint");
			}
			set
			{
				((SettingsBase)this).set_Item("AutoAsteroidPrePoint", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool AutoAsteroidKML
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AutoAsteroidKML");
			}
			set
			{
				((SettingsBase)this).set_Item("AutoAsteroidKML", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool AutoAsteroidHTM
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AutoAsteroidHTM");
			}
			set
			{
				((SettingsBase)this).set_Item("AutoAsteroidHTM", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool ShowCometElements
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("ShowCometElements");
			}
			set
			{
				((SettingsBase)this).set_Item("ShowCometElements", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public int GraphicsSaveFileType
		{
			get
			{
				return (int)((SettingsBase)this).get_Item("GraphicsSaveFileType");
			}
			set
			{
				((SettingsBase)this).set_Item("GraphicsSaveFileType", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool PlanetSearchLimitStarMag
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("PlanetSearchLimitStarMag");
			}
			set
			{
				((SettingsBase)this).set_Item("PlanetSearchLimitStarMag", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("9")]
		public decimal MagLimitMercury
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("MagLimitMercury");
			}
			set
			{
				((SettingsBase)this).set_Item("MagLimitMercury", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("9")]
		public decimal MagLimitVenus
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("MagLimitVenus");
			}
			set
			{
				((SettingsBase)this).set_Item("MagLimitVenus", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("10")]
		public decimal MagLimitMars
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("MagLimitMars");
			}
			set
			{
				((SettingsBase)this).set_Item("MagLimitMars", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("9")]
		public decimal MagLimitJupiter
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("MagLimitJupiter");
			}
			set
			{
				((SettingsBase)this).set_Item("MagLimitJupiter", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("9")]
		public decimal MagLimitSaturn
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("MagLimitSaturn");
			}
			set
			{
				((SettingsBase)this).set_Item("MagLimitSaturn", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("12")]
		public decimal MagLimitUranus
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("MagLimitUranus");
			}
			set
			{
				((SettingsBase)this).set_Item("MagLimitUranus", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("14")]
		public decimal MagLimitNeptune
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("MagLimitNeptune");
			}
			set
			{
				((SettingsBase)this).set_Item("MagLimitNeptune", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("17")]
		public decimal MagLimitPluto
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("MagLimitPluto");
			}
			set
			{
				((SettingsBase)this).set_Item("MagLimitPluto", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("100")]
		public decimal Site_GrazeTravelDisance
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("Site_GrazeTravelDisance");
			}
			set
			{
				((SettingsBase)this).set_Item("Site_GrazeTravelDisance", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("10")]
		public decimal Site_TelescopeAperture_cm
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("Site_TelescopeAperture_cm");
			}
			set
			{
				((SettingsBase)this).set_Item("Site_TelescopeAperture_cm", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public decimal Site_MagnitudeCorrection
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("Site_MagnitudeCorrection");
			}
			set
			{
				((SettingsBase)this).set_Item("Site_MagnitudeCorrection", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("1")]
		public int Site_PlotControl
		{
			get
			{
				return (int)((SettingsBase)this).get_Item("Site_PlotControl");
			}
			set
			{
				((SettingsBase)this).set_Item("Site_PlotControl", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool UseXMLforSiteFiles
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("UseXMLforSiteFiles");
			}
			set
			{
				((SettingsBase)this).set_Item("UseXMLforSiteFiles", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public int Sites_EditorSortOrder
		{
			get
			{
				return (int)((SettingsBase)this).get_Item("Sites_EditorSortOrder");
			}
			set
			{
				((SettingsBase)this).set_Item("Sites_EditorSortOrder", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("null")]
		public string SiteFileForAutoMap
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("SiteFileForAutoMap");
			}
			set
			{
				((SettingsBase)this).set_Item("SiteFileForAutoMap", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("null")]
		public string SiteFileForNormalMap
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("SiteFileForNormalMap");
			}
			set
			{
				((SettingsBase)this).set_Item("SiteFileForNormalMap", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("null")]
		public string SiteFileForMultiMap
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("SiteFileForMultiMap");
			}
			set
			{
				((SettingsBase)this).set_Item("SiteFileForMultiMap", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("null")]
		public string SiteFileForEclipseMap
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("SiteFileForEclipseMap");
			}
			set
			{
				((SettingsBase)this).set_Item("SiteFileForEclipseMap", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("null")]
		public string SiteFileLunarPredict
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("SiteFileLunarPredict");
			}
			set
			{
				((SettingsBase)this).set_Item("SiteFileLunarPredict", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("null")]
		public string ObserverLunarPredict
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("ObserverLunarPredict");
			}
			set
			{
				((SettingsBase)this).set_Item("ObserverLunarPredict", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("200")]
		public decimal Graze_TravelDistance_65
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("Graze_TravelDistance_65");
			}
			set
			{
				((SettingsBase)this).set_Item("Graze_TravelDistance_65", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("400")]
		public decimal Graze_TravelDistance_45
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("Graze_TravelDistance_45");
			}
			set
			{
				((SettingsBase)this).set_Item("Graze_TravelDistance_45", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("145")]
		public decimal GrazeStartLongitude
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("GrazeStartLongitude");
			}
			set
			{
				((SettingsBase)this).set_Item("GrazeStartLongitude", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("155")]
		public decimal GrazeEndLongitude
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("GrazeEndLongitude");
			}
			set
			{
				((SettingsBase)this).set_Item("GrazeEndLongitude", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0.5")]
		public decimal GrazeStepInterval
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("GrazeStepInterval");
			}
			set
			{
				((SettingsBase)this).set_Item("GrazeStepInterval", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("100")]
		public decimal GrazeNominalAlt
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("GrazeNominalAlt");
			}
			set
			{
				((SettingsBase)this).set_Item("GrazeNominalAlt", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool Lunar_IncludeStars
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("Lunar_IncludeStars");
			}
			set
			{
				((SettingsBase)this).set_Item("Lunar_IncludeStars", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool Lunar_IncludePlanets
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("Lunar_IncludePlanets");
			}
			set
			{
				((SettingsBase)this).set_Item("Lunar_IncludePlanets", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool GrazeProfile_in_km
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("GrazeProfile_in_km");
			}
			set
			{
				((SettingsBase)this).set_Item("GrazeProfile_in_km", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool GrazeProfile_LimitToGrazes
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("GrazeProfile_LimitToGrazes");
			}
			set
			{
				((SettingsBase)this).set_Item("GrazeProfile_LimitToGrazes", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool GrazeProfile_EventsInColour
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("GrazeProfile_EventsInColour");
			}
			set
			{
				((SettingsBase)this).set_Item("GrazeProfile_EventsInColour", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool Grazes_DMScoords
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("Grazes_DMScoords");
			}
			set
			{
				((SettingsBase)this).set_Item("Grazes_DMScoords", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool Lunar_IncludeAsteroids
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("Lunar_IncludeAsteroids");
			}
			set
			{
				((SettingsBase)this).set_Item("Lunar_IncludeAsteroids", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool GrazeSelection_North
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("GrazeSelection_North");
			}
			set
			{
				((SettingsBase)this).set_Item("GrazeSelection_North", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("-5")]
		public decimal Graze_DefaultStartLongitude
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("Graze_DefaultStartLongitude");
			}
			set
			{
				((SettingsBase)this).set_Item("Graze_DefaultStartLongitude", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("5")]
		public decimal Graze_DefaultEndLongitude
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("Graze_DefaultEndLongitude");
			}
			set
			{
				((SettingsBase)this).set_Item("Graze_DefaultEndLongitude", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0.5")]
		public decimal Graze_DefaultStep
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("Graze_DefaultStep");
			}
			set
			{
				((SettingsBase)this).set_Item("Graze_DefaultStep", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("100")]
		public decimal Graze_DefaultAltitude
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("Graze_DefaultAltitude");
			}
			set
			{
				((SettingsBase)this).set_Item("Graze_DefaultAltitude", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool MoonMapMirror
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("MoonMapMirror");
			}
			set
			{
				((SettingsBase)this).set_Item("MoonMapMirror", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool MoonMapNames
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("MoonMapNames");
			}
			set
			{
				((SettingsBase)this).set_Item("MoonMapNames", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool LunarElements_FilterBySite
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("LunarElements_FilterBySite");
			}
			set
			{
				((SettingsBase)this).set_Item("LunarElements_FilterBySite", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public int GrazeProfile_LibrationPlot
		{
			get
			{
				return (int)((SettingsBase)this).get_Item("GrazeProfile_LibrationPlot");
			}
			set
			{
				((SettingsBase)this).set_Item("GrazeProfile_LibrationPlot", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool GoogleEarthInstalled
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("GoogleEarthInstalled");
			}
			set
			{
				((SettingsBase)this).set_Item("GoogleEarthInstalled", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool LunarZipFiles
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("LunarZipFiles");
			}
			set
			{
				((SettingsBase)this).set_Item("LunarZipFiles", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool LunarDeleteZippedFiles
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("LunarDeleteZippedFiles");
			}
			set
			{
				((SettingsBase)this).set_Item("LunarDeleteZippedFiles", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("3")]
		public int GrazeProfile_FileFormat
		{
			get
			{
				return (int)((SettingsBase)this).get_Item("GrazeProfile_FileFormat");
			}
			set
			{
				((SettingsBase)this).set_Item("GrazeProfile_FileFormat", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("null")]
		public string SiteFileLunarMultiPredict
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("SiteFileLunarMultiPredict");
			}
			set
			{
				((SettingsBase)this).set_Item("SiteFileLunarMultiPredict", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool CallUpgradeDLL
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("CallUpgradeDLL");
			}
			set
			{
				((SettingsBase)this).set_Item("CallUpgradeDLL", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("null")]
		public string HomeSiteFileLunarPredict
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("HomeSiteFileLunarPredict");
			}
			set
			{
				((SettingsBase)this).set_Item("HomeSiteFileLunarPredict", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("null")]
		public string HomeSiteLunarPredict
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("HomeSiteLunarPredict");
			}
			set
			{
				((SettingsBase)this).set_Item("HomeSiteLunarPredict", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool LunarIncludeGoogleEarthKML
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("LunarIncludeGoogleEarthKML");
			}
			set
			{
				((SettingsBase)this).set_Item("LunarIncludeGoogleEarthKML", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool LunarIncludeProfile
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("LunarIncludeProfile");
			}
			set
			{
				((SettingsBase)this).set_Item("LunarIncludeProfile", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool GrazeProfileSaveSmall
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("GrazeProfileSaveSmall");
			}
			set
			{
				((SettingsBase)this).set_Item("GrazeProfileSaveSmall", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool GrazeProfileSaveMedium
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("GrazeProfileSaveMedium");
			}
			set
			{
				((SettingsBase)this).set_Item("GrazeProfileSaveMedium", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool GrazeProfileSaveLarge
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("GrazeProfileSaveLarge");
			}
			set
			{
				((SettingsBase)this).set_Item("GrazeProfileSaveLarge", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool LunarIncludeGrazeText
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("LunarIncludeGrazeText");
			}
			set
			{
				((SettingsBase)this).set_Item("LunarIncludeGrazeText", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool LunarIncludeHTML
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("LunarIncludeHTML");
			}
			set
			{
				((SettingsBase)this).set_Item("LunarIncludeHTML", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool LunarIncludeLocalHTML
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("LunarIncludeLocalHTML");
			}
			set
			{
				((SettingsBase)this).set_Item("LunarIncludeLocalHTML", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool LunarRegionalGrazeMap
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("LunarRegionalGrazeMap");
			}
			set
			{
				((SettingsBase)this).set_Item("LunarRegionalGrazeMap", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string FilePathMapsCMX
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("FilePathMapsCMX");
			}
			set
			{
				((SettingsBase)this).set_Item("FilePathMapsCMX", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string FilePathMapsGPX
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("FilePathMapsGPX");
			}
			set
			{
				((SettingsBase)this).set_Item("FilePathMapsGPX", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string FilePathMapsPLT
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("FilePathMapsPLT");
			}
			set
			{
				((SettingsBase)this).set_Item("FilePathMapsPLT", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string FilePathMapsGEN
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("FilePathMapsGEN");
			}
			set
			{
				((SettingsBase)this).set_Item("FilePathMapsGEN", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string OccelmntDirectory
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("OccelmntDirectory");
			}
			set
			{
				((SettingsBase)this).set_Item("OccelmntDirectory", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string FilePathMapsDeLorme
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("FilePathMapsDeLorme");
			}
			set
			{
				((SettingsBase)this).set_Item("FilePathMapsDeLorme", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public string LunarSingleSiteLongDeg
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("LunarSingleSiteLongDeg");
			}
			set
			{
				((SettingsBase)this).set_Item("LunarSingleSiteLongDeg", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public string LunarSingleSiteLongMin
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("LunarSingleSiteLongMin");
			}
			set
			{
				((SettingsBase)this).set_Item("LunarSingleSiteLongMin", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public string LunarSingleSiteLongSec
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("LunarSingleSiteLongSec");
			}
			set
			{
				((SettingsBase)this).set_Item("LunarSingleSiteLongSec", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public string LunarSingleSiteLatDeg
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("LunarSingleSiteLatDeg");
			}
			set
			{
				((SettingsBase)this).set_Item("LunarSingleSiteLatDeg", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public string LunarSingleSiteLatMin
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("LunarSingleSiteLatMin");
			}
			set
			{
				((SettingsBase)this).set_Item("LunarSingleSiteLatMin", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public string LunarSingleSiteLatSec
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("LunarSingleSiteLatSec");
			}
			set
			{
				((SettingsBase)this).set_Item("LunarSingleSiteLatSec", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public string LunarSingleSiteAlt
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("LunarSingleSiteAlt");
			}
			set
			{
				((SettingsBase)this).set_Item("LunarSingleSiteAlt", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("Test")]
		public string LunarSingleSiteName
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("LunarSingleSiteName");
			}
			set
			{
				((SettingsBase)this).set_Item("LunarSingleSiteName", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("15")]
		public decimal LunarSingleSiteAperture
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("LunarSingleSiteAperture");
			}
			set
			{
				((SettingsBase)this).set_Item("LunarSingleSiteAperture", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool LunarNoSpacesInFileName
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("LunarNoSpacesInFileName");
			}
			set
			{
				((SettingsBase)this).set_Item("LunarNoSpacesInFileName", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("World")]
		public string GMapName0
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("GMapName0");
			}
			set
			{
				((SettingsBase)this).set_Item("GMapName0", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("Australasia")]
		public string GMapName1
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("GMapName1");
			}
			set
			{
				((SettingsBase)this).set_Item("GMapName1", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("Europe")]
		public string GMapName2
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("GMapName2");
			}
			set
			{
				((SettingsBase)this).set_Item("GMapName2", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("Nth America")]
		public string GMapName3
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("GMapName3");
			}
			set
			{
				((SettingsBase)this).set_Item("GMapName3", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("Sth America")]
		public string GMapName4
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("GMapName4");
			}
			set
			{
				((SettingsBase)this).set_Item("GMapName4", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("East Asia")]
		public string GMapName5
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("GMapName5");
			}
			set
			{
				((SettingsBase)this).set_Item("GMapName5", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string GMapName6
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("GMapName6");
			}
			set
			{
				((SettingsBase)this).set_Item("GMapName6", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("-180")]
		public decimal GMapW0
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("GMapW0");
			}
			set
			{
				((SettingsBase)this).set_Item("GMapW0", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("110")]
		public decimal GMapW1
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("GMapW1");
			}
			set
			{
				((SettingsBase)this).set_Item("GMapW1", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("-10")]
		public decimal GMapW2
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("GMapW2");
			}
			set
			{
				((SettingsBase)this).set_Item("GMapW2", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("-130")]
		public decimal GMapW3
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("GMapW3");
			}
			set
			{
				((SettingsBase)this).set_Item("GMapW3", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("-85")]
		public decimal GMapW4
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("GMapW4");
			}
			set
			{
				((SettingsBase)this).set_Item("GMapW4", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("90")]
		public decimal GMapW5
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("GMapW5");
			}
			set
			{
				((SettingsBase)this).set_Item("GMapW5", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public decimal GMapW6
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("GMapW6");
			}
			set
			{
				((SettingsBase)this).set_Item("GMapW6", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("180")]
		public decimal GMapE0
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("GMapE0");
			}
			set
			{
				((SettingsBase)this).set_Item("GMapE0", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("179")]
		public decimal GMapE1
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("GMapE1");
			}
			set
			{
				((SettingsBase)this).set_Item("GMapE1", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("40")]
		public decimal GMapE2
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("GMapE2");
			}
			set
			{
				((SettingsBase)this).set_Item("GMapE2", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("-50")]
		public decimal GMapE3
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("GMapE3");
			}
			set
			{
				((SettingsBase)this).set_Item("GMapE3", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("-30")]
		public decimal GMapE4
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("GMapE4");
			}
			set
			{
				((SettingsBase)this).set_Item("GMapE4", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("150")]
		public decimal GMapE5
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("GMapE5");
			}
			set
			{
				((SettingsBase)this).set_Item("GMapE5", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public decimal GMapE6
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("GMapE6");
			}
			set
			{
				((SettingsBase)this).set_Item("GMapE6", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("90")]
		public decimal GMapN0
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("GMapN0");
			}
			set
			{
				((SettingsBase)this).set_Item("GMapN0", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("-90")]
		public decimal GMapS0
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("GMapS0");
			}
			set
			{
				((SettingsBase)this).set_Item("GMapS0", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("-15")]
		public decimal GMapN1
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("GMapN1");
			}
			set
			{
				((SettingsBase)this).set_Item("GMapN1", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("-52")]
		public decimal GMapS1
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("GMapS1");
			}
			set
			{
				((SettingsBase)this).set_Item("GMapS1", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("72")]
		public decimal GMapN2
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("GMapN2");
			}
			set
			{
				((SettingsBase)this).set_Item("GMapN2", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("35")]
		public decimal GMapS2
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("GMapS2");
			}
			set
			{
				((SettingsBase)this).set_Item("GMapS2", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("55")]
		public decimal GMapN3
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("GMapN3");
			}
			set
			{
				((SettingsBase)this).set_Item("GMapN3", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("20")]
		public decimal GMapS3
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("GMapS3");
			}
			set
			{
				((SettingsBase)this).set_Item("GMapS3", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("15")]
		public decimal GMapN4
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("GMapN4");
			}
			set
			{
				((SettingsBase)this).set_Item("GMapN4", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("-30")]
		public decimal GMapS4
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("GMapS4");
			}
			set
			{
				((SettingsBase)this).set_Item("GMapS4", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("55")]
		public decimal GMapN5
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("GMapN5");
			}
			set
			{
				((SettingsBase)this).set_Item("GMapN5", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("10")]
		public decimal GMapS5
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("GMapS5");
			}
			set
			{
				((SettingsBase)this).set_Item("GMapS5", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public decimal GMapN6
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("GMapN6");
			}
			set
			{
				((SettingsBase)this).set_Item("GMapN6", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public decimal GMapS6
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("GMapS6");
			}
			set
			{
				((SettingsBase)this).set_Item("GMapS6", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public decimal AsteroidSearchExpandedMissDistance
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("AsteroidSearchExpandedMissDistance");
			}
			set
			{
				((SettingsBase)this).set_Item("AsteroidSearchExpandedMissDistance", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("2575")]
		public decimal Radius_Titan
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("Radius_Titan");
			}
			set
			{
				((SettingsBase)this).set_Item("Radius_Titan", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("1353")]
		public decimal Radius_Triton
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("Radius_Triton");
			}
			set
			{
				((SettingsBase)this).set_Item("Radius_Triton", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("71492")]
		public decimal Radius_Jupiter
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("Radius_Jupiter");
			}
			set
			{
				((SettingsBase)this).set_Item("Radius_Jupiter", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("60268")]
		public decimal Radius_Saturn
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("Radius_Saturn");
			}
			set
			{
				((SettingsBase)this).set_Item("Radius_Saturn", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("25559")]
		public decimal Radius_Uranus
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("Radius_Uranus");
			}
			set
			{
				((SettingsBase)this).set_Item("Radius_Uranus", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("24764")]
		public decimal Radius_Neptune
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("Radius_Neptune");
			}
			set
			{
				((SettingsBase)this).set_Item("Radius_Neptune", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("1188")]
		public decimal Radius_Pluto
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("Radius_Pluto");
			}
			set
			{
				((SettingsBase)this).set_Item("Radius_Pluto", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string NOMAD_Path
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("NOMAD_Path");
			}
			set
			{
				((SettingsBase)this).set_Item("NOMAD_Path", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string UCAC3_Path
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("UCAC3_Path");
			}
			set
			{
				((SettingsBase)this).set_Item("UCAC3_Path", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool StarChartUseNomad
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("StarChartUseNomad");
			}
			set
			{
				((SettingsBase)this).set_Item("StarChartUseNomad", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string OBS_File_LastName
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("OBS_File_LastName");
			}
			set
			{
				((SettingsBase)this).set_Item("OBS_File_LastName", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public int OBS_File_LastIndex
		{
			get
			{
				return (int)((SettingsBase)this).get_Item("OBS_File_LastIndex");
			}
			set
			{
				((SettingsBase)this).set_Item("OBS_File_LastIndex", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string OBS_FileDirectory
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("OBS_FileDirectory");
			}
			set
			{
				((SettingsBase)this).set_Item("OBS_FileDirectory", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string NOMAD_Short_path
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("NOMAD_Short_path");
			}
			set
			{
				((SettingsBase)this).set_Item("NOMAD_Short_path", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool AsterPlotShowSolution
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsterPlotShowSolution");
			}
			set
			{
				((SettingsBase)this).set_Item("AsterPlotShowSolution", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool AsterPlotShowEllipse
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsterPlotShowEllipse");
			}
			set
			{
				((SettingsBase)this).set_Item("AsterPlotShowEllipse", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool AsterPlotShowAxes
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsterPlotShowAxes");
			}
			set
			{
				((SettingsBase)this).set_Item("AsterPlotShowAxes", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool AsterPlotColour
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsterPlotColour");
			}
			set
			{
				((SettingsBase)this).set_Item("AsterPlotColour", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string OCC_File_LastName
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("OCC_File_LastName");
			}
			set
			{
				((SettingsBase)this).set_Item("OCC_File_LastName", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public int OCC_File_LastIndex
		{
			get
			{
				return (int)((SettingsBase)this).get_Item("OCC_File_LastIndex");
			}
			set
			{
				((SettingsBase)this).set_Item("OCC_File_LastIndex", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string OCC_FileDirectory
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("OCC_FileDirectory");
			}
			set
			{
				((SettingsBase)this).set_Item("OCC_FileDirectory", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string ILOC_Place
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("ILOC_Place");
			}
			set
			{
				((SettingsBase)this).set_Item("ILOC_Place", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string ILOC_Address
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("ILOC_Address");
			}
			set
			{
				((SettingsBase)this).set_Item("ILOC_Address", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string ILOC_Email
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("ILOC_Email");
			}
			set
			{
				((SettingsBase)this).set_Item("ILOC_Email", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string ILOC_Representative
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("ILOC_Representative");
			}
			set
			{
				((SettingsBase)this).set_Item("ILOC_Representative", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string ILOC_ReportedTo
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("ILOC_ReportedTo");
			}
			set
			{
				((SettingsBase)this).set_Item("ILOC_ReportedTo", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string ILOC_DefaultSite
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("ILOC_DefaultSite");
			}
			set
			{
				((SettingsBase)this).set_Item("ILOC_DefaultSite", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string ILOC_DefaultName
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("ILOC_DefaultName");
			}
			set
			{
				((SettingsBase)this).set_Item("ILOC_DefaultName", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool ReductionProfile_LimitToGrazes
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("ReductionProfile_LimitToGrazes");
			}
			set
			{
				((SettingsBase)this).set_Item("ReductionProfile_LimitToGrazes", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool ReductionProfile_EventsInColour
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("ReductionProfile_EventsInColour");
			}
			set
			{
				((SettingsBase)this).set_Item("ReductionProfile_EventsInColour", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public decimal MapWest5
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("MapWest5");
			}
			set
			{
				((SettingsBase)this).set_Item("MapWest5", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public decimal MapWest6
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("MapWest6");
			}
			set
			{
				((SettingsBase)this).set_Item("MapWest6", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public decimal MapWest7
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("MapWest7");
			}
			set
			{
				((SettingsBase)this).set_Item("MapWest7", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public decimal MapWest8
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("MapWest8");
			}
			set
			{
				((SettingsBase)this).set_Item("MapWest8", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public decimal MapEast5
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("MapEast5");
			}
			set
			{
				((SettingsBase)this).set_Item("MapEast5", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public decimal MapEast6
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("MapEast6");
			}
			set
			{
				((SettingsBase)this).set_Item("MapEast6", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public decimal MapEast7
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("MapEast7");
			}
			set
			{
				((SettingsBase)this).set_Item("MapEast7", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public decimal MapEast8
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("MapEast8");
			}
			set
			{
				((SettingsBase)this).set_Item("MapEast8", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public decimal MapNorth5
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("MapNorth5");
			}
			set
			{
				((SettingsBase)this).set_Item("MapNorth5", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public decimal MapNorth6
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("MapNorth6");
			}
			set
			{
				((SettingsBase)this).set_Item("MapNorth6", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public decimal MapNorth7
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("MapNorth7");
			}
			set
			{
				((SettingsBase)this).set_Item("MapNorth7", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public decimal MapNorth8
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("MapNorth8");
			}
			set
			{
				((SettingsBase)this).set_Item("MapNorth8", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public decimal MapSouth5
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("MapSouth5");
			}
			set
			{
				((SettingsBase)this).set_Item("MapSouth5", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public decimal MapSouth6
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("MapSouth6");
			}
			set
			{
				((SettingsBase)this).set_Item("MapSouth6", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public decimal MapSouth7
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("MapSouth7");
			}
			set
			{
				((SettingsBase)this).set_Item("MapSouth7", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public decimal MapSouth8
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("MapSouth8");
			}
			set
			{
				((SettingsBase)this).set_Item("MapSouth8", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool Administrator
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("Administrator");
			}
			set
			{
				((SettingsBase)this).set_Item("Administrator", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("20")]
		public decimal ReportCentury
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("ReportCentury");
			}
			set
			{
				((SettingsBase)this).set_Item("ReportCentury", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string MPC_CON
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("MPC_CON");
			}
			set
			{
				((SettingsBase)this).set_Item("MPC_CON", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string MPC_MEA
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("MPC_MEA");
			}
			set
			{
				((SettingsBase)this).set_Item("MPC_MEA", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string MPC_ACK
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("MPC_ACK");
			}
			set
			{
				((SettingsBase)this).set_Item("MPC_ACK", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool MapRegionUseFirst
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("MapRegionUseFirst");
			}
			set
			{
				((SettingsBase)this).set_Item("MapRegionUseFirst", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public int StartHour
		{
			get
			{
				return (int)((SettingsBase)this).get_Item("StartHour");
			}
			set
			{
				((SettingsBase)this).set_Item("StartHour", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool AsterPlot_Colour
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsterPlot_Colour");
			}
			set
			{
				((SettingsBase)this).set_Item("AsterPlot_Colour", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool AsterPlot_ShowSolution
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsterPlot_ShowSolution");
			}
			set
			{
				((SettingsBase)this).set_Item("AsterPlot_ShowSolution", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool AsterPlot_ShowEllipse
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsterPlot_ShowEllipse");
			}
			set
			{
				((SettingsBase)this).set_Item("AsterPlot_ShowEllipse", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool AsterPlot_ShowAxes
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsterPlot_ShowAxes");
			}
			set
			{
				((SettingsBase)this).set_Item("AsterPlot_ShowAxes", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool AsterPlot_AlignDoubles
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsterPlot_AlignDoubles");
			}
			set
			{
				((SettingsBase)this).set_Item("AsterPlot_AlignDoubles", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool AsterPlot_IncludeTimeShift
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsterPlot_IncludeTimeShift");
			}
			set
			{
				((SettingsBase)this).set_Item("AsterPlot_IncludeTimeShift", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool AsterPlot_ShowMarkers
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsterPlot_ShowMarkers");
			}
			set
			{
				((SettingsBase)this).set_Item("AsterPlot_ShowMarkers", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool AsterPlot_ShowNumbers
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsterPlot_ShowNumbers");
			}
			set
			{
				((SettingsBase)this).set_Item("AsterPlot_ShowNumbers", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool AsterPlot_VideoOnly
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsterPlot_VideoOnly");
			}
			set
			{
				((SettingsBase)this).set_Item("AsterPlot_VideoOnly", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool AsterPlot_ShowErrors
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsterPlot_ShowErrors");
			}
			set
			{
				((SettingsBase)this).set_Item("AsterPlot_ShowErrors", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool AsterPlot_ShowOcculted
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsterPlot_ShowOcculted");
			}
			set
			{
				((SettingsBase)this).set_Item("AsterPlot_ShowOcculted", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool AsterPlot_ShowVisible
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsterPlot_ShowVisible");
			}
			set
			{
				((SettingsBase)this).set_Item("AsterPlot_ShowVisible", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool AsterPlot_ShowMiss
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsterPlot_ShowMiss");
			}
			set
			{
				((SettingsBase)this).set_Item("AsterPlot_ShowMiss", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool AsterPlot_ShowZeroWeighted
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsterPlot_ShowZeroWeighted");
			}
			set
			{
				((SettingsBase)this).set_Item("AsterPlot_ShowZeroWeighted", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool AsterPlot_ShowPredicted
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsterPlot_ShowPredicted");
			}
			set
			{
				((SettingsBase)this).set_Item("AsterPlot_ShowPredicted", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool AsterPlot_ShowClouded
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsterPlot_ShowClouded");
			}
			set
			{
				((SettingsBase)this).set_Item("AsterPlot_ShowClouded", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool AutoAsteroidColour
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AutoAsteroidColour");
			}
			set
			{
				((SettingsBase)this).set_Item("AutoAsteroidColour", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool AutoAsteroidBessel
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AutoAsteroidBessel");
			}
			set
			{
				((SettingsBase)this).set_Item("AutoAsteroidBessel", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool AsteroidPlot_SolidErrorLines
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsteroidPlot_SolidErrorLines");
			}
			set
			{
				((SettingsBase)this).set_Item("AsteroidPlot_SolidErrorLines", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool SummaryCombinedMag
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("SummaryCombinedMag");
			}
			set
			{
				((SettingsBase)this).set_Item("SummaryCombinedMag", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool Grazes_DDDcoords
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("Grazes_DDDcoords");
			}
			set
			{
				((SettingsBase)this).set_Item("Grazes_DDDcoords", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool Grazes_DMMcoords
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("Grazes_DMMcoords");
			}
			set
			{
				((SettingsBase)this).set_Item("Grazes_DMMcoords", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0.05")]
		public decimal GIFdelay
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("GIFdelay");
			}
			set
			{
				((SettingsBase)this).set_Item("GIFdelay", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public decimal GIFRepeats
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("GIFRepeats");
			}
			set
			{
				((SettingsBase)this).set_Item("GIFRepeats", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool DisplayOrbits
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("DisplayOrbits");
			}
			set
			{
				((SettingsBase)this).set_Item("DisplayOrbits", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool IncludeSmallMoons
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("IncludeSmallMoons");
			}
			set
			{
				((SettingsBase)this).set_Item("IncludeSmallMoons", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public int GraphicsAutoSaveFileType
		{
			get
			{
				return (int)((SettingsBase)this).get_Item("GraphicsAutoSaveFileType");
			}
			set
			{
				((SettingsBase)this).set_Item("GraphicsAutoSaveFileType", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool AutoGenerateUseNomad
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AutoGenerateUseNomad");
			}
			set
			{
				((SettingsBase)this).set_Item("AutoGenerateUseNomad", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("2")]
		public int AutoGenerateStarWidthPixels
		{
			get
			{
				return (int)((SettingsBase)this).get_Item("AutoGenerateStarWidthPixels");
			}
			set
			{
				((SettingsBase)this).set_Item("AutoGenerateStarWidthPixels", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("5")]
		public int AutoGenerateStarWidthDeg
		{
			get
			{
				return (int)((SettingsBase)this).get_Item("AutoGenerateStarWidthDeg");
			}
			set
			{
				((SettingsBase)this).set_Item("AutoGenerateStarWidthDeg", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("5")]
		public int AutoGenerateStarMag
		{
			get
			{
				return (int)((SettingsBase)this).get_Item("AutoGenerateStarMag");
			}
			set
			{
				((SettingsBase)this).set_Item("AutoGenerateStarMag", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool MoonMapTagPaths
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("MoonMapTagPaths");
			}
			set
			{
				((SettingsBase)this).set_Item("MoonMapTagPaths", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool MoonMapCraterNames
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("MoonMapCraterNames");
			}
			set
			{
				((SettingsBase)this).set_Item("MoonMapCraterNames", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool GraphicsSmoothed
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("GraphicsSmoothed");
			}
			set
			{
				((SettingsBase)this).set_Item("GraphicsSmoothed", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool PreserveFuture_dat
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("PreserveFuture_dat");
			}
			set
			{
				((SettingsBase)this).set_Item("PreserveFuture_dat", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("eopc01.iau2000.1846-now")]
		public string EOPpre62_File
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("EOPpre62_File");
			}
			set
			{
				((SettingsBase)this).set_Item("EOPpre62_File", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("ftp://hpiers.obspm.fr/iers/eop/eopc01/")]
		public string EOPpre62_Server
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("EOPpre62_Server");
			}
			set
			{
				((SettingsBase)this).set_Item("EOPpre62_Server", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("eopc04_IAU2000.62-now")]
		public string EOPpost62_File
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("EOPpost62_File");
			}
			set
			{
				((SettingsBase)this).set_Item("EOPpost62_File", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("ftp://hpiers.obspm.fr/iers/eop/eopc04/")]
		public string EOPpost62_Server
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("EOPpost62_Server");
			}
			set
			{
				((SettingsBase)this).set_Item("EOPpost62_Server", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string LocalArchiveLastFile
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("LocalArchiveLastFile");
			}
			set
			{
				((SettingsBase)this).set_Item("LocalArchiveLastFile", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string EMailServerName
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("EMailServerName");
			}
			set
			{
				((SettingsBase)this).set_Item("EMailServerName", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string Current_DE_Ephemeris
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("Current_DE_Ephemeris");
			}
			set
			{
				((SettingsBase)this).set_Item("Current_DE_Ephemeris", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool AsteroidUseCombinedMagnitudes
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsteroidUseCombinedMagnitudes");
			}
			set
			{
				((SettingsBase)this).set_Item("AsteroidUseCombinedMagnitudes", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("6")]
		public int GrazeProfile_LibrationPlot_Reduction
		{
			get
			{
				return (int)((SettingsBase)this).get_Item("GrazeProfile_LibrationPlot_Reduction");
			}
			set
			{
				((SettingsBase)this).set_Item("GrazeProfile_LibrationPlot_Reduction", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string ArchiveFile_C
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("ArchiveFile_C");
			}
			set
			{
				((SettingsBase)this).set_Item("ArchiveFile_C", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string LastLunarReportAddress
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("LastLunarReportAddress");
			}
			set
			{
				((SettingsBase)this).set_Item("LastLunarReportAddress", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string Email
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("Email");
			}
			set
			{
				((SettingsBase)this).set_Item("Email", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string EmailUser
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("EmailUser");
			}
			set
			{
				((SettingsBase)this).set_Item("EmailUser", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool AdministratorRegional
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AdministratorRegional");
			}
			set
			{
				((SettingsBase)this).set_Item("AdministratorRegional", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool AdministratorGlobal
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AdministratorGlobal");
			}
			set
			{
				((SettingsBase)this).set_Item("AdministratorGlobal", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool EmailUseSSL
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("EmailUseSSL");
			}
			set
			{
				((SettingsBase)this).set_Item("EmailUseSSL", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string EmailPort
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("EmailPort");
			}
			set
			{
				((SettingsBase)this).set_Item("EmailPort", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("30")]
		public decimal DisplayUpdatesFrequency
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("DisplayUpdatesFrequency");
			}
			set
			{
				((SettingsBase)this).set_Item("DisplayUpdatesFrequency", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("2007-01-01")]
		public DateTime UpdatePageLastDisplayed
		{
			get
			{
				return (DateTime)((SettingsBase)this).get_Item("UpdatePageLastDisplayed");
			}
			set
			{
				((SettingsBase)this).set_Item("UpdatePageLastDisplayed", (object)value);
			}
		}

		[ApplicationScopedSetting]
		[DebuggerNonUserCode]
		[SpecialSetting(/*Could not decode attribute arguments.*/)]
		[DefaultSettingValue("http://cdsws.u-strasbg.fr/axis/services/Sesame")]
		public string OccultUtilities_fr_u_strasbg_cdsws_SesameService => (string)((SettingsBase)this).get_Item("OccultUtilities_fr_u_strasbg_cdsws_SesameService");

		[ApplicationScopedSetting]
		[DebuggerNonUserCode]
		[SpecialSetting(/*Could not decode attribute arguments.*/)]
		[DefaultSettingValue("http://cdsws.u-strasbg.fr/axis/services/VizieR")]
		public string OccultUtilities_fr_u_strasbg_cdsws1_VizieRService => (string)((SettingsBase)this).get_Item("OccultUtilities_fr_u_strasbg_cdsws1_VizieRService");

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool LunarMultiSitePrediction_SunAlt
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("LunarMultiSitePrediction_SunAlt");
			}
			set
			{
				((SettingsBase)this).set_Item("LunarMultiSitePrediction_SunAlt", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string OELfile
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("OELfile");
			}
			set
			{
				((SettingsBase)this).set_Item("OELfile", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool AsterPlot_ShowScaleKM
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsterPlot_ShowScaleKM");
			}
			set
			{
				((SettingsBase)this).set_Item("AsterPlot_ShowScaleKM", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool AsterPlot_ChordsIncolour
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsterPlot_ChordsIncolour");
			}
			set
			{
				((SettingsBase)this).set_Item("AsterPlot_ChordsIncolour", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool AsterPlot_ShowEllipseInOutline
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsterPlot_ShowEllipseInOutline");
			}
			set
			{
				((SettingsBase)this).set_Item("AsterPlot_ShowEllipseInOutline", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool AsterPlot_ThickLines_Paths
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsterPlot_ThickLines_Paths");
			}
			set
			{
				((SettingsBase)this).set_Item("AsterPlot_ThickLines_Paths", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("2")]
		public decimal AsterPlot_LineThickness_Path
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("AsterPlot_LineThickness_Path");
			}
			set
			{
				((SettingsBase)this).set_Item("AsterPlot_LineThickness_Path", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("10")]
		public decimal AsterPlot_LargeFontSize
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("AsterPlot_LargeFontSize");
			}
			set
			{
				((SettingsBase)this).set_Item("AsterPlot_LargeFontSize", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool AsterPlot_LargeFont
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsterPlot_LargeFont");
			}
			set
			{
				((SettingsBase)this).set_Item("AsterPlot_LargeFont", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("100")]
		public string GoogleEarthHeightForLunarLimb
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("GoogleEarthHeightForLunarLimb");
			}
			set
			{
				((SettingsBase)this).set_Item("GoogleEarthHeightForLunarLimb", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("20")]
		public decimal EOPReminder
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("EOPReminder");
			}
			set
			{
				((SettingsBase)this).set_Item("EOPReminder", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string DoubleStarEmails
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("DoubleStarEmails");
			}
			set
			{
				((SettingsBase)this).set_Item("DoubleStarEmails", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string Save_SolarEclipses
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("Save_SolarEclipses");
			}
			set
			{
				((SettingsBase)this).set_Item("Save_SolarEclipses", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string Save_EphemerisData
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("Save_EphemerisData");
			}
			set
			{
				((SettingsBase)this).set_Item("Save_EphemerisData", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string Save_LunarEclipses
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("Save_LunarEclipses");
			}
			set
			{
				((SettingsBase)this).set_Item("Save_LunarEclipses", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string Save_Transits
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("Save_Transits");
			}
			set
			{
				((SettingsBase)this).set_Item("Save_Transits", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string Save_LunarPredictions
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("Save_LunarPredictions");
			}
			set
			{
				((SettingsBase)this).set_Item("Save_LunarPredictions", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string Save_LunarObservations
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("Save_LunarObservations");
			}
			set
			{
				((SettingsBase)this).set_Item("Save_LunarObservations", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string Save_AsteroidPredictions
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("Save_AsteroidPredictions");
			}
			set
			{
				((SettingsBase)this).set_Item("Save_AsteroidPredictions", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string Save_AsteroidObservations
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("Save_AsteroidObservations");
			}
			set
			{
				((SettingsBase)this).set_Item("Save_AsteroidObservations", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string Save_AsteroidResults
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("Save_AsteroidResults");
			}
			set
			{
				((SettingsBase)this).set_Item("Save_AsteroidResults", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string Save_LunarResults
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("Save_LunarResults");
			}
			set
			{
				((SettingsBase)this).set_Item("Save_LunarResults", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string Save_Stars
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("Save_Stars");
			}
			set
			{
				((SettingsBase)this).set_Item("Save_Stars", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string Save_LightCurve
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("Save_LightCurve");
			}
			set
			{
				((SettingsBase)this).set_Item("Save_LightCurve", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string Open_WorkingArchive
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("Open_WorkingArchive");
			}
			set
			{
				((SettingsBase)this).set_Item("Open_WorkingArchive", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationReductionProfile
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationReductionProfile");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationReductionProfile", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationObservationsEditor
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationObservationsEditor");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationObservationsEditor", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationListResiduals
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationListResiduals");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationListResiduals", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool ArchiveGrazesExcludeInvalid
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("ArchiveGrazesExcludeInvalid");
			}
			set
			{
				((SettingsBase)this).set_Item("ArchiveGrazesExcludeInvalid", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool ArchiveGrazesExcludeStartEnd
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("ArchiveGrazesExcludeStartEnd");
			}
			set
			{
				((SettingsBase)this).set_Item("ArchiveGrazesExcludeStartEnd", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool AsterPlotDrawLimb
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsterPlotDrawLimb");
			}
			set
			{
				((SettingsBase)this).set_Item("AsterPlotDrawLimb", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool ArchiveGrazesIncrement
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("ArchiveGrazesIncrement");
			}
			set
			{
				((SettingsBase)this).set_Item("ArchiveGrazesIncrement", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool ArchiveGrazesAutoDisplay
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("ArchiveGrazesAutoDisplay");
			}
			set
			{
				((SettingsBase)this).set_Item("ArchiveGrazesAutoDisplay", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string SignatureBlockName
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("SignatureBlockName");
			}
			set
			{
				((SettingsBase)this).set_Item("SignatureBlockName", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool MutualEvents_LimitToLocal
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("MutualEvents_LimitToLocal");
			}
			set
			{
				((SettingsBase)this).set_Item("MutualEvents_LimitToLocal", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool SatelliteEclipsesTransits_LimitToLocal
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("SatelliteEclipsesTransits_LimitToLocal");
			}
			set
			{
				((SettingsBase)this).set_Item("SatelliteEclipsesTransits_LimitToLocal", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool AutoAsteroidDoubleInfo
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AutoAsteroidDoubleInfo");
			}
			set
			{
				((SettingsBase)this).set_Item("AutoAsteroidDoubleInfo", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("autosearchWDSInterferometricToolStripMenuItem")]
		public string DoublesEditorAutosearchWDS_IF
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("DoublesEditorAutosearchWDS_IF");
			}
			set
			{
				((SettingsBase)this).set_Item("DoublesEditorAutosearchWDS_IF", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool AsteroidSearchNOMADExclude_B_N
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsteroidSearchNOMADExclude_B_N");
			}
			set
			{
				((SettingsBase)this).set_Item("AsteroidSearchNOMADExclude_B_N", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("ftp://cdsarc.cds.unistra.fr/pub/cats/B/wds/")]
		public string WDSDownloadServer
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("WDSDownloadServer");
			}
			set
			{
				((SettingsBase)this).set_Item("WDSDownloadServer", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("wds.dat.gz")]
		public string WDSDownload_File
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("WDSDownload_File");
			}
			set
			{
				((SettingsBase)this).set_Item("WDSDownload_File", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("http://www.astro.gsu.edu/wds/int4/")]
		public string IFdownloadServer
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("IFdownloadServer");
			}
			set
			{
				((SettingsBase)this).set_Item("IFdownloadServer", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("int4_all2.txt.gz")]
		public string IFdownloadFile
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("IFdownloadFile");
			}
			set
			{
				((SettingsBase)this).set_Item("IFdownloadFile", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("ftp://cdsarc.cds.unistra.fr/pub/cats/B/vsx/")]
		public string AAVSOdownloadServer
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("AAVSOdownloadServer");
			}
			set
			{
				((SettingsBase)this).set_Item("AAVSOdownloadServer", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("vsx_csv.dat")]
		public string AAVSOdownloadFile
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("AAVSOdownloadFile");
			}
			set
			{
				((SettingsBase)this).set_Item("AAVSOdownloadFile", (object)value);
			}
		}

		[ApplicationScopedSetting]
		[DebuggerNonUserCode]
		[SpecialSetting(/*Could not decode attribute arguments.*/)]
		[DefaultSettingValue("http://cdsws.u-strasbg.fr/axis/services/VizieRBeta")]
		public string OccultUtilities_fr_u_strasbg_cdsws_VizieRBetaService => (string)((SettingsBase)this).get_Item("OccultUtilities_fr_u_strasbg_cdsws_VizieRBetaService");

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationLunarChooseStar
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationLunarChooseStar");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationLunarChooseStar", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationLunarArchiveEditor
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationLunarArchiveEditor");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationLunarArchiveEditor", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationLunarDoubleList
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationLunarDoubleList");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationLunarDoubleList", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationLunarDoubleSolve
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationLunarDoubleSolve");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationLunarDoubleSolve", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationLunarDoubleReport
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationLunarDoubleReport");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationLunarDoubleReport", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationLunarReportAddresses
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationLunarReportAddresses");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationLunarReportAddresses", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationLunarEmail
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationLunarEmail");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationLunarEmail", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationLunarErrorReport
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationLunarErrorReport");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationLunarErrorReport", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationLunarGrazeMessage
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationLunarGrazeMessage");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationLunarGrazeMessage", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationLunarGrazeHistory
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationLunarGrazeHistory");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationLunarGrazeHistory", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationLunarHistorical
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationLunarHistorical");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationLunarHistorical", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationLunarListObservers
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationLunarListObservers");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationLunarListObservers", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationLunarListPlottedObservers
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationLunarListPlottedObservers");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationLunarListPlottedObservers", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationLunarMessageText
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationLunarMessageText");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationLunarMessageText", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationLunarSetEmailServer
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationLunarSetEmailServer");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationLunarSetEmailServer", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationLunarGrazeWeather
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationLunarGrazeWeather");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationLunarGrazeWeather", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationLunarPListObservers
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationLunarPListObservers");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationLunarPListObservers", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationLunarFindAnOccultation
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationLunarFindAnOccultation");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationLunarFindAnOccultation", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationLunarPlanetXYZ
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationLunarPlanetXYZ");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationLunarPlanetXYZ", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationLunarAutoGenerat
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationLunarAutoGenerat");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationLunarAutoGenerat", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationGrazeMagLimits
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationGrazeMagLimits");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationGrazeMagLimits", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationLunarGrazeProfile
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationLunarGrazeProfile");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationLunarGrazeProfile", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationLunarGrazeSelection
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationLunarGrazeSelection");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationLunarGrazeSelection", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationLunarGrazesUsed
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationLunarGrazesUsed");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationLunarGrazesUsed", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationLunarGrazeMap
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationLunarGrazeMap");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationLunarGrazeMap", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationLunarPredictions
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationLunarPredictions");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationLunarPredictions", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationLunarWorldMap
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationLunarWorldMap");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationLunarWorldMap", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationMoonInStarField
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationMoonInStarField");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationMoonInStarField", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationMoonMap
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationMoonMap");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationMoonMap", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationMoonSeriesViewer
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationMoonSeriesViewer");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationMoonSeriesViewer", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationLunarWorldMapAll
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationLunarWorldMapAll");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationLunarWorldMapAll", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationSatelliteMutuals
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationSatelliteMutuals");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationSatelliteMutuals", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationSatellitePlanet
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationSatellitePlanet");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationSatellitePlanet", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationSatellitePositions
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationSatellitePositions");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationSatellitePositions", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationStarsCompare
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationStarsCompare");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationStarsCompare", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationStarsIFMismatches
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationStarsIFMismatches");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationStarsIFMismatches", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationStarsCreateUserCatalogue
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationStarsCreateUserCatalogue");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationStarsCreateUserCatalogue", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationStarsNomad
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationStarsNomad");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationStarsNomad", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationStarsDisplayRanges
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationStarsDisplayRanges");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationStarsDisplayRanges", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationStarsCatalogueDtails
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationStarsCatalogueDtails");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationStarsCatalogueDtails", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationStarsEquivalents
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationStarsEquivalents");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationStarsEquivalents", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationStarsWDSIFdisplay
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationStarsWDSIFdisplay");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationStarsWDSIFdisplay", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationStarsXZManagement
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationStarsXZManagement");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationStarsXZManagement", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationStarsXZDoubles
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationStarsXZDoubles");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationStarsXZDoubles", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationStarsXZVariables
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationStarsXZVariables");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationStarsXZVariables", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationUserAsteroids
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationUserAsteroids");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationUserAsteroids", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationFileCloudForSite
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationFileCloudForSite");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationFileCloudForSite", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationFilesDownloads
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationFilesDownloads");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationFilesDownloads", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationFilesGIF
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationFilesGIF");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationFilesGIF", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationEphemCalendar
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationEphemCalendar");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationEphemCalendar", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationEphemComet
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationEphemComet");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationEphemComet", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationEphemDatums
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationEphemDatums");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationEphemDatums", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationEphemDiary
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationEphemDiary");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationEphemDiary", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationEphemJD
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationEphemJD");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationEphemJD", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationEphemMag
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationEphemMag");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationEphemMag", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationEphemMeridians
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationEphemMeridians");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationEphemMeridians", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationEphemMoonPhase
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationEphemMoonPhase");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationEphemMoonPhase", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationEphemMoonRiseSet
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationEphemMoonRiseSet");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationEphemMoonRiseSet", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationEphemConjunctions
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationEphemConjunctions");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationEphemConjunctions", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationEphemAltitude
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationEphemAltitude");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationEphemAltitude", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationEphemPlanets
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationEphemPlanets");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationEphemPlanets", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationEphemPlanetRiseSet
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationEphemPlanetRiseSet");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationEphemPlanetRiseSet", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationEphemPlanetViewer
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationEphemPlanetViewer");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationEphemPlanetViewer", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationEphemSiderial
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationEphemSiderial");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationEphemSiderial", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationEphemStarChart
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationEphemStarChart");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationEphemStarChart", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationEclipseLunar
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationEclipseLunar");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationEclipseLunar", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationEclipseLocalMap
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationEclipseLocalMap");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationEclipseLocalMap", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationEclipseLocalPredict
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationEclipseLocalPredict");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationEclipseLocalPredict", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationEclipseSolarMulti
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationEclipseSolarMulti");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationEclipseSolarMulti", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationEclipseSolarPath
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationEclipseSolarPath");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationEclipseSolarPath", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationEclipseSunWorld
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationEclipseSunWorld");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationEclipseSunWorld", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationEclipseTransit
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationEclipseTransit");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationEclipseTransit", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationEclipseTransitMulti
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationEclipseTransitMulti");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationEclipseTransitMulti", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationDefaultCheck
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationDefaultCheck");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationDefaultCheck", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationDefaultsDefaults
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationDefaultsDefaults");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationDefaultsDefaults", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationDefaultsDeltaT
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationDefaultsDeltaT");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationDefaultsDeltaT", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationDefaultsImportOccult3
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationDefaultsImportOccult3");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationDefaultsImportOccult3", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationDefaultsSiteEditor
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationDefaultsSiteEditor");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationDefaultsSiteEditor", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationBailyMain
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationBailyMain");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationBailyMain", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("323, 500")]
		public Point LocationBailyEclipseImage
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationBailyEclipseImage");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationBailyEclipseImage", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationBailyLimbHeights
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationBailyLimbHeights");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationBailyLimbHeights", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 325")]
		public Point LocationBailyLimbPlot
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationBailyLimbPlot");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationBailyLimbPlot", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("700, 0")]
		public Point LocationBailyResults
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationBailyResults");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationBailyResults", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationAsterDias
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationAsterDias");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationAsterDias", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationAsterIDSearch
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationAsterIDSearch");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationAsterIDSearch", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationAsterMultiLocn
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationAsterMultiLocn");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationAsterMultiLocn", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationAsterPath
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationAsterPath");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationAsterPath", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationAsterContactTimes
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationAsterContactTimes");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationAsterContactTimes", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationAsterPlotMultipath
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationAsterPlotMultipath");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationAsterPlotMultipath", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationAsterPlotPath
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationAsterPlotPath");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationAsterPlotPath", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationAsterPrepoint
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationAsterPrepoint");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationAsterPrepoint", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationAsterSearch
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationAsterSearch");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationAsterSearch", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationAsterSelectAsteroids
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationAsterSelectAsteroids");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationAsterSelectAsteroids", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationAsterSummary
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationAsterSummary");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationAsterSummary", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationAsterBinary
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationAsterBinary");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationAsterBinary", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationAsterConvert
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationAsterConvert");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationAsterConvert", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationAsterUserStar
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationAsterUserStar");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationAsterUserStar", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationAsterShowDias
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationAsterShowDias");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationAsterShowDias", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationAsterShowDistant
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationAsterShowDistant");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationAsterShowDistant", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationAsterShowDoubles
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationAsterShowDoubles");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationAsterShowDoubles", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationAsterShowErrors
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationAsterShowErrors");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationAsterShowErrors", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationAsterShowObservers
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationAsterShowObservers");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationAsterShowObservers", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationAsterShowPositions
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationAsterShowPositions");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationAsterShowPositions", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationAsterMissTimes
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationAsterMissTimes");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationAsterMissTimes", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationAsterMPCDetails
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationAsterMPCDetails");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationAsterMPCDetails", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationAsterObsEditor
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationAsterObsEditor");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationAsterObsEditor", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationAsterPlotObs
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationAsterPlotObs");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationAsterPlotObs", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationAsterPredictionOffset
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationAsterPredictionOffset");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationAsterPredictionOffset", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationAsterReductionPlots
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationAsterReductionPlots");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationAsterReductionPlots", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationAsterTimeBaseOffsets
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationAsterTimeBaseOffsets");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationAsterTimeBaseOffsets", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool AutoPredictLOLOALoRes
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AutoPredictLOLOALoRes");
			}
			set
			{
				((SettingsBase)this).set_Item("AutoPredictLOLOALoRes", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool AutoPredictLOLAHiRes
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AutoPredictLOLAHiRes");
			}
			set
			{
				((SettingsBase)this).set_Item("AutoPredictLOLAHiRes", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool AutoPredictLOLAoints
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AutoPredictLOLAoints");
			}
			set
			{
				((SettingsBase)this).set_Item("AutoPredictLOLAoints", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool MoonMapSouthAtTop
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("MoonMapSouthAtTop");
			}
			set
			{
				((SettingsBase)this).set_Item("MoonMapSouthAtTop", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public int CloudMapRegion
		{
			get
			{
				return (int)((SettingsBase)this).get_Item("CloudMapRegion");
			}
			set
			{
				((SettingsBase)this).set_Item("CloudMapRegion", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("6")]
		public int WeatherMapInterval
		{
			get
			{
				return (int)((SettingsBase)this).get_Item("WeatherMapInterval");
			}
			set
			{
				((SettingsBase)this).set_Item("WeatherMapInterval", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool WeatherAnimation
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("WeatherAnimation");
			}
			set
			{
				((SettingsBase)this).set_Item("WeatherAnimation", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public int VizierServer
		{
			get
			{
				return (int)((SettingsBase)this).get_Item("VizierServer");
			}
			set
			{
				((SettingsBase)this).set_Item("VizierServer", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("6")]
		public int LunarMultipleMagLimit
		{
			get
			{
				return (int)((SettingsBase)this).get_Item("LunarMultipleMagLimit");
			}
			set
			{
				((SettingsBase)this).set_Item("LunarMultipleMagLimit", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool AsteroidSearchDateSort
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsteroidSearchDateSort");
			}
			set
			{
				((SettingsBase)this).set_Item("AsteroidSearchDateSort", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("3")]
		public int DownloadFileAge
		{
			get
			{
				return (int)((SettingsBase)this).get_Item("DownloadFileAge");
			}
			set
			{
				((SettingsBase)this).set_Item("DownloadFileAge", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("null")]
		public string SiteFileAsteroidSelect
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("SiteFileAsteroidSelect");
			}
			set
			{
				((SettingsBase)this).set_Item("SiteFileAsteroidSelect", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string Save_AsteroidObservationsForm
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("Save_AsteroidObservationsForm");
			}
			set
			{
				((SettingsBase)this).set_Item("Save_AsteroidObservationsForm", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string Save_CloudMap
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("Save_CloudMap");
			}
			set
			{
				((SettingsBase)this).set_Item("Save_CloudMap", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool Graze_Observer_IncludeStarPath
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("Graze_Observer_IncludeStarPath");
			}
			set
			{
				((SettingsBase)this).set_Item("Graze_Observer_IncludeStarPath", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("15")]
		public decimal AsteroidSearchMinimumDiameter
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("AsteroidSearchMinimumDiameter");
			}
			set
			{
				((SettingsBase)this).set_Item("AsteroidSearchMinimumDiameter", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("3")]
		public int UpdateFrequency
		{
			get
			{
				return (int)((SettingsBase)this).get_Item("UpdateFrequency");
			}
			set
			{
				((SettingsBase)this).set_Item("UpdateFrequency", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("2000-01-01")]
		public DateTime LastUpdate
		{
			get
			{
				return (DateTime)((SettingsBase)this).get_Item("LastUpdate");
			}
			set
			{
				((SettingsBase)this).set_Item("LastUpdate", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("10")]
		public decimal AsteroidEphemDuration
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("AsteroidEphemDuration");
			}
			set
			{
				((SettingsBase)this).set_Item("AsteroidEphemDuration", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("1")]
		public decimal AsteroidEphemInterval
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("AsteroidEphemInterval");
			}
			set
			{
				((SettingsBase)this).set_Item("AsteroidEphemInterval", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool ShowAsteroidElements
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("ShowAsteroidElements");
			}
			set
			{
				((SettingsBase)this).set_Item("ShowAsteroidElements", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string Save_Var_LightCurve
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("Save_Var_LightCurve");
			}
			set
			{
				((SettingsBase)this).set_Item("Save_Var_LightCurve", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("https://www.occultations.org/sw/occult")]
		public string OccultServer
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("OccultServer");
			}
			set
			{
				((SettingsBase)this).set_Item("OccultServer", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("https://minplanobs.org/MPInfo/datazips/LCLIST_PUB_CURRENT.zip")]
		public string AsteroidLightCurveData_url
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("AsteroidLightCurveData_url");
			}
			set
			{
				((SettingsBase)this).set_Item("AsteroidLightCurveData_url", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("5")]
		public decimal MiriadeSteps
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("MiriadeSteps");
			}
			set
			{
				((SettingsBase)this).set_Item("MiriadeSteps", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("6")]
		public int MiriadeAnimationSpeed
		{
			get
			{
				return (int)((SettingsBase)this).get_Item("MiriadeAnimationSpeed");
			}
			set
			{
				((SettingsBase)this).set_Item("MiriadeAnimationSpeed", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string MiriadeSaveDirectory
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("MiriadeSaveDirectory");
			}
			set
			{
				((SettingsBase)this).set_Item("MiriadeSaveDirectory", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public string AsteroidSiteLongitude
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("AsteroidSiteLongitude");
			}
			set
			{
				((SettingsBase)this).set_Item("AsteroidSiteLongitude", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public string AsteroidSiteLatitude
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("AsteroidSiteLatitude");
			}
			set
			{
				((SettingsBase)this).set_Item("AsteroidSiteLatitude", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool AsteroidSiteChecked
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsteroidSiteChecked");
			}
			set
			{
				((SettingsBase)this).set_Item("AsteroidSiteChecked", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool AsteroidDistanceKMChecked
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsteroidDistanceKMChecked");
			}
			set
			{
				((SettingsBase)this).set_Item("AsteroidDistanceKMChecked", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("300")]
		public string AsteroidDistanceKM
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("AsteroidDistanceKM");
			}
			set
			{
				((SettingsBase)this).set_Item("AsteroidDistanceKM", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool AsteroidDistanceSecChecked
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsteroidDistanceSecChecked");
			}
			set
			{
				((SettingsBase)this).set_Item("AsteroidDistanceSecChecked", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue(".5")]
		public string AsteroidDistanceSec
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("AsteroidDistanceSec");
			}
			set
			{
				((SettingsBase)this).set_Item("AsteroidDistanceSec", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0.5")]
		public string AsteroidMagDrop
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("AsteroidMagDrop");
			}
			set
			{
				((SettingsBase)this).set_Item("AsteroidMagDrop", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool AsteroidMagDropChecked
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsteroidMagDropChecked");
			}
			set
			{
				((SettingsBase)this).set_Item("AsteroidMagDropChecked", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("1.0")]
		public string AsteroidDuration
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("AsteroidDuration");
			}
			set
			{
				((SettingsBase)this).set_Item("AsteroidDuration", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool AsteroidDiameterChecked
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsteroidDiameterChecked");
			}
			set
			{
				((SettingsBase)this).set_Item("AsteroidDiameterChecked", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("25")]
		public string AsteroidDiameter
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("AsteroidDiameter");
			}
			set
			{
				((SettingsBase)this).set_Item("AsteroidDiameter", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool AsteroidDurationChecked
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsteroidDurationChecked");
			}
			set
			{
				((SettingsBase)this).set_Item("AsteroidDurationChecked", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("1")]
		public decimal PlotScale
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("PlotScale");
			}
			set
			{
				((SettingsBase)this).set_Item("PlotScale", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool AsteroidTNO
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsteroidTNO");
			}
			set
			{
				((SettingsBase)this).set_Item("AsteroidTNO", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool AsteroidNumberContains
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsteroidNumberContains");
			}
			set
			{
				((SettingsBase)this).set_Item("AsteroidNumberContains", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool AsteroidNameChecked
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsteroidNameChecked");
			}
			set
			{
				((SettingsBase)this).set_Item("AsteroidNameChecked", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool AsteroidDateRangeChecked
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsteroidDateRangeChecked");
			}
			set
			{
				((SettingsBase)this).set_Item("AsteroidDateRangeChecked", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool BinaryAsteroids
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("BinaryAsteroids");
			}
			set
			{
				((SettingsBase)this).set_Item("BinaryAsteroids", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool GrazesOnly_LunarSearch
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("GrazesOnly_LunarSearch");
			}
			set
			{
				((SettingsBase)this).set_Item("GrazesOnly_LunarSearch", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool DoublesOnly_LunarSearch
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("DoublesOnly_LunarSearch");
			}
			set
			{
				((SettingsBase)this).set_Item("DoublesOnly_LunarSearch", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool LunarFilter_NoBrightLimb
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("LunarFilter_NoBrightLimb");
			}
			set
			{
				((SettingsBase)this).set_Item("LunarFilter_NoBrightLimb", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool LunarFilter_NoDaytime
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("LunarFilter_NoDaytime");
			}
			set
			{
				((SettingsBase)this).set_Item("LunarFilter_NoDaytime", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool LunarFilter_TimeRange
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("LunarFilter_TimeRange");
			}
			set
			{
				((SettingsBase)this).set_Item("LunarFilter_TimeRange", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool LunarFilter_BrightLimbMessage
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("LunarFilter_BrightLimbMessage");
			}
			set
			{
				((SettingsBase)this).set_Item("LunarFilter_BrightLimbMessage", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool LunarFilter_StarNameMessage
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("LunarFilter_StarNameMessage");
			}
			set
			{
				((SettingsBase)this).set_Item("LunarFilter_StarNameMessage", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool LunarFilter_DoubleStarMessage
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("LunarFilter_DoubleStarMessage");
			}
			set
			{
				((SettingsBase)this).set_Item("LunarFilter_DoubleStarMessage", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool LunarFilter_VariableStarMessage
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("LunarFilter_VariableStarMessage");
			}
			set
			{
				((SettingsBase)this).set_Item("LunarFilter_VariableStarMessage", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool LunarFilter_ExcludeDurations
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("LunarFilter_ExcludeDurations");
			}
			set
			{
				((SettingsBase)this).set_Item("LunarFilter_ExcludeDurations", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public decimal LunarFilter_Tstart
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("LunarFilter_Tstart");
			}
			set
			{
				((SettingsBase)this).set_Item("LunarFilter_Tstart", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("24")]
		public decimal LunarFilter_Tend
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("LunarFilter_Tend");
			}
			set
			{
				((SettingsBase)this).set_Item("LunarFilter_Tend", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool LunarFilter_ExcludeGrazeMessage
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("LunarFilter_ExcludeGrazeMessage");
			}
			set
			{
				((SettingsBase)this).set_Item("LunarFilter_ExcludeGrazeMessage", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("gwilliams@cfa.harvard.edu")]
		public string emailAddress_MPC
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("emailAddress_MPC");
			}
			set
			{
				((SettingsBase)this).set_Item("emailAddress_MPC", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string emailAddress_JPL
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("emailAddress_JPL");
			}
			set
			{
				((SettingsBase)this).set_Item("emailAddress_JPL", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string eMail_SignatureBlock
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("eMail_SignatureBlock");
			}
			set
			{
				((SettingsBase)this).set_Item("eMail_SignatureBlock", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string eMailAddress_other
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("eMailAddress_other");
			}
			set
			{
				((SettingsBase)this).set_Item("eMailAddress_other", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool EphemJD_Sidereal_HighPrecision
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("EphemJD_Sidereal_HighPrecision");
			}
			set
			{
				((SettingsBase)this).set_Item("EphemJD_Sidereal_HighPrecision", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("http://www.astro.gsu.edu/wds/orb6/")]
		public string Orb6DownloadServer
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("Orb6DownloadServer");
			}
			set
			{
				((SettingsBase)this).set_Item("Orb6DownloadServer", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("orb6orbits.txt")]
		public string Orb6DownloadFile
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("Orb6DownloadFile");
			}
			set
			{
				((SettingsBase)this).set_Item("Orb6DownloadFile", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool LunarPredict_DisplayApparentPosition
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("LunarPredict_DisplayApparentPosition");
			}
			set
			{
				((SettingsBase)this).set_Item("LunarPredict_DisplayApparentPosition", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("http://cdsportal.cds.unistra.fr/")]
		public string CDS_portal
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("CDS_portal");
			}
			set
			{
				((SettingsBase)this).set_Item("CDS_portal", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("1.0")]
		public decimal GoogleEarthLineWidth
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("GoogleEarthLineWidth");
			}
			set
			{
				((SettingsBase)this).set_Item("GoogleEarthLineWidth", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("2.0")]
		public decimal GoogleMapsLineWidth
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("GoogleMapsLineWidth");
			}
			set
			{
				((SettingsBase)this).set_Item("GoogleMapsLineWidth", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string StarDia_File_LastName
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("StarDia_File_LastName");
			}
			set
			{
				((SettingsBase)this).set_Item("StarDia_File_LastName", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool CSVPaste1
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("CSVPaste1");
			}
			set
			{
				((SettingsBase)this).set_Item("CSVPaste1", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool CSVPaste2
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("CSVPaste2");
			}
			set
			{
				((SettingsBase)this).set_Item("CSVPaste2", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool CSVPaste3
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("CSVPaste3");
			}
			set
			{
				((SettingsBase)this).set_Item("CSVPaste3", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool CSVPaste4
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("CSVPaste4");
			}
			set
			{
				((SettingsBase)this).set_Item("CSVPaste4", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool CSVPasteLiMovie
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("CSVPasteLiMovie");
			}
			set
			{
				((SettingsBase)this).set_Item("CSVPasteLiMovie", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool CSVPasteTangra
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("CSVPasteTangra");
			}
			set
			{
				((SettingsBase)this).set_Item("CSVPasteTangra", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string UCAC4_Path
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("UCAC4_Path");
			}
			set
			{
				((SettingsBase)this).set_Item("UCAC4_Path", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool StarChartUseUCAC4
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("StarChartUseUCAC4");
			}
			set
			{
				((SettingsBase)this).set_Item("StarChartUseUCAC4", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool AutoGenerateUseUCAC4
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AutoGenerateUseUCAC4");
			}
			set
			{
				((SettingsBase)this).set_Item("AutoGenerateUseUCAC4", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string PPMXL_Path
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("PPMXL_Path");
			}
			set
			{
				((SettingsBase)this).set_Item("PPMXL_Path", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool StarChartUsePPMXL
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("StarChartUsePPMXL");
			}
			set
			{
				((SettingsBase)this).set_Item("StarChartUsePPMXL", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool AutoGenerateUsePPMXL
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AutoGenerateUsePPMXL");
			}
			set
			{
				((SettingsBase)this).set_Item("AutoGenerateUsePPMXL", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool ISAMInColour
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("ISAMInColour");
			}
			set
			{
				((SettingsBase)this).set_Item("ISAMInColour", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool ISAMshowAxis
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("ISAMshowAxis");
			}
			set
			{
				((SettingsBase)this).set_Item("ISAMshowAxis", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string ISAMSaveDirectory
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("ISAMSaveDirectory");
			}
			set
			{
				((SettingsBase)this).set_Item("ISAMSaveDirectory", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool ISAM3D
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("ISAM3D");
			}
			set
			{
				((SettingsBase)this).set_Item("ISAM3D", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool RepositionAsteroidPrediction
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("RepositionAsteroidPrediction");
			}
			set
			{
				((SettingsBase)this).set_Item("RepositionAsteroidPrediction", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string Save_StarDiameters
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("Save_StarDiameters");
			}
			set
			{
				((SettingsBase)this).set_Item("Save_StarDiameters", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("616")]
		public int MoonMapWidth
		{
			get
			{
				return (int)((SettingsBase)this).get_Item("MoonMapWidth");
			}
			set
			{
				((SettingsBase)this).set_Item("MoonMapWidth", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("563")]
		public int MoonMapHeight
		{
			get
			{
				return (int)((SettingsBase)this).get_Item("MoonMapHeight");
			}
			set
			{
				((SettingsBase)this).set_Item("MoonMapHeight", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string Save_BinaryAsteroidEphem
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("Save_BinaryAsteroidEphem");
			}
			set
			{
				((SettingsBase)this).set_Item("Save_BinaryAsteroidEphem", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string AsteroidProfileData
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("AsteroidProfileData");
			}
			set
			{
				((SettingsBase)this).set_Item("AsteroidProfileData", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string AsteroidProfileImage
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("AsteroidProfileImage");
			}
			set
			{
				((SettingsBase)this).set_Item("AsteroidProfileImage", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string LightCurveImage
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("LightCurveImage");
			}
			set
			{
				((SettingsBase)this).set_Item("LightCurveImage", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string LightCurveData
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("LightCurveData");
			}
			set
			{
				((SettingsBase)this).set_Item("LightCurveData", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool UCAC4UseHip
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("UCAC4UseHip");
			}
			set
			{
				((SettingsBase)this).set_Item("UCAC4UseHip", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool PrePointSAOnums
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("PrePointSAOnums");
			}
			set
			{
				((SettingsBase)this).set_Item("PrePointSAOnums", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool DefaultPrepointAddSAO
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("DefaultPrepointAddSAO");
			}
			set
			{
				((SettingsBase)this).set_Item("DefaultPrepointAddSAO", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool UseUCAC4_StarCat
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("UseUCAC4_StarCat");
			}
			set
			{
				((SettingsBase)this).set_Item("UseUCAC4_StarCat", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool UseUCAC4_AsterObs
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("UseUCAC4_AsterObs");
			}
			set
			{
				((SettingsBase)this).set_Item("UseUCAC4_AsterObs", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("600")]
		public int LunarPredictFormHeight
		{
			get
			{
				return (int)((SettingsBase)this).get_Item("LunarPredictFormHeight");
			}
			set
			{
				((SettingsBase)this).set_Item("LunarPredictFormHeight", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("1000")]
		public int LunarPredictFormWidth
		{
			get
			{
				return (int)((SettingsBase)this).get_Item("LunarPredictFormWidth");
			}
			set
			{
				((SettingsBase)this).set_Item("LunarPredictFormWidth", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool GrazeEventCount_HiRes
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("GrazeEventCount_HiRes");
			}
			set
			{
				((SettingsBase)this).set_Item("GrazeEventCount_HiRes", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool AutoPredictNumberOfEvents
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AutoPredictNumberOfEvents");
			}
			set
			{
				((SettingsBase)this).set_Item("AutoPredictNumberOfEvents", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool MultipredictShortOutputLine
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("MultipredictShortOutputLine");
			}
			set
			{
				((SettingsBase)this).set_Item("MultipredictShortOutputLine", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool MultipredictUseCA
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("MultipredictUseCA");
			}
			set
			{
				((SettingsBase)this).set_Item("MultipredictUseCA", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string CraterTimingFile
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("CraterTimingFile");
			}
			set
			{
				((SettingsBase)this).set_Item("CraterTimingFile", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("1")]
		public decimal CatCvrtRAhrs
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("CatCvrtRAhrs");
			}
			set
			{
				((SettingsBase)this).set_Item("CatCvrtRAhrs", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("1")]
		public decimal CatCvrtRAmins
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("CatCvrtRAmins");
			}
			set
			{
				((SettingsBase)this).set_Item("CatCvrtRAmins", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("1")]
		public decimal CatCvrtRAsecs
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("CatCvrtRAsecs");
			}
			set
			{
				((SettingsBase)this).set_Item("CatCvrtRAsecs", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("2")]
		public decimal CatCvrtRAsecsCols
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("CatCvrtRAsecsCols");
			}
			set
			{
				((SettingsBase)this).set_Item("CatCvrtRAsecsCols", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool CatCvrtRAsecsDecimal
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("CatCvrtRAsecsDecimal");
			}
			set
			{
				((SettingsBase)this).set_Item("CatCvrtRAsecsDecimal", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("1")]
		public decimal CatCvrtDECdeg
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("CatCvrtDECdeg");
			}
			set
			{
				((SettingsBase)this).set_Item("CatCvrtDECdeg", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("1")]
		public decimal CatCvrtDECmin
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("CatCvrtDECmin");
			}
			set
			{
				((SettingsBase)this).set_Item("CatCvrtDECmin", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("1")]
		public decimal CatCvrtDECsecs
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("CatCvrtDECsecs");
			}
			set
			{
				((SettingsBase)this).set_Item("CatCvrtDECsecs", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("2")]
		public decimal CatCvrtDECsecsCols
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("CatCvrtDECsecsCols");
			}
			set
			{
				((SettingsBase)this).set_Item("CatCvrtDECsecsCols", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool CatCvrtDECsecsDecimal
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("CatCvrtDECsecsDecimal");
			}
			set
			{
				((SettingsBase)this).set_Item("CatCvrtDECsecsDecimal", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("1")]
		public decimal CatCvrtMv
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("CatCvrtMv");
			}
			set
			{
				((SettingsBase)this).set_Item("CatCvrtMv", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("4")]
		public decimal CatCvrtMvCols
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("CatCvrtMvCols");
			}
			set
			{
				((SettingsBase)this).set_Item("CatCvrtMvCols", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("1")]
		public decimal CatCvrtMr
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("CatCvrtMr");
			}
			set
			{
				((SettingsBase)this).set_Item("CatCvrtMr", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("4")]
		public decimal CatCvrtMrCols
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("CatCvrtMrCols");
			}
			set
			{
				((SettingsBase)this).set_Item("CatCvrtMrCols", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("1")]
		public decimal CatCvrtID
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("CatCvrtID");
			}
			set
			{
				((SettingsBase)this).set_Item("CatCvrtID", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("6")]
		public decimal CatCvrtIDCols
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("CatCvrtIDCols");
			}
			set
			{
				((SettingsBase)this).set_Item("CatCvrtIDCols", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool LunarEclipseOffsets
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("LunarEclipseOffsets");
			}
			set
			{
				((SettingsBase)this).set_Item("LunarEclipseOffsets", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool AsteroidElongationChecked
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsteroidElongationChecked");
			}
			set
			{
				((SettingsBase)this).set_Item("AsteroidElongationChecked", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool DoubleSolvePlotBW
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("DoubleSolvePlotBW");
			}
			set
			{
				((SettingsBase)this).set_Item("DoubleSolvePlotBW", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string LastFileOpened
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("LastFileOpened");
			}
			set
			{
				((SettingsBase)this).set_Item("LastFileOpened", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string Save_Form
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("Save_Form");
			}
			set
			{
				((SettingsBase)this).set_Item("Save_Form", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string Save_Plot
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("Save_Plot");
			}
			set
			{
				((SettingsBase)this).set_Item("Save_Plot", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("10")]
		public decimal NumPointsForBinning
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("NumPointsForBinning");
			}
			set
			{
				((SettingsBase)this).set_Item("NumPointsForBinning", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("764, 331")]
		public Size BinFormSize
		{
			get
			{
				return (Size)((SettingsBase)this).get_Item("BinFormSize");
			}
			set
			{
				((SettingsBase)this).set_Item("BinFormSize", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("10")]
		public decimal NumberOfTransitions
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("NumberOfTransitions");
			}
			set
			{
				((SettingsBase)this).set_Item("NumberOfTransitions", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool NoiseByStdDevn
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("NoiseByStdDevn");
			}
			set
			{
				((SettingsBase)this).set_Item("NoiseByStdDevn", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool NoiseAppliedToTestSignal
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("NoiseAppliedToTestSignal");
			}
			set
			{
				((SettingsBase)this).set_Item("NoiseAppliedToTestSignal", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("3")]
		public decimal MonteCarloNoiseLimit
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("MonteCarloNoiseLimit");
			}
			set
			{
				((SettingsBase)this).set_Item("MonteCarloNoiseLimit", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("200")]
		public decimal MonteCarloTrialNos
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("MonteCarloTrialNos");
			}
			set
			{
				((SettingsBase)this).set_Item("MonteCarloTrialNos", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("12")]
		public decimal PointsOutsideTransition
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("PointsOutsideTransition");
			}
			set
			{
				((SettingsBase)this).set_Item("PointsOutsideTransition", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("1026, 532")]
		public Size FormSize
		{
			get
			{
				return (Size)((SettingsBase)this).get_Item("FormSize");
			}
			set
			{
				((SettingsBase)this).set_Item("FormSize", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool AOTA_ShowCrossCorrln
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AOTA_ShowCrossCorrln");
			}
			set
			{
				((SettingsBase)this).set_Item("AOTA_ShowCrossCorrln", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool AOTA_ShowMeasurementMeans
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AOTA_ShowMeasurementMeans");
			}
			set
			{
				((SettingsBase)this).set_Item("AOTA_ShowMeasurementMeans", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("90")]
		public int ShapeModel_Scale
		{
			get
			{
				return (int)((SettingsBase)this).get_Item("ShapeModel_Scale");
			}
			set
			{
				((SettingsBase)this).set_Item("ShapeModel_Scale", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string DAMITmodelsSave
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("DAMITmodelsSave");
			}
			set
			{
				((SettingsBase)this).set_Item("DAMITmodelsSave", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string LastFileOpenedAOTA
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("LastFileOpenedAOTA");
			}
			set
			{
				((SettingsBase)this).set_Item("LastFileOpenedAOTA", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("8")]
		public int CameraIndex
		{
			get
			{
				return (int)((SettingsBase)this).get_Item("CameraIndex");
			}
			set
			{
				((SettingsBase)this).set_Item("CameraIndex", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public int AOTA_VTI
		{
			get
			{
				return (int)((SettingsBase)this).get_Item("AOTA_VTI");
			}
			set
			{
				((SettingsBase)this).set_Item("AOTA_VTI", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string Save_Fourier
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("Save_Fourier");
			}
			set
			{
				((SettingsBase)this).set_Item("Save_Fourier", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool AsteroidSolarElongationChecked
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsteroidSolarElongationChecked");
			}
			set
			{
				((SettingsBase)this).set_Item("AsteroidSolarElongationChecked", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool AsteroidPlanetChecked
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsteroidPlanetChecked");
			}
			set
			{
				((SettingsBase)this).set_Item("AsteroidPlanetChecked", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("20")]
		public string AsteroidSolarElong_deg
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("AsteroidSolarElong_deg");
			}
			set
			{
				((SettingsBase)this).set_Item("AsteroidSolarElong_deg", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("http://devel2.linea.gov.br/~braga.ribas/tableOccult/")]
		public string RIO_server
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("RIO_server");
			}
			set
			{
				((SettingsBase)this).set_Item("RIO_server", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("cfa-ftp.harvard.edu")]
		public string MPC_ftp_uploadAddress
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("MPC_ftp_uploadAddress");
			}
			set
			{
				((SettingsBase)this).set_Item("MPC_ftp_uploadAddress", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool ShowStarUncertainty
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("ShowStarUncertainty");
			}
			set
			{
				((SettingsBase)this).set_Item("ShowStarUncertainty", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("MPCORB.ZIP")]
		public string MPCOrb_Mirror_file
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("MPCOrb_Mirror_file");
			}
			set
			{
				((SettingsBase)this).set_Item("MPCOrb_Mirror_file", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool AOTA_ShowErrorBars
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AOTA_ShowErrorBars");
			}
			set
			{
				((SettingsBase)this).set_Item("AOTA_ShowErrorBars", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("ftp://cdsarc.u-strasbg.fr/pub/cats/B/wds/")]
		public string WDSCodesServer
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("WDSCodesServer");
			}
			set
			{
				((SettingsBase)this).set_Item("WDSCodesServer", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("refs.dat.gz")]
		public string WDSCodesFile
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("WDSCodesFile");
			}
			set
			{
				((SettingsBase)this).set_Item("WDSCodesFile", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("http://www.7timer.info/")]
		public string SevenTimer
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("SevenTimer");
			}
			set
			{
				((SettingsBase)this).set_Item("SevenTimer", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("8")]
		public int LunarCameraIndex
		{
			get
			{
				return (int)((SettingsBase)this).get_Item("LunarCameraIndex");
			}
			set
			{
				((SettingsBase)this).set_Item("LunarCameraIndex", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public int Lunar_VTI
		{
			get
			{
				return (int)((SettingsBase)this).get_Item("Lunar_VTI");
			}
			set
			{
				((SettingsBase)this).set_Item("Lunar_VTI", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool Lunar_isPAL
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("Lunar_isPAL");
			}
			set
			{
				((SettingsBase)this).set_Item("Lunar_isPAL", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string URAT1_Path
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("URAT1_Path");
			}
			set
			{
				((SettingsBase)this).set_Item("URAT1_Path", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("-1")]
		public int RIO_TNOcount
		{
			get
			{
				return (int)((SettingsBase)this).get_Item("RIO_TNOcount");
			}
			set
			{
				((SettingsBase)this).set_Item("RIO_TNOcount", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string LunarArchiveLastConsolidation
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("LunarArchiveLastConsolidation");
			}
			set
			{
				((SettingsBase)this).set_Item("LunarArchiveLastConsolidation", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool RednLOLAPoints
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("RednLOLAPoints");
			}
			set
			{
				((SettingsBase)this).set_Item("RednLOLAPoints", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool GrazeLOLAPoints
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("GrazeLOLAPoints");
			}
			set
			{
				((SettingsBase)this).set_Item("GrazeLOLAPoints", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0.010")]
		public decimal DoubleStarMinSep
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("DoubleStarMinSep");
			}
			set
			{
				((SettingsBase)this).set_Item("DoubleStarMinSep", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("10")]
		public decimal DoubleStarMaxSep
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("DoubleStarMaxSep");
			}
			set
			{
				((SettingsBase)this).set_Item("DoubleStarMaxSep", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool SolarEclipseLimbCorrections
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("SolarEclipseLimbCorrections");
			}
			set
			{
				((SettingsBase)this).set_Item("SolarEclipseLimbCorrections", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("5")]
		public int TimerOffsets_Lunar
		{
			get
			{
				return (int)((SettingsBase)this).get_Item("TimerOffsets_Lunar");
			}
			set
			{
				((SettingsBase)this).set_Item("TimerOffsets_Lunar", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("3")]
		public int LightCurveReminder
		{
			get
			{
				return (int)((SettingsBase)this).get_Item("LightCurveReminder");
			}
			set
			{
				((SettingsBase)this).set_Item("LightCurveReminder", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string LightCurve_CC_Addresses
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("LightCurve_CC_Addresses");
			}
			set
			{
				((SettingsBase)this).set_Item("LightCurve_CC_Addresses", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string LCLongDeg
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("LCLongDeg");
			}
			set
			{
				((SettingsBase)this).set_Item("LCLongDeg", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string LCLongMin
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("LCLongMin");
			}
			set
			{
				((SettingsBase)this).set_Item("LCLongMin", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string LCLongSec
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("LCLongSec");
			}
			set
			{
				((SettingsBase)this).set_Item("LCLongSec", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string LCLatDeg
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("LCLatDeg");
			}
			set
			{
				((SettingsBase)this).set_Item("LCLatDeg", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string LCLatMin
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("LCLatMin");
			}
			set
			{
				((SettingsBase)this).set_Item("LCLatMin", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string LCLatSec
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("LCLatSec");
			}
			set
			{
				((SettingsBase)this).set_Item("LCLatSec", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string LCAlt
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("LCAlt");
			}
			set
			{
				((SettingsBase)this).set_Item("LCAlt", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string LCObserver
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("LCObserver");
			}
			set
			{
				((SettingsBase)this).set_Item("LCObserver", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool GrazeLOLAHires
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("GrazeLOLAHires");
			}
			set
			{
				((SettingsBase)this).set_Item("GrazeLOLAHires", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool RednLOLAHires
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("RednLOLAHires");
			}
			set
			{
				((SettingsBase)this).set_Item("RednLOLAHires", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool GrazeEventCount_LowRes
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("GrazeEventCount_LowRes");
			}
			set
			{
				((SettingsBase)this).set_Item("GrazeEventCount_LowRes", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool LunarFilter_ExcludeUnlessK2
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("LunarFilter_ExcludeUnlessK2");
			}
			set
			{
				((SettingsBase)this).set_Item("LunarFilter_ExcludeUnlessK2", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool LunarFilter_NoLightCurve
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("LunarFilter_NoLightCurve");
			}
			set
			{
				((SettingsBase)this).set_Item("LunarFilter_NoLightCurve", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("C:\\")]
		public string TAPVizier_Directory
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("TAPVizier_Directory");
			}
			set
			{
				((SettingsBase)this).set_Item("TAPVizier_Directory", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool Asterplot_ShowScaleGrid
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("Asterplot_ShowScaleGrid");
			}
			set
			{
				((SettingsBase)this).set_Item("Asterplot_ShowScaleGrid", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("10")]
		public double Asterplot_GridScale
		{
			get
			{
				return (double)((SettingsBase)this).get_Item("Asterplot_GridScale");
			}
			set
			{
				((SettingsBase)this).set_Item("Asterplot_GridScale", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool ShapeModel_TopMost
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("ShapeModel_TopMost");
			}
			set
			{
				((SettingsBase)this).set_Item("ShapeModel_TopMost", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool IncludeAxisOfRotation
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("IncludeAxisOfRotation");
			}
			set
			{
				((SettingsBase)this).set_Item("IncludeAxisOfRotation", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool ShapeModel_DrawMean
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("ShapeModel_DrawMean");
			}
			set
			{
				((SettingsBase)this).set_Item("ShapeModel_DrawMean", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool ShapeModel_ShowMean
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("ShapeModel_ShowMean");
			}
			set
			{
				((SettingsBase)this).set_Item("ShapeModel_ShowMean", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("1000")]
		public int AnimationInterval
		{
			get
			{
				return (int)((SettingsBase)this).get_Item("AnimationInterval");
			}
			set
			{
				((SettingsBase)this).set_Item("AnimationInterval", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool StarChartUseGaia14
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("StarChartUseGaia14");
			}
			set
			{
				((SettingsBase)this).set_Item("StarChartUseGaia14", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool StarChartUseGaia
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("StarChartUseGaia");
			}
			set
			{
				((SettingsBase)this).set_Item("StarChartUseGaia", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool AutoGenerateUseTychoGaia
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AutoGenerateUseTychoGaia");
			}
			set
			{
				((SettingsBase)this).set_Item("AutoGenerateUseTychoGaia", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool AutoGenerateUseGaia14
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AutoGenerateUseGaia14");
			}
			set
			{
				((SettingsBase)this).set_Item("AutoGenerateUseGaia14", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool AsteroidLocalAltitude
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsteroidLocalAltitude");
			}
			set
			{
				((SettingsBase)this).set_Item("AsteroidLocalAltitude", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("15")]
		public decimal AsteroidLocalAltitudeValue
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("AsteroidLocalAltitudeValue");
			}
			set
			{
				((SettingsBase)this).set_Item("AsteroidLocalAltitudeValue", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public int HistoricalObserver
		{
			get
			{
				return (int)((SettingsBase)this).get_Item("HistoricalObserver");
			}
			set
			{
				((SettingsBase)this).set_Item("HistoricalObserver", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("2016-01-01")]
		public DateTime ISAM_DateLastChecked
		{
			get
			{
				return (DateTime)((SettingsBase)this).get_Item("ISAM_DateLastChecked");
			}
			set
			{
				((SettingsBase)this).set_Item("ISAM_DateLastChecked", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string C2A_Path
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("C2A_Path");
			}
			set
			{
				((SettingsBase)this).set_Item("C2A_Path", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string C2AFieldSize
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("C2AFieldSize");
			}
			set
			{
				((SettingsBase)this).set_Item("C2AFieldSize", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("SAO")]
		public string C2ACatalog
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("C2ACatalog");
			}
			set
			{
				((SettingsBase)this).set_Item("C2ACatalog", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool C2APreventMultipleInstances
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("C2APreventMultipleInstances");
			}
			set
			{
				((SettingsBase)this).set_Item("C2APreventMultipleInstances", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool OCR_Automatic
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("OCR_Automatic");
			}
			set
			{
				((SettingsBase)this).set_Item("OCR_Automatic", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool AOTA_LinesForTarget
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AOTA_LinesForTarget");
			}
			set
			{
				((SettingsBase)this).set_Item("AOTA_LinesForTarget", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool LightCurve_LinesForTarget
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("LightCurve_LinesForTarget");
			}
			set
			{
				((SettingsBase)this).set_Item("LightCurve_LinesForTarget", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0.3")]
		public decimal MagDropLimitInPredictions
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("MagDropLimitInPredictions");
			}
			set
			{
				((SettingsBase)this).set_Item("MagDropLimitInPredictions", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool MagDropLimitInPredictions_Checked
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("MagDropLimitInPredictions_Checked");
			}
			set
			{
				((SettingsBase)this).set_Item("MagDropLimitInPredictions_Checked", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0.5")]
		public decimal MagDropInListDisplay
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("MagDropInListDisplay");
			}
			set
			{
				((SettingsBase)this).set_Item("MagDropInListDisplay", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("1")]
		public decimal MaxDurnListDisplay
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("MaxDurnListDisplay");
			}
			set
			{
				((SettingsBase)this).set_Item("MaxDurnListDisplay", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("25")]
		public decimal DiameterListDisplay
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("DiameterListDisplay");
			}
			set
			{
				((SettingsBase)this).set_Item("DiameterListDisplay", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0.3")]
		public decimal DistArcSecListDisplay
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("DistArcSecListDisplay");
			}
			set
			{
				((SettingsBase)this).set_Item("DistArcSecListDisplay", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("300")]
		public decimal DistKMListDisplay
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("DistKMListDisplay");
			}
			set
			{
				((SettingsBase)this).set_Item("DistKMListDisplay", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("20")]
		public decimal SolarElongListDisplay
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("SolarElongListDisplay");
			}
			set
			{
				((SettingsBase)this).set_Item("SolarElongListDisplay", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("-140")]
		public decimal NWLong
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("NWLong");
			}
			set
			{
				((SettingsBase)this).set_Item("NWLong", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("-27")]
		public decimal NWLat
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("NWLat");
			}
			set
			{
				((SettingsBase)this).set_Item("NWLat", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("155")]
		public decimal NELong
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("NELong");
			}
			set
			{
				((SettingsBase)this).set_Item("NELong", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("-27")]
		public decimal NELat
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("NELat");
			}
			set
			{
				((SettingsBase)this).set_Item("NELat", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("140")]
		public decimal SWLong
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("SWLong");
			}
			set
			{
				((SettingsBase)this).set_Item("SWLong", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("-45")]
		public decimal SWLat
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("SWLat");
			}
			set
			{
				((SettingsBase)this).set_Item("SWLat", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("155")]
		public decimal SELong
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("SELong");
			}
			set
			{
				((SettingsBase)this).set_Item("SELong", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("-45")]
		public decimal SELat
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("SELat");
			}
			set
			{
				((SettingsBase)this).set_Item("SELat", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool PathCrossingRegion
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("PathCrossingRegion");
			}
			set
			{
				((SettingsBase)this).set_Item("PathCrossingRegion", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool AsteroidSearchAutoSave
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsteroidSearchAutoSave");
			}
			set
			{
				((SettingsBase)this).set_Item("AsteroidSearchAutoSave", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("http://lesia.obspm.fr/lucky-star/data/predictions/")]
		public string LuckyStar
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("LuckyStar");
			}
			set
			{
				((SettingsBase)this).set_Item("LuckyStar", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool AsteoidStarMagChecked
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsteoidStarMagChecked");
			}
			set
			{
				((SettingsBase)this).set_Item("AsteoidStarMagChecked", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("14")]
		public decimal StarMagListDisplay
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("StarMagListDisplay");
			}
			set
			{
				((SettingsBase)this).set_Item("StarMagListDisplay", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("https://newton.spacedys.com/~astdys2/catalogs/")]
		public string AstDys2_Server
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("AstDys2_Server");
			}
			set
			{
				((SettingsBase)this).set_Item("AstDys2_Server", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("allnum.cat")]
		public string AstDys2_AllNumFile
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("AstDys2_AllNumFile");
			}
			set
			{
				((SettingsBase)this).set_Item("AstDys2_AllNumFile", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("20000")]
		public decimal MaximumSearchEvents
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("MaximumSearchEvents");
			}
			set
			{
				((SettingsBase)this).set_Item("MaximumSearchEvents", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0.6")]
		public decimal AsteroidPathUncertEarthRadii
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("AsteroidPathUncertEarthRadii");
			}
			set
			{
				((SettingsBase)this).set_Item("AsteroidPathUncertEarthRadii", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool PathUncertaintyChecked
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("PathUncertaintyChecked");
			}
			set
			{
				((SettingsBase)this).set_Item("PathUncertaintyChecked", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool ExcludeIfInFutureChecked
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("ExcludeIfInFutureChecked");
			}
			set
			{
				((SettingsBase)this).set_Item("ExcludeIfInFutureChecked", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool HasShapeModelChecked
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("HasShapeModelChecked");
			}
			set
			{
				((SettingsBase)this).set_Item("HasShapeModelChecked", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool HasMoonsChecked
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("HasMoonsChecked");
			}
			set
			{
				((SettingsBase)this).set_Item("HasMoonsChecked", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool HasRingsChecked
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("HasRingsChecked");
			}
			set
			{
				((SettingsBase)this).set_Item("HasRingsChecked", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public int AsteroidListDisplayScaleIndex
		{
			get
			{
				return (int)((SettingsBase)this).get_Item("AsteroidListDisplayScaleIndex");
			}
			set
			{
				((SettingsBase)this).set_Item("AsteroidListDisplayScaleIndex", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public int AsteroidListDisplaySiteIndex
		{
			get
			{
				return (int)((SettingsBase)this).get_Item("AsteroidListDisplaySiteIndex");
			}
			set
			{
				((SettingsBase)this).set_Item("AsteroidListDisplaySiteIndex", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool PlotPathCentered
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("PlotPathCentered");
			}
			set
			{
				((SettingsBase)this).set_Item("PlotPathCentered", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("1.1")]
		public decimal AsteroidSearchMinimumDuration
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("AsteroidSearchMinimumDuration");
			}
			set
			{
				((SettingsBase)this).set_Item("AsteroidSearchMinimumDuration", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string MiriadeAsteroids
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("MiriadeAsteroids");
			}
			set
			{
				((SettingsBase)this).set_Item("MiriadeAsteroids", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool MiriadeBestOrbitOnly
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("MiriadeBestOrbitOnly");
			}
			set
			{
				((SettingsBase)this).set_Item("MiriadeBestOrbitOnly", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("2018-01-01")]
		public DateTime MiriadeBinaryUpdateDate
		{
			get
			{
				return (DateTime)((SettingsBase)this).get_Item("MiriadeBinaryUpdateDate");
			}
			set
			{
				((SettingsBase)this).set_Item("MiriadeBinaryUpdateDate", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("future.xml")]
		public string FutureFile_xml
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("FutureFile_xml");
			}
			set
			{
				((SettingsBase)this).set_Item("FutureFile_xml", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("Export.xml")]
		public string ExportFile
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("ExportFile");
			}
			set
			{
				((SettingsBase)this).set_Item("ExportFile", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("Export.xml")]
		public string ImportFile
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("ImportFile");
			}
			set
			{
				((SettingsBase)this).set_Item("ImportFile", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public int ImportFileLastEntry
		{
			get
			{
				return (int)((SettingsBase)this).get_Item("ImportFileLastEntry");
			}
			set
			{
				((SettingsBase)this).set_Item("ImportFileLastEntry", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("http://lesia.obspm.fr/lucky-star/campaigns/table_occult_sat/")]
		public string JupSats_Server
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("JupSats_Server");
			}
			set
			{
				((SettingsBase)this).set_Item("JupSats_Server", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool InvertImageColors
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("InvertImageColors");
			}
			set
			{
				((SettingsBase)this).set_Item("InvertImageColors", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool BlackToWhiteBackground
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("BlackToWhiteBackground");
			}
			set
			{
				((SettingsBase)this).set_Item("BlackToWhiteBackground", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("Herald, D.; Frappa, E.; Gault, D.; Giacchini, B.; Hayamizu, T.; Kerr, S.; Moore, J.")]
		public string PDSauthors
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("PDSauthors");
			}
			set
			{
				((SettingsBase)this).set_Item("PDSauthors", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool ISAMshowPhase
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("ISAMshowPhase");
			}
			set
			{
				((SettingsBase)this).set_Item("ISAMshowPhase", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool UseLocalDAMIT
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("UseLocalDAMIT");
			}
			set
			{
				((SettingsBase)this).set_Item("UseLocalDAMIT", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool ModelinBlackWhite
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("ModelinBlackWhite");
			}
			set
			{
				((SettingsBase)this).set_Item("ModelinBlackWhite", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool ModelFaceEdges
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("ModelFaceEdges");
			}
			set
			{
				((SettingsBase)this).set_Item("ModelFaceEdges", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool GaiaStarPlotInSearch
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("GaiaStarPlotInSearch");
			}
			set
			{
				((SettingsBase)this).set_Item("GaiaStarPlotInSearch", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("C:\\")]
		public string GaiaWorkingDir
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("GaiaWorkingDir");
			}
			set
			{
				((SettingsBase)this).set_Item("GaiaWorkingDir", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool SearchLimit1200km
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("SearchLimit1200km");
			}
			set
			{
				((SettingsBase)this).set_Item("SearchLimit1200km", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("-180")]
		public decimal SearchLongitude
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("SearchLongitude");
			}
			set
			{
				((SettingsBase)this).set_Item("SearchLongitude", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("-88")]
		public decimal SearchLatitude
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("SearchLatitude");
			}
			set
			{
				((SettingsBase)this).set_Item("SearchLatitude", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("2")]
		public int SearchDistance
		{
			get
			{
				return (int)((SettingsBase)this).get_Item("SearchDistance");
			}
			set
			{
				((SettingsBase)this).set_Item("SearchDistance", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool AsteroidApertureSet
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsteroidApertureSet");
			}
			set
			{
				((SettingsBase)this).set_Item("AsteroidApertureSet", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("20")]
		public decimal AsteroidAperture_cm
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("AsteroidAperture_cm");
			}
			set
			{
				((SettingsBase)this).set_Item("AsteroidAperture_cm", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool AsteroidIntegrateCamera
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsteroidIntegrateCamera");
			}
			set
			{
				((SettingsBase)this).set_Item("AsteroidIntegrateCamera", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("20")]
		public string Site_Aperture
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("Site_Aperture");
			}
			set
			{
				((SettingsBase)this).set_Item("Site_Aperture", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1")]
		public string LocalHorizon
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("LocalHorizon");
			}
			set
			{
				((SettingsBase)this).set_Item("LocalHorizon", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool AsteroidHorizon
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsteroidHorizon");
			}
			set
			{
				((SettingsBase)this).set_Item("AsteroidHorizon", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool IncludeStarChart_Prediction
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("IncludeStarChart_Prediction");
			}
			set
			{
				((SettingsBase)this).set_Item("IncludeStarChart_Prediction", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool AsterPlot_ShowAstrometry
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsterPlot_ShowAstrometry");
			}
			set
			{
				((SettingsBase)this).set_Item("AsterPlot_ShowAstrometry", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool AsteroidStarChartEnhanced
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsteroidStarChartEnhanced");
			}
			set
			{
				((SettingsBase)this).set_Item("AsteroidStarChartEnhanced", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool StarChartUseGaia16
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("StarChartUseGaia16");
			}
			set
			{
				((SettingsBase)this).set_Item("StarChartUseGaia16", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool AutoGenerateUseGaia16
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AutoGenerateUseGaia16");
			}
			set
			{
				((SettingsBase)this).set_Item("AutoGenerateUseGaia16", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("null")]
		public string SiteFileSolarTime
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("SiteFileSolarTime");
			}
			set
			{
				((SettingsBase)this).set_Item("SiteFileSolarTime", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("null")]
		public string ObserverSolarTime
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("ObserverSolarTime");
			}
			set
			{
				((SettingsBase)this).set_Item("ObserverSolarTime", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string Unistellar_Directory
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("Unistellar_Directory");
			}
			set
			{
				((SettingsBase)this).set_Item("Unistellar_Directory", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("01001001110001111101011100001101")]
		public string AsterPlot_Defaults
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("AsterPlot_Defaults");
			}
			set
			{
				((SettingsBase)this).set_Item("AsterPlot_Defaults", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool Asteroid_Open_Sort
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("Asteroid_Open_Sort");
			}
			set
			{
				((SettingsBase)this).set_Item("Asteroid_Open_Sort", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool Asteroid_Paste_Sort
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("Asteroid_Paste_Sort");
			}
			set
			{
				((SettingsBase)this).set_Item("Asteroid_Paste_Sort", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationLightCurveViewer
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationLightCurveViewer");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationLightCurveViewer", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationShapeModels
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationShapeModels");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationShapeModels", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationD_ShapeModels
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationD_ShapeModels");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationD_ShapeModels", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationD_Doubles
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationD_Doubles");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationD_Doubles", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationD_NearbyStars
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationD_NearbyStars");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationD_NearbyStars", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationMultiLightCurves
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationMultiLightCurves");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationMultiLightCurves", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationIRDiameter
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationIRDiameter");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationIRDiameter", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationLightCurveData
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationLightCurveData");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationLightCurveData", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationShowStarDiameter
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationShowStarDiameter");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationShowStarDiameter", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationPredictionOffset
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationPredictionOffset");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationPredictionOffset", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationRelativePathDistances
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationRelativePathDistances");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationRelativePathDistances", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationTimeBaseOffset
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationTimeBaseOffset");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationTimeBaseOffset", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationSatelliteSoln
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationSatelliteSoln");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationSatelliteSoln", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationStarDetails
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationStarDetails");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationStarDetails", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationChordLengths
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationChordLengths");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationChordLengths", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool AutoLightCurve
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AutoLightCurve");
			}
			set
			{
				((SettingsBase)this).set_Item("AutoLightCurve", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool AutoShapeModels
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AutoShapeModels");
			}
			set
			{
				((SettingsBase)this).set_Item("AutoShapeModels", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool AutoDoubles
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AutoDoubles");
			}
			set
			{
				((SettingsBase)this).set_Item("AutoDoubles", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool AutoNearbyStars
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AutoNearbyStars");
			}
			set
			{
				((SettingsBase)this).set_Item("AutoNearbyStars", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool AutoIRDiameter
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AutoIRDiameter");
			}
			set
			{
				((SettingsBase)this).set_Item("AutoIRDiameter", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool AutoStarDiameter
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AutoStarDiameter");
			}
			set
			{
				((SettingsBase)this).set_Item("AutoStarDiameter", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool AutoPredictionOffset
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AutoPredictionOffset");
			}
			set
			{
				((SettingsBase)this).set_Item("AutoPredictionOffset", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool AutoRelativePathDistances
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AutoRelativePathDistances");
			}
			set
			{
				((SettingsBase)this).set_Item("AutoRelativePathDistances", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool AutoTimeBaseOffset
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AutoTimeBaseOffset");
			}
			set
			{
				((SettingsBase)this).set_Item("AutoTimeBaseOffset", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool AutoSatelliteSolution
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AutoSatelliteSolution");
			}
			set
			{
				((SettingsBase)this).set_Item("AutoSatelliteSolution", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool AutoStarDetails
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AutoStarDetails");
			}
			set
			{
				((SettingsBase)this).set_Item("AutoStarDetails", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool AutoChordLengths
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AutoChordLengths");
			}
			set
			{
				((SettingsBase)this).set_Item("AutoChordLengths", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool AutoLightCurveData
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AutoLightCurveData");
			}
			set
			{
				((SettingsBase)this).set_Item("AutoLightCurveData", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool LightCurveToolTip
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("LightCurveToolTip");
			}
			set
			{
				((SettingsBase)this).set_Item("LightCurveToolTip", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("670")]
		public int StarChartWidth
		{
			get
			{
				return (int)((SettingsBase)this).get_Item("StarChartWidth");
			}
			set
			{
				((SettingsBase)this).set_Item("StarChartWidth", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool StarChartFlipHorizontal
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("StarChartFlipHorizontal");
			}
			set
			{
				((SettingsBase)this).set_Item("StarChartFlipHorizontal", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool StarChartFlipVertical
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("StarChartFlipVertical");
			}
			set
			{
				((SettingsBase)this).set_Item("StarChartFlipVertical", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public int StarChartRotate
		{
			get
			{
				return (int)((SettingsBase)this).get_Item("StarChartRotate");
			}
			set
			{
				((SettingsBase)this).set_Item("StarChartRotate", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("670")]
		public int StarChartWidthStandard
		{
			get
			{
				return (int)((SettingsBase)this).get_Item("StarChartWidthStandard");
			}
			set
			{
				((SettingsBase)this).set_Item("StarChartWidthStandard", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool StarChartCoords
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("StarChartCoords");
			}
			set
			{
				((SettingsBase)this).set_Item("StarChartCoords", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool StarChartVisual
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("StarChartVisual");
			}
			set
			{
				((SettingsBase)this).set_Item("StarChartVisual", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool StarChartTopMost
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("StarChartTopMost");
			}
			set
			{
				((SettingsBase)this).set_Item("StarChartTopMost", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("16")]
		public int StarChartMagLimitIndex
		{
			get
			{
				return (int)((SettingsBase)this).get_Item("StarChartMagLimitIndex");
			}
			set
			{
				((SettingsBase)this).set_Item("StarChartMagLimitIndex", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("1")]
		public int StarChartCatalogIndex
		{
			get
			{
				return (int)((SettingsBase)this).get_Item("StarChartCatalogIndex");
			}
			set
			{
				((SettingsBase)this).set_Item("StarChartCatalogIndex", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("Please update")]
		public string XYZfileVersion
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("XYZfileVersion");
			}
			set
			{
				((SettingsBase)this).set_Item("XYZfileVersion", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool AsterPlot_ShowRingPaths
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsterPlot_ShowRingPaths");
			}
			set
			{
				((SettingsBase)this).set_Item("AsterPlot_ShowRingPaths", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool AsterPlot_ShowScaleMAS
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsterPlot_ShowScaleMAS");
			}
			set
			{
				((SettingsBase)this).set_Item("AsterPlot_ShowScaleMAS", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string GoogleMaps_API_key
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("GoogleMaps_API_key");
			}
			set
			{
				((SettingsBase)this).set_Item("GoogleMaps_API_key", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string GoogleMapsAPIkey
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("GoogleMapsAPIkey");
			}
			set
			{
				((SettingsBase)this).set_Item("GoogleMapsAPIkey", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool GE_Alts_Auto
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("GE_Alts_Auto");
			}
			set
			{
				((SettingsBase)this).set_Item("GE_Alts_Auto", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool AsterPlot_Gray
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsterPlot_Gray");
			}
			set
			{
				((SettingsBase)this).set_Item("AsterPlot_Gray", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool AsterPlotWatermark
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsterPlotWatermark");
			}
			set
			{
				((SettingsBase)this).set_Item("AsterPlotWatermark", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool Asterplot_ShowFresnel
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("Asterplot_ShowFresnel");
			}
			set
			{
				((SettingsBase)this).set_Item("Asterplot_ShowFresnel", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool LunarMultiLong
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("LunarMultiLong");
			}
			set
			{
				((SettingsBase)this).set_Item("LunarMultiLong", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool LunarMultiShort
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("LunarMultiShort");
			}
			set
			{
				((SettingsBase)this).set_Item("LunarMultiShort", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool ShowShortEvent
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("ShowShortEvent");
			}
			set
			{
				((SettingsBase)this).set_Item("ShowShortEvent", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool AOTA_PlotUsingLarge
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AOTA_PlotUsingLarge");
			}
			set
			{
				((SettingsBase)this).set_Item("AOTA_PlotUsingLarge", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool ModelDark
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("ModelDark");
			}
			set
			{
				((SettingsBase)this).set_Item("ModelDark", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool AsteroidSearch_UseHorizonsEphem
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsteroidSearch_UseHorizonsEphem");
			}
			set
			{
				((SettingsBase)this).set_Item("AsteroidSearch_UseHorizonsEphem", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool AsteroidSearch_Visibility
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsteroidSearch_Visibility");
			}
			set
			{
				((SettingsBase)this).set_Item("AsteroidSearch_Visibility", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("20")]
		public decimal AsteroidSearch_Aperture
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("AsteroidSearch_Aperture");
			}
			set
			{
				((SettingsBase)this).set_Item("AsteroidSearch_Aperture", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool AsteroidSearch_Integration
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsteroidSearch_Integration");
			}
			set
			{
				((SettingsBase)this).set_Item("AsteroidSearch_Integration", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool AsteroidSearch_Diameter
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsteroidSearch_Diameter");
			}
			set
			{
				((SettingsBase)this).set_Item("AsteroidSearch_Diameter", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool AsteroidSearch_Duration
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsteroidSearch_Duration");
			}
			set
			{
				((SettingsBase)this).set_Item("AsteroidSearch_Duration", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public int AsteroidListDispRegion
		{
			get
			{
				return (int)((SettingsBase)this).get_Item("AsteroidListDispRegion");
			}
			set
			{
				((SettingsBase)this).set_Item("AsteroidListDispRegion", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string LightCurveReporterImage_Dir
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("LightCurveReporterImage_Dir");
			}
			set
			{
				((SettingsBase)this).set_Item("LightCurveReporterImage_Dir", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("100")]
		public int MonitorScale
		{
			get
			{
				return (int)((SettingsBase)this).get_Item("MonitorScale");
			}
			set
			{
				((SettingsBase)this).set_Item("MonitorScale", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0, 0")]
		public Point LocationVelocities
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("LocationVelocities");
			}
			set
			{
				((SettingsBase)this).set_Item("LocationVelocities", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool AOTA_AddName
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AOTA_AddName");
			}
			set
			{
				((SettingsBase)this).set_Item("AOTA_AddName", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string AOTA_NameToAdd
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("AOTA_NameToAdd");
			}
			set
			{
				((SettingsBase)this).set_Item("AOTA_NameToAdd", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool AOTA_Integrity_DecPlaces
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AOTA_Integrity_DecPlaces");
			}
			set
			{
				((SettingsBase)this).set_Item("AOTA_Integrity_DecPlaces", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool AOTAshowComparisons
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AOTAshowComparisons");
			}
			set
			{
				((SettingsBase)this).set_Item("AOTAshowComparisons", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool StarMapDoubleClick
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("StarMapDoubleClick");
			}
			set
			{
				((SettingsBase)this).set_Item("StarMapDoubleClick", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool AsterPlot_ShowTitle
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsterPlot_ShowTitle");
			}
			set
			{
				((SettingsBase)this).set_Item("AsterPlot_ShowTitle", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool MultiPredict3Sigma
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("MultiPredict3Sigma");
			}
			set
			{
				((SettingsBase)this).set_Item("MultiPredict3Sigma", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool AsterPlotProvisional
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsterPlotProvisional");
			}
			set
			{
				((SettingsBase)this).set_Item("AsterPlotProvisional", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool AsterPlot_EllipseIncolour
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsterPlot_EllipseIncolour");
			}
			set
			{
				((SettingsBase)this).set_Item("AsterPlot_EllipseIncolour", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool AsterPlot_EventsIncolour
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsterPlot_EventsIncolour");
			}
			set
			{
				((SettingsBase)this).set_Item("AsterPlot_EventsIncolour", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool Asterplot_ThickLinesOther
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("Asterplot_ThickLinesOther");
			}
			set
			{
				((SettingsBase)this).set_Item("Asterplot_ThickLinesOther", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("2")]
		public float AsterPlot_LineThickness_Other
		{
			get
			{
				return (float)((SettingsBase)this).get_Item("AsterPlot_LineThickness_Other");
			}
			set
			{
				((SettingsBase)this).set_Item("AsterPlot_LineThickness_Other", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("818, 554")]
		public Point SizeAsterPlot
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("SizeAsterPlot");
			}
			set
			{
				((SettingsBase)this).set_Item("SizeAsterPlot", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("1093, 676")]
		public Point SizeAsterEditor
		{
			get
			{
				return (Point)((SettingsBase)this).get_Item("SizeAsterEditor");
			}
			set
			{
				((SettingsBase)this).set_Item("SizeAsterEditor", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool AsterPlot_ShowStar
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsterPlot_ShowStar");
			}
			set
			{
				((SettingsBase)this).set_Item("AsterPlot_ShowStar", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0.0")]
		public string Asterplot_StarDia
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("Asterplot_StarDia");
			}
			set
			{
				((SettingsBase)this).set_Item("Asterplot_StarDia", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("20")]
		public decimal LCsim_Major
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("LCsim_Major");
			}
			set
			{
				((SettingsBase)this).set_Item("LCsim_Major", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("20")]
		public decimal LCsim_Minor
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("LCsim_Minor");
			}
			set
			{
				((SettingsBase)this).set_Item("LCsim_Minor", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("4")]
		public decimal LCsim_Parallax_asec
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("LCsim_Parallax_asec");
			}
			set
			{
				((SettingsBase)this).set_Item("LCsim_Parallax_asec", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public decimal LCsim_PA
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("LCsim_PA");
			}
			set
			{
				((SettingsBase)this).set_Item("LCsim_PA", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("14")]
		public decimal LCsim_MvAst
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("LCsim_MvAst");
			}
			set
			{
				((SettingsBase)this).set_Item("LCsim_MvAst", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("10")]
		public decimal LCsim_MvStar
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("LCsim_MvStar");
			}
			set
			{
				((SettingsBase)this).set_Item("LCsim_MvStar", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("1")]
		public decimal LCsim_DiaStar
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("LCsim_DiaStar");
			}
			set
			{
				((SettingsBase)this).set_Item("LCsim_DiaStar", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("1")]
		public decimal LCsim_Motion
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("LCsim_Motion");
			}
			set
			{
				((SettingsBase)this).set_Item("LCsim_Motion", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0.04")]
		public decimal LCsim_Limb
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("LCsim_Limb");
			}
			set
			{
				((SettingsBase)this).set_Item("LCsim_Limb", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool AsterPlot_MissDashed
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("AsterPlot_MissDashed");
			}
			set
			{
				((SettingsBase)this).set_Item("AsterPlot_MissDashed", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("5.0")]
		public string PDS_Version
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("PDS_Version");
			}
			set
			{
				((SettingsBase)this).set_Item("PDS_Version", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("")]
		public string GaiaMags_File
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("GaiaMags_File");
			}
			set
			{
				((SettingsBase)this).set_Item("GaiaMags_File", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool IncludeMinorRings
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("IncludeMinorRings");
			}
			set
			{
				((SettingsBase)this).set_Item("IncludeMinorRings", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool Asterplot_ShowSatellites
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("Asterplot_ShowSatellites");
			}
			set
			{
				((SettingsBase)this).set_Item("Asterplot_ShowSatellites", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("Initial")]
		public string QualityFileName
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("QualityFileName");
			}
			set
			{
				((SettingsBase)this).set_Item("QualityFileName", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public decimal LunarFilter_MinimumAltitude
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("LunarFilter_MinimumAltitude");
			}
			set
			{
				((SettingsBase)this).set_Item("LunarFilter_MinimumAltitude", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("False")]
		public bool LunarFilter_ApplyAlt
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("LunarFilter_ApplyAlt");
			}
			set
			{
				((SettingsBase)this).set_Item("LunarFilter_ApplyAlt", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("0")]
		public int ShapeModelBackColor
		{
			get
			{
				return (int)((SettingsBase)this).get_Item("ShapeModelBackColor");
			}
			set
			{
				((SettingsBase)this).set_Item("ShapeModelBackColor", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("1.1")]
		public decimal AsteroidSearchMinimumD
		{
			get
			{
				return (decimal)((SettingsBase)this).get_Item("AsteroidSearchMinimumD");
			}
			set
			{
				((SettingsBase)this).set_Item("AsteroidSearchMinimumD", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("True")]
		public bool DrawDamitID
		{
			get
			{
				return (bool)((SettingsBase)this).get_Item("DrawDamitID");
			}
			set
			{
				((SettingsBase)this).set_Item("DrawDamitID", (object)value);
			}
		}

		[UserScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("2022")]
		public string ADES_version
		{
			get
			{
				return (string)((SettingsBase)this).get_Item("ADES_version");
			}
			set
			{
				((SettingsBase)this).set_Item("ADES_version", (object)value);
			}
		}

		private void SettingChangingEventHandler(object sender, SettingChangingEventArgs e)
		{
		}

		private void SettingsSavingEventHandler(object sender, CancelEventArgs e)
		{
		}
	}
}
