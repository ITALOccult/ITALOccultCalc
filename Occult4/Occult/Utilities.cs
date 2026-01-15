using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Configuration;
using System.Diagnostics;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Drawing.Imaging;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Net;
using System.Net.NetworkInformation;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading;
using System.Windows.Forms;
using LightCurves;
using Occult.Asteroid_Observations;
using Occult.File_Actions;
using Occult.Lunar_Observations;
using Occult.Mapping;
using Occult.Properties;
using Occult.Star_Catalogues;

namespace Occult
{
	public class Utilities
	{
		private static uint _savedVolumeLevel;

		private static bool VolumeLevelSaved = false;

		public static string AppPath;

		public static bool RunningInDevelopmentEnvironment = false;

		public const double Radian = 180.0 / Math.PI;

		public const double TwoPi = Math.PI * 2.0;

		public const double JD_2000 = 2451545.0;

		public const double UnitLightTime = 0.00577551833;

		public const double GaussK = 0.01720209895;

		public const double k0 = 172020.9895;

		public const double LightSpeedMsec = 299792458.0;

		public const double LightSpeedKMsec = 299792.458;

		public const double AstroUnit_m = 149597870700.0;

		public const double AstroUnit_KM = 149597870.7;

		public const double SolarDiskRadius = 959.63;

		public const double SolarRadiusAt1Parsec_mas = 4.65;

		public const double EarthRadius = 6378.137;

		public const double LightTimeEarthRadius_sec = 0.02127517497454856;

		public const double SolarParallax = 8.794143836182533;

		public const double SolarParallaxRadian = 4.26352124542639E-05;

		public static readonly string[] Romans = new string[21]
		{
			"", "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX",
			"X", "XI", "XII", "XIII", "XIV", "XV", "XVII", "XVIII", "XVIII", "IXX",
			"IXX"
		};

		public static char[] GreekUnicode = new char[25]
		{
			'\0', 'α', 'β', 'γ', 'δ', 'ε', 'ζ', 'η', 'θ', 'ι',
			'κ', 'λ', 'μ', 'ν', 'ξ', 'ο', 'π', 'ρ', 'σ', 'τ',
			'υ', 'φ', 'χ', 'ψ', 'ω'
		};

		public static double Ecliptic2000_deg_2006 = 23.439279444444445;

		public static double Ecliptic2000_deg_1976 = 23.439291111111114;

		public static readonly double[] cosEcliptic2000 = new double[2]
		{
			Math.Cos(Ecliptic2000_deg_1976 / (180.0 / Math.PI)),
			Math.Cos(Ecliptic2000_deg_2006 / (180.0 / Math.PI))
		};

		public static readonly double[] sinEcliptic2000 = new double[2]
		{
			Math.Sin(Ecliptic2000_deg_1976 / (180.0 / Math.PI)),
			Math.Sin(Ecliptic2000_deg_2006 / (180.0 / Math.PI))
		};

		public static int EclipticID = 0;

		public const double MoonRadius = 0.2725076;

		public const double MoonRadiusArcSec = 932.58;

		public const double MoonRadiusKM = 1737.4;

		public const double EarthFlattening = 0.0033528106647474805;

		public const double EarthPolar = 0.9966471893352525;

		public static readonly double EarthEllipticitySqrd = 0.0066943799901413165;

		public static readonly double sqrt_1LessEarthEllipticitySqrd = Math.Sqrt(1.0 - EarthEllipticitySqrd);

		public const double SiderealDay = 0.99726956633;

		public const double SiderealIn1Hour = 0.2625161707907961;

		public static readonly string[] Planets = new string[10] { "", "Mercury", "Venus", "Earth", "Mars", "Jupiter", "Saturn", "Uranus", "Neptune", "Pluto" };

		internal static readonly string[] SatelliteNames_All = new string[52]
		{
			"Phobos", "Deimos", "Io", "Europa", "Ganymede", "Callisto", "Amalthea", "Himalia", "Elara", "Pasiphae",
			"Sinope", "Lysithea", "Carme", "Ananke", "Leda", "Thebe", "Adrastea", "Metis", "Mimas", "Enceladus",
			"Tethys", "Dione", "Rhea", "Titan", "Hyperion", "Iapetus", "Phoebe", "Helene", "Telesto", "Calypso",
			"Ariel", "Umbriel", "Titania", "Oberon", "Miranda", "Cordelia", "Ophelia", "Bianca", "Cressida", "Desdemona",
			"Juliet", "Portia", "Rosalind", "Belinda", "Puck", "Triton", "Nereid", "Charon", "Nix", "Hydra",
			"Kerberos", "Styx"
		};

		public static readonly string[] ShortMonths = new string[13]
		{
			"", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
			"Oct", "Nov", "Dec"
		};

		public static readonly string[] Months = new string[13]
		{
			"", "January", "February", "March", "April", "May", "June", "July", "August", "September",
			"October", "November", "December"
		};

		private static int MoonFileRecordLength = 74;

		public static string AsteroidObservationsFile = "Asteroid~Observations.xml";

		public static bool DisableInternetAccess = false;

		private static double[] Cons_RAstart;

		private static double[] Cons_RAend;

		private static double[] Cons_Declination;

		private static int[] Cons_Index;

		private static string[] ConstellationAbbrev;

		private static string[] ConstellationName;

		internal static List<Star_IDs> StarId = new List<Star_IDs>();

		private static bool StarIDNotAvailable = false;

		internal static List<StarNamesFromHip> HipName = new List<StarNamesFromHip>();

		private static bool StarNameNotAvailable = false;

		private static readonly double[,] deltaTspline = new double[71, 6]
		{
			{ -2000.0, -1900.0, 46000.0, -2500.0, 0.0, 0.0 },
			{ -1900.0, -1800.0, 43500.0, -2500.0, 0.0, 0.0 },
			{ -1800.0, -1700.0, 41000.0, -2100.0, 0.0, 0.0 },
			{ -1700.0, -1600.0, 38900.0, -2100.0, 0.0, 0.0 },
			{ -1600.0, -1500.0, 36800.0, -2100.0, 0.0, 0.0 },
			{ -1500.0, -1400.0, 34800.0, -2000.0, 0.0, 0.0 },
			{ -1400.0, -1300.0, 32800.0, -2000.0, 0.0, 0.0 },
			{ -1300.0, -1200.0, 31000.0, -1800.0, 0.0, 0.0 },
			{ -1200.0, -1100.0, 29200.0, -1800.0, 0.0, 0.0 },
			{ -1100.0, -1000.0, 27400.0, -1800.0, 0.0, 0.0 },
			{ -1000.0, -900.0, 25600.0, -1800.0, 0.0, 0.0 },
			{ -900.0, -800.0, 23800.0, -1800.0, 0.0, 0.0 },
			{ -800.0, -720.0, 22000.0, -1228.0, 0.0, 0.0 },
			{ -720.0, -100.0, 20371.848, -9999.586, 776.247, 409.16 },
			{ -100.0, 400.0, 11557.668, -5822.27, 1303.151, -503.433 },
			{ 400.0, 1000.0, 6535.116, -5671.519, -298.291, 1085.087 },
			{ 1000.0, 1150.0, 1650.393, -753.21, 184.811, -25.346 },
			{ 1150.0, 1300.0, 1056.647, -459.628, 108.771, -24.641 },
			{ 1300.0, 1500.0, 681.149, -421.345, 61.953, -29.414 },
			{ 1500.0, 1600.0, 292.343, -192.841, -6.572, 16.197 },
			{ 1600.0, 1650.0, 109.127, -78.697, 10.505, 3.018 },
			{ 1650.0, 1720.0, 43.952, -68.089, 38.333, -2.127 },
			{ 1720.0, 1800.0, 12.068, 2.507, 41.731, -37.939 },
			{ 1800.0, 1810.0, 18.367, -3.481, -1.126, 1.918 },
			{ 1810.0, 1820.0, 15.678, 0.021, 4.629, -3.812 },
			{ 1820.0, 1830.0, 16.516, -2.157, -6.806, 3.25 },
			{ 1830.0, 1840.0, 10.804, -6.018, 2.944, -0.096 },
			{ 1840.0, 1850.0, 7.634, -0.416, 2.658, -0.539 },
			{ 1850.0, 1855.0, 9.338, 1.642, 0.261, -0.883 },
			{ 1855.0, 1860.0, 10.357, -0.486, -2.389, 1.558 },
			{ 1860.0, 1865.0, 9.04, -0.591, 2.284, -2.477 },
			{ 1865.0, 1870.0, 8.255, -3.456, -5.148, 2.72 },
			{ 1870.0, 1875.0, 2.371, -5.593, 3.011, -0.914 },
			{ 1875.0, 1880.0, -1.126, -2.314, 0.269, -0.039 },
			{ 1880.0, 1885.0, -3.21, -1.893, 0.152, 0.563 },
			{ 1885.0, 1890.0, -4.388, 0.101, 1.842, -1.438 },
			{ 1890.0, 1895.0, -3.884, -0.531, -2.474, 1.871 },
			{ 1895.0, 1900.0, -5.017, 0.134, 3.138, -0.232 },
			{ 1900.0, 1905.0, -1.977, 5.715, 2.443, -1.257 },
			{ 1905.0, 1910.0, 4.923, 6.828, -1.329, 0.72 },
			{ 1910.0, 1915.0, 11.142, 6.33, 0.831, -0.825 },
			{ 1915.0, 1920.0, 17.479, 5.518, -1.643, 0.262 },
			{ 1920.0, 1925.0, 21.617, 3.02, -0.856, 0.008 },
			{ 1925.0, 1930.0, 23.789, 1.333, -0.831, 0.127 },
			{ 1930.0, 1935.0, 24.418, 0.052, -0.449, 0.142 },
			{ 1935.0, 1940.0, 24.164, -0.419, -0.022, 0.702 },
			{ 1940.0, 1945.0, 24.426, 1.645, 2.086, -1.106 },
			{ 1945.0, 1950.0, 27.05, 2.499, -1.232, 0.614 },
			{ 1950.0, 1953.0, 28.932, 1.127, 0.22, -0.277 },
			{ 1953.0, 1956.0, 30.002, 0.737, -0.61, 0.631 },
			{ 1956.0, 1959.0, 30.76, 1.409, 1.282, -0.799 },
			{ 1959.0, 1962.0, 32.652, 1.577, -1.115, 0.507 },
			{ 1962.0, 1965.0, 33.621, 0.868, 0.406, 0.199 },
			{ 1965.0, 1968.0, 35.093, 2.275, 1.002, -0.414 },
			{ 1968.0, 1971.0, 37.956, 3.035, -0.242, 0.202 },
			{ 1971.0, 1974.0, 40.951, 3.157, 0.364, -0.229 },
			{ 1974.0, 1977.0, 44.244, 3.199, -0.323, 0.172 },
			{ 1977.0, 1980.0, 47.291, 3.069, 0.193, -0.192 },
			{ 1980.0, 1983.0, 50.361, 2.878, -0.384, 0.081 },
			{ 1983.0, 1986.0, 52.936, 2.354, -0.14, -0.165 },
			{ 1986.0, 1989.0, 54.984, 1.577, -0.637, 0.448 },
			{ 1989.0, 1992.0, 56.373, 1.648, 0.708, -0.276 },
			{ 1992.0, 1995.0, 58.453, 2.235, -0.121, 0.11 },
			{ 1995.0, 1998.0, 60.678, 2.324, 0.21, -0.313 },
			{ 1998.0, 2001.0, 62.898, 1.804, -0.729, 0.109 },
			{ 2001.0, 2004.0, 64.083, 0.674, -0.402, 0.199 },
			{ 2004.0, 2007.0, 64.553, 0.466, 0.194, -0.017 },
			{ 2007.0, 2010.0, 65.197, 0.804, 0.144, -0.084 },
			{ 2010.0, 2013.0, 66.061, 0.839, -0.109, 0.128 },
			{ 2013.0, 2016.0, 66.92, 1.007, 0.277, -0.095 },
			{ 2016.0, 2019.0, 68.109, 1.277, -0.007, -0.139 }
		};

		private static double[,] deltaTUncerts = new double[36, 2]
		{
			{ -2000.0, 1000.0 },
			{ -1600.0, 720.0 },
			{ -900.0, 360.0 },
			{ -720.0, 180.0 },
			{ -700.0, 170.0 },
			{ -600.0, 160.0 },
			{ -500.0, 150.0 },
			{ -400.0, 130.0 },
			{ -300.0, 120.0 },
			{ -200.0, 110.0 },
			{ -100.0, 100.0 },
			{ 0.0, 90.0 },
			{ 100.0, 80.0 },
			{ 200.0, 70.0 },
			{ 300.0, 60.0 },
			{ 400.0, 50.0 },
			{ 500.0, 40.0 },
			{ 600.0, 40.0 },
			{ 700.0, 30.0 },
			{ 800.0, 25.0 },
			{ 900.0, 20.0 },
			{ 1000.0, 15.0 },
			{ 1610.0, 15.0 },
			{ 1620.0, 20.0 },
			{ 1650.0, 20.0 },
			{ 1660.0, 15.0 },
			{ 1670.0, 10.0 },
			{ 1680.0, 5.0 },
			{ 1720.0, 5.0 },
			{ 1730.0, 2.0 },
			{ 1760.0, 2.0 },
			{ 1770.0, 1.0 },
			{ 1780.0, 1.0 },
			{ 1790.0, 1.0 },
			{ 1800.0, 0.5 },
			{ 1810.0, 0.5 }
		};

		internal static List<deltaTAvalues> delta_TA = new List<deltaTAvalues>();

		internal static DeleteSupercededFiles DSF;

		private static byte[] GeoHeights;

		private static bool GeoHeightsIsFilled = false;

		internal static bool DE_LongEphemerisFileExists = false;

		internal static bool DE_EphemerisFileExists = false;

		internal static bool MustUseVSOP87 = false;

		public static bool LOLAFileExists;

		private static string occultVersion = "";

		public static HelpNavigator Help_Navigator = (HelpNavigator)(-2147483642);

		internal static DisplayData DataBoxDia;

		private static double LastValidEOPdate = 0.0;

		internal static bool EOPdateSet = false;

		private static FileStream AD;

		private static BinaryReader ADread;

		private static long FileLength = 1000000L;

		public static string OccultVersion
		{
			get
			{
				if (occultVersion == "")
				{
					occultVersion = Assembly.GetExecutingAssembly().GetName(copiedName: false).Version!.ToString();
				}
				return occultVersion;
			}
		}

		public static string OccultVersion_Short => OccultVersion;

		public static bool IsInVisualStudio => Debugger.IsAttached;

		public static bool Administrator => Settings.Default.Administrator;

		public static bool AdministratorRegional => Settings.Default.AdministratorRegional;

		public static bool AdministratorGlobal => Settings.Default.AdministratorGlobal;

		public static bool DisplayDownloadsAtStartUP
		{
			get
			{
				if (Settings.Default.DisplayUpdatesFrequency == 0m)
				{
					return false;
				}
				DateTime updatePageLastDisplayed = Settings.Default.UpdatePageLastDisplayed;
				DateTime value = DateTime.Now.AddDays(0.0 - (double)Settings.Default.DisplayUpdatesFrequency);
				if (updatePageLastDisplayed.CompareTo(value) < 0)
				{
					return true;
				}
				return false;
			}
		}

		public static bool DisplayNewInstallationDownloadMessage
		{
			get
			{
				DateTime updatePageLastDisplayed = Settings.Default.UpdatePageLastDisplayed;
				DateTime value = DateTime.Now.AddDays(-400.0);
				if (updatePageLastDisplayed.CompareTo(value) < 0)
				{
					return true;
				}
				return false;
			}
		}

		[DllImport("kernel32")]
		private static extern long WritePrivateProfileString(string section, string key, string val, string filePath);

		[DllImport("kernel32")]
		private static extern int GetPrivateProfileString(string section, string key, string def, StringBuilder retVal, int size, string filePath);

		[DllImport("kernel32")]
		private static extern int GetLocaleInfo(int Locale, int LCType, StringBuilder lpLCData, int cchData);

		[DllImport("user32")]
		private static extern int GetSystemMetrics(int nIndex);

		[DllImport("winmm.dll")]
		public static extern int waveOutGetVolume(IntPtr h, out uint dwVolume);

		[DllImport("winmm.dll")]
		public static extern int waveOutSetVolume(IntPtr h, uint dwVolume);

		public static void Set_Path_for_DLL_Data(string ApplicationPath)
		{
			if (Thread.CurrentThread.CurrentCulture.NumberFormat.NumberDecimalSeparator != ".")
			{
				Thread.CurrentThread.CurrentCulture = new CultureInfo("en-US");
			}
			AppPath = ApplicationPath;
			Asteroid_Observations_Reports.AppPath = ApplicationPath;
			GetStarPosition.AppPath = ApplicationPath;
			DisplayMPOccultations.AppPath = ApplicationPath;
			DoubleStars.AppPath = ApplicationPath;
			ftp.AppPath = ApplicationPath;
			GoogleEarth.AppPath = ApplicationPath;
			JPL_DE.AppPath = ApplicationPath;
			LunarProfile.AppPath = ApplicationPath;
			LunarObservations.AppPath = ApplicationPath;
			LunarOccultations.AppPath = ApplicationPath;
			Maps.AppPath = ApplicationPath;
			MinorPlanetOccultationElements.AppPath = ApplicationPath;
			Output.AppPath = ApplicationPath;
			PlanetOccultationElements.AppPath = ApplicationPath;
			ReductionProfile.AppPath = ApplicationPath;
			Satellites.AppPath = ApplicationPath;
			SolarEclipses.AppPath = ApplicationPath;
			XZ80Q.AppPath = ApplicationPath;
		}

		public static void Set_JPLDE_Availability()
		{
			DE_EphemerisFileExists = File.Exists(AppPath + "\\Resource Files\\DE_Ephemeris.bin");
			DE_LongEphemerisFileExists = File.Exists(AppPath + "\\Resource Files\\DE_LongEphemeris.bin");
		}

		public static DateTime GetUpdateDate(string InLine)
		{
			int num = InLine.IndexOf(">");
			if (!int.TryParse(InLine.Substring(num + 1, 2), out var result))
			{
				result = 0;
			}
			if (!int.TryParse(InLine.Substring(num + 3, 2), out var result2))
			{
				result2 = 0;
			}
			if (!int.TryParse(InLine.Substring(num + 5), out var result3))
			{
				result3 = 0;
			}
			return new DateTime(result3, result2, result);
		}

		internal static void SetAnimatedCursor(ref int i)
		{
			switch (i)
			{
			case 0:
				Cursor.set_Current(Cursors.get_PanNorth());
				break;
			case 1:
				Cursor.set_Current(Cursors.get_PanNE());
				break;
			case 2:
				Cursor.set_Current(Cursors.get_PanEast());
				break;
			case 3:
				Cursor.set_Current(Cursors.get_PanSE());
				break;
			case 4:
				Cursor.set_Current(Cursors.get_PanSouth());
				break;
			case 5:
				Cursor.set_Current(Cursors.get_PanSW());
				break;
			case 6:
				Cursor.set_Current(Cursors.get_PanWest());
				break;
			case 7:
				Cursor.set_Current(Cursors.get_PanNW());
				break;
			}
			i++;
			if (i > 7)
			{
				i = 0;
			}
		}

		public static string OccultServer()
		{
			return Settings.Default.OccultServer;
		}

		public static void AdviseDE_EphemStatus(bool Fatal)
		{
			//IL_0051: Unknown result type (might be due to invalid IL or missing references)
			if (!DE_EphemerisFileExists & !DE_LongEphemerisFileExists)
			{
				string text = "For accurate predictions, Occult relies\r\n upon the JPL-DE ephemerides.\r\n\r\nYour installation of Occult does not include \r\nthe relevant files.";
				text = ((!Fatal) ? (text + " The effect of not having\r\nthem may compromise the accuracy of predictions \r\nand/or reductions") : (text + " The effect of not having\r\nthem may be fatal to functions on this page"));
				text += "\r\n\r\nYou are strongly urged to download either\r\n #34 JPL Planetary ephemeris (96MB), or\r\n #35 JPL 6000-year planetary ephemeris (550MB)";
				MessageBox.Show(text, "JPL-DE files are not present", (MessageBoxButtons)0, (MessageBoxIcon)64, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
			}
		}

		public static void UpdateSettingsForDLL()
		{
			if (Settings.Default.CallUpgradeDLL)
			{
				((SettingsBase)Settings.Default).Save();
				Settings.Default.CallUpgradeDLL = false;
			}
		}

		public static void UpgradeSettings()
		{
			((ApplicationSettingsBase)Settings.Default).Upgrade();
		}

		public static void RefreshOccultServer()
		{
			Settings.Default.OccultServer = ((SettingsBase)Settings.Default).get_Properties().get_Item("OccultServer").get_DefaultValue()
				.ToString();
		}

		public static void UpdateDefaultDownloadAddresses()
		{
			if (Settings.Default.ASTORB_Server.Substring(0, 3) == "ftp")
			{
				Settings.Default.ASTORB_Server = ((SettingsBase)Settings.Default).get_Properties().get_Item("ASTORB_Server").get_DefaultValue()
					.ToString();
			}
			if (Settings.Default.LuckyStar.Contains("star/pred"))
			{
				Settings.Default.LuckyStar = ((SettingsBase)Settings.Default).get_Properties().get_Item("LuckyStar").get_DefaultValue()
					.ToString();
			}
			if (Settings.Default.ASTORB_Mirror.Contains("u-strasbg.fr"))
			{
				Settings.Default.ASTORB_Mirror = ((SettingsBase)Settings.Default).get_Properties().get_Item("ASTORB_Mirror").get_DefaultValue()
					.ToString();
			}
			if (Settings.Default.WDSDownloadServer.Contains("u-strasbg.fr"))
			{
				Settings.Default.WDSDownloadServer = ((SettingsBase)Settings.Default).get_Properties().get_Item("WDSDownloadServer").get_DefaultValue()
					.ToString();
			}
			if (Settings.Default.WDSCodesServer.Contains("u-strasbg.fr"))
			{
				Settings.Default.WDSCodesServer = ((SettingsBase)Settings.Default).get_Properties().get_Item("WDSCodesServer").get_DefaultValue()
					.ToString();
			}
			if (Settings.Default.WDSCodesServer.Contains("astorb"))
			{
				Settings.Default.WDSCodesServer = ((SettingsBase)Settings.Default).get_Properties().get_Item("WDSCodesServer").get_DefaultValue()
					.ToString();
			}
			if (Settings.Default.AAVSOdownloadServer.Contains("u-strasbg.fr"))
			{
				Settings.Default.AAVSOdownloadServer = ((SettingsBase)Settings.Default).get_Properties().get_Item("AAVSOdownloadServer").get_DefaultValue()
					.ToString();
			}
			if (Settings.Default.CDS_portal.Contains("u-strasbg.fr"))
			{
				Settings.Default.CDS_portal = ((SettingsBase)Settings.Default).get_Properties().get_Item("CDS_portal").get_DefaultValue()
					.ToString();
			}
		}

		public static void SaveDefaultSettings()
		{
			((SettingsBase)Settings.Default).Save();
		}

		public static bool InternetIsAvailable()
		{
			if (DisableInternetAccess)
			{
				return false;
			}
			if (!NetworkInterface.GetIsNetworkAvailable())
			{
				return false;
			}
			try
			{
				Dns.GetHostEntry("naif.jpl.nasa.gov");
				return true;
			}
			catch
			{
			}
			try
			{
				Dns.GetHostEntry("google.com");
				return true;
			}
			catch
			{
			}
			return false;
		}

		public static string GetSiteLongitude()
		{
			return Settings.Default.Site_Longitude_dd_d;
		}

		public static string GetSiteLatitude()
		{
			return Settings.Default.Site_Latitude_dd_d;
		}

		public static string GetSiteAperture()
		{
			return Settings.Default.Site_Aperture.ToString();
		}

		public static bool GetPreserveFuture()
		{
			return Settings.Default.PreserveFuture_dat;
		}

		public static DateTime GetLastUpdateDate()
		{
			return Settings.Default.LastUpdate;
		}

		public static void SetLastUpdateDate(DateTime LastUpdateDate)
		{
			Settings.Default.LastUpdate = LastUpdateDate;
		}

		public static bool UpdateIsDue()
		{
			int updateFrequency = Settings.Default.UpdateFrequency;
			DateTime dateTime = DateTime.Now;
			switch (updateFrequency)
			{
			case 1:
				dateTime = GetLastUpdateDate().AddHours(18.0);
				break;
			case 2:
				dateTime = GetLastUpdateDate().AddDays(7.0).AddHours(-6.0);
				break;
			case 3:
				dateTime = GetLastUpdateDate().AddMonths(1);
				break;
			case 4:
				dateTime = GetLastUpdateDate().AddYears(1);
				break;
			case 5:
				dateTime = GetLastUpdateDate().AddYears(10);
				break;
			}
			return dateTime <= DateTime.Now;
		}

		public static void UploadLightCurvesIsDue(bool AutoShow)
		{
			//IL_0069: Unknown result type (might be due to invalid IL or missing references)
			//IL_00a2: Unknown result type (might be due to invalid IL or missing references)
			//IL_00a8: Invalid comparison between Unknown and I4
			string path = AppPath + "\\Observations\\LightCurves\\";
			if (!Directory.Exists(path))
			{
				return;
			}
			List<FileInfo> list = (from f in new DirectoryInfo(path).GetFiles("*.dat")
				orderby f.LastWriteTime
				select f).ToList();
			if (list.Count == 0)
			{
				if (!AutoShow)
				{
					MessageBox.Show("No unsubmitted light curves to manage", "No light curves", (MessageBoxButtons)0);
				}
				return;
			}
			DateTime dateTime = list[0].LastWriteTime.AddMonths(1);
			if ((!AutoShow || !(dateTime >= DateTime.Now)) && (int)((Form)new LightCurveReminder(list.Count)).ShowDialog() == 1)
			{
				http.Email_LightCurveReports();
			}
		}

		public static void Check_UCAC4_Updated()
		{
			UCAC4.CheckUCAC4_Updated(CheckAll: false);
		}

		public static string IniReadValue(string Section, string Key, string INIPath)
		{
			if (INIPath == null)
			{
				return "Invalid read; Path not set";
			}
			StringBuilder stringBuilder = new StringBuilder(255);
			GetPrivateProfileString(Section, Key, "", stringBuilder, 255, INIPath);
			return stringBuilder.ToString();
		}

		public static void GetScreenResolution(out int X, out int Y, string INIPath)
		{
			X = GetSystemMetrics(0);
			Y = GetSystemMetrics(1);
		}

		public static void GetSystemNumberFormat(out string DecimalPoint, out string Thousands, string INIPath)
		{
			StringBuilder stringBuilder = new StringBuilder(100);
			GetLocaleInfo(1024, 14, stringBuilder, 99);
			DecimalPoint = Convert.ToString(stringBuilder);
			GetLocaleInfo(1024, 15, stringBuilder, 99);
			Thousands = Convert.ToString(stringBuilder);
		}

		public static void VolumeOn()
		{
			if (VolumeLevelSaved)
			{
				waveOutSetVolume(IntPtr.Zero, _savedVolumeLevel);
			}
		}

		public static void VolumeOff()
		{
			waveOutGetVolume(IntPtr.Zero, out _savedVolumeLevel);
			VolumeLevelSaved = true;
			waveOutSetVolume(IntPtr.Zero, 0u);
		}

		public static void SetLunarLimbAvailability()
		{
			LOLAFileExists = File.Exists(AppPath + "\\Resource Files\\LOLA128.bin");
		}

		public static void ReSetCALL_URL()
		{
			Settings.Default.AsteroidLightCurveData_url = ((SettingsBase)Settings.Default).get_Properties().get_Item("AsteroidLightCurveData_url").get_DefaultValue()
				.ToString();
		}

		public static string Date_from_JD(double JD)
		{
			Date_from_JD(JD, out var Year, out var Month, out var day);
			return Year + Month.ToString().PadLeft(2, '0') + day.ToString().PadLeft(2, '0');
		}

		public static string DateShort_from_JD(double JD)
		{
			Date_from_JD(JD, out var Year, out var Month, out var day);
			return Year + ShortMonths[Month] + ((int)day).ToString().PadLeft(2, '0');
		}

		public static double DayOfYear(double JD)
		{
			Date_from_JD(JD, out var Year, out var _, out var _);
			return JD - JD_from_Date(Year, 1, 1.0);
		}

		public static string DateTime_from_JD(double JD)
		{
			return DateTime_from_JD(JD, ToSecs: false);
		}

		public static string DateTime_from_JD(double JD, bool ToSecs)
		{
			Date_from_JD(JD, out var Year, out var Month, out var day);
			int num = (int)Math.Floor(day);
			if (ToSecs)
			{
				return Year + " " + ShortMonths[Month] + " " + num.ToString().PadLeft(2, '0') + DEGtoDMS(24.0 * (day - (double)num), 3, 1, MinutesOnly: false).Insert(6, "m").Insert(3, "h") + "s";
			}
			return Year + " " + ShortMonths[Month] + " " + num.ToString().PadLeft(2, '0') + DEGtoDMS(24.0 * (day - (double)num), 3, 0, MinutesOnly: true).Insert(3, "h") + "m";
		}

		public static string Date_from_JD(double JD, int DecimalPlaces)
		{
			return Date_from_JD(JD, DecimalPlaces, Use_BC: true);
		}

		public static string Date_from_JD(double JD, int DecimalPlaces, bool Use_BC)
		{
			StringBuilder stringBuilder = new StringBuilder();
			Date_from_JD(JD, out var Year, out var Month, out var day);
			if (Year <= 0 && Use_BC)
			{
				stringBuilder.AppendFormat("{0,4:F0}BC ", -Year + 1);
			}
			else
			{
				stringBuilder.AppendFormat("{0,4:F0} ", Year);
			}
			stringBuilder.Append(ShortMonths[Month]);
			switch (DecimalPlaces)
			{
			case 0:
				stringBuilder.AppendFormat("{0,3:F0}", Math.Floor(day));
				break;
			case 1:
				stringBuilder.AppendFormat("{0,5:F1}", day);
				break;
			case 2:
				stringBuilder.AppendFormat("{0,6:F2}", day);
				break;
			case 3:
				stringBuilder.AppendFormat("{0,7:F3}", day);
				break;
			case 4:
				stringBuilder.AppendFormat("{0,8:F4}", day);
				break;
			default:
				stringBuilder.AppendFormat("{0,9:F5}", day);
				break;
			}
			return stringBuilder.ToString();
		}

		public static void Date_from_JD(double JD, out int Year, out int Month, out double day)
		{
			double num = Math.Floor(JD) + 0.5;
			if (num > JD)
			{
				num -= 1.0;
			}
			double num2 = JD - num;
			double num4;
			double num3;
			if (num >= 2299160.5)
			{
				num3 = Math.Floor(num - 1720981.0);
				num4 = Math.Floor((num3 - 122.1) / 365.25);
				num4 = Math.Floor((num3 - 15.0 + Math.Floor(num4 / 100.0) - Math.Floor(num4 / 400.0) - 122.1) / 365.25);
				num3 = num3 - 15.0 + Math.Floor(num4 / 100.0) - Math.Floor(num4 / 400.0);
			}
			else
			{
				num3 = Math.Floor(num - 1720994.0);
				num4 = Math.Floor((num3 - 122.1) / 365.25);
			}
			double num5 = Math.Floor((num3 - Math.Floor(365.25 * num4)) / 30.6001);
			day = num3 - Math.Floor(365.25 * num4) - Math.Floor(30.6001 * num5) + num2;
			Month = Convert.ToInt32(num5);
			Year = Convert.ToInt32(num4);
			if (Month > 13)
			{
				Month -= 13;
				Year++;
			}
			else
			{
				Month--;
			}
		}

		public static double JD_from_Date(int Year, int Month, double day)
		{
			double num = 12.0 * (Convert.ToDouble(Year) + 4800.0) + (double)Month - 3.0;
			double num2 = Math.Floor((2.0 * (num % 12.0) + 7.0 + 365.0 * num) / 12.0) + day + Math.Floor(num / 48.0) - 32083.5;
			if (Year > 1582 || (Year == 1582 && Month > 10) || (Year == 1582 && Month == 10 && day > 4.0))
			{
				num2 = num2 + Math.Floor(num / 4800.0) - Math.Floor(num / 1200.0) + 38.0;
			}
			return num2;
		}

		public static double JD_from_Date(string Yr, string Mth, string Day, string Hour)
		{
			if (!int.TryParse(Yr, out var result))
			{
				result = 2000;
			}
			if (!int.TryParse(Mth, out var result2))
			{
				result2 = 1;
			}
			if (!double.TryParse(Day, out var result3))
			{
				result3 = 1.0;
			}
			if (!double.TryParse(Hour, out var result4))
			{
				result4 = 0.0;
			}
			return JD_from_Date(result, result2, result3 + result4 / 24.0);
		}

		public static double MJD_now()
		{
			return JD_from_Date(DateTime.Now.ToUniversalTime().Year, DateTime.Now.ToUniversalTime().Month, DateTime.Now.ToUniversalTime().Day) - 2400000.5 + ((double)DateTime.Now.ToUniversalTime().Hour + (double)DateTime.Now.ToUniversalTime().Minute / 60.0) / 24.0;
		}

		public static void Date_from_MJD(double JD, out int Year, out int Month, out double day)
		{
			Date_from_JD(JD + 2400000.5, out Year, out Month, out day);
		}

		public static double BesselianYear(int Year, int Month, double Day)
		{
			return 2000.0 + (JD_from_Date(Year, Month, Day) - 2451545.0) / 365.25;
		}

		public static double BesselianYear(double JD)
		{
			return 2000.0 + (JD - 2451545.0) / 365.25;
		}

		public static double BesselianYear_to_JD(double BY)
		{
			return 2451545.0 + 365.25 * (BY - 2000.0);
		}

		public static int MaximumDaysInMonth(int Year, int Month)
		{
			int result = 31;
			if (Month == 4 || Month == 6 || Month == 9 || Month == 11)
			{
				result = 30;
			}
			if (Month == 2)
			{
				result = 28;
				if (Year % 4 == 0)
				{
					result = 29;
					if (Year % 400 == 0)
					{
						result = 28;
					}
				}
			}
			return result;
		}

		public static void DateOfEasterSunday(int Year, out int Month, out int Day)
		{
			if (Year > 1582)
			{
				int num = Year % 19;
				int num2 = Year / 100;
				int num3 = Year % 100;
				int num4 = num2 / 4;
				int num5 = num2 % 4;
				int num6 = (num2 + 8) / 25;
				int num7 = (num2 - num6 + 1) / 3;
				int num8 = (19 * num + num2 - num4 - num7 + 15) % 30;
				int num9 = num3 / 4;
				int num10 = num3 % 4;
				int num11 = (32 + 2 * num5 + 2 * num9 - num8 - num10) % 7;
				int num12 = (num + 11 * num8 + 22 * num11) / 451;
				Month = (num8 + num11 - 7 * num12 + 114) / 31;
				Day = (num8 + num11 - 7 * num12 + 114) % 31 + 1;
			}
			else
			{
				int num = Year % 4;
				int num2 = Year % 7;
				int num3 = Year % 19;
				int num4 = (19 * num3 + 15) % 30;
				int num5 = (2 * num + 4 * num2 - num4 + 34) % 7;
				Month = (num4 + num5 + 114) / 31;
				Day = (num4 + num5 + 114) % 31 + 1;
			}
		}

		public static string DEGtoDMS(double Degree, int DegreesLength, int DecimalLength, bool MinutesOnly)
		{
			return DEGtoDMS(Degree, DegreesLength, DecimalLength, MinutesOnly, IncludeLeadingZeros: false, IncludePlusSymbol: false, IncludeDMS: false, IncludeHMS: false, IncludeHourDegLeadingZero: false);
		}

		public static string DEGtoDMS(double Degree, int DegreesLength, int DecimalLength, bool MinutesOnly, bool IncludeLeadingZeros)
		{
			return DEGtoDMS(Degree, DegreesLength, DecimalLength, MinutesOnly, IncludeLeadingZeros, IncludePlusSymbol: false, IncludeDMS: false, IncludeHMS: false, IncludeHourDegLeadingZero: false);
		}

		public static string DEGtoDMS(double Degree, int DegreesLength, int DecimalLength, bool MinutesOnly, bool IncludeLeadingZeros, bool IncludePlusSymbol)
		{
			return DEGtoDMS(Degree, DegreesLength, DecimalLength, MinutesOnly, IncludeLeadingZeros, IncludePlusSymbol, IncludeDMS: false, IncludeHMS: false, IncludeHourDegLeadingZero: false);
		}

		public static string DEGtoDMS(double Degree, int DegreesLength, int DecimalLength, bool MinutesOnly, bool IncludeLeadingZeros, bool IncludePlusSymbol, bool IncludeDMS, bool IncludeHMS)
		{
			return DEGtoDMS(Degree, DegreesLength, DecimalLength, MinutesOnly, IncludeLeadingZeros, IncludePlusSymbol, IncludeDMS, IncludeHMS, IncludeHourDegLeadingZero: false);
		}

		public static string DEGtoDMS(double Degree, int DegreesLength, int DecimalLength, bool MinutesOnly, bool IncludeLeadingZeros, bool IncludePlusSymbol, bool IncludeDMS, bool IncludeHMS, bool IncludeHourDegLeadingZero)
		{
			double[] array = new double[8] { 0.5, 0.05, 0.005, 0.0005, 5E-05, 5E-06, 5E-07, 5E-08 };
			char paddingChar = ' ';
			if (IncludeLeadingZeros)
			{
				paddingChar = '0';
			}
			double num = 0.0;
			if (DegreesLength < 2 || DegreesLength > 4)
			{
				DegreesLength = 4;
			}
			if (DecimalLength > 7)
			{
				DecimalLength = 4;
			}
			double num2 = Math.Abs(Degree);
			double num3 = 1.0;
			if (!MinutesOnly)
			{
				num3 = 60.0;
			}
			if (DecimalLength >= 0)
			{
				num2 += array[DecimalLength] / 60.0 / num3;
			}
			double num4 = Math.Floor(num2);
			double num5 = 60.0 * (num2 - num4);
			double num6;
			if (MinutesOnly)
			{
				num6 = num5 - array[DecimalLength] + 1E-10;
			}
			else
			{
				num = Math.Floor(num5);
				num6 = 0.0;
				if (DecimalLength >= 0)
				{
					num6 = 60.0 * (num5 - num) - array[DecimalLength];
				}
			}
			string text = ((!IncludeHourDegLeadingZero) ? Convert.ToString(num4).PadLeft(DegreesLength) : Convert.ToString(num4).PadLeft(DegreesLength, '0'));
			if (Degree < 0.0)
			{
				text = text.Remove(0, 1).Insert(0, "-");
			}
			else if (IncludePlusSymbol)
			{
				text = text.Remove(0, 1).Insert(0, "+");
			}
			if (IncludeDMS)
			{
				text += "°";
			}
			else if (IncludeHMS)
			{
				text += "h";
			}
			if (!MinutesOnly)
			{
				text += num.ToString().PadLeft(2, paddingChar).PadLeft(3);
				if (IncludeDMS)
				{
					text += "'";
				}
				else if (IncludeHMS)
				{
					text += "m";
				}
			}
			text = DecimalLength switch
			{
				0 => text + string.Format("{0,1:F0}", num6).PadLeft(2, paddingChar).PadLeft(3), 
				1 => text + string.Format("{0,1:F1}", num6).PadLeft(4, paddingChar).PadLeft(5), 
				2 => text + string.Format("{0,1:F2}", num6).PadLeft(5, paddingChar).PadLeft(6), 
				3 => text + string.Format("{0,1:F3}", num6).PadLeft(6, paddingChar).PadLeft(7), 
				4 => text + string.Format("{0,1:F4}", num6).PadLeft(7, paddingChar).PadLeft(8), 
				5 => text + string.Format("{0,1:F5}", num6).PadLeft(7, paddingChar).PadLeft(9), 
				6 => text + string.Format("{0,1:F6}", num6).PadLeft(7, paddingChar).PadLeft(10), 
				7 => text + string.Format("{0,1:F7}", num6).PadLeft(7, paddingChar).PadLeft(11), 
				_ => text + string.Format("{0,1:F2}", num6).PadLeft(5, paddingChar).PadLeft(6), 
			};
			if (!MinutesOnly)
			{
				if (IncludeDMS)
				{
					text += "\"";
				}
				else if (IncludeHMS)
				{
					text += "s";
				}
			}
			else if (IncludeDMS)
			{
				text += "'";
			}
			else if (IncludeHMS)
			{
				text += "m";
			}
			return text;
		}

		public static string RA_Dec_ToIAUStarID(double RADeg, double DecDeg, int PLacesInDecSecs)
		{
			string text = DEGtoDMS(RADeg / 15.0, 2, PLacesInDecSecs + 1, MinutesOnly: false, IncludeLeadingZeros: true);
			if (text.Substring(0, 1) == " ")
			{
				text = "0" + text.Substring(1);
			}
			string text2 = DEGtoDMS(DecDeg, 3, PLacesInDecSecs, MinutesOnly: false, IncludeLeadingZeros: true);
			if (text2.Substring(0, 1) == " ")
			{
				text2 = "+" + text2.Substring(1);
			}
			if (text2.Substring(1, 1) == " ")
			{
				text2 = text2.Substring(0, 1) + "0" + text2.Substring(2);
			}
			return text.Substring(0, 2) + text.Substring(3, 2) + text.Substring(6) + text2.Substring(0, 3) + text2.Substring(4, 2) + text2.Substring(7);
		}

		public static double DMStoDeg(string sDDD_MM_DD)
		{
			bool flag = sDDD_MM_DD.IndexOf("-") >= 0;
			if (flag)
			{
				sDDD_MM_DD = sDDD_MM_DD.Replace("-", " ");
			}
			double result;
			double result2;
			double result3;
			bool num = double.TryParse(sDDD_MM_DD.Substring(1, 3), out result) & double.TryParse(sDDD_MM_DD.Substring(5, 2), out result2) & double.TryParse(sDDD_MM_DD.Substring(8), out result3);
			if (num)
			{
				result = Math.Abs(result) + result2 / 60.0 + result3 / 3600.0;
			}
			if (flag)
			{
				result = 0.0 - result;
			}
			if (!num)
			{
				return -1000.0;
			}
			return result;
		}

		public static double DegFromDMS_TextBoxes(string DegText, string MinText, string SecText)
		{
			if (!double.TryParse(DegText.Replace("-", ""), out var result))
			{
				result = 0.0;
			}
			if (!double.TryParse(MinText, out var result2))
			{
				result2 = 0.0;
			}
			if (!double.TryParse(SecText, out var result3))
			{
				result3 = 0.0;
			}
			double num = result + result2 / 60.0 + result3 / 3600.0;
			if (DegText.Contains("-"))
			{
				num = 0.0 - num;
			}
			return num;
		}

		public static int DecimalPlaces(string X)
		{
			string text = X.Trim();
			int num = text.IndexOf('.');
			int length = text.Length;
			if (text == ".")
			{
				return -1;
			}
			if (num < 0)
			{
				return 0;
			}
			return length - num - 1;
		}

		public static string FormatNumber(int Number, int LeadingDigits, int DecimalPlaces)
		{
			int num = LeadingDigits + DecimalPlaces + 1;
			if (DecimalPlaces == 0)
			{
				return string.Format("{0," + LeadingDigits + ":F0}.", Number);
			}
			return string.Format("{0," + num + ":F" + DecimalPlaces + "}", Number);
		}

		public static string FormatNumber(double Number, int LeadingDigits, int DecimalPlaces)
		{
			int num = LeadingDigits + DecimalPlaces + 1;
			if (DecimalPlaces < 0)
			{
				return "";
			}
			if (DecimalPlaces == 0)
			{
				return string.Format("{0," + LeadingDigits + ":F0}.", Number);
			}
			return string.Format("{0," + num + ":F" + DecimalPlaces + "}", Number);
		}

		public static string FormatNumber(float Number, int LeadingDigits, int DecimalPlaces)
		{
			int num = LeadingDigits + DecimalPlaces + 1;
			if (DecimalPlaces == 0)
			{
				return string.Format("{0," + LeadingDigits + ":F0}.", Number);
			}
			return string.Format("{0," + num + ":F" + DecimalPlaces + "}", Number);
		}

		public static int Integer_SignificantDigits(double num, int digits)
		{
			double num2 = Math.Log10(Math.Abs(num)) - (double)digits;
			if (num2 < 0.0)
			{
				num2 = 0.0;
			}
			num2 = ((!((num2 > 0.0) & (num2 % 1.0 < 0.2))) ? Math.Ceiling(num2) : Math.Floor(num2));
			double num3 = Math.Pow(10.0, num2);
			return (int)(num / num3) * (int)num3;
		}

		public static int SignificantDigitLocation_DecimalNumbers(double Uncertainty)
		{
			Uncertainty = Math.Abs(Uncertainty);
			if (Uncertainty == 0.0)
			{
				return 1;
			}
			if (Uncertainty >= 1.99999)
			{
				return 0;
			}
			if (Uncertainty >= 1.0)
			{
				return 1;
			}
			return (int)Math.Ceiling(0.0 - Math.Log10(Uncertainty / 1.99999));
		}

		public static string HTML_EncodeString(string ToEncode)
		{
			return ToEncode.Replace("&", "&amp;").Replace("<", "&lt;").Replace(">", "&gt;")
				.Replace("\"", "&quot;")
				.Replace("'", "&apos;");
		}

		public static string HTML_DecodeString(string ToDecode)
		{
			return ToDecode.Replace("&amp;", "&").Replace("&lt;", "<").Replace("&gt;", ">")
				.Replace("&quot;", "\"")
				.Replace("&apos;", "'");
		}

		public static string Roman_from_Numeral(int Numeral)
		{
			if (Numeral <= 21)
			{
				return Romans[Numeral];
			}
			return "";
		}

		public static int Numeral_from_Roman(string Roman)
		{
			Roman = Roman.ToUpper();
			for (int i = 0; i < Romans.Length; i++)
			{
				if (Romans[i] == Roman)
				{
					return i;
				}
			}
			return 0;
		}

		public static double delta_T(double JD)
		{
			Date_from_JD(JD, out var Year, out var Month, out var day);
			double UT1UTC;
			double x;
			double y;
			return delta_T(Year, Month, day, out UT1UTC, out x, out y);
		}

		public static double delta_T(double JD, out double UT1UTC, out double x, out double y)
		{
			Date_from_JD(JD, out var Year, out var Month, out var day);
			return delta_T(Year, Month, day, out UT1UTC, out x, out y);
		}

		public static double delta_T(int Year, int Month, double Day)
		{
			double UT1UTC;
			double x;
			double y;
			return delta_T(Year, Month, Day, out UT1UTC, out x, out y);
		}

		public static double delta_T(int Year, int Month, double Day, out double UT1UTC)
		{
			double x;
			double y;
			return delta_T(Year, Month, Day, out UT1UTC, out x, out y);
		}

		public static double delta_T(int Year, int Month, double Day, out double UT1UTC, out double x, out double y)
		{
			double num = 0.0;
			double num2 = 0.0;
			double num3 = 0.0;
			double num4 = 0.0;
			double num5 = JD_from_Date(Year, Month, Day);
			x = (y = 0.0);
			UT1UTC = 0.0;
			if (delta_TA.Count < 1)
			{
				string path = AppPath + "\\Resource Files\\deltaTA.dat";
				if (File.Exists(path))
				{
					using StreamReader streamReader = new StreamReader(path);
					do
					{
						string text = streamReader.ReadLine();
						deltaTAvalues deltaTAvalues2 = new deltaTAvalues();
						deltaTAvalues2.Year = double.Parse(text.Substring(0, 7));
						deltaTAvalues2.LeapSecond = int.Parse(text.Substring(8, 4));
						deltaTAvalues2.dUT1 = double.Parse(text.Substring(12));
						delta_TA.Add(deltaTAvalues2);
					}
					while (!streamReader.EndOfStream);
				}
			}
			if (Year > 1960 && Year < 1972)
			{
				double num6 = num5 - 2400000.5;
				if (Year == 1961)
				{
					num = 33.606818 + 0.001296 * (num6 - 37300.0);
					if (Month > 7)
					{
						num -= 0.05;
					}
				}
				else if (Year == 1962 || Year == 1963)
				{
					num = 34.029858 + 0.0011232 * (num6 - 37665.0);
					if (Year == 1963 && Month > 10)
					{
						num += 0.1;
					}
				}
				if (Year > 1963 && Year < 1966)
				{
					num = 35.42413 + 0.001296 * (num6 - 38761.0);
					if (Year == 1964)
					{
						if (Month > 3)
						{
							num += 0.1;
						}
						if (Month > 8)
						{
							num += 0.1;
						}
					}
					else
					{
						num += 0.1;
						if (Month > 2)
						{
							num += 0.1;
						}
						if (Month > 6)
						{
							num += 0.1;
						}
						if (Month > 8)
						{
							num += 0.1;
						}
					}
				}
				if (Year >= 1966)
				{
					num = 36.49717 + 0.002592 * (num6 - 39126.0);
					if (Year > 1968 || (Year == 1968 && Month > 1))
					{
						num -= 0.1;
					}
				}
				EarthOrientationParameters(num5, out x, out y, out UT1UTC);
				return Math.Floor(100.0 * num) / 100.0;
			}
			double dUT;
			if (Year > 1971)
			{
				double num7 = (double)Year + ((double)Month - 0.5) / 12.0;
				for (int i = 0; i < delta_TA.Count; i++)
				{
					if (num7 < delta_TA[i].Year)
					{
						if (delta_TA[i].Year != num3)
						{
							UT1UTC = num2 + (num7 - num3) / (delta_TA[i].Year - num3) * (delta_TA[i].dUT1 - 1.0 - num2);
						}
						else
						{
							UT1UTC = delta_TA[i].dUT1;
						}
						UT1UTC = Math.Floor(100.0 * UT1UTC) / 100.0;
						if (EarthOrientationParameters(num5, out x, out y, out dUT))
						{
							UT1UTC = dUT;
						}
						return num4 + 32.184;
					}
					if (num7 >= delta_TA[i].Year)
					{
						num4 = delta_TA[i].LeapSecond;
					}
					num2 = delta_TA[i].dUT1;
					num3 = delta_TA[i].Year;
				}
			}
			if (Year >= 1846)
			{
				EarthOrientationParameters(num5, out x, out y, out dUT);
			}
			double num8 = BesselianYear(Year, Month, Day);
			_ = (num8 - 1825.0) / 100.0;
			double num9 = (num8 - 1820.0) / 100.0;
			if (num8 < deltaTspline[0, 0] || num8 > 2500.0)
			{
				return -20.0 + 32.0 * num9 * num9;
			}
			if ((num8 <= 2500.0) & (num8 > deltaTspline[deltaTspline.GetLength(0) - 1, 1]))
			{
				return 60.0 * num9 - 22.35 * num9 * num9 + 4.723 * num9 * num9 * num9;
			}
			int num10 = 0;
			for (num10 = 0; num10 < deltaTspline.GetLength(0) - 1 && !((num8 >= deltaTspline[num10, 0]) & (num8 <= deltaTspline[num10, 1])); num10++)
			{
			}
			double num11 = (num8 - deltaTspline[num10, 0]) / (deltaTspline[num10, 1] - deltaTspline[num10, 0]);
			return deltaTspline[num10, 2] + num11 * deltaTspline[num10, 3] + num11 * num11 * deltaTspline[num10, 4] + num11 * num11 * num11 * deltaTspline[num10, 5];
		}

		public static void deltaT_Uncertainty(int year, out int deltaTUncertainty, out string deltaTUncertainty_string)
		{
			deltaTUncertainty = 0;
			double num = (double)year / 100.0 - 18.2;
			if (year < -720 || year > 2500)
			{
				deltaTUncertainty = (int)(1.8 * num * num);
			}
			else if (year > 2020 && year <= 2500)
			{
				deltaTUncertainty = (int)(-3.1 * num + 1.67 * num * num + 0.14 * num * num * num);
			}
			else if (year < 1810)
			{
				int num2 = 0;
				for (num2 = 0; num2 < deltaTUncerts.GetLength(0) - 1 && !(((double)year >= deltaTUncerts[num2, 0]) & ((double)year < deltaTUncerts[num2 + 1, 0])); num2++)
				{
				}
				if (((double)year - deltaTUncerts[num2, 0]) / (deltaTUncerts[num2 + 1, 0] - deltaTUncerts[num2, 0]) < 0.5)
				{
					deltaTUncertainty = (int)deltaTUncerts[num2, 1];
				}
				else
				{
					deltaTUncertainty = (int)deltaTUncerts[num2 + 1, 1];
				}
			}
			if (deltaTUncertainty < 1)
			{
				deltaTUncertainty_string = "";
			}
			else if (deltaTUncertainty < 200)
			{
				deltaTUncertainty_string = string.Format(" ±{0,1:f0}s", deltaTUncertainty);
			}
			else if (deltaTUncertainty < 1200)
			{
				deltaTUncertainty_string = string.Format(" ±{0,1:f1}m", (double)deltaTUncertainty / 60.0);
			}
			else if (deltaTUncertainty < 7000)
			{
				deltaTUncertainty_string = string.Format(" ±{0,1:f0}m", (double)deltaTUncertainty / 60.0);
			}
			else if (deltaTUncertainty < 72000)
			{
				deltaTUncertainty_string = string.Format(" ±{0,1:f1}h", (double)deltaTUncertainty / 3600.0);
			}
			else
			{
				deltaTUncertainty_string = string.Format(" ±{0,1:f0}h", (double)deltaTUncertainty / 3600.0);
			}
		}

		public static bool EarthOrientationParameters(double JD, out double x, out double y, out double dUT1)
		{
			x = (y = (dUT1 = 0.0));
			bool result = false;
			if (!EOPdateSet)
			{
				if (File.Exists(AppPath + "\\Resource Files\\EOP_2020plus.dat"))
				{
					int num = (int)new FileInfo(AppPath + "\\Resource Files\\EOP_present.dat").Length / 4;
					LastValidEOPdate = 2437664.5 + (double)num - 5.0;
				}
				else
				{
					LastValidEOPdate = 2480000.0;
				}
				EOPdateSet = true;
			}
			if ((JD >= LastValidEOPdate) & File.Exists(AppPath + "\\Resource Files\\EOP_2020plus.dat"))
			{
				FileStream fileStream = new FileStream(AppPath + "\\Resource Files\\EOP_2020plus.dat", FileMode.Open, FileAccess.Read);
				BinaryReader binaryReader = new BinaryReader(fileStream);
				if ((double)fileStream.Length > 4.0 * (JD - 2458849.5))
				{
					fileStream.Seek(4 * (int)(JD - 2458849.5), SeekOrigin.Begin);
					x = (double)binaryReader.ReadSByte() / 100.0;
					y = (double)binaryReader.ReadSByte() / 100.0;
					dUT1 = (double)binaryReader.ReadInt16() / 1000.0;
					result = true;
				}
			}
			else if (JD >= 2437665.5)
			{
				if (File.Exists(AppPath + "\\Resource Files\\EOP_present.dat"))
				{
					FileStream fileStream2 = new FileStream(AppPath + "\\Resource Files\\EOP_present.dat", FileMode.Open, FileAccess.Read);
					BinaryReader binaryReader2 = new BinaryReader(fileStream2);
					if ((double)fileStream2.Length > 4.0 * (JD - 2437665.5))
					{
						fileStream2.Seek(4 * (int)(JD - 2437665.5), SeekOrigin.Begin);
						x = (double)binaryReader2.ReadSByte() / 100.0;
						y = (double)binaryReader2.ReadSByte() / 100.0;
						dUT1 = (double)binaryReader2.ReadInt16() / 1000.0;
						result = true;
					}
					else
					{
						double num2 = (double)(fileStream2.Length / 4) + 2437664.5;
						if (JD - num2 < 120.0)
						{
							fileStream2.Seek(4 * (int)(num2 - 2437665.5), SeekOrigin.Begin);
							double num3 = (double)binaryReader2.ReadSByte() / 100.0;
							double num4 = (double)binaryReader2.ReadSByte() / 100.0;
							double num5 = (double)binaryReader2.ReadInt16() / 1000.0;
							fileStream2.Seek(4 * (int)(num2 - 2437665.5 - 40.0), SeekOrigin.Begin);
							double num6 = (double)binaryReader2.ReadSByte() / 100.0;
							double num7 = (double)binaryReader2.ReadSByte() / 100.0;
							double num8 = (double)binaryReader2.ReadInt16() / 1000.0;
							if (num5 > 0.0 && num8 < 0.0)
							{
								num8 += 1.0;
							}
							double num9 = (JD - num2) / 40.0;
							x = num3 + num9 * (num3 - num6);
							y = num4 + num9 * (num4 - num7);
							dUT1 = num5 + num9 * (num5 - num8);
						}
					}
					binaryReader2.Close();
				}
			}
			else if (JD > 2395296.0)
			{
				double num9;
				int num11;
				if (JD > 2411367.0)
				{
					double num10 = 20.0 * ((JD - 2411367.0) / 365.25);
					num11 = (int)Math.Floor(num10);
					num9 = num10 - (double)num11;
					num11 += 440;
				}
				else
				{
					double num12 = 10.0 * ((JD - 2395296.0) / 365.25);
					num11 = (int)Math.Floor(num12);
					num9 = num12 - (double)num11;
				}
				if (File.Exists(AppPath + "\\Resource Files\\EOP_old.dat"))
				{
					FileStream fileStream3 = new FileStream(AppPath + "\\Resource Files\\EOP_old.dat", FileMode.Open, FileAccess.Read);
					BinaryReader binaryReader3 = new BinaryReader(fileStream3);
					if (fileStream3.Length > 4 * num11)
					{
						fileStream3.Seek(4 * num11, SeekOrigin.Begin);
						double num3 = (double)binaryReader3.ReadSByte() / 100.0;
						double num4 = (double)binaryReader3.ReadSByte() / 100.0;
						double num5 = (double)binaryReader3.ReadInt16() / 1000.0;
						result = true;
						double num6 = (double)binaryReader3.ReadSByte() / 100.0;
						double num7 = (double)binaryReader3.ReadSByte() / 100.0;
						double num8 = (double)binaryReader3.ReadInt16() / 1000.0;
						x = num3 + num9 * (num6 - num3);
						y = num4 + num9 * (num7 - num4);
						dUT1 = num5 + num9 * (num8 - num5);
					}
					binaryReader3.Close();
				}
			}
			return result;
		}

		public static void Besselian_XYtoGeodetic_LatLong(double X, double Y, double RAstar, double DecStar, double SiderialTime_Rad, out bool OnEarth, out double Longitude, out double Latitude)
		{
			double num = 0.0;
			Longitude = (Latitude = 0.0);
			OnEarth = true;
			X /= 6378.137;
			Y /= 6378.137;
			double num2 = Math.Sqrt(0.00669437999014133);
			for (int i = 0; i < 2; i++)
			{
				double num3 = Math.Sqrt(1.0 - num2 * num2 * Math.Cos(DecStar) * Math.Cos(DecStar)) - num;
				double num4 = Y / num3;
				double num5 = 1.0 - X * X - num4 * num4;
				if (num5 >= 0.0)
				{
					OnEarth = true;
					num5 = Math.Sqrt(num5);
					double num6 = 0.02127517497454856 * num5 / 86400.0 * (Math.PI * 2.0);
					double num7 = Math.Sin(DecStar) / num3;
					double num8 = Math.Sqrt(1.0 - num2 * num2) * Math.Cos(DecStar) / num3;
					double num9 = Math.Atan2(X, num5 * num8 - num4 * num7);
					double y = num4 * num8 + num5 * num7;
					double num10 = X / Math.Sin(num9);
					Latitude = Math.Atan2(y, Math.Sqrt(1.0 - num2 * num2) * num10);
					Longitude = num9 - SiderialTime_Rad - num6 + RAstar;
					num = GeoidHeight(Longitude * (180.0 / Math.PI), Latitude * (180.0 / Math.PI)) / 6378.137 / 1000.0;
					continue;
				}
				Latitude = 0.0;
				Longitude = 0.0;
				OnEarth = false;
				return;
			}
			while (Longitude < Math.PI)
			{
				Longitude += Math.PI * 2.0;
			}
			while (Longitude > Math.PI)
			{
				Longitude -= Math.PI * 2.0;
			}
		}

		public static void FundamentalPlane_XY_to_GeoEquatorialXYZ(double JD, double RA_Star_J2000, double Dec_Star_J2000, bool use2006values_Not1976, double ReferenceTimeJD, double FundamentalPlane_X, double FundamentalPlane_Y, out double EquatorialJ2000_X, out double EquatorialJ2000_Y, out double EquatorialJ2000_Z)
		{
			double num;
			double num2;
			if (JPL_DE.DE_EphemerisAvailable)
			{
				JPL_DE.DE_AberrationCoefficients(ReferenceTimeJD, out var dX, out var dY, out var dZ);
				num = 0.00577551833 * ((0.0 - dX) * Math.Sin(RA_Star_J2000) + dY * Math.Cos(RA_Star_J2000)) / Math.Cos(Dec_Star_J2000);
				num2 = 0.00577551833 * ((0.0 - dX) * Math.Cos(RA_Star_J2000) * Math.Sin(Dec_Star_J2000) - dY * Math.Sin(RA_Star_J2000) * Math.Sin(Dec_Star_J2000) + dZ * Math.Cos(Dec_Star_J2000));
				num += 3.335661198016599E-05 * (dX * Math.Sin(RA_Star_J2000) - dY * Math.Cos(RA_Star_J2000)) * (dX * Math.Cos(RA_Star_J2000) + dY * Math.Sin(RA_Star_J2000)) / Math.Cos(Dec_Star_J2000) / Math.Cos(Dec_Star_J2000);
				num2 -= 1.6678305990082995E-05 * (Math.Pow(dX * Math.Sin(RA_Star_J2000) - dY * Math.Cos(RA_Star_J2000), 2.0) * Math.Tan(Dec_Star_J2000));
				num2 += 3.335661198016599E-05 * (dX * Math.Cos(Dec_Star_J2000) * Math.Cos(RA_Star_J2000) + dY * Math.Cos(Dec_Star_J2000) * Math.Sin(RA_Star_J2000) + dZ * Math.Sin(Dec_Star_J2000)) * (dX * Math.Sin(Dec_Star_J2000) * Math.Cos(RA_Star_J2000) + dY * Math.Sin(Dec_Star_J2000) * Math.Sin(RA_Star_J2000) - dZ * Math.Cos(Dec_Star_J2000));
			}
			else
			{
				Aberration_CD(JD, out var C, out var D);
				num = (C * Math.Cos(RA_Star_J2000) + D * Math.Sin(RA_Star_J2000)) / Math.Cos(Dec_Star_J2000);
				num2 = C * (Math.Tan(Ecliptic2000_deg_1976 / (180.0 / Math.PI)) * Math.Cos(Dec_Star_J2000) - Math.Sin(RA_Star_J2000) * Math.Sin(Dec_Star_J2000)) + D * Math.Cos(RA_Star_J2000) * Math.Sin(Dec_Star_J2000);
			}
			double num3 = RA_Star_J2000 + num;
			double num4 = Dec_Star_J2000 + num2;
			double x = 0.0;
			double num5 = 0.0;
			double num6 = 0.0;
			double num7 = 0.0;
			double num8 = 0.0;
			double num9 = 0.0002908882086657216 / Math.Cos(Dec_Star_J2000);
			ApparentStarPosition(RA_Star_J2000, Dec_Star_J2000, 0.0, 0.0, 0.0, 2000, IncludeRelativisticBending: false, ReferenceTimeJD, use2006values_Not1976, out var RA_end, out var Dec_end);
			double num10;
			double num11;
			do
			{
				ApparentStarPosition(num3 + num5, num4 + num6, 0.0, 0.0, 0.0, 2000, IncludeAberration: false, IncludeRelativisticBending: false, ReferenceTimeJD, use2006values_Not1976, out var RA_end2, out var Dec_end2);
				num10 = RA_end - (RA_end2 + num9);
				num5 += num10;
				num11 = Dec_end - Dec_end2;
				num6 += num11;
			}
			while ((Math.Abs(num10) > 1E-07) | (Math.Abs(num11) > 1E-07));
			do
			{
				ApparentStarPosition(num3 + num7, num4 + num8, 0.0, 0.0, 0.0, 2000, IncludeAberration: false, IncludeRelativisticBending: false, ReferenceTimeJD, use2006values_Not1976, out var RA_end3, out var Dec_end3);
				num10 = RA_end - (RA_end3 - num9);
				num7 += num10;
				num11 = Dec_end - Dec_end3;
				num8 += num11;
			}
			while ((Math.Abs(num10) > 1E-07) | (Math.Abs(num11) > 1E-07));
			double num12 = Math.Atan((num6 - num8) / (num5 - num7) / Math.Cos(Dec_Star_J2000)) * (180.0 / Math.PI);
			RotateXY(FundamentalPlane_X, FundamentalPlane_Y, 0.0 - num12, out var x2, out var y);
			RotateXY(x, y, 90.0 - num4 * (180.0 / Math.PI), out EquatorialJ2000_Z, out var y2);
			RotateXY(x2, y2, 0.0 - (90.0 + num3 * (180.0 / Math.PI)), out EquatorialJ2000_X, out EquatorialJ2000_Y);
		}

		public static void ConvertLatLong_forEOP(double Long, double Lat, double EOPx, double EOPy, out double EOPLong, out double EOPLat)
		{
			double num = EOPx * Math.Cos(Long) - EOPy * Math.Sin(Long);
			EOPLat = Lat + num / 3600.0 / (180.0 / Math.PI);
			double num2 = (EOPx * Math.Sin(Long) + EOPy * Math.Cos(Long)) * Math.Tan(Lat);
			EOPLong = Long + num2 / 3600.0 / (180.0 / Math.PI);
		}

		public static void ConvertLatLong_forEOP(ref double Long_Deg, ref double Lat_Deg, double EOPx, double EOPy)
		{
			double num = Long_Deg / (180.0 / Math.PI);
			double num2 = (EOPx * Math.Sin(num) + EOPy * Math.Cos(num)) * Math.Tan(Lat_Deg / (180.0 / Math.PI));
			double num3 = EOPx * Math.Cos(num) - EOPy * Math.Sin(num);
			Long_Deg += num2 / 3600.0;
			Lat_Deg += num3 / 3600.0;
		}

		public static double Refraction_deg(double ObservedAltitude_deg, double Pressure_mBar, double Temp_Celsius)
		{
			if (Pressure_mBar == 0.0)
			{
				Pressure_mBar = 1016.0;
			}
			if (double.IsNaN(ObservedAltitude_deg))
			{
				ObservedAltitude_deg = 0.0;
			}
			if (double.IsNaN(Temp_Celsius))
			{
				Temp_Celsius = 0.0;
			}
			if (ObservedAltitude_deg >= 15.0)
			{
				return 0.00452 * Pressure_mBar / (273.0 + Temp_Celsius) / Math.Tan(ObservedAltitude_deg / (180.0 / Math.PI));
			}
			return Pressure_mBar * (0.1594 + 0.0196 * ObservedAltitude_deg + 2E-05 * ObservedAltitude_deg * ObservedAltitude_deg) / (273.0 + Temp_Celsius) / (1.0 + 0.505 * ObservedAltitude_deg + 0.0845 * ObservedAltitude_deg * ObservedAltitude_deg);
		}

		public static void Rectangular_to_Polar(double X, double Y, out double R, out double theta, bool flagFullCircle)
		{
			R = Math.Sqrt(X * X + Y * Y);
			theta = Math.Atan2(X, Y);
			if (flagFullCircle && theta < 0.0)
			{
				theta += Math.PI * 2.0;
			}
		}

		public static void RotateXY(double x, double y, double Angle_deg, out double x2, out double y2)
		{
			double num = Angle_deg / (180.0 / Math.PI);
			double num2 = Math.Sin(num);
			double num3 = Math.Cos(num);
			x2 = x * num3 + y * num2;
			y2 = (0.0 - x) * num2 + y * num3;
		}

		public static void FitQuadraticTo3Points(double X_1, double X0, double X1, out double Xat0, out double dX, out double d2X)
		{
			Xat0 = X0;
			dX = (X1 - X_1) / 2.0;
			d2X = (X1 + X_1 - 2.0 * X0) / 2.0;
		}

		public static void FitCubicTo4_EqualSpaced_Points(double X_1, double X0, double X1, double X2, out double Xat0, out double dX, out double d2X, out double d3X)
		{
			Xat0 = X0;
			X_1 -= X0;
			X1 -= X0;
			X2 -= X0;
			d2X = (X1 + X_1) / 2.0;
			X_1 -= d2X;
			X2 -= 4.0 * d2X;
			X2 /= 2.0;
			d3X = (X_1 + X2) / 3.0;
			dX = X1 - d2X - d3X;
		}

		public static void FitCubicTo4_Random_Points(double Arg0, double Value0, double Arg1, double Value1, double Arg2, double Value2, double Arg3, double Value3, out double Coeff_a, out double Coeff_b, out double Coeff_c, out double Coeff_d)
		{
			double[] array = new double[5]
			{
				Value0,
				1.0,
				Arg0,
				Math.Pow(Arg0, 2.0),
				Math.Pow(Arg0, 3.0)
			};
			double[] array2 = new double[5]
			{
				Value1,
				1.0,
				Arg1,
				Math.Pow(Arg1, 2.0),
				Math.Pow(Arg1, 3.0)
			};
			double[] array3 = new double[5]
			{
				Value2,
				1.0,
				Arg2,
				Math.Pow(Arg2, 2.0),
				Math.Pow(Arg2, 3.0)
			};
			double[] array4 = new double[5]
			{
				Value3,
				1.0,
				Arg3,
				Math.Pow(Arg3, 2.0),
				Math.Pow(Arg3, 3.0)
			};
			double[] array5 = new double[5];
			for (int i = 0; i < 5; i++)
			{
				array5[i] = array2[i] - array[i];
			}
			double[] array6 = new double[5];
			for (int j = 0; j < 5; j++)
			{
				array6[j] = array3[j] - array[j];
			}
			double[] array7 = new double[5];
			for (int k = 0; k < 5; k++)
			{
				array7[k] = array4[k] - array[k];
			}
			double num = array5[2];
			for (int l = 0; l < 5; l++)
			{
				array5[l] /= num;
			}
			num = array6[2];
			for (int m = 0; m < 5; m++)
			{
				array6[m] /= num;
			}
			num = array7[2];
			for (int n = 0; n < 5; n++)
			{
				array7[n] /= num;
			}
			double[] array8 = new double[5];
			for (int num2 = 0; num2 < 5; num2++)
			{
				array8[num2] = array6[num2] - array5[num2];
			}
			double[] array9 = new double[5];
			for (int num3 = 0; num3 < 5; num3++)
			{
				array9[num3] = array7[num3] - array5[num3];
			}
			num = array8[3];
			for (int num4 = 0; num4 < 5; num4++)
			{
				array8[num4] /= num;
			}
			num = array9[3];
			for (int num5 = 0; num5 < 5; num5++)
			{
				array9[num5] /= num;
			}
			double[] array10 = new double[5];
			for (int num6 = 0; num6 < 5; num6++)
			{
				array10[num6] = array9[num6] - array8[num6];
			}
			Coeff_d = array10[0] / array10[4];
			Coeff_c = (array9[0] - Coeff_d * array9[4]) / array9[3];
			Coeff_b = (array7[0] - Coeff_c * array7[3] - Coeff_d * array7[4]) / array7[2];
			Coeff_a = array4[0] - Coeff_b * array4[2] - Coeff_c * array4[3] - Coeff_d * array4[4];
		}

		public static void Mean_Sdev(List<double> L, out double Mean, out double SDev)
		{
			double num = 0.0;
			double num2 = 0.0;
			for (int i = 0; i < L.Count; i++)
			{
				num += L[i];
			}
			Mean = num / (double)L.Count;
			for (int j = 0; j < L.Count; j++)
			{
				num2 += Math.Pow(L[j] - Mean, 2.0);
			}
			SDev = Math.Sqrt(num2 / (double)L.Count);
		}

		internal static double QuadratureAddition(double A, double B)
		{
			return Math.Sqrt(A * A + B * B);
		}

		public static void Distance(double RA_OfOrigin, double Dec_OfOrigin, double RA_OfTarget, double Dec_Target, out double Distance, out double PA_atOrigin)
		{
			double num = RA_OfTarget - RA_OfOrigin;
			double num2 = Math.Cos(Dec_Target) * Math.Sin(num);
			double num3 = Math.Cos(Dec_OfOrigin) * Math.Sin(Dec_Target) - Math.Sin(Dec_OfOrigin) * Math.Cos(Dec_Target) * Math.Cos(num);
			double x = Math.Sin(Dec_OfOrigin) * Math.Sin(Dec_Target) + Math.Cos(Dec_OfOrigin) * Math.Cos(Dec_Target) * Math.Cos(num);
			PA_atOrigin = Math.Atan2(num2, num3);
			if (PA_atOrigin < 0.0)
			{
				PA_atOrigin += Math.PI * 2.0;
			}
			Distance = Math.Atan2(Math.Sqrt(num2 * num2 + num3 * num3), x);
			if (Distance < 0.0)
			{
				Distance += Math.PI;
			}
		}

		public static double NormaliseDegrees(double X)
		{
			while (X < 0.0)
			{
				X += 360.0;
			}
			while (X >= 360.0)
			{
				X -= 360.0;
			}
			return X;
		}

		public static double Normalise_0to180(double X)
		{
			while (X < 0.0)
			{
				X += 180.0;
			}
			while (X > 180.0)
			{
				X -= 180.0;
			}
			return X;
		}

		public static double NormaliseHours(double X)
		{
			int DayChange;
			return NormaliseHours(X, out DayChange);
		}

		public static double NormaliseHours(double X, out int DayChange)
		{
			DayChange = 0;
			while (X < 0.0)
			{
				X += 24.0;
				DayChange--;
			}
			while (X >= 24.0)
			{
				X -= 24.0;
				DayChange++;
			}
			return X;
		}

		public static bool AsteroidNumber_Unpack(string Packed_ID, out int UnpackedIDnum, out string UnpackedID)
		{
			Packed_ID = (UnpackedID = Packed_ID.PadLeft(5, '0'));
			UnpackedIDnum = 0;
			if (Packed_ID.Length > 5)
			{
				return false;
			}
			if (Packed_ID.Substring(4, 1) == "P")
			{
				do
				{
					UnpackedID = UnpackedID.Substring(1);
				}
				while (UnpackedID.Substring(0, 1) == "0");
				return false;
			}
			if (Packed_ID.Substring(0, 1) != "~")
			{
				UnpackedIDnum = 10000 * "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz".IndexOf(Packed_ID.Substring(0, 1)) + int.Parse(Packed_ID.Substring(1));
				if (UnpackedIDnum == 0)
				{
					UnpackedID = "";
				}
				else
				{
					UnpackedID = UnpackedIDnum.ToString();
				}
				return true;
			}
			UnpackedIDnum = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz".IndexOf(Packed_ID.Substring(1, 1)) * 62 * 62 * 62 + "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz".IndexOf(Packed_ID.Substring(2, 1)) * 62 * 62 + "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz".IndexOf(Packed_ID.Substring(3, 1)) * 62 + "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz".IndexOf(Packed_ID.Substring(4, 1)) + 620000;
			UnpackedID = UnpackedIDnum.ToString();
			return true;
		}

		public static bool AsteroidProvisionalDesignation_Unpack(string Packed_ID, out string UnpackedID)
		{
			UnpackedID = "";
			if (Packed_ID.Trim().Length != 7)
			{
				return false;
			}
			Packed_ID = Packed_ID.PadRight(6);
			string text = Packed_ID.Substring(0, 1);
			string text2 = Packed_ID.Substring(1, 2);
			string text3 = Packed_ID.Substring(3, 1) + Packed_ID.Substring(6, 1).Trim();
			string value = Packed_ID.Substring(4, 1);
			string value2 = Packed_ID.Substring(5, 1);
			int num = "0123456789ABCDEFGHJKLMNOPQRSTUVWXY".IndexOf(value);
			int num2 = "0123456789ABCDEFGHJKLMNOPQRSTUVWXY".IndexOf(value2);
			int num3 = 10 * num + num2;
			if (text == "K")
			{
				UnpackedID = "20";
			}
			else
			{
				UnpackedID = "19";
			}
			UnpackedID = UnpackedID + text2 + " " + text3;
			if (num3 > 0)
			{
				UnpackedID += num3;
			}
			return true;
		}

		public static bool GetCircle(double x1, double y1, double x2, double y2, double x3, double y3, out double Xc, out double Yc, out double Rc)
		{
			bool result = false;
			Xc = (Yc = (Rc = 0.0));
			double num = x2 * x2 + y2 * y2;
			double num2 = (x1 * x1 + y1 * y1 - num) / 2.0;
			double num3 = (num - x3 * x3 - y3 * y3) / 2.0;
			double num4 = (x1 - x2) * (y2 - y3) - (x2 - x3) * (y1 - y2);
			if (Math.Abs(num4) > 1E-06)
			{
				num4 = 1.0 / num4;
				Xc = (num2 * (y2 - y3) - num3 * (y1 - y2)) * num4;
				Yc = ((x1 - x2) * num3 - (x2 - x3) * num2) * num4;
				Rc = Math.Sqrt((Xc - x1) * (Xc - x1) + (Yc - y1) * (Yc - y1));
				result = true;
			}
			return result;
		}

		public static string ConstellationFromCoordinates(int StartEquinox, double RA2000, double Dec2000)
		{
			if (Cons_RAstart == null && !GetConstellationData())
			{
				return "";
			}
			if (double.IsNaN(Cons_RAstart[350]) && !GetConstellationData())
			{
				return "";
			}
			double RA2001 = RA2000;
			double Dec2001 = Dec2000;
			Precession(StartEquinox, 2405890.5, use2006values_Not1976: false, ref RA2001, ref Dec2001, 0.0, 0.0);
			RA2001 *= 12.0 / Math.PI;
			Dec2001 *= 180.0 / Math.PI;
			int num = -1;
			do
			{
				num++;
			}
			while (Cons_Declination[num] > Dec2001);
			do
			{
				num--;
				do
				{
					num++;
				}
				while (Cons_RAend[num] <= RA2001);
				num--;
				do
				{
					num++;
				}
				while (Cons_RAstart[num] > RA2001);
			}
			while (Cons_RAend[num] <= RA2001);
			return ConstellationName[Cons_Index[num]];
		}

		public static string Constellation(double RA2000, double Dec2000, bool AbbreviatedName)
		{
			if (Cons_RAstart == null && !GetConstellationData())
			{
				return "";
			}
			if (double.IsNaN(Cons_RAstart[350]) && !GetConstellationData())
			{
				return "";
			}
			double RA2001 = RA2000;
			double Dec2001 = Dec2000;
			PrecessStartToEnd(2451545.0, 2405890.5, use2006values_Not1976: false, ref RA2001, ref Dec2001);
			RA2001 = RA2001 * (180.0 / Math.PI) / 15.0;
			Dec2001 *= 180.0 / Math.PI;
			int num = -1;
			do
			{
				num++;
			}
			while (Cons_Declination[num] > Dec2001);
			do
			{
				num--;
				do
				{
					num++;
				}
				while (Cons_RAend[num] <= RA2001);
				num--;
				do
				{
					num++;
				}
				while (Cons_RAstart[num] > RA2001);
			}
			while (Cons_RAend[num] <= RA2001);
			if (AbbreviatedName)
			{
				return ConstellationAbbrev[Cons_Index[num]];
			}
			return ConstellationName[Cons_Index[num]];
		}

		private static bool GetConstellationData()
		{
			Cons_RAstart = new double[357];
			Cons_RAend = new double[357];
			Cons_Declination = new double[357];
			Cons_Index = new int[357];
			ConstellationAbbrev = new string[90];
			ConstellationName = new string[90];
			using (StreamReader streamReader = new StreamReader(AppPath + "\\Resource Files\\Constellation id.dat"))
			{
				for (int i = 0; i < 357; i++)
				{
					Cons_RAstart[i] = double.Parse(streamReader.ReadLine());
					Cons_RAend[i] = double.Parse(streamReader.ReadLine());
					Cons_Declination[i] = double.Parse(streamReader.ReadLine());
					Cons_Index[i] = int.Parse(streamReader.ReadLine()) - 1;
				}
			}
			using (StreamReader streamReader2 = new StreamReader(AppPath + "\\Resource Files\\Constellation Abbrevs.dat"))
			{
				using StreamReader streamReader3 = new StreamReader(AppPath + "\\Resource Files\\Constellation Names.dat");
				for (int j = 0; j < 89; j++)
				{
					ConstellationAbbrev[j] = streamReader2.ReadLine();
					ConstellationName[j] = streamReader3.ReadLine();
				}
			}
			return true;
		}

		public static string ProperCase(string X)
		{
			int length = X.Length;
			if (length < 1)
			{
				return "";
			}
			bool flag = false;
			string text = X.Substring(0, 1).ToUpper();
			for (int i = 1; i < length; i++)
			{
				string text2 = X.Substring(i, 1);
				text = ((!flag) ? (text + text2.ToLower()) : (text + text2.ToUpper()));
				flag = " .,-'([".Contains(text2);
			}
			int num;
			if (!(text.Contains("Machado") | text.Contains("Mackie") | text.Contains("Mackintosh") | text.Contains("Machholz") | text.Contains("Machim")))
			{
				num = text.IndexOf("Mc");
				if (num >= 0 && num < length - 3)
				{
					text = text.Substring(0, num + 2) + text.Substring(num + 2, 1).ToUpper() + text.Substring(num + 3);
				}
				num = text.IndexOf("Mac");
				if (num >= 0 && num < length - 4)
				{
					text = text.Substring(0, num + 3) + text.Substring(num + 3, 1).ToUpper() + text.Substring(num + 4);
				}
			}
			if (text.Contains(" Van'T "))
			{
				text = text.Replace(" Van'T ", " Van't ");
			}
			num = text.IndexOf("_");
			if (num > 0 && num < length - 2)
			{
				text = text.Substring(0, num + 1) + text.Substring(num + 1, 1).ToUpper() + text.Substring(num + 2);
			}
			return text;
		}

		internal static string SplitStringWithCommas(string Input)
		{
			string[] array = new string[10] { " ", "  ", "   ", "    ", "     ", "      ", "       ", "        ", "         ", "          " };
			for (int num = 9; num >= 0; num--)
			{
				Input = Input.Replace(array[num], ",");
			}
			return Input;
		}

		public static string InitialPlusName(string K)
		{
			return InitialPlusName(K, NameLast: false);
		}

		public static string InitialPlusName(string K, bool NameLast)
		{
			int num = K.LastIndexOf(" ");
			if (K.LastIndexOf("De ") > 0)
			{
				num = K.LastIndexOf(" De ");
			}
			if (K.LastIndexOf("Di ") > 0)
			{
				num = K.LastIndexOf(" Di ");
			}
			if (K.LastIndexOf("Le ") > 0)
			{
				num = K.LastIndexOf(" Le ");
			}
			if (K.LastIndexOf("Van ") > 0)
			{
				num = K.LastIndexOf(" Van ");
			}
			if (K.LastIndexOf("Van't ") > 0)
			{
				num = K.LastIndexOf(" Van't ");
			}
			if (K.LastIndexOf("Van Den ") > 0)
			{
				num = K.LastIndexOf(" Van Den ");
			}
			if (K.LastIndexOf("Von ") > 0)
			{
				num = K.LastIndexOf(" Von ");
			}
			if (num < 1)
			{
				return K;
			}
			if (!NameLast)
			{
				return K.Substring(0, 1) + "." + K.Substring(num);
			}
			return K.Substring(num + 1) + ", " + K.Substring(0, 1) + ".";
		}

		public static double NormalDistributionValue(double LocationValue, double MeanValue, double StandardDeviation)
		{
			return Math.Exp((0.0 - (LocationValue - MeanValue)) * (LocationValue - MeanValue) / 2.0 / StandardDeviation / StandardDeviation) / Math.Sqrt(Math.PI * 2.0 * StandardDeviation * StandardDeviation);
		}

		internal static bool CheckForPipesAndNonASCII(string Text, string TextBoxName, bool CheckForPipes, out string RevisedText)
		{
			//IL_00aa: Unknown result type (might be due to invalid IL or missing references)
			RevisedText = Text;
			if (CheckForPipes)
			{
				Text = Text.Replace('|', '_');
			}
			StringBuilder stringBuilder = new StringBuilder(RevisedText.Length);
			string text = RevisedText;
			foreach (char c in text)
			{
				if (c <= '\u007f' && c >= ' ' && c != '"')
				{
					stringBuilder.Append(c);
				}
			}
			RevisedText = stringBuilder.ToString();
			if (RevisedText != Text)
			{
				MessageBox.Show("The text has been amended to remove any non-ASCII characters in the Text Box '" + TextBoxName + "' from\r\n\r\n" + Text + "\r\n   to\r\n" + RevisedText + "\r\n\r\nAs a result any accented 'a', and 'e' characters (for example), as well as any line breaks, will have been removed.\r\n\r\nPlease review the text and make appropriate corrections.", "Text has been adjusted", (MessageBoxButtons)0, (MessageBoxIcon)64, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
			}
			return RevisedText == Text;
		}

		public static double MeanEclipticOfDate(double JD, bool Use1976Value)
		{
			double num = (JD - 2451545.0) / 36525.0;
			if (Use1976Value)
			{
				return (84381.448 - 46.815 * num - 0.00059 * num * num + 0.001813 * num * num * num) / 3600.0 / (180.0 / Math.PI);
			}
			return (84381.406 - 46.836769 * num - 0.0001831 * num * num + 0.0020034 * num * num * num - 5.76E-07 * num * num * num * num - 4.34E-08 * num * num * num * num * num) / 3600.0 / (180.0 / Math.PI);
		}

		public static void StellarParallaxCoefficients(double JD, double RAStar, double DecStar, out double ParallaxCoeff_RA, out double ParallaxCoeff_Dec)
		{
			double X = 0.0;
			double Y = 0.0;
			double Z = 0.0;
			PlanetXYZ(JD, 3, Heliocentric: false, 1E-05, ref X, ref Y, ref Z, out var _);
			JPL_DE.Barycenter_FromEarth(JD, 0.0, out X, out Y, out Z, out var _);
			ParallaxCoeff_RA = (0.0 - (Y * Math.Cos(RAStar) - X * Math.Sin(RAStar))) / Math.Cos(DecStar);
			ParallaxCoeff_Dec = 0.0 - (Z * Math.Cos(DecStar) - X * Math.Cos(RAStar) * Math.Sin(DecStar) - Y * Math.Sin(RAStar) * Math.Sin(DecStar));
		}

		public static void Aberration_CD(double JD, out double C, out double D)
		{
			double[] array = new double[3];
			double[] array2 = new double[7];
			array2[0] = 0.0;
			array2[1] = 1.0;
			array2[2] = (JD - 2451545.0) / 365250.0;
			array2[3] = array2[2] * array2[2];
			array2[4] = array2[3] * array2[3];
			array2[5] = array2[4] * array2[4];
			array2[6] = array2[5] * array2[5];
			double num = (13.96971 * array2[2] + 0.03086 * array2[3]) / (180.0 / Math.PI);
			double num2 = Math.Cos(MeanEclipticOfDate(JD, Use1976Value: true) / (180.0 / Math.PI));
			FileStream fileStream = new FileStream(AppPath + "\\Resource Files\\earth.bin", FileMode.Open, FileAccess.Read);
			BinaryReader binaryReader = new BinaryReader(fileStream);
			array[1] = 0.0;
			array[2] = 0.0;
			long num3 = fileStream.Length / 28;
			for (int i = 1; i <= num3; i++)
			{
				int num4 = binaryReader.ReadInt16();
				int num5 = binaryReader.ReadInt16();
				double num6 = binaryReader.ReadDouble();
				double num7 = binaryReader.ReadDouble();
				double num8 = binaryReader.ReadDouble();
				double num9 = num7 + num8 * array2[2];
				if (num4 != 3)
				{
					array[num4] -= num6 * ((double)num5 * Math.Cos(num9) * array2[num5] - num8 * Math.Sin(num9) * array2[num5 + 1]) / 365250.0;
				}
				if (num6 < 1E-05)
				{
					break;
				}
			}
			fileStream.Close();
			double num10 = 1191.28 * array[1] / (180.0 / Math.PI) / 3600.0;
			double num11 = -1191.28 * array[2] * num2 / (180.0 / Math.PI) / 3600.0;
			C = num11 - num * num2 * num10;
			D = num10 + num / num2 * num11;
		}

		public static void Aberration_Differential(double JD, double RA, double Dec, double dRA_asec, double dDec_asec, out double RAcorrn_Rad, out double DecCorrn_Rad)
		{
			double num = (23.4 - DayOfYear(JD) / 15.2) / 24.0 * (Math.PI * 2.0) + RA;
			double num2 = -5.701 * Math.Cos(num) / Math.Cos(Dec);
			double num3 = -0.38 * Math.Sin(num) / Math.Cos(Dec) * Math.Tan(Dec);
			double num4 = 8.552 * Math.Sin(num) * Math.Sin(Dec);
			double num5 = -0.57 * Math.Cos(num) * Math.Cos(Dec);
			double num6 = dRA_asec / 15.0 / Math.Cos(Dec) / 60.0;
			double num7 = dDec_asec / 60.0;
			RAcorrn_Rad = num2 * num6 + num3 * num7;
			DecCorrn_Rad = num4 * num6 + num5 * num7;
			RAcorrn_Rad = RAcorrn_Rad * 0.001 / 240.0 / (180.0 / Math.PI);
			DecCorrn_Rad = DecCorrn_Rad * 0.01 / 3600.0 / (180.0 / Math.PI);
		}

		public static void Nutation(double JD_TT, out double Longitude_arcsec, out double Obliquity_arcsec, out double TrueEcliptic)
		{
			double num = (JD_TT - 2451545.0) / 36525.0;
			if (JPL_DE.DE_EphemerisAvailable && JPL_DE.DE_Nutation(JD_TT, out var NutL, out var NutE, out var _))
			{
				Longitude_arcsec = NutL * (180.0 / Math.PI) * 3600.0;
				Obliquity_arcsec = NutE * (180.0 / Math.PI) * 3600.0;
				TrueEcliptic = MeanEclipticOfDate(JD_TT, Use1976Value: true) + NutE;
				return;
			}
			double num2 = (297.8502042 + 445267.1115168 * num - 0.0016335 * num * num + num * num * num / 546300.0) / (180.0 / Math.PI);
			double num3 = (93.2720993 + 483202.0175273 * num - 0.0034064 * num * num - num * num * num / 6550000.0) / (180.0 / Math.PI);
			double num4 = (357.5291092 + 35999.0502909 * num - 0.000156 * num * num - num * num * num / 2280000.0) / (180.0 / Math.PI);
			double num5 = (134.9634114 + 477198.8676313 * num + 0.0089937 * num * num + num * num * num / 73725.0) / (180.0 / Math.PI);
			double num6 = (218.3164591 + 481267.88134236 * num - 0.0013298 * num * num + num * num * num / 546300.0) / (180.0 / Math.PI) - num3;
			FileStream fileStream = new FileStream(AppPath + "\\Resource Files\\nutation.bin", FileMode.Open, FileAccess.Read);
			BinaryReader binaryReader = new BinaryReader(fileStream);
			double num7 = 0.0;
			double num8 = 0.0;
			for (int i = 1; i < 64; i++)
			{
				int num9 = binaryReader.ReadInt16();
				int num10 = binaryReader.ReadInt16();
				int num11 = binaryReader.ReadInt16();
				int num12 = binaryReader.ReadInt16();
				int num13 = binaryReader.ReadInt16();
				double num14 = binaryReader.ReadDouble();
				float num15 = binaryReader.ReadSingle();
				double num16 = binaryReader.ReadDouble();
				int num17 = binaryReader.ReadInt16();
				double num18 = (double)num9 * num2 + (double)num10 * num4 + (double)num11 * num5 + (double)num12 * num3 + (double)num13 * num6;
				num7 += (num14 + (double)num15 * num) * Math.Sin(num18);
				num8 += (num16 + (double)num17 * num) * Math.Cos(num18);
			}
			fileStream.Close();
			Longitude_arcsec = num7 / 10000.0;
			Obliquity_arcsec = num8 / 10000.0;
			TrueEcliptic = MeanEclipticOfDate(JD_TT, Use1976Value: true) + Obliquity_arcsec / 3600.0 / (180.0 / Math.PI);
		}

		public static void ProperMotion(double RA, double Dec, double muRA_Annual_Rad, double muDec_Annual_Rad, double Parallax_Rad, double RadialVelocity_KmSec, double TfromEpoch_years, out double RAatEpoch, out double DecatEpoch)
		{
			ProperMotion(ref RA, ref Dec, muRA_Annual_Rad, muDec_Annual_Rad, Parallax_Rad, RadialVelocity_KmSec, TfromEpoch_years);
			RAatEpoch = RA;
			DecatEpoch = Dec;
		}

		public static void ProperMotion(ref double RA, ref double Dec, double muRA_Annual_Rad, double muDec_Annual_Rad, double Parallax_Rad, double RadialVelocity_KmSec, double TfromEpoch_years)
		{
			double num = Math.Cos(RA) * Math.Cos(Dec);
			double num2 = Math.Sin(RA) * Math.Cos(Dec);
			double num3 = Math.Sin(Dec);
			double num4 = (0.0 - muRA_Annual_Rad) * Math.Cos(Dec) * Math.Sin(RA) - muDec_Annual_Rad * Math.Sin(Dec) * Math.Cos(RA) + 0.2109495 * RadialVelocity_KmSec * Parallax_Rad * Math.Cos(Dec) * Math.Cos(RA);
			double num5 = muRA_Annual_Rad * Math.Cos(Dec) * Math.Cos(RA) - muDec_Annual_Rad * Math.Sin(Dec) * Math.Sin(RA) + 0.2109495 * RadialVelocity_KmSec * Parallax_Rad * Math.Cos(Dec) * Math.Sin(RA);
			double num6 = muDec_Annual_Rad * Math.Cos(Dec) + 0.2109495 * RadialVelocity_KmSec * Parallax_Rad * Math.Sin(Dec);
			num += TfromEpoch_years * num4;
			num2 += TfromEpoch_years * num5;
			num3 += TfromEpoch_years * num6;
			RA = Math.Atan2(num2, num);
			if (RA < 0.0)
			{
				RA += Math.PI * 2.0;
			}
			Dec = Math.Atan(num3 / Math.Sqrt(num * num + num2 * num2));
		}

		public static void Precession(int StartEquinox, double JDEnd, bool use2006values_Not1976, double RA_Start, double Dec_Start, double muRA, double muDec, out double RA_End, out double Dec_End)
		{
			double RA = RA_Start;
			double Dec = Dec_Start;
			Precession(StartEquinox, JDEnd, use2006values_Not1976, ref RA, ref Dec, muRA, muDec);
			RA_End = RA;
			Dec_End = Dec;
		}

		public static void Precession(int StartEquinox, double JDEnd, bool use2006values_Not1976, ref double RA, ref double Dec, double muRA, double muDec)
		{
			double num = (JDEnd - 2451545.0) / 36525.0;
			if (num != 0.0)
			{
				double rA = RA + muRA * num * 100.0;
				double dec = Dec + muDec * num * 100.0;
				PrecessFromJ2000(JDEnd, use2006values_Not1976, rA, dec, out var RA_date, out var Dec_date);
				RA = RA_date;
				Dec = Dec_date;
			}
		}

		public static void PrecessStartToEnd(double JDStart, double JDEnd, bool use2006values_Not1976, ref double RA, ref double Dec)
		{
			PrecessToJ2000(JDStart, use2006values_Not1976, ref RA, ref Dec);
			PrecessFromJ2000(JDEnd, use2006values_Not1976, ref RA, ref Dec);
		}

		public static double PrecessionalRotationToJ2000_deg(double JDofObservation, double RA, double Dec)
		{
			return PrecessionalRotation_Deg(JDofObservation, 2451545.0, RA, Dec);
		}

		public static double PrecessionalRotation_Deg(double JDOfObservation, double JDofReferenceEpoch, double RA, double Dec)
		{
			double num = (JDOfObservation - JDofReferenceEpoch) / 365.25;
			return (0.0 - (20.0431 - 0.008533 * ((JDOfObservation + JDofReferenceEpoch) / 2.0 - 2451545.0) / 36525.0)) / Math.Cos(Dec) * Math.Sin(RA) * num / 3600.0;
		}

		internal static void PrecessionalValues(double JD, bool use2006values_Not1976, out double zetaA, out double zA, out double thetaA)
		{
			double num = (JD - 2451545.0) / 36525.0;
			if (use2006values_Not1976)
			{
				zetaA = (2.650545 + 2306.083227 * num + 0.2988499 * num * num + 0.01801828 * num * num * num - 5.971E-06 * num * num * num * num - 3.173E-07 * num * num * num * num * num) / (180.0 / Math.PI) / 3600.0;
				zA = (-2.650545 + 2306.077181 * num + 1.0927348 * num * num + 0.01826837 * num * num * num - 2.8596E-05 * num * num * num * num - 2.904E-07 * num * num * num * num * num) / (180.0 / Math.PI) / 3600.0;
				thetaA = (2004.191903 * num - 0.4294934 * num * num - 0.04182264 * num * num * num - 7.089E-06 * num * num * num * num - 1.274E-07 * num * num * num * num * num) / (180.0 / Math.PI) / 3600.0;
			}
			else
			{
				zetaA = (2306.2181 * num + 0.30188 * num * num + 0.017998 * num * num * num) / (180.0 / Math.PI) / 3600.0;
				zA = (2306.2181 * num + 1.09468 * num * num + 0.018203 * num * num * num) / (180.0 / Math.PI) / 3600.0;
				thetaA = (2004.3109 * num - 0.42665 * num * num - 0.041833 * num * num * num) / (180.0 / Math.PI) / 3600.0;
			}
		}

		public static void PrecessToJ2000(double JDStart, bool use2006values_Not1976, ref double RA, ref double Dec)
		{
			if (!use2006values_Not1976)
			{
				PrecessionalValues(JDStart, use2006values_Not1976, out var zetaA, out var zA, out var thetaA);
				RA -= zA;
				double num = Math.Cos(Dec) * Math.Sin(RA);
				double num2 = Math.Cos(thetaA) * Math.Cos(Dec) * Math.Cos(RA) + Math.Sin(thetaA) * Math.Sin(Dec);
				double num3 = Math.Cos(thetaA) * Math.Sin(Dec) - Math.Sin(thetaA) * Math.Cos(Dec) * Math.Cos(RA);
				RA = Math.Atan2(num, num2) - zetaA;
				if (RA < 0.0)
				{
					RA += Math.PI * 2.0;
				}
				if (RA > Math.PI * 2.0)
				{
					RA -= Math.PI * 2.0;
				}
				Dec = Math.Atan(num3 / Math.Sqrt(num * num + num2 * num2));
				return;
			}
			double num4 = (-2451545.0 + JDStart) / 36525.0;
			double num5 = 84381.406;
			double num6 = ((((-9.51E-08 * num4 + 0.000132851) * num4 - 0.00114045) * num4 - 1.0790069) * num4 + 5038.481507) * num4;
			double num7 = ((((3.337E-07 * num4 - 4.67E-07) * num4 - 0.00772503) * num4 + 0.0512623) * num4 - 0.025754) * num4 + num5;
			double num8 = ((((-5.6E-08 * num4 + 0.000170663) * num4 - 0.00121197) * num4 - 2.3814292) * num4 + 10.556403) * num4;
			num5 = num5 / (180.0 / Math.PI) / 3600.0;
			num6 = num6 / (180.0 / Math.PI) / 3600.0;
			num7 = num7 / (180.0 / Math.PI) / 3600.0;
			double num9 = num8 / (180.0 / Math.PI) / 3600.0;
			double num10 = Math.Sin(num5);
			double num11 = Math.Cos(num5);
			double num12 = Math.Sin(0.0 - num6);
			double num13 = Math.Cos(0.0 - num6);
			double num14 = Math.Sin(0.0 - num7);
			double num15 = Math.Cos(0.0 - num7);
			double num16 = Math.Sin(num9);
			double num17 = Math.Cos(num9);
			double num18 = num17 * num13 - num12 * num16 * num15;
			double num19 = num17 * num12 * num11 + num16 * num15 * num13 * num11 - num10 * num16 * num14;
			double num20 = num17 * num12 * num10 + num16 * num15 * num13 * num10 + num11 * num16 * num14;
			double num21 = (0.0 - num16) * num13 - num12 * num17 * num15;
			double num22 = (0.0 - num16) * num12 * num11 + num17 * num15 * num13 * num11 - num10 * num17 * num14;
			double num23 = (0.0 - num16) * num12 * num10 + num17 * num15 * num13 * num10 + num11 * num17 * num14;
			double num24 = num12 * num14;
			double num25 = (0.0 - num14) * num13 * num11 - num10 * num15;
			double num26 = (0.0 - num14) * num13 * num10 + num15 * num11;
			double num27 = Math.Cos(Dec) * Math.Cos(RA);
			double num28 = Math.Cos(Dec) * Math.Sin(RA);
			double num29 = Math.Sin(Dec);
			double num30 = num18 * num27 + num21 * num28 + num24 * num29;
			double num31 = num19 * num27 + num22 * num28 + num25 * num29;
			double num32 = num20 * num27 + num23 * num28 + num26 * num29;
			RA = Math.Atan2(num31, num30);
			Dec = Math.Atan(num32 / Math.Sqrt(num30 * num30 + num31 * num31));
			if (RA < 0.0)
			{
				RA += Math.PI * 2.0;
			}
			if (RA > Math.PI * 2.0)
			{
				RA -= Math.PI * 2.0;
			}
		}

		public static void PrecessFromJ2000(double JDEnd, bool use2006values_Not1976, double RA2000, double Dec2000, out double RA_date, out double Dec_date)
		{
			double RA2001 = RA2000;
			double Dec2001 = Dec2000;
			PrecessFromJ2000(JDEnd, use2006values_Not1976, ref RA2001, ref Dec2001);
			RA_date = RA2001;
			Dec_date = Dec2001;
		}

		public static void PrecessFromJ2000(double JDEnd, bool use2006values_Not1976, ref double RA, ref double Dec)
		{
			if (!use2006values_Not1976)
			{
				PrecessionalValues(JDEnd, use2006values_Not1976, out var zetaA, out var zA, out var thetaA);
				RA += zetaA;
				double num = Math.Cos(Dec) * Math.Sin(RA);
				double num2 = Math.Cos(thetaA) * Math.Cos(Dec) * Math.Cos(RA) - Math.Sin(thetaA) * Math.Sin(Dec);
				double num3 = Math.Cos(thetaA) * Math.Sin(Dec) + Math.Sin(thetaA) * Math.Cos(Dec) * Math.Cos(RA);
				RA = Math.Atan2(num, num2) + zA;
				if (RA < 0.0)
				{
					RA += Math.PI * 2.0;
				}
				if (RA > Math.PI * 2.0)
				{
					RA -= Math.PI * 2.0;
				}
				Dec = Math.Atan(num3 / Math.Sqrt(num * num + num2 * num2));
				return;
			}
			double num4 = (JDEnd - 2451545.0) / 36525.0;
			double num5 = 84381.406;
			double num6 = ((((-9.51E-08 * num4 + 0.000132851) * num4 - 0.00114045) * num4 - 1.0790069) * num4 + 5038.481507) * num4;
			double num7 = ((((3.337E-07 * num4 - 4.67E-07) * num4 - 0.00772503) * num4 + 0.0512623) * num4 - 0.025754) * num4 + num5;
			double num8 = ((((-5.6E-08 * num4 + 0.000170663) * num4 - 0.00121197) * num4 - 2.3814292) * num4 + 10.556403) * num4;
			num5 = num5 / (180.0 / Math.PI) / 3600.0;
			num6 = num6 / (180.0 / Math.PI) / 3600.0;
			num7 = num7 / (180.0 / Math.PI) / 3600.0;
			double num9 = num8 / (180.0 / Math.PI) / 3600.0;
			double num10 = Math.Sin(num5);
			double num11 = Math.Cos(num5);
			double num12 = Math.Sin(0.0 - num6);
			double num13 = Math.Cos(0.0 - num6);
			double num14 = Math.Sin(0.0 - num7);
			double num15 = Math.Cos(0.0 - num7);
			double num16 = Math.Sin(num9);
			double num17 = Math.Cos(num9);
			double num18 = num17 * num13 - num12 * num16 * num15;
			double num19 = num17 * num12 * num11 + num16 * num15 * num13 * num11 - num10 * num16 * num14;
			double num20 = num17 * num12 * num10 + num16 * num15 * num13 * num10 + num11 * num16 * num14;
			double num21 = (0.0 - num16) * num13 - num12 * num17 * num15;
			double num22 = (0.0 - num16) * num12 * num11 + num17 * num15 * num13 * num11 - num10 * num17 * num14;
			double num23 = (0.0 - num16) * num12 * num10 + num17 * num15 * num13 * num10 + num11 * num17 * num14;
			double num24 = num12 * num14;
			double num25 = (0.0 - num14) * num13 * num11 - num10 * num15;
			double num26 = (0.0 - num14) * num13 * num10 + num15 * num11;
			double num27 = Math.Cos(Dec) * Math.Cos(RA);
			double num28 = Math.Cos(Dec) * Math.Sin(RA);
			double num29 = Math.Sin(Dec);
			double num30 = num18 * num27 + num19 * num28 + num20 * num29;
			double num31 = num21 * num27 + num22 * num28 + num23 * num29;
			double num32 = num24 * num27 + num25 * num28 + num26 * num29;
			RA = Math.Atan2(num31, num30);
			Dec = Math.Atan(num32 / Math.Sqrt(num30 * num30 + num31 * num31));
			if (RA < 0.0)
			{
				RA += Math.PI * 2.0;
			}
			if (RA > Math.PI * 2.0)
			{
				RA -= Math.PI * 2.0;
			}
		}

		public static void PrecessLatLongFromJ2000(double JD, ref double L, ref double B)
		{
			double num = (JD - 2451545.0) / 36525.0;
			double num2 = (5.12362 + 0.241614 * num + 0.0001122 * num * num) / (180.0 / Math.PI);
			double num3 = (5028.7962 * num + 1.1054 * num * num) / 3600.0 / (180.0 / Math.PI);
			double num4 = (46.999 * num - 0.0335 * num * num) / 3600.0 / (180.0 / Math.PI);
			double num5 = L;
			double num6 = B;
			L = num5 + num3 - num4 * Math.Cos(num5 + num2) * Math.Tan(num6);
			B = num6 + num4 * Math.Sin(num5 + num2);
		}

		public static void ConvertXYZ_1950to2000(ref double x, ref double y, ref double z)
		{
			double num = 0.999925678 * x - 0.011182061 * y - 0.004857948 * z;
			double num2 = 0.011182061 * x + 0.999937478 * y - 2.7165E-05 * z;
			double num3 = 0.004857948 * x - 2.7148E-05 * y + 0.9999881997 * z;
			x = num;
			y = num2;
			z = num3;
		}

		public static void ConvertXYZ_2000To1950(ref double x, ref double y, ref double z)
		{
			double num = 0.999925679 * x + 0.0111814828 * y + 0.004859004 * z;
			double num2 = -0.011181483 * x + 0.999937485 * y - 2.7177E-05 * z;
			double num3 = -0.004859004 * x - 2.7156E-05 * y + 0.9999881946 * z;
			x = num;
			y = num2;
			z = num3;
		}

		public static double CombinedMagnitude(double Mag1, double Mag2)
		{
			return -2.5 * Math.Log10(Math.Pow(10.0, Mag1 / -2.5) + Math.Pow(10.0, Mag2 / -2.5));
		}

		public static void ApparentStarPosition(ref double RA, ref double Dec, double muRA, double muDec, int Equinox, double JD, bool use2006values_Not1976)
		{
			double rA_Start_rad = RA;
			double dec_Start_rad = Dec;
			double stellar_Parallax_rad = 0.0;
			bool includeRelativisticBending = true;
			ApparentStarPosition(rA_Start_rad, dec_Start_rad, muRA, muDec, stellar_Parallax_rad, Equinox, includeRelativisticBending, JD, use2006values_Not1976, out var RA_end, out var Dec_end);
			RA = RA_end;
			Dec = Dec_end;
		}

		public static void ApparentStarPosition(double RA_Start_rad, double Dec_Start_rad, double muRA_rad, double muDec_rad, double Stellar_Parallax_rad, int Equinox, bool IncludeRelativisticBending, double JDend, bool use2006values_Not1976, out double RA_end, out double Dec_end)
		{
			ApparentStarPosition(RA_Start_rad, Dec_Start_rad, muRA_rad, muDec_rad, Stellar_Parallax_rad, Equinox, IncludeAberration: true, IncludeRelativisticBending, JDend, use2006values_Not1976, out RA_end, out Dec_end);
		}

		public static void ApparentStarPosition(double RA_Start_rad, double Dec_Start_rad, double muRA_rad, double muDec_rad, double Stellar_Parallax_rad, int Equinox, bool IncludeAberration, bool IncludeRelativisticBending, double JDend, bool use2006values_Not1976, out double RA_end, out double Dec_end)
		{
			double C = 0.0;
			double D = 0.0;
			double dRA = 0.0;
			double dDec = 0.0;
			double ParallaxCoeff_RA = 0.0;
			double ParallaxCoeff_Dec = 0.0;
			double dX = 0.0;
			double dY = 0.0;
			double dZ = 0.0;
			double num = 0.0;
			double num2 = 0.0;
			RA_end = RA_Start_rad;
			Dec_end = Dec_Start_rad;
			if (IncludeAberration && JPL_DE.DE_EphemerisAvailable)
			{
				JPL_DE.DE_AberrationCoefficients(JDend, out dX, out dY, out dZ);
				num = 0.00577551833 * ((0.0 - dX) * Math.Sin(RA_Start_rad) + dY * Math.Cos(RA_Start_rad)) / Math.Cos(Dec_Start_rad);
				num2 = 0.00577551833 * ((0.0 - dX) * Math.Cos(RA_Start_rad) * Math.Sin(Dec_Start_rad) - dY * Math.Sin(RA_Start_rad) * Math.Sin(Dec_Start_rad) + dZ * Math.Cos(Dec_Start_rad));
				num += 3.335661198016599E-05 * (dX * Math.Sin(RA_Start_rad) - dY * Math.Cos(RA_Start_rad)) * (dX * Math.Cos(RA_Start_rad) + dY * Math.Sin(RA_Start_rad)) / Math.Cos(Dec_Start_rad) / Math.Cos(Dec_Start_rad);
				num2 -= 1.6678305990082995E-05 * (Math.Pow(dX * Math.Sin(RA_Start_rad) - dY * Math.Cos(RA_Start_rad), 2.0) * Math.Tan(Dec_Start_rad));
				num2 += 3.335661198016599E-05 * (dX * Math.Cos(Dec_Start_rad) * Math.Cos(RA_Start_rad) + dY * Math.Cos(Dec_Start_rad) * Math.Sin(RA_Start_rad) + dZ * Math.Sin(Dec_Start_rad)) * (dX * Math.Sin(Dec_Start_rad) * Math.Cos(RA_Start_rad) + dY * Math.Sin(Dec_Start_rad) * Math.Sin(RA_Start_rad) - dZ * Math.Cos(Dec_Start_rad));
			}
			Precession(Equinox, JDend, use2006values_Not1976, RA_Start_rad + num, Dec_Start_rad + num2, muRA_rad, muDec_rad, out RA_end, out Dec_end);
			Nutation(JDend, out var Longitude_arcsec, out var Obliquity_arcsec, out var _);
			double num3 = MeanEclipticOfDate(JDend, Use1976Value: true);
			RA_end += (Longitude_arcsec * (Math.Cos(num3) + Math.Sin(num3) * Math.Sin(RA_end) * Math.Tan(Dec_end)) - Obliquity_arcsec * Math.Cos(RA_end) * Math.Tan(Dec_end)) / 3600.0 / (180.0 / Math.PI);
			Dec_end += (Longitude_arcsec * Math.Sin(num3) * Math.Cos(RA_end) + Obliquity_arcsec * Math.Sin(RA_end)) / 3600.0 / (180.0 / Math.PI);
			if (IncludeAberration && !JPL_DE.DE_EphemerisAvailable)
			{
				Aberration_CD(JDend, out C, out D);
				RA_end += (C * Math.Cos(RA_end) + D * Math.Sin(RA_end)) / Math.Cos(Dec_end);
				Dec_end += C * (Math.Tan(num3) * Math.Cos(Dec_end) - Math.Sin(RA_end) * Math.Sin(Dec_end)) + D * Math.Cos(RA_end) * Math.Sin(Dec_end);
			}
			if (Stellar_Parallax_rad != 0.0)
			{
				StellarParallaxCoefficients(JDend, RA_end, Dec_end, out ParallaxCoeff_RA, out ParallaxCoeff_Dec);
				RA_end += Stellar_Parallax_rad * ParallaxCoeff_RA;
				Dec_end += Stellar_Parallax_rad * ParallaxCoeff_Dec;
			}
			if (IncludeRelativisticBending)
			{
				Relativistic_Correction(JDend, RA_end, Dec_end, out dRA, out dDec, out var _);
				RA_end += dRA;
				Dec_end += dDec;
			}
		}

		public static void ApparentPositionToJ2000(double JD, double RAapparent, double DecApparent, out double RA2000, out double Dec2000)
		{
			double num = 0.0;
			double num2 = 0.0;
			RA2000 = RAapparent;
			Dec2000 = DecApparent;
			for (int i = 0; i < 4; i++)
			{
				ApparentStarPosition(RA2000, Dec2000, 0.0, 0.0, 0.0, 2000, IncludeRelativisticBending: false, JD, use2006values_Not1976: true, out var RA_end, out var Dec_end);
				num = RAapparent - RA_end;
				num2 = DecApparent - Dec_end;
				RA2000 += num;
				Dec2000 += num2;
			}
		}

		public static void ApparentOffsetToJ2000Offset(double JD, double StarRA2000, double StarDec2000, double ApparentOffsetRA_asec, double ApparentOffsetDec_asec, out double J2000OffsetRA_asec, out double J2000OffsetDec_asec)
		{
			J2000OffsetRA_asec = (J2000OffsetDec_asec = 0.0);
			ApparentStarPosition(StarRA2000, StarDec2000, 0.0, 0.0, 0.0, 2000, IncludeRelativisticBending: false, JD, use2006values_Not1976: true, out var RA_end, out var Dec_end);
			double num = RA_end + ApparentOffsetRA_asec / Math.Cos(Dec_end) * 4.84813681109536E-06;
			double num2 = Dec_end + ApparentOffsetDec_asec * 4.84813681109536E-06;
			J2000OffsetRA_asec = ApparentOffsetRA_asec;
			J2000OffsetDec_asec = ApparentOffsetDec_asec;
			for (int i = 0; i < 3; i++)
			{
				double rA_Start_rad = StarRA2000 + J2000OffsetRA_asec / Math.Cos(StarDec2000) * 4.84813681109536E-06;
				double dec_Start_rad = StarDec2000 + J2000OffsetDec_asec * 4.84813681109536E-06;
				ApparentStarPosition(rA_Start_rad, dec_Start_rad, 0.0, 0.0, 0.0, 2000, IncludeRelativisticBending: false, JD, use2006values_Not1976: true, out var RA_end2, out var Dec_end2);
				double num3 = (num - RA_end2) * Math.Cos(Dec_end) / 4.84813681109536E-06;
				double num4 = (num2 - Dec_end2) / 4.84813681109536E-06;
				J2000OffsetRA_asec += num3;
				J2000OffsetDec_asec += num4;
			}
		}

		public static double SiderealTime_deg(double JD, bool Apparent)
		{
			double num = Math.Floor(JD) + 0.5;
			double num2 = (num - 2451545.0) / 36525.0;
			double num3 = 100.46062241 + 36000.769977233 * num2 + 0.00038797 * num2 * num2;
			num3 += 360.985647 * (JD - num);
			num3 %= 360.0;
			if (Apparent)
			{
				Nutation(JD, out var Longitude_arcsec, out var _, out var TrueEcliptic);
				num3 += Longitude_arcsec * Math.Cos(TrueEcliptic) / 3600.0;
			}
			return NormaliseDegrees(num3);
		}

		public static double Relativistic_Correction(double JD, double RAStar_2000, double DecStar_2000, out double dRA, out double dDec, out double Elongation_deg)
		{
			return Relativistic_Correction(JD, RAStar_2000, DecStar_2000, 1000000000.0, out dRA, out dDec, out Elongation_deg);
		}

		public static double Relativistic_Correction(double JD, double RA_Object_2000, double Dec_Object_2000, double Distance_Object, out double dRA, out double dDec, out double Elongation_deg)
		{
			JPL_DE.Planet_PositionFromEarth(JD, 0, out var x, out var y, out var z, out var _, out var _, out var _, out var _);
			XYZ_to_RA_Dec(x, y, z, out var RA, out var Dec);
			double num = Math.Sqrt(x * x + y * y + z * z);
			Utilities.Distance(RA_Object_2000, Dec_Object_2000, RA, Dec, out var Distance, out var PA_atOrigin);
			double num2 = PA_atOrigin + Math.PI;
			double num3 = 1.9741257130033782E-08 / num;
			double num4 = Math.Atan2(Distance_Object * Math.Sin(Distance), num - Distance_Object * Math.Cos(Distance));
			double num5 = num3 * Math.Sin(num4) / (1.0 + Math.Cos(num4)) / num;
			dRA = num5 * Math.Sin(num2) / Math.Cos(Dec_Object_2000);
			dDec = num5 * Math.Cos(num2);
			Elongation_deg = Distance * (180.0 / Math.PI);
			return num5 * (180.0 / Math.PI) * 3600.0;
		}

		public static double Relativistic_Differential_Correction(double JD, double RAStar_2000, double DecStar_2000, double D_Asteroid, out double dRA_ComparedToStar, out double dDec_ComparedToStar)
		{
			double num = 1.0;
			JPL_DE.Planet_PositionFromEarth(JD, 0, out var x, out var y, out var z, out var _, out var _, out var _, out var _);
			XYZ_to_RA_Dec(x, y, z, out var RA, out var Dec);
			num = Math.Sqrt(x * x + y * y + z * z);
			Utilities.Distance(RAStar_2000, DecStar_2000, RA, Dec, out var Distance, out var PA_atOrigin);
			double num2 = PA_atOrigin + Math.PI;
			double num3 = 1.9741257487038883E-08 / num;
			double num4 = Math.PI - Distance;
			double num5 = Math.Atan2(D_Asteroid * Math.Sin(Distance), num - D_Asteroid * Math.Cos(Distance));
			double num6 = num3 * Math.Sin(num4) / (1.0 + Math.Cos(num4)) / num;
			double num7 = num3 * Math.Sin(num5) / (1.0 + Math.Cos(num5)) / num;
			dRA_ComparedToStar = (num7 - num6) * Math.Sin(num2) / Math.Cos(DecStar_2000);
			dDec_ComparedToStar = (num7 - num6) * Math.Cos(num2);
			return (num7 - num6) * (180.0 / Math.PI) * 3600.0;
		}

		public static double StarDiameter_CHARM2_CADARS(double RAhrs, double DecDeg, double MagV, double MagB, double MagR, out string Basis, out int NumMeasures, out bool InvalidDiameter)
		{
			Basis = "CHARM2/CADARS";
			NumMeasures = 0;
			InvalidDiameter = true;
			double Dia = 0.0;
			InvalidDiameter = !StarDiameters.Charm2CadarsStarDia(RAhrs, DecDeg, MagV, out Dia, out NumMeasures);
			return Dia;
		}

		internal static double StarDiameterMAS_FromGaia(double Parallax_asec, double Gaia_FlameRadius)
		{
			return 9.304 * Parallax_asec * Gaia_FlameRadius;
		}

		internal static bool StarTypeFromBVR(double MagB, double MagV, double MagR, out string SpecType)
		{
			SpecType = "";
			double num = MagB - MagV;
			double num2 = MagV - MagR;
			string[] array = new string[42]
			{
				"B0", "B1", "B2", "B3", "B4", "B5", "B6", "B7", "B8", "B9",
				"A0", "A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9",
				"F0", "F1", "F2", "F5", "F8", "G0", "G2", "G3", "G5", "G8",
				"K0", "K1", "K2", "K3", "K4", "K5", "K7", "M0", "M1", "M2",
				"M3", "M4"
			};
			int num3 = -1;
			int num4 = -1;
			if (num > -0.3 && num < 1.5)
			{
				num3 = ((num < -0.04) ? ((int)((num + 0.29) / 0.0253)) : ((num < 0.4) ? ((int)((num + 0.3464) / 0.0329)) : ((num < 0.61) ? ((int)((num + 1.468) / 0.083)) : ((num < 1.1) ? ((int)((num + 0.6105) / 0.0473)) : ((!(num < 1.42)) ? ((int)((num + 0.4967) / 0.025)) : ((int)((num + 2.912) / 0.116)))))));
				if (num3 > 41)
				{
					num3 = 41;
				}
				if (num3 < 0)
				{
					num3 = 0;
				}
			}
			if (num2 > -0.19 && num2 < 0.95)
			{
				num4 = ((num2 < 0.4) ? ((int)((num2 + 0.1822) / 0.0148)) : ((!(num2 < 1.25)) ? ((int)((num2 + 1.1936) / 0.0518)) : ((int)((num2 + 0.6489) / 0.0361))));
				if (num4 > 41)
				{
					num4 = 41;
				}
				if (num4 < 0)
				{
					num4 = 0;
				}
			}
			int num5 = ((num3 >= 0 && num4 >= 0) ? ((num3 + num4) / 2) : ((num3 < 0) ? num4 : num3));
			if (num5 < 0)
			{
				return false;
			}
			SpecType = array[num5];
			return true;
		}

		internal static string StarIdentifier_ToMag6(double RAStar_Deg, double DecStar_Deg, double StarMag, bool WithConstellation)
		{
			if (StarIDNotAvailable)
			{
				return "";
			}
			if (StarMag > 6.0)
			{
				return "";
			}
			PopulateStarIDs();
			int count = StarId.Count;
			int num = StarId.Count - 1;
			int num2 = 0;
			double num3 = RAStar_Deg - 0.02;
			if (num3 < 0.0)
			{
				num3 = 0.0;
			}
			int num4;
			do
			{
				num4 = (num + num2) / 2;
				if (StarId[num4].RAStar <= num3)
				{
					num2 = num4 + 1;
				}
				else if (StarId[num4].RAStar >= num3)
				{
					num = num4 - 1;
				}
			}
			while (num >= num2);
			num4--;
			if (num4 < 0)
			{
				num4 = 0;
			}
			double num5 = 1.0;
			double Distance = 0.0;
			string result = "";
			while (num4 < count)
			{
				Utilities.Distance(RAStar_Deg / (180.0 / Math.PI), DecStar_Deg / (180.0 / Math.PI), StarId[num4].RAStar / (180.0 / Math.PI), StarId[num4].DecStar / (180.0 / Math.PI), out Distance, out var _);
				Distance *= 180.0 / Math.PI;
				if (Distance < num5)
				{
					result = ((!WithConstellation) ? StarId[num4].Star_identifier : StarId[num4].Star_identifierWithConstellation);
					num5 = Distance;
				}
				num4++;
				if (num4 >= count || !(StarId[num4].RAStar < RAStar_Deg + 0.02))
				{
					break;
				}
			}
			return result;
		}

		internal static void PopulateStarIDs()
		{
			if (StarId.Count >= 1)
			{
				return;
			}
			if (File.Exists(AppPath + "\\Resource Files\\Bayer.bin"))
			{
				using (FileStream fileStream = new FileStream(AppPath + "\\Resource Files\\Bayer.bin", FileMode.Open, FileAccess.Read))
				{
					int num = (int)(fileStream.Length / 12);
					using BinaryReader binaryReader = new BinaryReader(fileStream);
					for (int i = 0; i < num; i++)
					{
						Star_IDs star_IDs = new Star_IDs();
						star_IDs.RAStar = Convert.ToDouble(binaryReader.ReadSingle());
						star_IDs.DecStar = Convert.ToDouble(binaryReader.ReadSingle());
						star_IDs.BayerNumber = Convert.ToInt16(binaryReader.ReadInt16());
						star_IDs.FlamsteedNumber = Convert.ToInt16(binaryReader.ReadInt16());
						StarId.Add(star_IDs);
					}
				}
				StarId.Sort();
			}
			else
			{
				StarIDNotAvailable = true;
			}
		}

		internal static string StarNameFromHip(int HipNumber, double StarMag)
		{
			if (StarNameNotAvailable)
			{
				return "";
			}
			if (StarMag > 4.0)
			{
				return "";
			}
			PopulateStarNames();
			int num = HipName.Count - 1;
			int num2 = 0;
			do
			{
				int num3 = (num + num2) / 2;
				if (HipName[num3].Hip == HipNumber)
				{
					return HipName[num3].Name;
				}
				if (HipName[num3].Hip > HipNumber)
				{
					num = num3 - 1;
				}
				else
				{
					num2 = num3 + 1;
				}
			}
			while (num >= num2);
			return "";
		}

		internal static void PopulateStarNames()
		{
			if (!File.Exists(AppPath + "\\Resource Files\\StarNames_Hipparcos.csv") && InternetIsAvailable())
			{
				StarNameNotAvailable = !http.DownloadHTTP(Settings.Default.OccultServer, "StarNames.zip", AppPath + "\\Resource Files\\", unzip: true, gunzip: false, ShowMessages: false);
			}
			if (HipName.Count >= 1)
			{
				return;
			}
			using StreamReader streamReader = new StreamReader(AppPath + "\\Resource Files\\StarNames_Hipparcos.csv");
			do
			{
				string[] array = streamReader.ReadLine()!.Split(new char[1] { ',' });
				StarNamesFromHip starNamesFromHip = new StarNamesFromHip();
				starNamesFromHip.Hip = int.Parse(array[0]);
				starNamesFromHip.Name = array[1].Trim();
				HipName.Add(starNamesFromHip);
			}
			while (!streamReader.EndOfStream);
		}

		public static string EphemerisBasis()
		{
			string text = "";
			string text2 = "";
			if (File.Exists(AppPath + "\\Resource Files\\DE_LongEphemeris.bin"))
			{
				text2 = JPL_DE.GetDE_VersionDetails("DE_LongEphemeris.bin");
			}
			if (File.Exists(AppPath + "\\Resource Files\\" + JPL_DE.DE_Ephemeris_Version))
			{
				text = JPL_DE.GetDE_VersionDetails(JPL_DE.DE_Ephemeris_Version);
			}
			if ((text.Length == 0) & (text2.Length == 0))
			{
				return "VSOP87A";
			}
			if ((text.Length > 0) & (text2.Length > 0))
			{
				return text + ", " + text2;
			}
			if (text2.Length == 0)
			{
				return text + ", VSOP87A";
			}
			return text2;
		}

		public static string EphemerisBasis(double jd_TT)
		{
			if ((jd_TT >= JPL_DE.JDStart) & (jd_TT < JPL_DE.JDEnd))
			{
				return "DE" + JPL_DE.EphemerisVersion;
			}
			if ((jd_TT >= JPL_DE.JDStartLong) & (jd_TT < JPL_DE.JDEndLong))
			{
				return "DE" + JPL_DE.LongEphemerisVersion;
			}
			return "VSOP87A";
		}

		public static bool ValidateArchiveFileReductionBasis(out string FNames)
		{
			bool flag = true;
			string value = EphemerisBasis();
			FNames = "";
			string[] files = Directory.GetFiles(AppPath + "\\Resource Files", "RArchive*.*");
			foreach (string path in files)
			{
				using StreamReader streamReader = new StreamReader(path);
				for (int j = 0; j < 10; j++)
				{
					string text = streamReader.ReadLine();
					if (text.Contains("Ephemeris      :"))
					{
						flag &= text.Contains(value);
						if (!text.Contains(value))
						{
							FNames = FNames + Path.GetFileName(path) + "\r\n";
						}
						break;
					}
					if (streamReader.EndOfStream)
					{
						break;
					}
				}
			}
			return flag;
		}

		public static void PurgeLunarEphemerisCache_UpdatedEphemeris()
		{
			string text = EphemerisBasis();
			if (!(Settings.Default.Current_DE_Ephemeris == text))
			{
				Settings.Default.Current_DE_Ephemeris = text;
				string[] files = Directory.GetFiles(AppPath + "\\Resource Files\\Moon", "Moon*.bin");
				for (int i = 0; i < files.Length; i++)
				{
					File.Delete(files[i]);
				}
			}
		}

		public static void PurgeLunarEphemerisCache()
		{
			string[] files = Directory.GetFiles(AppPath + "\\Resource Files\\Moon", "Moon*.bin");
			for (int i = 0; i < files.Length; i++)
			{
				File.Delete(files[i]);
			}
		}

		public static void PlanetXYZ(double JD_TT, int PlanetNumber, bool Heliocentric, double Accuracy, ref double X, ref double Y, ref double Z, out int Version)
		{
			Version = 87;
			if (((DE_EphemerisFileExists | DE_LongEphemerisFileExists) & !MustUseVSOP87) && JPL_DE.Planet_XYZCoords(JD_TT, PlanetNumber, Heliocentric, out X, out Y, out Z, out Version))
			{
				return;
			}
			double[] array = new double[4];
			double[] array2 = new double[6];
			FileStream fileStream = new FileStream(AppPath + "\\Resource Files\\" + Planets[PlanetNumber] + ".bin", FileMode.Open, FileAccess.Read);
			BinaryReader binaryReader = new BinaryReader(fileStream);
			array2[0] = 1.0;
			array2[1] = (JD_TT - 2451545.0) / 365250.0;
			array2[2] = array2[1] * array2[1];
			array2[3] = array2[2] * array2[1];
			array2[4] = array2[3] * array2[1];
			array2[5] = array2[4] * array2[1];
			if (PlanetNumber > 0 && PlanetNumber < 9)
			{
				array[1] = 0.0;
				array[2] = 0.0;
				array[3] = 0.0;
				long num = fileStream.Length / 28;
				for (long num2 = 1L; num2 <= num; num2++)
				{
					int num3 = binaryReader.ReadInt16();
					int num4 = binaryReader.ReadInt16();
					double num5 = binaryReader.ReadDouble();
					double num6 = binaryReader.ReadDouble();
					double num7 = binaryReader.ReadDouble();
					array[num3] += num5 * Math.Cos(num6 + num7 * array2[1]) * array2[num4];
					if (num5 < Accuracy)
					{
						break;
					}
				}
				X = array[1] + 4.4036E-07 * array[2] - 1.90919E-07 * array[3];
				Y = -4.79966E-07 * array[1] + 0.917482137087 * array[2] - 0.397776982902 * array[3];
				Z = 0.397776982902 * array[2] + 0.917482137087 * array[3];
				fileStream.Close();
			}
			else if (PlanetNumber == 9)
			{
				if (JD_TT > 2341972.5 || JD_TT < 2488092.5)
				{
					double num8 = 2.0 * (JD_TT - 2341972.5) / 146120.0 - 1.0;
					double num9 = num8 * 73060.0;
					FileStream fileStream2 = new FileStream(AppPath + "\\Resource Files\\Pluto.bin", FileMode.Open, FileAccess.Read);
					BinaryReader binaryReader2 = new BinaryReader(fileStream2);
					X = 0.0;
					Y = 0.0;
					Z = 0.0;
					for (long num2 = 1L; num2 < 83; num2++)
					{
						double num10 = binaryReader2.ReadDouble();
						double num11 = binaryReader2.ReadDouble();
						double num12 = binaryReader2.ReadDouble();
						double num13 = binaryReader2.ReadDouble();
						double num14 = binaryReader2.ReadDouble();
						double num15 = binaryReader2.ReadDouble();
						double num16 = binaryReader2.ReadDouble();
						double num17 = num10 * num9;
						double num18 = Math.Cos(num17);
						double num19 = Math.Sin(num17);
						X = X + num11 * num18 + num12 * num19;
						Y = Y + num13 * num18 + num14 * num19;
						Z = Z + num15 * num18 + num16 * num19;
					}
					for (long num2 = 83L; num2 < 102; num2++)
					{
						double num10 = binaryReader2.ReadDouble();
						double num11 = binaryReader2.ReadDouble();
						double num12 = binaryReader2.ReadDouble();
						double num13 = binaryReader2.ReadDouble();
						double num14 = binaryReader2.ReadDouble();
						double num15 = binaryReader2.ReadDouble();
						double num16 = binaryReader2.ReadDouble();
						double num20 = num10 * num9;
						double num18 = Math.Cos(num20);
						double num19 = Math.Sin(num20);
						X += num8 * (num11 * num18 + num12 * num19);
						Y += num8 * (num13 * num18 + num14 * num19);
						Z += num8 * (num15 * num18 + num16 * num19);
					}
					for (long num2 = 102L; num2 < 107; num2++)
					{
						double num10 = binaryReader2.ReadDouble();
						double num11 = binaryReader2.ReadDouble();
						double num12 = binaryReader2.ReadDouble();
						double num13 = binaryReader2.ReadDouble();
						double num14 = binaryReader2.ReadDouble();
						double num15 = binaryReader2.ReadDouble();
						double num16 = binaryReader2.ReadDouble();
						double num21 = num10 * num9;
						double num18 = Math.Cos(num21);
						double num19 = Math.Sin(num21);
						X += num8 * num8 * (num11 * num18 + num12 * num19);
						Y += num8 * num8 * (num13 * num18 + num14 * num19);
						Z += num8 * num8 * (num15 * num18 + num16 * num19);
					}
					double num22 = binaryReader2.ReadDouble();
					double num23 = binaryReader2.ReadDouble();
					double num24 = binaryReader2.ReadDouble();
					double num25 = binaryReader2.ReadDouble();
					X = X + num22 + num23 * num8 + num24 * num8 * num8 + num25 * num8 * num8 * num8;
					num22 = binaryReader2.ReadDouble();
					num23 = binaryReader2.ReadDouble();
					num24 = binaryReader2.ReadDouble();
					num25 = binaryReader2.ReadDouble();
					Y = Y + num22 + num23 * num8 + num24 * num8 * num8 + num25 * num8 * num8 * num8;
					num22 = binaryReader2.ReadDouble();
					num23 = binaryReader2.ReadDouble();
					num24 = binaryReader2.ReadDouble();
					num25 = binaryReader2.ReadDouble();
					Z = Z + num22 + num23 * num8 + num24 * num8 * num8 + num25 * num8 * num8 * num8;
					fileStream2.Close();
				}
				else
				{
					double num26 = 10.0 * array2[1] + 1.0;
					double num27 = (238.74 + 3034.9057 * num26) / (180.0 / Math.PI);
					double num28 = (267.26 + 1222.1138 * num26) / (180.0 / Math.PI);
					double num29 = (93.48 + 144.96 * num26) / (180.0 / Math.PI);
					double num30 = 0.0;
					double num31 = 0.0;
					double num32 = 0.0;
					FileStream fileStream3 = new FileStream(AppPath + "\\Resource Files\\Pluto.dat", FileMode.Open, FileAccess.Read);
					StreamReader streamReader = new StreamReader(fileStream3);
					for (long num2 = 1L; num2 < 44; num2++)
					{
						double num33 = Convert.ToDouble(streamReader.ReadLine());
						double num34 = Convert.ToDouble(streamReader.ReadLine());
						double num35 = Convert.ToDouble(streamReader.ReadLine());
						double num36 = Convert.ToDouble(streamReader.ReadLine());
						double num37 = Convert.ToDouble(streamReader.ReadLine());
						double num38 = Convert.ToDouble(streamReader.ReadLine());
						double num39 = Convert.ToDouble(streamReader.ReadLine());
						double num40 = Convert.ToDouble(streamReader.ReadLine());
						double num41 = Convert.ToDouble(streamReader.ReadLine());
						double num42 = num27 * num33 + num28 * num34 + num29 * num35;
						double num43 = Math.Sin(num42);
						double num44 = Math.Cos(num42);
						num30 = num30 + num43 * num36 + num44 * num37;
						num31 = num31 + num43 * num38 + num44 * num39;
						num32 = num32 + num43 * num40 + num44 * num41;
					}
					fileStream3.Close();
					num30 = 93.297471 + 144.96 * num26 + 1E-06 * num30;
					num31 = -3.909434 + 1E-06 * num31;
					num32 = 40.7247248 + 1E-07 * num32;
					num30 -= (double)(360 * Convert.ToInt16(num30 / 360.0));
					double num45 = num30 / (180.0 / Math.PI);
					double a = num31 / (180.0 / Math.PI);
					num30 = (num30 + 0.6981595 - 0.006543 * Math.Cos(num45 + 0.1048) * Math.Tan(a)) / (180.0 / Math.PI);
					num31 = (num31 + 0.006543 * Math.Sin(num45 + 0.1048)) / (180.0 / Math.PI);
					X = num32 * Math.Cos(num30) * Math.Cos(num31);
					Y = num32 * (Math.Cos(num31) * Math.Sin(num30) * cosEcliptic2000[EclipticID] - Math.Sin(num31) * sinEcliptic2000[EclipticID]);
					Z = num32 * (Math.Cos(num31) * Math.Sin(num30) * sinEcliptic2000[EclipticID] + Math.Sin(num31) * cosEcliptic2000[EclipticID]);
				}
			}
			else
			{
				X = 0.0;
				Y = 0.0;
				Z = 0.0;
			}
		}

		public static void HeliocentricLatLong(double JD_TT, int PlanetNumber, double Accuracy, bool EquinoxOfDate, out double L, out double B, out double R)
		{
			double X;
			double Y;
			double Z;
			L = (B = (R = (X = (Y = (Z = 0.0)))));
			PlanetXYZ(JD_TT, PlanetNumber, Heliocentric: true, Accuracy, ref X, ref Y, ref Z, out var _);
			double num = X;
			double num2 = Y * cosEcliptic2000[EclipticID] + Z * sinEcliptic2000[EclipticID];
			double num3 = (0.0 - Y) * sinEcliptic2000[EclipticID] + Z * cosEcliptic2000[EclipticID];
			L = Math.Atan2(num2, num);
			if (L < 0.0)
			{
				L += Math.PI * 2.0;
			}
			B = Math.Atan(num3 / Math.Sqrt(num * num + num2 * num2));
			R = Math.Sqrt(X * X + Y * Y + Z * Z);
			if (EquinoxOfDate)
			{
				PrecessLatLongFromJ2000(JD_TT, ref L, ref B);
			}
		}

		public static void TopocentricPlanet(double JD_UT, int Planet, double Longitude, double Latitude, double Elevation, out double RA_Planet, out double Dec_Planet, out double PlanetRadius)
		{
			double HorizonAltitude = 0.0;
			TopocentricPlanet(JD_UT, Planet, Longitude, Latitude, Elevation, out RA_Planet, out Dec_Planet, out PlanetRadius, out HorizonAltitude);
		}

		public static void TopocentricPlanet(double JD_UT, int Planet, double Longitude, double Latitude, double Elevation, out double RA_Planet, out double Dec_Planet, out double PlanetRadius, out double HorizonAltitude)
		{
			double day = 0.0;
			int Year = 0;
			int Month = 0;
			Date_from_JD(JD_UT, out Year, out Month, out day);
			PlanetGeocentric(JD_UT + delta_T(Year, Month, day, out var UT1UTC) / 86400.0, Planet, 0.0, 0, physicalFlag: false, out var _, out var _, out var _, out RA_Planet, out Dec_Planet, out var TrueDistance, out PlanetRadius, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _);
			double a = 4.26345E-05 / TrueDistance;
			PlanetRadius = PlanetRadius / 7200.0 / (180.0 / Math.PI);
			GetGeodetic(Latitude, Elevation, Altitude_is_MSL: true, out var pCos, out var pSin, 0, ref Longitude);
			double num = SiderealTime_deg(JD_UT + UT1UTC / 86400.0, Apparent: true) / (180.0 / Math.PI) + Longitude;
			double num2 = num - RA_Planet;
			double num3 = Math.Cos(Dec_Planet) * Math.Sin(num2);
			double num4 = Math.Cos(Dec_Planet) * Math.Cos(num2) - pCos * Math.Sin(a);
			double num5 = Math.Sin(Dec_Planet) - pSin * Math.Sin(a);
			double num6 = Math.Atan2(num3, num4);
			for (RA_Planet = num - num6; RA_Planet > Math.PI * 2.0; RA_Planet -= Math.PI * 2.0)
			{
			}
			while (RA_Planet < 0.0)
			{
				RA_Planet += Math.PI * 2.0;
			}
			Dec_Planet = Math.Atan(num5 / Math.Sqrt(num3 * num3 + num4 * num4));
			HorizonAltitude = Math.Asin(pSin * Math.Sin(Dec_Planet) + pCos * Math.Cos(Dec_Planet) * Math.Cos(num2));
		}

		public static void TopocentricSun(double JD_UT, double Longitude, double Latitude, double Elevation, out double raPlanet, out double decPlanet, out double PlanetRadius, out double HorizonAltitude)
		{
			TopocentricPlanet(JD_UT, 0, Longitude, Latitude, Elevation, out raPlanet, out decPlanet, out PlanetRadius, out HorizonAltitude);
		}

		public static void TopocentricPosition(double JD_UT, double GeoRA, double GeoDec, double Parallax, double Longitude, double Latitude, double Elevation, out double RA_Planet, out double Dec_Planet)
		{
			GetGeodetic(Latitude, Elevation, Altitude_is_MSL: true, out var pCos, out var pSin, 0, ref Longitude);
			Date_from_JD(JD_UT, out var Year, out var Month, out var day);
			delta_T(Year, Month, day, out var UT1UTC);
			double num = SiderealTime_deg(JD_UT + UT1UTC / 86400.0, Apparent: true) / (180.0 / Math.PI) + Longitude;
			double num2 = num - GeoRA;
			double num3 = Math.Cos(GeoDec) * Math.Sin(num2);
			double num4 = Math.Cos(GeoDec) * Math.Cos(num2) - pCos * Math.Sin(Parallax);
			double num5 = Math.Sin(GeoDec) - pSin * Math.Sin(Parallax);
			double num6 = Math.Atan2(num3, num4);
			for (RA_Planet = num - num6; RA_Planet > Math.PI * 2.0; RA_Planet -= Math.PI * 2.0)
			{
			}
			while (RA_Planet < 0.0)
			{
				RA_Planet += Math.PI * 2.0;
			}
			Dec_Planet = Math.Atan(num5 / Math.Sqrt(num3 * num3 + num4 * num4));
		}

		public static void XYZ_to_RA_Dec(double X, double Y, double Z, out double RA, out double Dec)
		{
			XYZ_to_RA_Dec(X, Y, Z, out RA, out Dec, out var _);
		}

		public static void XYZ_to_RA_Dec(double X, double Y, double Z, out double RA, out double Dec, out double Distance)
		{
			if (X == 0.0)
			{
				RA = Math.Atan(Y);
			}
			else
			{
				RA = Math.Atan2(Y, X);
			}
			if (RA < 0.0)
			{
				RA += Math.PI * 2.0;
			}
			if (X == 0.0 || Y == 0.0)
			{
				Dec = 0.0;
			}
			else
			{
				Dec = Math.Atan2(Z, Math.Sqrt(X * X + Y * Y));
			}
			Distance = Math.Sqrt(X * X + Y * Y + Z * Z);
		}

		public static void RA_DEC_to_Long_Lat(double RA, double Dec, double Ecliptic, ref double Longitude, ref double Latitude)
		{
			double num = Math.Cos(Dec) * Math.Cos(RA);
			double num2 = Math.Cos(Dec) * Math.Sin(RA) * Math.Cos(Ecliptic) + Math.Sin(Dec) * Math.Sin(Ecliptic);
			double num3 = (0.0 - Math.Cos(Dec)) * Math.Sin(RA) * Math.Sin(Ecliptic) + Math.Sin(Dec) * Math.Cos(Ecliptic);
			Longitude = Math.Atan2(num2, num);
			if (Longitude < 0.0)
			{
				Longitude += Math.PI * 2.0;
			}
			Latitude = Math.Atan(num3 / Math.Sqrt(num * num + num2 * num2));
		}

		public static void Long_Lat_to_RA_DEC(double Longitude, double Latitude, double Ecliptic, ref double RA, ref double Dec)
		{
			double num = Math.Cos(Latitude) * Math.Cos(Longitude);
			double num2 = Math.Cos(Latitude) * Math.Sin(Longitude) * Math.Cos(Ecliptic) - Math.Sin(Latitude) * Math.Sin(Ecliptic);
			double num3 = Math.Cos(Latitude) * Math.Sin(Longitude) * Math.Sin(Ecliptic) + Math.Sin(Latitude) * Math.Cos(Ecliptic);
			RA = Math.Atan2(num2, num);
			if (RA < 0.0)
			{
				RA += Math.PI * 2.0;
			}
			Dec = Math.Atan(num3 / Math.Sqrt(num * num + num2 * num2));
		}

		public static void AllPlanetsXYZ_Daily(string JDstartNNNN)
		{
			if (DE_EphemerisFileExists | DE_LongEphemerisFileExists)
			{
				JPL_DE.DE_HelioXYZ_AllPLanets(JDstartNNNN);
				return;
			}
			double X = 0.0;
			double Y = 0.0;
			double Z = 0.0;
			double accuracy = 0.0;
			string text = JDstartNNNN;
			int num = 1;
			text = text.Trim() + "0000";
			double num2 = Convert.ToDouble(text) + 0.5;
			FileStream fileStream = new FileStream(AppPath + "\\Resource Files\\" + text + "_XYZ.bin", FileMode.Create, FileAccess.Write);
			BinaryWriter binaryWriter = new BinaryWriter(fileStream);
			PBar pBar = new PBar();
			((Control)pBar).set_Text("Generating daily planetary XYZ from JD" + text);
			pBar.pBarFTP.set_Minimum(0);
			pBar.pBarFTP.set_Maximum(10001);
			((Control)pBar).Show();
			for (int i = 0; i < 10000; i += num)
			{
				if (i % 100 == 0)
				{
					pBar.pBarFTP.set_Value(i);
				}
				double jD_TT = num2 + (double)i;
				for (int j = 1; j < 10; j++)
				{
					PlanetXYZ(jD_TT, j, Heliocentric: true, accuracy, ref X, ref Y, ref Z, out var _);
					if (j == 3)
					{
						binaryWriter.Write(X);
						binaryWriter.Write(Y);
						binaryWriter.Write(Z);
					}
					else
					{
						binaryWriter.Write(Convert.ToSingle(X));
						binaryWriter.Write(Convert.ToSingle(Y));
						binaryWriter.Write(Convert.ToSingle(Z));
					}
				}
			}
			fileStream.Close();
			((Form)pBar).Close();
			((Component)(object)pBar).Dispose();
		}

		public static double Altitude(double JD, double RA, double Dec, double Longitude, double Latitude)
		{
			double num = SiderealTime_deg(JD, Apparent: false) / (180.0 / Math.PI);
			return Math.Asin(Math.Sin(Latitude) * Math.Sin(Dec) + Math.Cos(Latitude) * Math.Cos(Dec) * Math.Cos(num + Longitude - RA));
		}

		public static void AltAz(double JD, double RA, double Dec, double Longitude, double Latitude, out double Altitude, out double Azimuth)
		{
			ApparentStarPosition(ref RA, ref Dec, 0.0, 0.0, 2000, JD, use2006values_Not1976: false);
			double num = SiderealTime_deg(JD, Apparent: true) / (180.0 / Math.PI) + Longitude - RA;
			double num2 = (0.0 - Math.Cos(Dec)) * Math.Sin(num);
			double num3 = Math.Sin(Dec) * Math.Cos(Latitude) - Math.Cos(Dec) * Math.Cos(num) * Math.Sin(Latitude);
			double num4 = Math.Sin(Dec) * Math.Sin(Latitude) + Math.Cos(Dec) * Math.Cos(num) * Math.Cos(Latitude);
			Azimuth = Math.Atan2(num2, num3);
			if (Azimuth < 0.0)
			{
				Azimuth += Math.PI * 2.0;
			}
			Altitude = Math.Atan(num4 / Math.Sqrt(num2 * num2 + num3 * num3));
		}

		public static void RiseSet_LocalTime(double Altitude_deg, double RA_apparent, double Dec_apparent, double JD, double Longitude_deg, double Latitude_deg, float TimeZone_hrs, out double TRise_hrs, out double TSet_hrs)
		{
			RiseSet_LocalTime(Altitude_deg, RA_apparent, 0.0, Dec_apparent, 0.0, JD, Longitude_deg, Latitude_deg, TimeZone_hrs, out TRise_hrs, out TSet_hrs);
		}

		public static void RiseSet_LocalTime(double Altitude_deg, double RA_apparent, double dRA, double Dec_apparent, double dDec, double JD, double Longitude_deg, double Latitude_deg, float TimeZone_hrs, out double TRise_hrs, out double TSet_hrs)
		{
			bool RiseInPreviousUTDay = false;
			bool SetInFollowingUTDay = false;
			double UT_of_LocalTransit = 0.0;
			RiseSet_UT(Altitude_deg, RA_apparent, dRA, Dec_apparent, dDec, JD, Longitude_deg, Latitude_deg, out TRise_hrs, out RiseInPreviousUTDay, out TSet_hrs, out SetInFollowingUTDay, out UT_of_LocalTransit);
			TRise_hrs += TimeZone_hrs;
			while (TRise_hrs < 0.0)
			{
				TRise_hrs += 0.99726956633 * (24.0 + dRA * (180.0 / Math.PI) / 15.0);
			}
			while (TRise_hrs > 24.0)
			{
				TRise_hrs -= 0.99726956633 * (24.0 + dRA * (180.0 / Math.PI) / 15.0);
			}
			TSet_hrs += TimeZone_hrs;
			while (TSet_hrs < 0.0)
			{
				TSet_hrs += 0.99726956633 * (24.0 + dRA * (180.0 / Math.PI) / 15.0);
			}
			while (TSet_hrs > 24.0)
			{
				TSet_hrs -= 0.99726956633 * (24.0 + dRA * (180.0 / Math.PI) / 15.0);
			}
		}

		public static void RiseSet_UT(double Altitude_deg, double RA_apparent, double dRA, double Dec_apparent, double dDec, double JD, double Longitude_deg, double Latitude_deg, out double TRise_hrs, out bool RiseInPreviousUTDay, out double TSet_hrs, out bool SetInFollowingUTDay, out double UT_of_LocalTransit)
		{
			double num = dRA * (180.0 / Math.PI) / 360.0;
			RiseInPreviousUTDay = (SetInFollowingUTDay = false);
			TRise_hrs = (TSet_hrs = 0.0);
			JD = Math.Floor(JD - 0.5) + 0.5;
			UT_of_LocalTransit = 0.0;
			for (int i = 0; i <= 2; i++)
			{
				UT_of_LocalTransit = 0.99726956633 * NormaliseDegrees((RA_apparent + dRA / 24.0 * UT_of_LocalTransit) * (180.0 / Math.PI) - SiderealTime_deg(JD, Apparent: true) - Longitude_deg) / 15.0;
				if (UT_of_LocalTransit < 0.0)
				{
					UT_of_LocalTransit += 23.93446959192;
				}
				if (UT_of_LocalTransit > 23.93446959192)
				{
					UT_of_LocalTransit -= 23.93446959192;
				}
			}
			double num2 = (0.0 - Math.Tan(Latitude_deg / (180.0 / Math.PI))) * Math.Tan(Dec_apparent) + Math.Cos((90.0 - Altitude_deg) / (180.0 / Math.PI)) / Math.Cos(Latitude_deg / (180.0 / Math.PI)) / Math.Cos(Dec_apparent);
			double d = (0.0 - Math.Tan(Latitude_deg / (180.0 / Math.PI))) * Math.Tan(Dec_apparent + dDec) + Math.Cos((90.0 - Altitude_deg) / (180.0 / Math.PI)) / Math.Cos(Latitude_deg / (180.0 / Math.PI)) / Math.Cos(Dec_apparent);
			if (num2 >= 1.0)
			{
				TRise_hrs = (TSet_hrs = 12.0);
				return;
			}
			if (num2 <= -1.0)
			{
				TRise_hrs = (TSet_hrs = 0.0);
				return;
			}
			double num3 = Math.Acos(num2) * (180.0 / Math.PI) / 15.0;
			double num4 = (Math.Acos(d) * (180.0 / Math.PI) / 15.0 - num3) / 24.0;
			TRise_hrs = UT_of_LocalTransit - 0.99726956633 * (num3 + num4 * TRise_hrs - num * num3);
			TRise_hrs = UT_of_LocalTransit - 0.99726956633 * (num3 + num4 * TRise_hrs - num * (TRise_hrs - UT_of_LocalTransit));
			if (TRise_hrs < 0.0)
			{
				TRise_hrs += 24.0;
				RiseInPreviousUTDay = true;
			}
			TSet_hrs = UT_of_LocalTransit + 0.99726956633 * (num3 + num4 * TSet_hrs + num * num3);
			TSet_hrs = UT_of_LocalTransit + 0.99726956633 * (num3 + num4 * TSet_hrs + num * (TSet_hrs - UT_of_LocalTransit));
			if (TSet_hrs > 24.0)
			{
				TSet_hrs -= 24.0;
				SetInFollowingUTDay = true;
			}
		}

		public static bool ObjectUp_SunBelowCivil(double JD, double UT_hrs, double RA, double Dec, double Longitude, double Latitude, bool RequireSunDown)
		{
			return ObjectUp_SunBelowCivil(JD, UT_hrs, RA, Dec, Longitude, Latitude, RequireSunDown, -12.0);
		}

		public static bool ObjectUp_SunBelowCivil(double JD, double UT_hrs, double RA, double Dec, double Longitude, double Latitude, bool RequireSunDown, double RiseSet_Altitude)
		{
			double TRise_hrs = 0.0;
			double TSet_hrs = 0.0;
			double TRise_hrs2 = 24.0;
			double TSet_hrs2 = 0.0;
			double RA2 = 0.0;
			double Dec2 = 0.0;
			double Distance = 0.0;
			double Longitude2 = 0.0;
			double Latitude2 = 0.0;
			double PlanetHeliocentricDistance = 0.0;
			bool flag = false;
			bool flag2 = false;
			RiseSet_LocalTime(RiseSet_Altitude, RA, Dec, JD, Longitude * (180.0 / Math.PI), Latitude * (180.0 / Math.PI), 0f, out TRise_hrs, out TSet_hrs);
			if (TSet_hrs < TRise_hrs)
			{
				TSet_hrs += 24.0;
			}
			double num;
			for (num = UT_hrs - TRise_hrs; num < 0.0; num += 24.0)
			{
			}
			if (num < TSet_hrs - TRise_hrs)
			{
				flag = true;
			}
			if (flag)
			{
				if (RequireSunDown)
				{
					flag2 = false;
					QuickPlanet(JD, 3, EquinoxOfDate: true, out RA2, out Dec2, out Distance, out Longitude2, out Latitude2, out PlanetHeliocentricDistance);
					RiseSet_LocalTime(-6.0, RA2, Dec2, JD, Longitude * (180.0 / Math.PI), Latitude * (180.0 / Math.PI), 0f, out TRise_hrs2, out TSet_hrs2);
					if (TRise_hrs2 < TSet_hrs2)
					{
						TRise_hrs2 += 24.0;
					}
					double num2;
					for (num2 = UT_hrs - TSet_hrs2; num2 < 0.0; num2 += 24.0)
					{
					}
					if (num2 < TRise_hrs2 - TSet_hrs2)
					{
						flag2 = true;
					}
				}
				else
				{
					flag2 = true;
				}
			}
			return flag && flag2;
		}

		public static void PlanetsRiseSetTime(double JD, double Longitude_deg, double Latitude_deg, float TimeZone_hrs, ref double[] TRiseSet_hrs)
		{
			double RA = 0.0;
			double Dec = 0.0;
			double GeocentricDist = 0.0;
			double TRise_hrs = 0.0;
			double TSet_hrs = 0.0;
			double num = 0.0;
			double num2 = 0.0;
			JD = Math.Floor(JD - 0.5) + 0.5;
			for (int i = 1; i < 9; i++)
			{
				double altitude_deg = -0.5667;
				if (i == 3)
				{
					altitude_deg = -0.8333;
				}
				PlanetGeocentric(JD + 1.0, i, 1E-06, 0, out var RA2, out var Dec2, out GeocentricDist);
				PlanetGeocentric(JD, i, 1E-06, 0, out RA, out Dec, out GeocentricDist);
				num = RA2 - RA;
				num2 = Dec2 - Dec;
				if (num < Math.PI)
				{
					num += Math.PI * 2.0;
				}
				if (num > Math.PI)
				{
					num -= Math.PI * 2.0;
				}
				RiseSet_LocalTime(altitude_deg, RA, num, Dec, num2, JD, Longitude_deg, Latitude_deg, TimeZone_hrs, out TRise_hrs, out TSet_hrs);
				TRiseSet_hrs[2 * i - 2] = TRise_hrs;
				TRiseSet_hrs[2 * i - 1] = TSet_hrs;
			}
		}

		public static double PlanetTransitTime_HrsDT(double JD_at0hrs, int Planet, double Longitude_Deg, double TimeZone_hrs)
		{
			double num = 12.0;
			double num2 = 0.0;
			num2 = SiderealTime_deg(JD_at0hrs, Apparent: true);
			for (int i = 0; i <= 2; i++)
			{
				PlanetGeocentric(JD_at0hrs + num / 24.0, Planet, 1E-06, 0, out var RA, out var _, out var _);
				double num3;
				for (num3 = (RA * (180.0 / Math.PI) - num2) / 15.0; num3 >= 24.0; num3 -= 24.0)
				{
				}
				for (; num3 < 0.0; num3 += 24.0)
				{
				}
				num = 0.99726956633 * num3 - 0.99726956633 * Longitude_Deg / 15.0;
			}
			return num + TimeZone_hrs;
		}

		public static void PoleOrientation(double RA_Pole_deg, double Dec_Pole_deg, double RA, double Dec, out double Planetocentric_Latitude_deg, out double PAPole_deg)
		{
			double num = RA_Pole_deg / (180.0 / Math.PI);
			double num2 = Dec_Pole_deg / (180.0 / Math.PI);
			double num3 = Math.Cos(num2) * Math.Sin(num - RA);
			double num4 = Math.Sin(num2) * Math.Cos(Dec) - Math.Cos(num2) * Math.Sin(Dec) * Math.Cos(num - RA);
			double num5 = (0.0 - Math.Sin(num2)) * Math.Sin(Dec) - Math.Cos(num2) * Math.Cos(Dec) * Math.Cos(num - RA);
			Planetocentric_Latitude_deg = Math.Atan(num5 / Math.Sqrt(num3 * num3 + num4 * num4)) * (180.0 / Math.PI);
			PAPole_deg = Math.Atan2(num3, num4) * (180.0 / Math.PI);
			if (PAPole_deg < 0.0)
			{
				PAPole_deg += 360.0;
			}
		}

		public static void PoleOrientationEcliptic(double InclinationOnEcliptic_deg, double Node_deg, double RA_Asteroid, double Dec_Asteroid, out double Planetocentric_Latitude_deg, out double PAPole_deg)
		{
			double RA = 0.0;
			double Dec = 0.0;
			double longitude = (Node_deg - 90.0) / (180.0 / Math.PI);
			double latitude = (90.0 - InclinationOnEcliptic_deg) / (180.0 / Math.PI);
			Long_Lat_to_RA_DEC(longitude, latitude, Ecliptic2000_deg_1976 / (180.0 / Math.PI), ref RA, ref Dec);
			double num = Math.Cos(Dec) * Math.Sin(RA - RA_Asteroid);
			double num2 = Math.Sin(Dec) * Math.Cos(Dec_Asteroid) - Math.Cos(Dec) * Math.Sin(Dec_Asteroid) * Math.Cos(RA - RA_Asteroid);
			double num3 = (0.0 - Math.Sin(Dec)) * Math.Sin(Dec_Asteroid) - Math.Cos(Dec) * Math.Cos(Dec_Asteroid) * Math.Cos(RA - RA_Asteroid);
			Planetocentric_Latitude_deg = Math.Atan(num3 / Math.Sqrt(num * num + num2 * num2)) * (180.0 / Math.PI);
			PAPole_deg = Math.Atan2(num, num2) * (180.0 / Math.PI);
			if (PAPole_deg < 0.0)
			{
				PAPole_deg += 360.0;
			}
		}

		public static bool SaturnRingsSunlit(double JD_TT, out double B, out double Bprime)
		{
			double RA = 0.0;
			double Dec = 0.0;
			bool result = true;
			double num = (JD_TT - 2451545.0) / 36525.0;
			double rA_Pole_deg = 40.66 - 0.036 * num;
			double dec_Pole_deg = 83.52 - 0.004 * num;
			PlanetGeocentric(JD_TT, 6, 1E-06, 2, out var RA2, out var Dec2, out var GeocentricDist, out var _, out var _);
			PoleOrientation(rA_Pole_deg, dec_Pole_deg, RA2, Dec2, out B, out var _);
			HeliocentricLatLong(JD_TT, 6, 1E-06, EquinoxOfDate: false, out var L, out var B2, out GeocentricDist);
			Long_Lat_to_RA_DEC(L, B2, Math.Atan(sinEcliptic2000[EclipticID] / cosEcliptic2000[EclipticID]), ref RA, ref Dec);
			PoleOrientation(rA_Pole_deg, dec_Pole_deg, RA, Dec, out Bprime, out var _);
			if (Math.Sign(B) != Math.Sign(Bprime))
			{
				result = false;
			}
			return result;
		}

		public static void Planet_LunarEclipseElongations(double JD_TT, double Partial_Semiduration, double UmbraRadius_Pi_deg, out string[] Elongations)
		{
			double PhaseAngle_deg = 0.0;
			double Elongation = 0.0;
			double illumination = 0.0;
			double Magnitude = 0.0;
			double Diameter_arcsec = 0.0;
			double PAlimb_deg = 0.0;
			double Planetocentric_Latitude_deg = 0.0;
			double PAPole_deg = 0.0;
			bool flag = false;
			Elongations = new string[9];
			for (int i = 0; i < 9; i++)
			{
				Elongations[i] = "";
			}
			double num = delta_T(JD_TT) / 3600.0 / 24.0;
			GeocentricMoon(JD_TT - num - Partial_Semiduration, out var RA, out var Dec, out var PiMoon, out var l, out var b, out var C);
			PrecessToJ2000(JD_TT, use2006values_Not1976: false, ref RA, ref Dec);
			GeocentricMoon(JD_TT - num + Partial_Semiduration, out var RA2, out var Dec2, out PiMoon, out l, out b, out C);
			PrecessToJ2000(JD_TT, use2006values_Not1976: false, ref RA2, ref Dec2);
			GeocentricMoon(JD_TT - num, out var RA3, out var Dec3, out PiMoon, out l, out b, out C);
			PrecessToJ2000(JD_TT, use2006values_Not1976: false, ref RA3, ref Dec3);
			for (int j = 4; j < 9; j++)
			{
				PlanetGeocentric(JD_TT, j, 1E-05, 2, physicalFlag: true, out var _, out var _, out var _, out var RA4, out var Dec4, out var _, out Diameter_arcsec, out Magnitude, out Elongation, out var EW, out PhaseAngle_deg, out illumination, out PAlimb_deg, out Planetocentric_Latitude_deg, out PAPole_deg, out var _);
				Elongation *= 180.0 / Math.PI;
				Elongation = 180.0 - Elongation;
				EW = ((!(EW.ToUpper() == "E")) ? "E" : "W");
				if (!(Elongation < 3.0))
				{
					continue;
				}
				Utilities.Distance(RA3, Dec3, RA4, Dec4, out var Distance, out var PA_atOrigin);
				Utilities.Distance(RA, Dec, RA4, Dec4, out var Distance2, out PA_atOrigin);
				Utilities.Distance(RA2, Dec2, RA4, Dec4, out var Distance3, out PA_atOrigin);
				Distance *= 180.0 / Math.PI;
				Distance2 *= 180.0 / Math.PI;
				Distance3 *= 180.0 / Math.PI;
				double num2 = PiMoon * (180.0 / Math.PI) * 1.28;
				flag = (Distance2 < num2 || Distance3 < num2 || Distance < num2) && Partial_Semiduration > 0.0;
				if (Elongation < UmbraRadius_Pi_deg)
				{
					if (flag)
					{
						Elongations[j] = string.Format("{0,1:f0}' ", Elongation * 60.0) + EW.ToUpper() + " ‡";
					}
					else
					{
						Elongations[j] = string.Format("{0,1:f1}° ", Elongation) + EW.ToUpper() + " †";
					}
				}
				else if (Elongation < 3.0)
				{
					Elongations[j] = string.Format("{0,1:f1}° ", Elongation) + EW.ToUpper();
				}
			}
		}

		public static void Planet_SolarElongations(double JD_TT, out string[] Elongations)
		{
			double PhaseAngle_deg = 0.0;
			double Elongation = 0.0;
			double illumination = 0.0;
			double Magnitude = 0.0;
			double Diameter_arcsec = 0.0;
			double PAlimb_deg = 0.0;
			double Planetocentric_Latitude_deg = 0.0;
			double PAPole_deg = 0.0;
			Elongations = new string[9];
			for (int i = 0; i < 9; i++)
			{
				Elongations[i] = "";
			}
			PlanetGeocentric(JD_TT, 0, 1E-05, 0, physicalFlag: true, out var x, out var y, out var z, out var _, out var _, out var _, out var Diameter_arcsec2, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var Planetocentric_SunLat_deg);
			Diameter_arcsec2 /= 3600.0;
			for (int j = 1; j < 9; j++)
			{
				if (j == 3)
				{
					continue;
				}
				PlanetGeocentric(JD_TT, j, 1E-05, 0, physicalFlag: true, out x, out y, out z, out var _, out var _, out var TrueDistance2, out Diameter_arcsec, out Magnitude, out Elongation, out var EW2, out PhaseAngle_deg, out illumination, out PAlimb_deg, out Planetocentric_Latitude_deg, out PAPole_deg, out Planetocentric_SunLat_deg);
				Elongation *= 180.0 / Math.PI;
				if (Elongation < Diameter_arcsec2 + (Diameter_arcsec / 2.0 + 8.8 / TrueDistance2) / 3600.0)
				{
					if (TrueDistance2 < 1.0)
					{
						Elongations[j] = string.Format("{0,1:f1}' ", Elongation * 60.0) + EW2.ToUpper() + " §";
					}
					else
					{
						Elongations[j] = string.Format("{0,1:f1}' ", Elongation * 60.0) + EW2.ToUpper() + " †";
					}
				}
				else if (Elongation < 1.0)
				{
					Elongations[j] = string.Format("{0,1:f0}' ", Elongation * 60.0) + EW2.ToUpper() + " ‡";
				}
				else if (Elongation < 2.0)
				{
					Elongations[j] = string.Format("{0,1:f1}° ", Elongation) + EW2.ToUpper();
				}
				else
				{
					Elongations[j] = string.Format("{0,1:f0}° ", Elongation) + EW2.ToUpper();
				}
			}
		}

		public static void PlanetGeocentric(double JD_TT, int PlanetNo, double Accuracy, int EquinoxFlag_0apparent_1mean_2J2000_4helioLatLong, out double RA, out double Dec, out double GeocentricDist, out double Magnitude, out double Diameter)
		{
			if (PlanetNo == 3)
			{
				PlanetNo = 0;
			}
			PlanetGeocentric(JD_TT, PlanetNo, Accuracy, EquinoxFlag_0apparent_1mean_2J2000_4helioLatLong, physicalFlag: true, out var _, out var _, out var _, out RA, out Dec, out GeocentricDist, out Diameter, out Magnitude, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _);
		}

		public static void PlanetGeocentric(double JD_TT, int PlanetNo, double Accuracy, int EquinoxFlag_0apparent_1mean_2J2000_4helioLatLong, out double RA, out double Dec, out double GeocentricDist)
		{
			if (PlanetNo == 3)
			{
				PlanetNo = 0;
			}
			PlanetGeocentric(JD_TT, PlanetNo, Accuracy, EquinoxFlag_0apparent_1mean_2J2000_4helioLatLong, physicalFlag: false, out var _, out var _, out var _, out RA, out Dec, out GeocentricDist, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _);
		}

		public static void PlanetGeocentric(double JD_TT, int PlanetNo, out double geocentricDist, out double Magnitude, out double Diameter_arcsec, out double PhaseAngle_deg, out double illumination, out double PAlimb_deg, out double Planetocentric_Latitude_deg, out double PAPole_deg)
		{
			if (PlanetNo == 3)
			{
				PlanetNo = 0;
			}
			PlanetGeocentric(JD_TT, PlanetNo, 1E-05, 2, physicalFlag: true, out var _, out var _, out var _, out var _, out var _, out geocentricDist, out Diameter_arcsec, out Magnitude, out var _, out var _, out PhaseAngle_deg, out illumination, out PAlimb_deg, out Planetocentric_Latitude_deg, out PAPole_deg, out var _);
		}

		public static void PlanetGeocentric(double JD_TT, int PlanetNo, double Accuracy, int EquinoxFlag_0apparent_1mean_2J2000_4helioLatLong, bool physicalFlag, out double x2000, out double y2000, out double z2000, out double RA, out double Dec, out double TrueDistance, out double Diameter_arcsec, out double Magnitude, out double Elongation, out string EW, out double PhaseAngle_deg, out double illumination, out double PAlimb_deg, out double Planetocentric_Latitude_deg, out double PAPole_deg, out double Planetocentric_SunLat_deg)
		{
			double GeocentricDistance = 0.0;
			if (PlanetNo == 3)
			{
				PlanetNo = 0;
			}
			PlanetGeocentric(JD_TT, PlanetNo, Accuracy, EquinoxFlag_0apparent_1mean_2J2000_4helioLatLong, physicalFlag, out x2000, out y2000, out z2000, out RA, out Dec, out GeocentricDistance, out TrueDistance, out Diameter_arcsec, out Magnitude, out Elongation, out EW, out PhaseAngle_deg, out illumination, out PAlimb_deg, out Planetocentric_Latitude_deg, out PAPole_deg, out Planetocentric_SunLat_deg);
		}

		public static void PlanetGeocentric(double JD_TT, int PlanetNo, double Accuracy, int EquinoxFlag_0apparent_1mean_2J2000_4helioLatLong, bool physicalFlag, out double x2000, out double y2000, out double z2000, out double RA, out double Dec, out double GeocentricDistance, out double TrueDistance, out double Diameter_arcsec, out double Magnitude, out double Elongation, out string EW, out double PhaseAngle_deg, out double illumination, out double PAlimb_deg, out double Planetocentric_Latitude_deg, out double PAPole_deg, out double Planetocentric_SunLat_deg)
		{
			double num = 1.0;
			double num2 = 0.0;
			double RA2 = 0.0;
			double Dec2 = 0.0;
			double num3 = 0.0;
			double num4 = 0.0;
			double dRA = 0.0;
			double dDec = 0.0;
			double x2001;
			double y2001;
			double z2001;
			double RA3;
			double Dec3;
			double Longitude_arcsec;
			double Obliquity_arcsec;
			double TrueEcliptic;
			double num5;
			double X;
			double Y;
			double Z;
			RA = (Dec = (x2001 = (y2001 = (z2001 = (X = (Y = (Z = (RA3 = (Dec3 = (Longitude_arcsec = (Obliquity_arcsec = (num5 = (TrueEcliptic = (x2000 = (y2000 = (z2000 = 0.0))))))))))))))));
			double PlanetRadiusVector = (Magnitude = (PAlimb_deg = (Elongation = (illumination = (Diameter_arcsec = (PhaseAngle_deg = (Planetocentric_Latitude_deg = (PAPole_deg = (Planetocentric_SunLat_deg = 0.0)))))))));
			EW = "";
			TrueDistance = 1.0;
			if ((EquinoxFlag_0apparent_1mean_2J2000_4helioLatLong & 4) == 4)
			{
				physicalFlag = false;
			}
			double num6 = (JD_TT - 2451545.0) / 36525.0;
			num5 = MeanEclipticOfDate(JD_TT, Use1976Value: true);
			double PlanetRadiusVector2;
			double Elongation_deg;
			int EphemVersion2;
			if (JPL_DE.DE_EphemerisAvailable)
			{
				JPL_DE.Planet_PositionFromEarth(JD_TT, PlanetNo, out x2000, out y2000, out z2000, out PlanetRadiusVector2, out GeocentricDistance, out TrueDistance, out var _);
				GeocentricDistance = Math.Sqrt(x2000 * x2000 + y2000 * y2000 + z2000 * z2000);
				XYZ_to_RA_Dec(x2000, y2000, z2000, out RA, out Dec);
				if (physicalFlag && PlanetNo != 3)
				{
					JPL_DE.Planet_PositionFromEarth(JD_TT, 0, out x2001, out y2001, out z2001, out PlanetRadiusVector, out var _, out Elongation_deg, out EphemVersion2);
					num = Math.Sqrt(x2001 * x2001 + y2001 * y2001 + z2001 * z2001);
					X = x2000 - x2001;
					Y = y2000 - y2001;
					Z = z2000 - z2001;
					XYZ_to_RA_Dec(X, Y, Z, out RA2, out Dec2);
				}
				if (EquinoxFlag_0apparent_1mean_2J2000_4helioLatLong == 0)
				{
					JPL_DE.DE_AberrationCoefficients(JD_TT, out var dX, out var dY, out var dZ);
					num3 = 0.00577551833 * ((0.0 - dX) * Math.Sin(RA) + dY * Math.Cos(RA)) / Math.Cos(Dec);
					num4 = 0.00577551833 * ((0.0 - dX) * Math.Cos(RA) * Math.Sin(Dec) - dY * Math.Sin(RA) * Math.Sin(Dec) + dZ * Math.Cos(Dec));
					Relativistic_Correction(JD_TT, RA, Dec, GeocentricDistance, out dRA, out dDec, out Elongation_deg);
				}
			}
			else
			{
				if (PlanetNo != 3)
				{
					PlanetXYZ(JD_TT, PlanetNo, Heliocentric: true, 0.0001, ref X, ref Y, ref Z, out EphemVersion2);
				}
				if ((EquinoxFlag_0apparent_1mean_2J2000_4helioLatLong & 4) != 4 || PlanetNo == 3)
				{
					PlanetXYZ(JD_TT, 3, Heliocentric: true, 0.0001, ref x2001, ref y2001, ref z2001, out EphemVersion2);
				}
				x2000 = X - x2001;
				y2000 = Y - y2001;
				z2000 = Z - z2001;
				GeocentricDistance = Math.Sqrt(x2000 * x2000 + y2000 * y2000 + z2000 * z2000);
				for (int i = 0; i < 3; i++)
				{
					double num7 = 0.00577551833 * GeocentricDistance;
					if ((EquinoxFlag_0apparent_1mean_2J2000_4helioLatLong & 4) == 4 && PlanetNo != 3)
					{
						x2001 = (y2001 = (z2001 = 0.0));
					}
					else if (EquinoxFlag_0apparent_1mean_2J2000_4helioLatLong == 0)
					{
						PlanetXYZ(JD_TT - num7, 3, Heliocentric: true, Accuracy, ref x2001, ref y2001, ref z2001, out EphemVersion2);
					}
					else
					{
						PlanetXYZ(JD_TT, 3, Heliocentric: true, Accuracy, ref x2001, ref y2001, ref z2001, out EphemVersion2);
					}
					if (PlanetNo != 3)
					{
						PlanetXYZ(JD_TT - num7, PlanetNo, Heliocentric: true, Accuracy, ref X, ref Y, ref Z, out EphemVersion2);
					}
					x2000 = X - x2001;
					y2000 = Y - y2001;
					z2000 = Z - z2001;
					GeocentricDistance = Math.Sqrt(x2000 * x2000 + y2000 * y2000 + z2000 * z2000);
				}
				XYZ_to_RA_Dec(x2000, y2000, z2000, out RA, out Dec);
				PlanetRadiusVector2 = Math.Sqrt(X * X + Y * Y + Z * Z);
				num = Math.Sqrt(x2001 * x2001 + y2001 * y2001 + z2001 * z2001);
				if (EquinoxFlag_0apparent_1mean_2J2000_4helioLatLong == 0)
				{
					Aberration_CD(JD_TT, out var C, out var D);
					num3 = (C * Math.Cos(RA) + D * Math.Sin(RA)) / Math.Cos(Dec);
					num4 = C * (Math.Tan(num5) * Math.Cos(Dec) - Math.Sin(RA) * Math.Sin(Dec)) + D * Math.Cos(RA) * Math.Sin(Dec);
				}
			}
			RA += num3 + dRA;
			Dec += num4 + dDec;
			num2 = 0.0;
			if ((EquinoxFlag_0apparent_1mean_2J2000_4helioLatLong & 2) == 0)
			{
				RA -= 2.617993877991494E-07;
				Precession(2000, JD_TT, use2006values_Not1976: false, ref RA, ref Dec, 0.0, 0.0);
				Nutation(JD_TT, out Longitude_arcsec, out Obliquity_arcsec, out TrueEcliptic);
				RA += (Longitude_arcsec * (Math.Cos(num5) + Math.Sin(num5) * Math.Sin(RA) * Math.Tan(Dec)) - Obliquity_arcsec * Math.Cos(RA) * Math.Tan(Dec)) / 3600.0 / (180.0 / Math.PI);
				Dec += (Longitude_arcsec * Math.Sin(num5) * Math.Cos(RA) + Obliquity_arcsec * Math.Sin(RA)) / 3600.0 / (180.0 / Math.PI);
				PrecessFromJ2000(JD_TT, use2006values_Not1976: false, ref RA2, ref Dec2);
				num2 = 1.0;
			}
			double num8;
			double num9;
			double num10;
			if ((EquinoxFlag_0apparent_1mean_2J2000_4helioLatLong & 4) == 4)
			{
				num8 = Math.Cos(Dec) * Math.Cos(RA);
				num9 = Math.Cos(Dec) * Math.Sin(RA) * Math.Cos(num5) + Math.Sin(Dec) * Math.Sin(num5);
				num10 = (0.0 - Math.Cos(Dec)) * Math.Sin(RA) * Math.Sin(num5) + Math.Sin(Dec) * Math.Cos(num5);
				RA = Math.Atan2(num9, num8);
				if (RA < 0.0)
				{
					RA += Math.PI * 2.0;
				}
				Dec = Math.Atan(num10 / Math.Sqrt(num8 * num8 + num9 * num9));
			}
			switch (PlanetNo)
			{
			case 1:
				Diameter_arcsec = 6.68 / GeocentricDistance;
				break;
			case 2:
				Diameter_arcsec = 16.82 / GeocentricDistance;
				break;
			case 0:
				if (!physicalFlag)
				{
					Diameter_arcsec = 1919.26 / GeocentricDistance;
				}
				else
				{
					Diameter_arcsec = 959.63 / GeocentricDistance;
				}
				break;
			case 4:
				Diameter_arcsec = 9.36 / GeocentricDistance;
				break;
			case 5:
				Diameter_arcsec = 196.94 / GeocentricDistance;
				break;
			case 6:
				Diameter_arcsec = 166.66 / GeocentricDistance;
				break;
			case 7:
				Diameter_arcsec = 68.56 / GeocentricDistance;
				break;
			case 8:
				Diameter_arcsec = 73.12 / GeocentricDistance;
				break;
			case 9:
				Diameter_arcsec = 6.8 / GeocentricDistance;
				break;
			}
			if (!(physicalFlag && PlanetNo != 0))
			{
				return;
			}
			XYZ_to_RA_Dec(x2001, y2001, z2001, out RA3, out Dec3);
			if ((EquinoxFlag_0apparent_1mean_2J2000_4helioLatLong & 2) == 0)
			{
				Precession(2000, JD_TT, use2006values_Not1976: false, ref RA3, ref Dec3, 0.0, 0.0);
				if (EquinoxFlag_0apparent_1mean_2J2000_4helioLatLong == 0)
				{
					RA3 += (Longitude_arcsec * (Math.Cos(num5) + Math.Sin(num5) * Math.Sin(RA3) * Math.Tan(Dec3)) - Obliquity_arcsec * Math.Cos(RA3) * Math.Tan(Dec3)) / 3600.0 / (180.0 / Math.PI);
					Dec3 += (Longitude_arcsec * Math.Sin(num5) * Math.Cos(RA3) + Obliquity_arcsec * Math.Sin(RA3)) / 3600.0 / (180.0 / Math.PI);
				}
			}
			num8 = Math.Cos(Dec3) * Math.Sin(RA3 - RA);
			num9 = Math.Sin(Dec3) * Math.Cos(Dec) - Math.Cos(Dec3) * Math.Sin(Dec) * Math.Cos(RA3 - RA);
			num10 = Math.Sin(Dec3) * Math.Sin(Dec) + Math.Cos(Dec3) * Math.Cos(Dec) * Math.Cos(RA3 - RA);
			PAlimb_deg = Math.Atan2(num8, num9) * (180.0 / Math.PI);
			if (PAlimb_deg < 0.0)
			{
				PAlimb_deg += 360.0;
			}
			Elongation = Math.Atan(Math.Sqrt(num8 * num8 + num9 * num9) / num10);
			if (Elongation < 0.0)
			{
				Elongation += Math.PI;
			}
			double num11 = RA - RA3;
			if (num11 > Math.PI)
			{
				num11 -= Math.PI * 2.0;
			}
			if (num11 < -Math.PI)
			{
				num11 += Math.PI * 2.0;
			}
			EW = "w";
			if (num11 >= 0.0)
			{
				EW = "e";
			}
			PhaseAngle_deg = Math.Atan(Math.Sin(Elongation) / (GeocentricDistance / num - Math.Cos(Elongation)));
			if (PhaseAngle_deg < 0.0)
			{
				PhaseAngle_deg += Math.PI;
			}
			illumination = 0.5 * (1.0 + Math.Cos(PhaseAngle_deg));
			PhaseAngle_deg *= 180.0 / Math.PI;
			double num12 = PhaseAngle_deg / 100.0;
			XYZ_to_RA_Dec(x2001, y2001, z2001, out RA3, out Dec3);
			Magnitude = 5.0 * Math.Log10(PlanetRadiusVector2 * GeocentricDistance);
			switch (PlanetNo)
			{
			case 1:
			{
				Magnitude += -0.6 + 4.98 * num12 - 4.88 * num12 * num12 + 3.02 * num12 * num12 * num12;
				double rA_Pole_deg8 = 281.01 + (0.2768 * num2 - 0.033) * num6;
				double dec_Pole_deg = 61.414 + (0.1064 * num2 - 0.005) * num6;
				PoleOrientation(rA_Pole_deg8, dec_Pole_deg, RA, Dec, out Planetocentric_Latitude_deg, out PAPole_deg);
				PoleOrientation(rA_Pole_deg8, dec_Pole_deg, RA2, Dec2, out Planetocentric_SunLat_deg, out Elongation_deg);
				break;
			}
			case 2:
			{
				if (num12 < 1.636)
				{
					Magnitude += -4.47 + 1.03 * num12 + 0.57 * num12 * num12 + 0.13 * num12 * num12 * num12;
				}
				else
				{
					Magnitude += 0.98 - 1.02 * num12;
				}
				double rA_Pole_deg7 = 272.76 - 0.0423 * num2 * num6;
				double dec_Pole_deg = 67.16 + 0.027 * num2 * num6;
				PoleOrientation(rA_Pole_deg7, dec_Pole_deg, RA, Dec, out Planetocentric_Latitude_deg, out PAPole_deg);
				PoleOrientation(rA_Pole_deg7, dec_Pole_deg, RA2, Dec2, out Planetocentric_SunLat_deg, out Elongation_deg);
				break;
			}
			case 0:
				Magnitude = -26.0;
				Planetocentric_Latitude_deg = 0.0;
				PAPole_deg = 0.0;
				break;
			case 4:
			{
				Magnitude += -1.52 + 0.016 * PhaseAngle_deg;
				double rA_Pole_deg6 = 317.681 + (0.7859 * num2 - 0.106) * num6;
				double dec_Pole_deg = 52.887 + (0.4117 * num2 - 0.061) * num6;
				PoleOrientation(rA_Pole_deg6, dec_Pole_deg, RA, Dec, out Planetocentric_Latitude_deg, out PAPole_deg);
				PoleOrientation(rA_Pole_deg6, dec_Pole_deg, RA2, Dec2, out Planetocentric_SunLat_deg, out Elongation_deg);
				break;
			}
			case 5:
			{
				Magnitude += -9.25 + 0.005 * PhaseAngle_deg;
				double rA_Pole_deg5 = 268.057 + (0.1152 * num2 - 0.006) * num6;
				double dec_Pole_deg = 64.495 + (-0.0189 * num2 + 0.002) * num6;
				PoleOrientation(rA_Pole_deg5, dec_Pole_deg, RA, Dec, out Planetocentric_Latitude_deg, out PAPole_deg);
				PoleOrientation(rA_Pole_deg5, dec_Pole_deg, RA2, Dec2, out Planetocentric_SunLat_deg, out Elongation_deg);
				break;
			}
			case 6:
			{
				double rA_Pole_deg4 = 40.589 + (4.47549 * num2 - 0.036) * num6;
				double dec_Pole_deg = 83.537 + (0.4223 * num2 - 0.004) * num6;
				PoleOrientation(rA_Pole_deg4, dec_Pole_deg, RA, Dec, out Planetocentric_Latitude_deg, out PAPole_deg);
				double num13 = Math.Sin(Planetocentric_Latitude_deg / (180.0 / Math.PI));
				Magnitude += -8.88 + 0.044 * PhaseAngle_deg - 2.6 * Math.Abs(num13) + 1.25 * num13 * num13;
				PoleOrientation(rA_Pole_deg4, dec_Pole_deg, RA2, Dec2, out Planetocentric_SunLat_deg, out Elongation_deg);
				break;
			}
			case 7:
			{
				Magnitude += -7.19 + 0.0028 * PhaseAngle_deg;
				double rA_Pole_deg3 = 257.311 + 1.4279 * num2 * num6;
				double dec_Pole_deg = -15.175 - 0.1212 * num2 * num6;
				PoleOrientation(rA_Pole_deg3, dec_Pole_deg, RA, Dec, out Planetocentric_Latitude_deg, out PAPole_deg);
				PoleOrientation(rA_Pole_deg3, dec_Pole_deg, RA2, Dec2, out Planetocentric_SunLat_deg, out Elongation_deg);
				break;
			}
			case 8:
			{
				Magnitude += -6.87;
				double rA_Pole_deg2 = 299.36 + 0.8492 * num2 * num6 + 0.7 * Math.Sin((357.85 + 52.316 * num6) / (180.0 / Math.PI));
				double dec_Pole_deg = 43.46 + 0.2382 * num2 * num6 - 0.51 * Math.Cos((357.85 + 52.316 * num6) / (180.0 / Math.PI));
				PoleOrientation(rA_Pole_deg2, dec_Pole_deg, RA, Dec, out Planetocentric_Latitude_deg, out PAPole_deg);
				PoleOrientation(rA_Pole_deg2, dec_Pole_deg, RA2, Dec2, out Planetocentric_SunLat_deg, out Elongation_deg);
				break;
			}
			case 9:
			{
				Magnitude += -1.01 + 0.041 * PhaseAngle_deg;
				double rA_Pole_deg = 132.99 + 1.2508 * num2 * num6;
				double dec_Pole_deg = -6.16 + 0.3698 * num2 * num6;
				PoleOrientation(rA_Pole_deg, dec_Pole_deg, RA, Dec, out Planetocentric_Latitude_deg, out PAPole_deg);
				PoleOrientation(rA_Pole_deg, dec_Pole_deg, RA2, Dec2, out Planetocentric_SunLat_deg, out Elongation_deg);
				break;
			}
			case 3:
				break;
			}
		}

		private static double Asinh(double x)
		{
			return Math.Log(x + Math.Sqrt(x * x + 1.0));
		}

		private static double CUBE_ROOT(double X)
		{
			double num = 1.0;
			if (X < 0.0)
			{
				num = -1.0;
			}
			return num * Math.Exp(Math.Log(Math.Abs(X)) / 3.0);
		}

		public static void Kepler(double MeanAnomaly, double ecc, out double TrueAnomaly, out double EccentricAnomaly)
		{
			int num = 0;
			bool flag = false;
			TrueAnomaly = (EccentricAnomaly = 0.0);
			if (ecc < 1.0)
			{
				while (MeanAnomaly > Math.PI)
				{
					MeanAnomaly -= Math.PI * 2.0;
				}
				while (MeanAnomaly < -Math.PI)
				{
					MeanAnomaly += Math.PI * 2.0;
				}
			}
			if (MeanAnomaly == 0.0)
			{
				return;
			}
			if (ecc == 1.0)
			{
				double num2 = 3.0 * MeanAnomaly / Math.Sqrt(8.0);
				double num3 = Math.Sqrt(num2 * num2 + 1.0);
				double value = num2 + num3;
				double value2 = num2 - num3;
				double d = (double)Math.Sign(value) * (Math.Pow(Math.Abs(value), 1.0 / 3.0) + (double)Math.Sign(value2) * Math.Pow(Math.Abs(value2), 1.0 / 3.0));
				TrueAnomaly = 2.0 * Math.Atan(d);
				EccentricAnomaly = Math.Tan(TrueAnomaly / 2.0);
				return;
			}
			if (ecc < 0.3)
			{
				double num4 = Math.Atan2(Math.Sin(MeanAnomaly), Math.Cos(MeanAnomaly) - ecc);
				double num5;
				do
				{
					num5 = num4 - ecc * Math.Sin(num4) - MeanAnomaly;
					num4 -= num5 / (1.0 - ecc * Math.Cos(num4));
				}
				while (Math.Abs(num5) > 1E-10);
				EccentricAnomaly = num4;
			}
			else
			{
				if (MeanAnomaly < 0.0)
				{
					MeanAnomaly = 0.0 - MeanAnomaly;
					flag = true;
				}
				double num4 = MeanAnomaly;
				double num6 = 1E-10 * Math.Abs(1.0 - ecc);
				if ((ecc > 0.8 && MeanAnomaly < Math.PI / 3.0) || ecc > 1.0)
				{
					double num7 = MeanAnomaly / Math.Abs(1.0 - ecc);
					if (num7 * num7 > 6.0 * Math.Abs(1.0 - ecc))
					{
						num7 = ((!(MeanAnomaly < Math.PI)) ? Asinh(MeanAnomaly / ecc) : CUBE_ROOT(6.0 * MeanAnomaly));
					}
					num4 = num7;
				}
				if (ecc < 1.0)
				{
					double num5 = num4 - ecc * Math.Sin(num4) - MeanAnomaly;
					while (Math.Abs(num5) > num6)
					{
						num++;
						num4 -= num5 / (1.0 - ecc * Math.Cos(num4));
						num5 = num4 - ecc * Math.Sin(num4) - MeanAnomaly;
					}
				}
				else
				{
					double num5 = ecc * Math.Sinh(num4) - num4 - MeanAnomaly;
					while (Math.Abs(num5) > num6)
					{
						num++;
						num4 -= num5 / (ecc * Math.Cosh(num4) - 1.0);
						num5 = ecc * Math.Sinh(num4) - num4 - MeanAnomaly;
					}
				}
				EccentricAnomaly = num4;
			}
			if (ecc < 1.0)
			{
				TrueAnomaly = 2.0 * Math.Atan(Math.Tan(EccentricAnomaly / 2.0) * Math.Sqrt((1.0 + ecc) / (1.0 - ecc)));
			}
			else if (ecc > 1.0)
			{
				TrueAnomaly = 2.0 * Math.Atan(Math.Tanh(EccentricAnomaly / 2.0) * Math.Sqrt((ecc + 1.0) / (ecc - 1.0)));
			}
			if (flag)
			{
				EccentricAnomaly = 0.0 - EccentricAnomaly;
				TrueAnomaly = 0.0 - TrueAnomaly;
			}
		}

		public static double OrbitRadius_from_Period(double MainBodyDia_km, double SatelliteDia_km, double Density_g_cc, double Period_Days)
		{
			double num = 39.47841760435743;
			double num2 = 4.1887902047863905 * Math.Pow(MainBodyDia_km * 100000.0 / 2.0, 3.0);
			double num3 = 4.1887902047863905 * Math.Pow(SatelliteDia_km * 100000.0 / 2.0, 3.0);
			double num4 = num2 + num3;
			double num5 = Density_g_cc * num4 / 1000.0;
			return Math.Pow(6.67259E-11 * num5 * Math.Pow(Period_Days * 86400.0, 2.0) / num, 1.0 / 3.0) / 1000.0;
		}

		public static double OrbitPeriod_from_Radius(double MainBodyDia_km, double SatelliteDia_km, double Density_g_cc, double Radius_km)
		{
			double num = 39.47841760435743;
			double num2 = 4.1887902047863905 * Math.Pow(MainBodyDia_km * 100000.0 / 2.0, 3.0);
			double num3 = 4.1887902047863905 * Math.Pow(SatelliteDia_km * 100000.0 / 2.0, 3.0);
			double num4 = num2 + num3;
			double num5 = Density_g_cc * num4 / 1000.0;
			return Math.Pow(Math.Pow(Radius_km * 1000.0, 3.0) * num / num5 / 6.67259E-11, 0.5) / 86400.0;
		}

		public static void PositionfromElements(double JD_TT, double SiteLongitude, double Dxy, double dZ, double PerihelionDate, double OsculatingDate, double MeanAnomaly, double q, double e, double perihelion, double node, double i, double magH0_Const, double MagG_PhaseCoeff, double Mag_CoeffOfR, double Accuracy, out double RA, out double Dec, out double RadiusVector, out double AstrometricGeocentricDistance, out double Magnitude, out double Elongation, out double PhaseAngle)
		{
			PositionfromElements(JD_TT, SiteLongitude, Dxy, dZ, PerihelionDate, OsculatingDate, MeanAnomaly, q, e, perihelion, node, i, magH0_Const, MagG_PhaseCoeff, Mag_CoeffOfR, Accuracy, out RA, out Dec, out RadiusVector, out AstrometricGeocentricDistance, out Magnitude, out Elongation, out PhaseAngle, out var _);
		}

		public static void PositionfromElements(double JD_TT, double SiteLongitude, double Dxy, double dZ, double PerihelionDate, double OsculatingDate, double MeanAnomaly, double q, double e, double perihelion, double node, double i, double magH0_Const, double MagG_PhaseCoeff, double Mag_CoeffOfR, double Accuracy, out double RA, out double Dec, out double RadiusVector, out double AstrometricGeocentricDistance, out double Magnitude, out double Elongation, out double PhaseAngle, out double TrueGeocentricDistance)
		{
			if (OsculatingDate == 0.0)
			{
				OsculatingDate = PerihelionDate;
			}
			if (PerihelionDate == 0.0)
			{
				PerihelionDate = OsculatingDate;
			}
			double num = 0.0;
			double num2;
			double EccentricAnomaly = (AstrometricGeocentricDistance = (RadiusVector = (num2 = 0.0)));
			double y;
			double z;
			double num5;
			double num4;
			double num3;
			double x = (y = (z = (num5 = (num4 = (num3 = EccentricAnomaly)))));
			int EphemVersion;
			for (int j = 0; j < 3; j++)
			{
				double TrueAnomaly;
				if (e == 0.0)
				{
					TrueAnomaly = MeanAnomaly;
				}
				else if (e == 1.0)
				{
					double num6 = (JD_TT - PerihelionDate - num) * Math.Sqrt((1.0 + e) / 2.0);
					Kepler(0.01720209895 * num6 / Math.Pow(q, 1.5), e, out TrueAnomaly, out EccentricAnomaly);
				}
				else
				{
					double num7 = 0.01720209895 * Math.Pow(Math.Abs(q / (1.0 - e)), -1.5);
					Kepler(MeanAnomaly + num7 * (JD_TT - OsculatingDate - num), e, out TrueAnomaly, out EccentricAnomaly);
				}
				RadiusVector = q * (e + 1.0) / (1.0 + e * Math.Cos(TrueAnomaly));
				double num8 = TrueAnomaly + perihelion;
				double num9 = Math.Cos(num8) * Math.Cos(node) - Math.Sin(num8) * Math.Sin(node) * Math.Cos(i);
				double num10 = Math.Cos(num8) * Math.Sin(node) + Math.Sin(num8) * Math.Cos(node) * Math.Cos(i);
				double num11 = Math.Sin(num8) * Math.Sin(i);
				double num12 = Math.Atan2(num10, num9);
				double num13 = Math.Atan(num11 / Math.Sqrt(num9 * num9 + num10 * num10));
				num5 = RadiusVector * Math.Cos(num12) * Math.Cos(num13);
				num4 = RadiusVector * (Math.Cos(num13) * Math.Sin(num12) * cosEcliptic2000[EclipticID] - Math.Sin(num13) * sinEcliptic2000[EclipticID]);
				num3 = RadiusVector * (Math.Cos(num13) * Math.Sin(num12) * sinEcliptic2000[EclipticID] + Math.Sin(num13) * cosEcliptic2000[EclipticID]);
				JPL_DE.Sun_FromEarth_ForHeliocentricOrbits(JD_TT, AstrometricGeocentricDistance, out x, out y, out z, out EphemVersion);
				if (Dxy != 0.0)
				{
					double num14 = SiderealTime_deg(JD_TT, Apparent: false) / (180.0 / Math.PI) + SiteLongitude;
					x -= Dxy * Math.Cos(num14);
					y -= Dxy * Math.Sin(num14);
					z -= dZ;
				}
				num2 = Math.Sqrt(x * x + y * y + z * z);
				AstrometricGeocentricDistance = Math.Sqrt((num5 - x) * (num5 - x) + (num4 - y) * (num4 - y) + (num3 - z) * (num3 - z));
				num = 0.00577551833 * AstrometricGeocentricDistance;
			}
			XYZ_to_RA_Dec(num5 - x, num4 - y, num3 - z, out RA, out Dec);
			Elongation = Math.Acos((num2 * num2 + AstrometricGeocentricDistance * AstrometricGeocentricDistance - RadiusVector * RadiusVector) / 2.0 / num2 / AstrometricGeocentricDistance);
			PhaseAngle = Math.Atan(num2 * Math.Sin(Elongation) / (AstrometricGeocentricDistance - num2 * Math.Cos(Elongation)));
			if (PhaseAngle < 0.0)
			{
				PhaseAngle += Math.PI;
			}
			Magnitude = magH0_Const + (5.0 * Math.Log10(AstrometricGeocentricDistance) + Mag_CoeffOfR * Math.Log10(RadiusVector));
			if (MagG_PhaseCoeff != 0.0)
			{
				Magnitude -= 2.5 * Math.Log10((1.0 - MagG_PhaseCoeff) * Math.Pow(Math.E, -3.33 * Math.Pow(Math.Tan(PhaseAngle / 2.0), 0.63)) + MagG_PhaseCoeff * Math.Pow(Math.E, -1.87 * Math.Pow(Math.Tan(PhaseAngle / 2.0), 1.22)));
			}
			JPL_DE.Sun_FromEarth_ForHeliocentricOrbits(JD_TT - num, AstrometricGeocentricDistance, out var x2, out var y2, out var z2, out EphemVersion);
			TrueGeocentricDistance = Math.Sqrt((num5 - x2) * (num5 - x2) + (num4 - y2) * (num4 - y2) + (num3 - z2) * (num3 - z2));
		}

		public static void QuickLongLatMercuryVenusEarth(double JD_TT, int PlanetNo, out double Longitude, out double Latitude, out double HeliocentricDistance)
		{
			Longitude = (Latitude = (HeliocentricDistance = 0.0));
			double num = JD_TT - 2415020.0;
			double num2 = num / 36525.0;
			double num3 = num * num / 100000000.0;
			switch (PlanetNo)
			{
			case 1:
			{
				double num11 = (102.279381 + 4.0923344364 * num + 5E-07 * num3) / (180.0 / Math.PI);
				double num12 = (47.146 + 1.1856444 * num2) / (180.0 / Math.PI);
				double num13 = Math.Sin((7.0028806 + 0.00186 * num2) / (180.0 / Math.PI));
				double num5 = (84378.0 + 8.0 * num2) * Math.Sin(num11) + (10733.0 + 2.0 * num2) * Math.Sin(2.0 * num11) + 1892.0 * Math.Sin(3.0 * num11) + 381.0 * Math.Sin(4.0 * num11) + 83.0 * Math.Sin(5.0 * num11);
				double num6 = -407564.0 - (87879.0 + 8.0 * num2) * Math.Cos(num11) - 13416.0 * Math.Cos(2.0 * num11) - 2579.0 * Math.Cos(3.0 * num11) - 548.0 * Math.Cos(4.0 * num11) - 123.0 * Math.Cos(5.0 * num11);
				Longitude = (178.179136 + 4.0923770242 * num + 1.6E-05 * num3 + num5 / 3600.0) % 360.0 / (180.0 / Math.PI);
				double num14 = Longitude - num12;
				Longitude -= 0.003743 * Math.Sin(2.0 * num14);
				Latitude = Math.Asin(num13 * Math.Sin(num14));
				HeliocentricDistance = Math.Pow(10.0, num6 / 1000000.0);
				break;
			}
			case 2:
			{
				double num7 = (212.603219 + 1.60213015 * num) / (180.0 / Math.PI);
				double num8 = (75.779647 + 0.89985 * num2) / (180.0 / Math.PI);
				double num9 = Math.Sin((3.39363 + 0.0010058 * num2) / (180.0 / Math.PI));
				double num5 = (2814.0 - 20.0 * num2) * Math.Sin(num7);
				double num6 = -140657.0 - (2962.0 - 21.0 * num2) * Math.Cos(num7);
				Longitude = (342.767053 + 1.6021687 * num + 2E-05 * num3 + num5 / 3600.0) % 360.0 / (180.0 / Math.PI);
				double num10 = Longitude - num8;
				Longitude -= 0.0008775 * Math.Sin(2.0 * num10);
				Latitude = Math.Asin(num9 * Math.Sin(num10));
				HeliocentricDistance = Math.Pow(10.0, num6 / 1000000.0);
				break;
			}
			case 3:
			{
				double num4 = (358.475845 + 0.985600267 * num - 1E-05 * num3) / (180.0 / Math.PI);
				double num5 = (6910.0 - 17.0 * num2) * Math.Sin(num4) + 72.0 * Math.Sin(2.0 * num4);
				double num6 = 31.0 - (7274.0 - 18.0 * num2) * Math.Cos(num4) - 91.0 * Math.Cos(2.0 * num4) - Math.Cos(3.0 * num4);
				Longitude = (279.696678 + 0.985647335 * num + 2.2E-05 * num3 + num5 / 3600.0 + 180.0) % 360.0 / (180.0 / Math.PI);
				Latitude = 0.0;
				HeliocentricDistance = Math.Pow(10.0, num6 / 1000000.0);
				break;
			}
			}
		}

		public static void QuickPlanet(double JD_TT, int PlanetNo, bool EquinoxOfDate, out double RA, out double Dec, out double GeocentricDistance)
		{
			double Longitude = 0.0;
			double Latitude = 0.0;
			double PlanetHeliocentricDistance = 0.0;
			QuickPlanet(JD_TT, PlanetNo, EquinoxOfDate, out RA, out Dec, out GeocentricDistance, out Longitude, out Latitude, out PlanetHeliocentricDistance);
		}

		public static void QuickPlanet(double JD_TT, int PlanetNo, bool EquinoxOfDate, out double RA, out double Dec, out double Distance, out double Longitude, out double Latitude, out double PlanetHeliocentricDistance)
		{
			double Y2;
			double Z2;
			double X;
			double Y;
			double Z;
			double num3;
			double num2;
			double num;
			double X2 = (Y2 = (Z2 = (X = (Y = (Z = (num3 = (num2 = (num = (PlanetHeliocentricDistance = 0.0)))))))));
			int Version;
			if (PlanetNo != 3)
			{
				PlanetXYZ(JD_TT, PlanetNo, Heliocentric: true, 0.0001, ref X, ref Y, ref Z, out Version);
				PlanetHeliocentricDistance = Math.Sqrt(X * X + Y * Y + Z * Z);
			}
			PlanetXYZ(JD_TT, 3, Heliocentric: true, 0.0001, ref X2, ref Y2, ref Z2, out Version);
			num3 = X - X2;
			num2 = Y - Y2;
			num = Z - Z2;
			Distance = Math.Sqrt(num3 * num3 + num2 * num2 + num * num);
			XYZ_to_RA_Dec(num3, num2, num, out RA, out Dec);
			if (EquinoxOfDate)
			{
				Precession(2000, JD_TT, use2006values_Not1976: false, ref RA, ref Dec, 0.0, 0.0);
				Nutation(JD_TT, out var Longitude_arcsec, out var Obliquity_arcsec, out var _);
				double num4 = MeanEclipticOfDate(JD_TT, Use1976Value: true);
				RA += (Longitude_arcsec * (Math.Cos(num4) + Math.Sin(num4) * Math.Sin(RA) * Math.Tan(Dec)) - Obliquity_arcsec * Math.Cos(RA) * Math.Tan(Dec)) / 3600.0 / (180.0 / Math.PI);
				Dec += (Longitude_arcsec * Math.Sin(num4) * Math.Cos(RA) + Obliquity_arcsec * Math.Sin(RA)) / 3600.0 / (180.0 / Math.PI);
			}
			double num5 = Math.Cos(Dec) * Math.Cos(RA);
			double num6 = Math.Cos(Dec) * Math.Sin(RA) * cosEcliptic2000[EclipticID] + Math.Sin(Dec) * sinEcliptic2000[EclipticID];
			double num7 = (0.0 - Math.Cos(Dec)) * Math.Sin(RA) * sinEcliptic2000[EclipticID] + Math.Sin(Dec) * cosEcliptic2000[EclipticID];
			Longitude = Math.Atan2(num6, num5);
			if (Longitude < 0.0)
			{
				Longitude += Math.PI * 2.0;
			}
			Latitude = Math.Atan(num7 / Math.Sqrt(num5 * num5 + num6 * num6));
		}

		public static void QuickSolarElongation_PhaseAngle(double JD, double RAobject, double DecObject, double DistObject, out double Elongation, out double PhaseAngle, out double SolarDistObject)
		{
			QuickPlanet(JD, 3, EquinoxOfDate: false, out var RA, out var Dec, out var Distance, out var _, out var _, out var _);
			double num = Math.Cos(Dec) * Math.Sin(RA - RAobject);
			double num2 = Math.Sin(Dec) * Math.Cos(DecObject) - Math.Cos(Dec) * Math.Sin(DecObject) * Math.Cos(RA - RAobject);
			double num3 = Math.Sin(Dec) * Math.Sin(DecObject) + Math.Cos(Dec) * Math.Cos(DecObject) * Math.Cos(RA - RAobject);
			Elongation = Math.Atan(Math.Sqrt(num * num + num2 * num2) / num3);
			if (Elongation < 0.0)
			{
				Elongation += Math.PI;
			}
			double num4 = Distance * Math.Sin(Elongation);
			double num5 = DistObject - Distance * Math.Cos(Elongation);
			SolarDistObject = Math.Sqrt(num4 * num4 + num5 * num5);
			PhaseAngle = Math.Atan(Math.Abs(num4 / num5)) * (180.0 / Math.PI);
			if (SolarDistObject * SolarDistObject + DistObject * DistObject - Distance * Distance < 0.0)
			{
				PhaseAngle = 180.0 - PhaseAngle;
			}
			Elongation *= 180.0 / Math.PI;
		}

		public static void Meridians(double JD_UT, int PlanetNo, out double Long1_deg, out double Long2_deg, out double B_deg, out double P_deg)
		{
			double Dec;
			double Distance;
			double Longitude;
			double Latitude;
			double PlanetHeliocentricDistance;
			double RA = (Dec = (Distance = (Longitude = (Latitude = (PlanetHeliocentricDistance = 0.0)))));
			double num5;
			double num4;
			double num3;
			double num2;
			double num;
			double num6 = (num5 = (num4 = (num3 = (num2 = (num = 0.0)))));
			double num7 = (JD_UT - 2451545.0) / 36525.0;
			double num8 = JD_UT - 2451545.0 - delta_T(JD_UT) / 86400.0;
			switch (PlanetNo)
			{
			case 1:
				num4 = 281.01 + 0.2738 * num7;
				num3 = 61.45 + 0.10139999999999999 * num7;
				num2 = 329.71 + 6.1385025 * num8;
				num = 0.03545;
				break;
			case 2:
				num4 = 272.76 - 0.0423 * num7;
				num3 = 67.16 + 0.027 * num7;
				num2 = 160.26 - 1.4813596 * num8;
				num = -0.00856;
				break;
			case 3:
				num4 = 286.13 + 0.1857 * num7;
				num3 = 63.87 + 0.1531 * num7;
				num2 = 84.1 + 14.1844 * num8;
				num = 0.0;
				break;
			case 4:
				num4 = 317.681 + 0.6799000000000001 * num7;
				num3 = 52.887 + 0.3507 * num7;
				num2 = 176.868 + 350.891983 * num8;
				num = 2.02661;
				break;
			case 5:
				num4 = 268.05 + 0.1062 * num7;
				num3 = 64.49 + -0.0159 * num7;
				num2 = 67.1 + 877.9 * num8;
				num = 5.0704;
				num6 = 43.3 + 870.27 * num8;
				num5 = 5.02633;
				break;
			case 6:
				num4 = 40.5954 + 4.41779 * num7;
				num3 = 83.538 + 0.4157 * num7;
				num2 = 227.2037 + 844.3 * num8;
				num = 4.87634;
				break;
			case 7:
				num4 = 257.43 + 1.4279 * num7;
				num3 = -15.1 - 0.1212 * num7;
				num2 = 203.81 - 501.1600928 * num8;
				num = -2.8945;
				break;
			case 8:
				num4 = 299.36 + 0.8492 * num7 + 0.07 * Math.Sin((357.85 + 52.316 * num7) / (180.0 / Math.PI));
				num3 = 43.46 + 0.2382 * num7 - 0.51 * Math.Cos((357.85 + 52.316 * num7) / (180.0 / Math.PI));
				num2 = 253.18 + 536.3128492 * num8 - 0.48 * Math.Sin((357.85 + 52.316 * num7) / (180.0 / Math.PI));
				num = 3.0975;
				break;
			case 9:
				num4 = 313.02 + 1.2508 * num7;
				num3 = 9.09 + 0.3698 * num7;
				num2 = 236.77 - 56.3623195 * num8;
				num = -0.3255;
				break;
			}
			double num9 = num4 / (180.0 / Math.PI);
			double num10 = num3 / (180.0 / Math.PI);
			QuickPlanet(JD_UT, PlanetNo, EquinoxOfDate: true, out RA, out Dec, out Distance, out Longitude, out Latitude, out PlanetHeliocentricDistance);
			double y = (0.0 - Math.Cos(num10)) * Math.Sin(Dec) + Math.Sin(num10) * Math.Cos(Dec) * Math.Cos(num9 - RA);
			double x = Math.Cos(Dec) * Math.Sin(num9 - RA);
			double num11 = Math.Atan2(y, x);
			Long1_deg = num2 - num11 * (180.0 / Math.PI) - num * Distance;
			Long1_deg %= 360.0;
			if (Long1_deg < 0.0)
			{
				Long1_deg += 360.0;
			}
			Long2_deg = -1.0;
			if (PlanetNo == 5)
			{
				Long2_deg = num6 - num11 * (180.0 / Math.PI) - num5 * Distance;
				Long2_deg %= 360.0;
				if (Long2_deg < 0.0)
				{
					Long2_deg += 360.0;
				}
			}
			if (PlanetNo == 3 || num < 0.0)
			{
				Long1_deg = 360.0 - Long1_deg;
			}
			double num12 = Math.Cos(num10) * Math.Sin(num9 - RA);
			double num13 = Math.Sin(num10) * Math.Cos(Dec) - Math.Cos(num10) * Math.Sin(Dec) * Math.Cos(num9 - RA);
			double num14 = (0.0 - Math.Sin(num10)) * Math.Sin(Dec) - Math.Cos(num10) * Math.Cos(Dec) * Math.Cos(num9 - RA);
			P_deg = Math.Atan2(num12, num13) * (180.0 / Math.PI);
			if (P_deg < 0.0)
			{
				P_deg += 360.0;
			}
			B_deg = Math.Atan(num14 / Math.Sqrt(num12 * num12 + num13 * num13)) * (180.0 / Math.PI);
		}

		public static double Planet_MaxLimbHeightAboveReferenceLine(double MinorAxis, double PA_Pole_deg, double PAofMoonContact_deg)
		{
			double num = -1.1;
			double num2 = 0.0;
			double num3 = 0.0;
			double num4 = NormaliseDegrees(PA_Pole_deg - PAofMoonContact_deg);
			if (num4 > 180.0)
			{
				num4 -= 180.0;
			}
			double num5 = Math.Cos(num4 / (180.0 / Math.PI));
			double num6 = Math.Sin(num4 / (180.0 / Math.PI));
			for (int i = 0; i <= 360; i++)
			{
				double num7 = Math.Sin((double)i / (180.0 / Math.PI));
				num2 = MinorAxis * Math.Cos((double)i / (180.0 / Math.PI));
				num3 = (0.0 - num7) * num6 + num2 * num5;
				if (num3 > num)
				{
					num = num3;
				}
			}
			return num;
		}

		public static double Planet_MaxTerminatorHeightAboveReferenceLine(double PhaseAngle, double PA_BrightLimb_deg, double PAofMoonContact_deg, out bool DarkLimbClosest)
		{
			DarkLimbClosest = false;
			double num = -1.1;
			double num2 = 0.0;
			double num3 = 0.0;
			double num4 = Math.Cos(PhaseAngle / (180.0 / Math.PI));
			double num5 = NormaliseDegrees(PA_BrightLimb_deg - PAofMoonContact_deg);
			if (num5 > 90.0 && num5 < 270.0)
			{
				DarkLimbClosest = true;
			}
			double num6 = Math.Cos(num5 / (180.0 / Math.PI));
			double num7 = Math.Sin(num5 / (180.0 / Math.PI));
			if (PhaseAngle >= 90.0)
			{
				num = Math.Abs(num7);
			}
			else
			{
				for (int i = -180; i <= 180; i++)
				{
					double num8 = Math.Sin((double)i / (180.0 / Math.PI));
					num2 = num4 * Math.Cos((double)i / (180.0 / Math.PI));
					num3 = (0.0 - num8) * num7 + num2 * num6;
					if (num3 > num)
					{
						num = num3;
					}
				}
			}
			return num;
		}

		internal static bool Get_SingleAsteroid_Diameter_and_UncertaintyInfo(int AsteroidNumber, out double Diameter, out double DiameterUncertainty, out string Source)
		{
			if (!File.Exists(AppPath + "\\Resource Files\\AsteroidDias_Indiv.bin"))
			{
				http.Download_AsteroidDias(SupressMessages: false);
			}
			Diameter = 1.0;
			DiameterUncertainty = 0.1;
			Source = "M";
			string text = AppPath + "\\Resource Files\\AsteroidDiameters.bin";
			int num = 550000;
			if (File.Exists(text))
			{
				num = (int)new FileInfo(text).Length / 3;
			}
			using FileStream fileStream = new FileStream(text, FileMode.Open, FileAccess.Read);
			using BinaryReader binaryReader = new BinaryReader(fileStream);
			if (AsteroidNumber < num && AsteroidNumber > 0)
			{
				fileStream.Seek(2 * (AsteroidNumber - 1), SeekOrigin.Begin);
				Diameter = (double)binaryReader.ReadInt16() / 10.0;
				fileStream.Seek(2 * num + AsteroidNumber - 1, SeekOrigin.Begin);
				DiameterUncertainty = (double)(int)binaryReader.ReadByte() / 5.0;
				if (Diameter < 0.0)
				{
					Diameter = 0.0 - Diameter;
					Source = "6";
					if (Diameter < 10.0)
					{
						DiameterUncertainty = Diameter / 10.0;
					}
				}
				return true;
			}
			return false;
		}

		internal static void OpenAsteroidDiameterFileForReading()
		{
			AD = new FileStream(AppPath + "\\Resource Files\\AsteroidDiameters.bin", FileMode.Open, FileAccess.Read);
			ADread = new BinaryReader(AD);
			FileLength = new FileInfo(AppPath + "\\Resource Files\\AsteroidDiameters.bin").Length;
		}

		internal static void GetAsteroidDiameter(int AsteroidNumber, double H0, out double MeanDiameter, out double DiameterUncertainty, out string DiaSource, out bool SatelliteMeasure)
		{
			GetAsteroidDiameter(AsteroidNumber, H0, 0.15, out MeanDiameter, out DiameterUncertainty, out DiaSource, out SatelliteMeasure);
		}

		internal static void GetAsteroidDiameter(int AsteroidNumber, double H0, double Albedo, out double MeanDiameter, out double DiameterUncertainty, out string DiaSource, out bool SatelliteMeasure)
		{
			DiameterUncertainty = 1.0;
			MeanDiameter = 0.001;
			DiaSource = "M";
			SatelliteMeasure = true;
			if (AsteroidNumber < 550000 && AsteroidNumber > 0)
			{
				AD.Seek(2 * (AsteroidNumber - 1), SeekOrigin.Begin);
				MeanDiameter = (double)ADread.ReadInt16() / 10.0;
				AD.Seek(1100000 + AsteroidNumber - 1, SeekOrigin.Begin);
				DiameterUncertainty = (double)(int)ADread.ReadByte() / 5.0;
				if (MeanDiameter <= 0.0)
				{
					MeanDiameter = 0.0 - MeanDiameter;
					DiaSource = "6";
					if (MeanDiameter < 10.0)
					{
						DiameterUncertainty = MeanDiameter / 10.0;
					}
				}
			}
			else if (H0 > 0.0)
			{
				MeanDiameter = Math.Pow(10.0, 3.1236 - 0.5 * Math.Log10(Albedo) - 0.2 * H0);
				if (MeanDiameter < 0.1)
				{
					MeanDiameter = 0.1;
				}
				DiameterUncertainty = MeanDiameter / 10.0;
				DiaSource = "6";
				SatelliteMeasure = false;
			}
		}

		internal static void CloseAsteroidDiameterFileForReading()
		{
			AD.Close();
		}

		public static void Swap<T>(ref T A, ref T B)
		{
			T val = A;
			A = B;
			B = val;
		}

		public static void Swap<T>(ref T[] A, int Row, int Offset)
		{
			T val = A[Row];
			A[Row] = A[Row + Offset];
			A[Row + Offset] = val;
		}

		internal static Image Image_SetBlackBackground_ToWhite(Image source)
		{
			Bitmap bitmap = new Bitmap(source);
			for (int i = 0; i <= bitmap.Height - 1; i++)
			{
				for (int j = 0; j <= bitmap.Width - 1; j++)
				{
					if (bitmap.GetPixel(j, i) == Color.FromArgb(255, 0, 0, 0))
					{
						bitmap.SetPixel(j, i, Color.FromArgb(255, 255, 255, 255));
					}
				}
			}
			return bitmap;
		}

		internal static Image InvertImage(Image source)
		{
			Bitmap bitmap = new Bitmap(source.Width, source.Height);
			Graphics graphics = Graphics.FromImage(bitmap);
			ColorMatrix colorMatrix = new ColorMatrix(new float[5][]
			{
				new float[5] { -1f, 0f, 0f, 0f, 0f },
				new float[5] { 0f, -1f, 0f, 0f, 0f },
				new float[5] { 0f, 0f, -1f, 0f, 0f },
				new float[5] { 0f, 0f, 0f, 1f, 0f },
				new float[5] { 1f, 1f, 1f, 0f, 1f }
			});
			ImageAttributes imageAttributes = new ImageAttributes();
			imageAttributes.SetColorMatrix(colorMatrix);
			graphics.DrawImage(source, new Rectangle(0, 0, source.Width, source.Height), 0, 0, source.Width, source.Height, GraphicsUnit.Pixel, imageAttributes);
			graphics.Dispose();
			return bitmap;
		}

		internal static void DrawRotatedTextAt(Graphics gr, float angle_DegClockwise, string txt, int x, int y, bool xy_isMiddleOfString, Font the_font, Brush the_brush, Brush the_background)
		{
			GraphicsState gstate = gr.Save();
			gr.ResetTransform();
			gr.RotateTransform(angle_DegClockwise);
			if (xy_isMiddleOfString)
			{
				y += (int)gr.MeasureString(txt, the_font).Width / 2;
			}
			gr.TranslateTransform(x, y, MatrixOrder.Append);
			gr.FillRectangle(the_background, 0f, 0f, gr.MeasureString(txt, the_font).Width, gr.MeasureString(txt, the_font).Height);
			gr.DrawString(txt, the_font, the_brush, 0f, 0f);
			gr.Restore(gstate);
		}

		internal static bool ScreenSizeTooLargeForImageSaves()
		{
			//IL_0048: Unknown result type (might be due to invalid IL or missing references)
			if ((Screen.get_PrimaryScreen().get_WorkingArea().Width > 1920) | (Screen.get_PrimaryScreen().get_WorkingArea().Height > 1200))
			{
				MessageBox.Show("The screen resolution is such that the inbuit saving of form images will not match the displayed form. \r\n\r\nTo save a correct image of the form, you will need to either\r\n * reduce the screen resolution to 1980 x 1200 (or less)\r\n       OR\r\n * use a tool (such as the Windows 'Snipping Tool') to copy and save the image", "Screen resolution issue", (MessageBoxButtons)0, (MessageBoxIcon)64, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
				return true;
			}
			return false;
		}

		public static double FresnelLength_m(double DistanceInAU, double LightWavelength_nm, double BandWidth_nanoMeters, out double mas)
		{
			double num = 0.0;
			double num2 = 0.0;
			double num3 = 149597870700.0 * DistanceInAU;
			double num4 = LightWavelength_nm * 1E-09;
			if (BandWidth_nanoMeters < 50.0)
			{
				num = Math.Sqrt(num4 * num3 / 2.0);
			}
			else
			{
				double num5 = 5.0;
				double num6 = BandWidth_nanoMeters / 2.0 * 1E-09;
				for (double num7 = 0.0 - num6; num7 <= num6 + 1E-09; num7 += num5)
				{
					num += Math.Sqrt((num4 + num7) * num3 / 2.0);
					num2 += 1.0;
				}
				num /= num2;
			}
			mas = num / num3 * (180.0 / Math.PI) * 3600.0 * 1000.0;
			return num;
		}

		public static void Moon(double JD_TT)
		{
			if (!((DE_EphemerisFileExists | DE_LongEphemerisFileExists) & !MustUseVSOP87) || !Moon_DailySeriesUsing_JPLDE(JD_TT))
			{
				Moon_DailySeriesUsing_Chapront(JD_TT);
			}
		}

		public static bool Moon_DailySeriesUsing_JPLDE(double JD_TT)
		{
			double[] array = new double[7];
			double[] array2 = new double[7];
			double[] array3 = new double[7];
			int[] MSeries = new int[6];
			double RA = 0.0;
			double Dec = 0.0;
			double x = 0.0;
			double y = 0.0;
			double z = 0.0;
			float num = 0f;
			float num2 = 0f;
			short num3 = 0;
			short num4 = 0;
			short value = 413;
			bool flag = false;
			double day = 0.0;
			int Year = 0;
			int Month = 0;
			JD_TT = Math.Floor(JD_TT + 0.5) - 0.5;
			Date_from_JD(JD_TT, out Year, out Month, out day);
			double num5 = JD_from_Date(Year, 1, 1.0);
			for (int i = 0; i < 6; i++)
			{
				if (DE_EphemerisFileExists | DE_LongEphemerisFileExists)
				{
					flag = JPL_DE.Lunar_Position(JD_TT + (double)i * 0.2, out x, out y, out z, out var EphemVersion);
					value = (short)EphemVersion;
				}
				if (!flag)
				{
					return flag;
				}
				XYZ_to_RA_Dec(x, y, z, out RA, out Dec);
				double num6 = Math.Sqrt(x * x + y * y + z * z);
				array3[i] = Math.Asin(6378.137 / num6) * (180.0 / Math.PI);
				Precession(2000, JD_TT + (double)i / 5.0, use2006values_Not1976: false, ref RA, ref Dec, 0.0, 0.0);
				Nutation(JD_TT + (double)i / 5.0, out var Longitude_arcsec, out var Obliquity_arcsec, out var _);
				double num7 = MeanEclipticOfDate(JD_TT, Use1976Value: true);
				RA += (Longitude_arcsec * (Math.Cos(num7) + Math.Sin(num7) * Math.Sin(RA) * Math.Tan(Dec)) - Obliquity_arcsec * Math.Cos(RA) * Math.Tan(Dec)) / 3600.0 / (180.0 / Math.PI);
				Dec += (Longitude_arcsec * Math.Sin(num7) * Math.Cos(RA) + Obliquity_arcsec * Math.Sin(RA)) / 3600.0 / (180.0 / Math.PI);
				array[i] = RA * (180.0 / Math.PI);
				if ((i > 0) & (array[i] < array[0]))
				{
					array[i] += 360.0;
				}
				array2[i] = Dec * (180.0 / Math.PI);
			}
			FileStream fileStream = new FileStream(AppPath + "\\Resource Files\\Moon\\Moon_" + Convert.ToString(Year) + ".bin", FileMode.OpenOrCreate, FileAccess.Write);
			long num8 = fileStream.Length / MoonFileRecordLength;
			long num9 = Convert.ToInt32(JD_TT - num5);
			if (num9 - num8 > 1)
			{
				StreamWriter streamWriter = new StreamWriter(fileStream);
				for (long num10 = num8 + 1; num10 < num9; num10++)
				{
					fileStream.Seek(num9 * MoonFileRecordLength, SeekOrigin.Begin);
					streamWriter.Write("".PadRight(MoonFileRecordLength, '\0'));
				}
			}
			BinaryWriter binaryWriter = new BinaryWriter(fileStream);
			fileStream.Seek(num9 * MoonFileRecordLength, SeekOrigin.Begin);
			Make_DailyMoonSeries(array, 0, ref MSeries);
			for (int j = 0; j < 6; j++)
			{
				binaryWriter.Write(MSeries[j]);
			}
			Make_DailyMoonSeries(array2, 1, ref MSeries);
			for (int k = 0; k < 6; k++)
			{
				binaryWriter.Write(MSeries[k]);
			}
			Make_DailyMoonSeries(array3, 2, ref MSeries);
			for (int l = 0; l < 5; l++)
			{
				binaryWriter.Write(MSeries[l]);
			}
			num3 = Convert.ToInt16(num * 10000f);
			num4 = Convert.ToInt16(num2 * 10000f);
			binaryWriter.Write(num3);
			binaryWriter.Write(num4);
			binaryWriter.Write(value);
			fileStream.Close();
			return flag;
		}

		public static void Moon_DailySeriesUsing_Chapront(double JD_TT)
		{
			double[] array = new double[6];
			double[] array2 = new double[6];
			double[] array3 = new double[6];
			double[] array4 = new double[6];
			double[] array5 = new double[6];
			"".PadRight(MoonFileRecordLength, '\0');
			int[] MSeries = new int[6];
			double day = 0.0;
			int Year = 0;
			int Month = 0;
			JD_TT = Math.Floor(JD_TT + 0.5) - 0.5;
			Date_from_JD(JD_TT, out Year, out Month, out day);
			double num = JD_from_Date(Year, 1, 1.0);
			double num2 = (JD_TT - 2451545.0) / 36525.0;
			double num3 = (JD_TT - 2451544.0) / 36525.0;
			double num4 = 0.9508 / (0.9508 + 0.0518 * Math.Cos((134.9634113777778 + 477198.8676313445 * num2) / (180.0 / Math.PI)) + 0.0095 * Math.Cos((2.0 * (297.8502042 + 445267.11151673336 * num2) - 134.9634113777778 - 477198.8676313445 * num2) / (180.0 / Math.PI)) + 0.0078 * Math.Cos(2.0 * (297.8502042 + 445267.11151673336 * num2) / (180.0 / Math.PI)) + 0.0028 * Math.Cos(2.0 * (134.9634113777778 + 477198.8676313445 * num2) / (180.0 / Math.PI)));
			double num5 = 0.9508 / (0.9508 + 0.0518 * Math.Cos((134.9634113777778 + 477198.8676313445 * num3) / (180.0 / Math.PI)) + 0.0095 * Math.Cos((2.0 * (297.8502042 + 445267.11151673336 * num3) - 134.9634113777778 - 477198.8676313445 * num3) / (180.0 / Math.PI)) + 0.0078 * Math.Cos(2.0 * (297.8502042 + 445267.11151673336 * num3) / (180.0 / Math.PI)) + 0.0028 * Math.Cos(2.0 * (134.9634113777778 + 477198.8676313445 * num3) / (180.0 / Math.PI)));
			double num6 = (num5 - num4) / 5.0;
			double num7 = 1.4853893980875536E-05 * num4;
			double num8 = 1.4853893980875536E-05 * num5;
			num2 = (JD_TT - 2451545.0 - num7) / 36525.0;
			num3 = (JD_TT - 2451544.0 - num8) / 36525.0;
			double num9 = num2 * num2;
			double num10 = num9 * num2;
			double num11 = num10 * num2;
			double num12 = (297.8502042 + 445267.11151673336 * num2 + -0.0016300277777777778 * num9 + 1.8319444444444444E-06 * num10 + -8.844444444444446E-09 * num11) / (180.0 / Math.PI);
			double num13 = (-2.4708908166666674 + 35999.05029097223 * num2 + -0.00015358333333333332 * num9 + 4.0833333333333335E-08 * num10 + 4.1666666666666665E-11 * num11) / (180.0 / Math.PI);
			double num14 = (134.9634113777778 + 477198.8676313445 * num2 + 0.008997027777777777 * num9 + 1.43475E-05 * num10 + -6.797222222222222E-08 * num11) / (180.0 / Math.PI);
			double num15 = (93.27209931944445 + 483202.01752728893 * num2 + -0.003402916666666667 * num9 + -2.8361111111111134E-07 * num10 + 1.1583333333333321E-09 * num11) / (180.0 / Math.PI);
			double num16 = (218.3166543638889 + 481267.88134240004 * num2) / (180.0 / Math.PI);
			double num17 = 1.3969712777777776 * num2 + 0.00030888888888888893 * num9 + 2.138888888888889E-08 * num10 + -6.5361111111111115E-09 * num11;
			double num18 = 218.3166543638889 + 481266.48437112226 * num2 + -0.001635638888888889 * num9 + 1.8344444444444444E-06 * num10 + -8.80277777777778E-09 * num11 + num17;
			double num19 = (125.04455504444444 + -1935.5331561666667 * num2 + 0.0017672777777777778 * num9 + 2.1180555555555557E-06 * num10 + -9.961111111111112E-09 * num11 + num17) / (180.0 / Math.PI);
			double num20 = (23.439280230555557 + -0.013004166666666666 * num2 + -1.638888888888889E-07 * num9 + 5.036111111111111E-07 * num10) / (180.0 / Math.PI);
			double num21 = (252.2509 + 149472.67464 * num2) / (180.0 / Math.PI);
			double num22 = (181.9798 + 58517.81568 * num2) / (180.0 / Math.PI);
			double num23 = (100.4665 + 35999.37285 * num2) / (180.0 / Math.PI);
			double num24 = (355.4333 + 19140.29933 * num2) / (180.0 / Math.PI);
			double num25 = (34.3515 + 3034.905967 * num2) / (180.0 / Math.PI);
			double num26 = (50.0775 + 1222.11379 * num2) / (180.0 / Math.PI);
			double num27 = (314.055 + 428.466998 * num2) / (180.0 / Math.PI);
			double num28 = (304.3487 + 218.4862 * num2) / (180.0 / Math.PI);
			double num29 = (297.8502042 + 445267.11151673336 * num3 + -0.0016300277777777778 * num9 + 1.8319444444444444E-06 * num10 + -8.844444444444446E-09 * num11) / (180.0 / Math.PI);
			double num30 = (-2.4708908166666674 + 35999.05029097223 * num3 + -0.00015358333333333332 * num9 + 4.0833333333333335E-08 * num10 + 4.1666666666666665E-11 * num11) / (180.0 / Math.PI);
			double num31 = (134.9634113777778 + 477198.8676313445 * num3 + 0.008997027777777777 * num9 + 1.43475E-05 * num10 + -6.797222222222222E-08 * num11) / (180.0 / Math.PI);
			double num32 = (93.27209931944445 + 483202.01752728893 * num3 + -0.003402916666666667 * num9 + -2.8361111111111134E-07 * num10 + 1.1583333333333321E-09 * num11) / (180.0 / Math.PI);
			double num33 = (218.3166543638889 + 481267.88134240004 * num3) / (180.0 / Math.PI);
			double num34 = 1.3969712777777776 * num3 + 0.00030888888888888893 * num9 + 2.138888888888889E-08 * num10 + -6.5361111111111115E-09 * num11;
			double num35 = 218.3166543638889 + 481266.48437112226 * num3 + -0.001635638888888889 * num9 + 1.8344444444444444E-06 * num10 + -8.80277777777778E-09 * num11 + num34;
			double num36 = (125.04455504444444 + -1935.5331561666667 * num3 + 0.0017672777777777778 * num9 + 2.1180555555555557E-06 * num10 + -9.961111111111112E-09 * num11 + num34) / (180.0 / Math.PI);
			double num37 = (23.439280230555557 + -0.013004166666666666 * num3 + -1.638888888888889E-07 * num9 + 5.036111111111111E-07 * num10) / (180.0 / Math.PI);
			double num38 = (252.2509 + 149472.67464 * num3) / (180.0 / Math.PI);
			double num39 = (181.9798 + 58517.81568 * num3) / (180.0 / Math.PI);
			double num40 = (100.4665 + 35999.37285 * num3) / (180.0 / Math.PI);
			double num41 = (355.4333 + 19140.29933 * num3) / (180.0 / Math.PI);
			double num42 = (34.3515 + 3034.905967 * num3) / (180.0 / Math.PI);
			double num43 = (50.0775 + 1222.11379 * num3) / (180.0 / Math.PI);
			double num44 = (314.055 + 428.466998 * num3) / (180.0 / Math.PI);
			double num45 = (304.3487 + 218.4862 * num3) / (180.0 / Math.PI);
			FileStream fileStream = new FileStream(AppPath + "\\Resource Files\\MoonSeries.bin", FileMode.Open, FileAccess.Read);
			BinaryReader binaryReader = new BinaryReader(fileStream);
			for (int i = 1; i <= 27; i++)
			{
				int num46 = binaryReader.ReadInt16();
				int num47 = binaryReader.ReadInt16();
				int num48 = binaryReader.ReadInt16();
				int num49 = binaryReader.ReadInt16();
				double num50 = binaryReader.ReadDouble();
				double num51 = (double)num46 * num12 + (double)num48 * num14 + (double)num47 * num13 + (double)num49 * num15;
				double num52 = ((double)num46 * num29 + (double)num48 * num31 + (double)num47 * num30 + (double)num49 * num32 - num51) / 5.0;
				for (int j = 0; j < 6; j++)
				{
					array[j] += num50 * Math.Sin(num51 + (double)j * num52);
				}
			}
			for (int k = 1; k <= 14; k++)
			{
				int num46 = binaryReader.ReadInt16();
				int num47 = binaryReader.ReadInt16();
				int num48 = binaryReader.ReadInt16();
				int num49 = binaryReader.ReadInt16();
				double num50 = binaryReader.ReadDouble();
				double num51 = (double)num46 * num12 + (double)num48 * num14 + (double)num47 * num13 + (double)num49 * num15;
				double num52 = ((double)num46 * num29 + (double)num48 * num31 + (double)num47 * num30 + (double)num49 * num32 - num51) / 5.0;
				for (int l = 0; l < 6; l++)
				{
					array2[l] += num50 * Math.Sin(num51 + (double)l * num52);
				}
			}
			for (int m = 1; m <= 26; m++)
			{
				int num46 = binaryReader.ReadInt16();
				int num47 = binaryReader.ReadInt16();
				int num48 = binaryReader.ReadInt16();
				int num49 = binaryReader.ReadInt16();
				double num50 = binaryReader.ReadDouble();
				double num51 = (double)num46 * num12 + (double)num48 * num14 + (double)num47 * num13 + (double)num49 * num15;
				double num52 = ((double)num46 * num29 + (double)num48 * num31 + (double)num47 * num30 + (double)num49 * num32 - num51) / 5.0;
				for (int n = 0; n < 6; n++)
				{
					array3[n] += num50 * Math.Cos(num51 + (double)n * num52);
				}
			}
			for (int num53 = 1; num53 <= 422; num53++)
			{
				int num54 = binaryReader.ReadInt16();
				int num55 = binaryReader.ReadInt16();
				int num56 = binaryReader.ReadInt16();
				int num57 = binaryReader.ReadInt16();
				float num58 = binaryReader.ReadSingle();
				double num51 = (double)num54 * num12 + (double)num56 * num14 + (double)num55 * num13 + (double)num57 * num15;
				double num52 = ((double)num54 * num29 + (double)num56 * num31 + (double)num55 * num30 + (double)num57 * num32 - num51) / 5.0;
				for (int num59 = 0; num59 < 6; num59++)
				{
					array[num59] += (double)num58 * Math.Sin(num51 + (double)num59 * num52);
				}
			}
			for (int num60 = 1; num60 <= 386; num60++)
			{
				int num54 = binaryReader.ReadInt16();
				int num55 = binaryReader.ReadInt16();
				int num56 = binaryReader.ReadInt16();
				int num57 = binaryReader.ReadInt16();
				float num58 = binaryReader.ReadSingle();
				double num51 = (double)num54 * num12 + (double)num56 * num14 + (double)num55 * num13 + (double)num57 * num15;
				double num52 = ((double)num54 * num29 + (double)num56 * num31 + (double)num55 * num30 + (double)num57 * num32 - num51) / 5.0;
				for (int num61 = 0; num61 < 6; num61++)
				{
					array2[num61] += (double)num58 * Math.Sin(num51 + (double)num61 * num52);
				}
			}
			for (int num62 = 1; num62 <= 332; num62++)
			{
				int num54 = binaryReader.ReadInt16();
				int num55 = binaryReader.ReadInt16();
				int num56 = binaryReader.ReadInt16();
				int num57 = binaryReader.ReadInt16();
				float num58 = binaryReader.ReadSingle();
				double num51 = (double)num54 * num12 + (double)num56 * num14 + (double)num55 * num13 + (double)num57 * num15;
				double num52 = ((double)num54 * num29 + (double)num56 * num31 + (double)num55 * num30 + (double)num57 * num32 - num51) / 5.0;
				for (int num63 = 0; num63 < 6; num63++)
				{
					array3[num63] += (double)num58 * Math.Cos(num51 + (double)num63 * num52);
				}
			}
			for (int num64 = 1; num64 <= 1398; num64++)
			{
				int num65 = binaryReader.ReadInt16();
				int num66 = binaryReader.ReadInt16();
				int num67 = binaryReader.ReadInt16();
				int num68 = binaryReader.ReadInt16();
				int num69 = binaryReader.ReadInt16();
				int num70 = binaryReader.ReadInt16();
				int num71 = binaryReader.ReadInt16();
				int num72 = binaryReader.ReadInt16();
				int num73 = binaryReader.ReadInt16();
				int num74 = binaryReader.ReadInt16();
				int num75 = binaryReader.ReadInt16();
				float num76 = binaryReader.ReadSingle();
				float num77 = binaryReader.ReadSingle();
				double num51 = (double)num65 * num21 + (double)num66 * num22 + (double)num67 * num23 + (double)num68 * num24 + (double)num69 * num25 + (double)num70 * num26 + (double)num71 * num27 + (double)num72 * num28 + (double)num73 * num12 + (double)num74 * num14 + (double)num75 * num15 + (double)num76;
				double num52 = ((double)num65 * num38 + (double)num66 * num39 + (double)num67 * num40 + (double)num68 * num41 + (double)num69 * num42 + (double)num70 * num43 + (double)num71 * num44 + (double)num72 * num45 + (double)num73 * num29 + (double)num74 * num31 + (double)num75 * num32 + (double)num76 - num51) / 5.0;
				for (int num78 = 0; num78 < 6; num78++)
				{
					array[num78] += (double)num77 * Math.Sin(num51 + (double)num78 * num52);
				}
			}
			for (int num79 = 1; num79 <= 590; num79++)
			{
				int num65 = binaryReader.ReadInt16();
				int num66 = binaryReader.ReadInt16();
				int num67 = binaryReader.ReadInt16();
				int num68 = binaryReader.ReadInt16();
				int num69 = binaryReader.ReadInt16();
				int num70 = binaryReader.ReadInt16();
				int num71 = binaryReader.ReadInt16();
				int num72 = binaryReader.ReadInt16();
				int num73 = binaryReader.ReadInt16();
				int num74 = binaryReader.ReadInt16();
				int num75 = binaryReader.ReadInt16();
				float num76 = binaryReader.ReadSingle();
				float num77 = binaryReader.ReadSingle();
				double num51 = (double)num65 * num21 + (double)num66 * num22 + (double)num67 * num23 + (double)num68 * num24 + (double)num69 * num25 + (double)num70 * num26 + (double)num71 * num27 + (double)num72 * num28 + (double)num73 * num12 + (double)num74 * num14 + (double)num75 * num15 + (double)num76;
				double num52 = ((double)num65 * num38 + (double)num66 * num39 + (double)num67 * num40 + (double)num68 * num41 + (double)num69 * num42 + (double)num70 * num43 + (double)num71 * num44 + (double)num72 * num45 + (double)num73 * num29 + (double)num74 * num31 + (double)num75 * num32 + (double)num76 - num51) / 5.0;
				for (int num80 = 0; num80 < 6; num80++)
				{
					array2[num80] += (double)num77 * Math.Sin(num51 + (double)num80 * num52);
				}
			}
			for (int num81 = 1; num81 <= 1047; num81++)
			{
				int num65 = binaryReader.ReadInt16();
				int num66 = binaryReader.ReadInt16();
				int num67 = binaryReader.ReadInt16();
				int num68 = binaryReader.ReadInt16();
				int num69 = binaryReader.ReadInt16();
				int num70 = binaryReader.ReadInt16();
				int num71 = binaryReader.ReadInt16();
				int num72 = binaryReader.ReadInt16();
				int num73 = binaryReader.ReadInt16();
				int num74 = binaryReader.ReadInt16();
				int num75 = binaryReader.ReadInt16();
				float num76 = binaryReader.ReadSingle();
				float num77 = binaryReader.ReadSingle();
				double num51 = (double)num65 * num21 + (double)num66 * num22 + (double)num67 * num23 + (double)num68 * num24 + (double)num69 * num25 + (double)num70 * num26 + (double)num71 * num27 + (double)num72 * num28 + (double)num73 * num12 + (double)num74 * num14 + (double)num75 * num15 + (double)num76;
				double num52 = ((double)num65 * num38 + (double)num66 * num39 + (double)num67 * num40 + (double)num68 * num41 + (double)num69 * num42 + (double)num70 * num43 + (double)num71 * num44 + (double)num72 * num45 + (double)num73 * num29 + (double)num74 * num31 + (double)num75 * num32 + (double)num76 - num51) / 5.0;
				for (int num82 = 0; num82 < 6; num82++)
				{
					array3[num82] += (double)num77 * Math.Sin(num51 + (double)num82 * num52);
				}
			}
			for (int num83 = 1; num83 <= 50; num83++)
			{
				int num65 = binaryReader.ReadInt16();
				int num66 = binaryReader.ReadInt16();
				int num67 = binaryReader.ReadInt16();
				int num68 = binaryReader.ReadInt16();
				int num69 = binaryReader.ReadInt16();
				int num70 = binaryReader.ReadInt16();
				int num71 = binaryReader.ReadInt16();
				int num72 = binaryReader.ReadInt16();
				int num73 = binaryReader.ReadInt16();
				int num74 = binaryReader.ReadInt16();
				int num75 = binaryReader.ReadInt16();
				float num76 = binaryReader.ReadSingle();
				float num77 = binaryReader.ReadSingle();
				double num51 = (double)num65 * num21 + (double)num66 * num22 + (double)num67 * num23 + (double)num68 * num24 + (double)num69 * num25 + (double)num70 * num26 + (double)num71 * num27 + (double)num72 * num28 + (double)num73 * num12 + (double)num74 * num14 + (double)num75 * num15 + (double)num76;
				double num52 = ((double)num65 * num38 + (double)num66 * num39 + (double)num67 * num40 + (double)num68 * num41 + (double)num69 * num42 + (double)num70 * num43 + (double)num71 * num44 + (double)num72 * num45 + (double)num73 * num29 + (double)num74 * num31 + (double)num75 * num32 + (double)num76 - num51) / 5.0;
				for (int num84 = 0; num84 < 6; num84++)
				{
					array[num84] += num2 * (double)num77 * Math.Sin(num51 + (double)num84 * num52);
				}
			}
			for (int num85 = 1; num85 <= 8; num85++)
			{
				int num65 = binaryReader.ReadInt16();
				int num66 = binaryReader.ReadInt16();
				int num67 = binaryReader.ReadInt16();
				int num68 = binaryReader.ReadInt16();
				int num69 = binaryReader.ReadInt16();
				int num70 = binaryReader.ReadInt16();
				int num71 = binaryReader.ReadInt16();
				int num72 = binaryReader.ReadInt16();
				int num73 = binaryReader.ReadInt16();
				int num74 = binaryReader.ReadInt16();
				int num75 = binaryReader.ReadInt16();
				float num76 = binaryReader.ReadSingle();
				float num77 = binaryReader.ReadSingle();
				double num51 = (double)num65 * num21 + (double)num66 * num22 + (double)num67 * num23 + (double)num68 * num24 + (double)num69 * num25 + (double)num70 * num26 + (double)num71 * num27 + (double)num72 * num28 + (double)num73 * num12 + (double)num74 * num14 + (double)num75 * num15 + (double)num76;
				double num52 = ((double)num65 * num38 + (double)num66 * num39 + (double)num67 * num40 + (double)num68 * num41 + (double)num69 * num42 + (double)num70 * num43 + (double)num71 * num44 + (double)num72 * num45 + (double)num73 * num29 + (double)num74 * num31 + (double)num75 * num32 + (double)num76 - num51) / 5.0;
				for (int num86 = 0; num86 < 6; num86++)
				{
					array2[num86] += num2 * (double)num77 * Math.Sin(num51 + (double)num86 * num52);
				}
			}
			for (int num87 = 1; num87 <= 26; num87++)
			{
				int num65 = binaryReader.ReadInt16();
				int num66 = binaryReader.ReadInt16();
				int num67 = binaryReader.ReadInt16();
				int num68 = binaryReader.ReadInt16();
				int num69 = binaryReader.ReadInt16();
				int num70 = binaryReader.ReadInt16();
				int num71 = binaryReader.ReadInt16();
				int num72 = binaryReader.ReadInt16();
				int num73 = binaryReader.ReadInt16();
				int num74 = binaryReader.ReadInt16();
				int num75 = binaryReader.ReadInt16();
				float num76 = binaryReader.ReadSingle();
				float num77 = binaryReader.ReadSingle();
				double num51 = (double)num65 * num21 + (double)num66 * num22 + (double)num67 * num23 + (double)num68 * num24 + (double)num69 * num25 + (double)num70 * num26 + (double)num71 * num27 + (double)num72 * num28 + (double)num73 * num12 + (double)num74 * num14 + (double)num75 * num15 + (double)num76;
				double num52 = ((double)num65 * num38 + (double)num66 * num39 + (double)num67 * num40 + (double)num68 * num41 + (double)num69 * num42 + (double)num70 * num43 + (double)num71 * num44 + (double)num72 * num45 + (double)num73 * num29 + (double)num74 * num31 + (double)num75 * num32 + (double)num76 - num51) / 5.0;
				for (int num88 = 0; num88 < 6; num88++)
				{
					array3[num88] += num2 * (double)num77 * Math.Sin(num51 + (double)num88 * num52);
				}
			}
			for (int num89 = 1; num89 <= 30; num89++)
			{
				int num65 = binaryReader.ReadInt16();
				int num66 = binaryReader.ReadInt16();
				int num67 = binaryReader.ReadInt16();
				int num68 = binaryReader.ReadInt16();
				int num69 = binaryReader.ReadInt16();
				int num70 = binaryReader.ReadInt16();
				int num71 = binaryReader.ReadInt16();
				int num72 = binaryReader.ReadInt16();
				int num73 = binaryReader.ReadInt16();
				int num74 = binaryReader.ReadInt16();
				int num75 = binaryReader.ReadInt16();
				float num76 = binaryReader.ReadSingle();
				float num77 = binaryReader.ReadSingle();
				double num51 = (double)num65 * num21 + (double)num66 * num22 + (double)num67 * num23 + (double)num68 * num24 + (double)num69 * num25 + (double)num70 * num26 + (double)num71 * num27 + (double)num72 * num12 + (double)num73 * num13 + (double)num74 * num14 + (double)num75 * num15 + (double)num76;
				double num52 = ((double)num65 * num38 + (double)num66 * num39 + (double)num67 * num40 + (double)num68 * num41 + (double)num69 * num42 + (double)num70 * num43 + (double)num71 * num44 + (double)num72 * num29 + (double)num73 * num30 + (double)num74 * num31 + (double)num75 * num32 + (double)num76 - num51) / 5.0;
				for (int num90 = 0; num90 < 6; num90++)
				{
					array[num90] += (double)num77 * Math.Sin(num51 + (double)num90 * num52);
				}
			}
			for (int num91 = 1; num91 <= 24; num91++)
			{
				int num65 = binaryReader.ReadInt16();
				int num66 = binaryReader.ReadInt16();
				int num67 = binaryReader.ReadInt16();
				int num68 = binaryReader.ReadInt16();
				int num69 = binaryReader.ReadInt16();
				int num70 = binaryReader.ReadInt16();
				int num71 = binaryReader.ReadInt16();
				int num72 = binaryReader.ReadInt16();
				int num73 = binaryReader.ReadInt16();
				int num74 = binaryReader.ReadInt16();
				int num75 = binaryReader.ReadInt16();
				float num76 = binaryReader.ReadSingle();
				float num77 = binaryReader.ReadSingle();
				double num51 = (double)num65 * num21 + (double)num66 * num22 + (double)num67 * num23 + (double)num68 * num24 + (double)num69 * num25 + (double)num70 * num26 + (double)num71 * num27 + (double)num72 * num12 + (double)num73 * num13 + (double)num74 * num14 + (double)num75 * num15 + (double)num76;
				double num52 = ((double)num65 * num38 + (double)num66 * num39 + (double)num67 * num40 + (double)num68 * num41 + (double)num69 * num42 + (double)num70 * num43 + (double)num71 * num44 + (double)num72 * num29 + (double)num73 * num30 + (double)num74 * num31 + (double)num75 * num32 + (double)num76 - num51) / 5.0;
				for (int num92 = 0; num92 < 6; num92++)
				{
					array2[num92] += (double)num77 * Math.Sin(num51 + (double)num92 * num52);
				}
			}
			for (int num93 = 1; num93 <= 27; num93++)
			{
				int num65 = binaryReader.ReadInt16();
				int num66 = binaryReader.ReadInt16();
				int num67 = binaryReader.ReadInt16();
				int num68 = binaryReader.ReadInt16();
				int num69 = binaryReader.ReadInt16();
				int num70 = binaryReader.ReadInt16();
				int num71 = binaryReader.ReadInt16();
				int num72 = binaryReader.ReadInt16();
				int num73 = binaryReader.ReadInt16();
				int num74 = binaryReader.ReadInt16();
				int num75 = binaryReader.ReadInt16();
				float num76 = binaryReader.ReadSingle();
				float num77 = binaryReader.ReadSingle();
				double num51 = (double)num65 * num21 + (double)num66 * num22 + (double)num67 * num23 + (double)num68 * num24 + (double)num69 * num25 + (double)num70 * num26 + (double)num71 * num27 + (double)num72 * num12 + (double)num73 * num13 + (double)num74 * num14 + (double)num75 * num15 + (double)num76;
				double num52 = ((double)num65 * num38 + (double)num66 * num39 + (double)num67 * num40 + (double)num68 * num41 + (double)num69 * num42 + (double)num70 * num43 + (double)num71 * num44 + (double)num72 * num29 + (double)num73 * num30 + (double)num74 * num31 + (double)num75 * num32 + (double)num76 - num51) / 5.0;
				for (int num94 = 0; num94 < 6; num94++)
				{
					array3[num94] += (double)num77 * Math.Sin(num51 + (double)num94 * num52);
				}
			}
			for (int num95 = 1; num95 <= 65; num95++)
			{
				int num65 = binaryReader.ReadInt16();
				int num66 = binaryReader.ReadInt16();
				int num67 = binaryReader.ReadInt16();
				int num68 = binaryReader.ReadInt16();
				int num69 = binaryReader.ReadInt16();
				int num70 = binaryReader.ReadInt16();
				int num71 = binaryReader.ReadInt16();
				int num72 = binaryReader.ReadInt16();
				int num73 = binaryReader.ReadInt16();
				int num74 = binaryReader.ReadInt16();
				int num75 = binaryReader.ReadInt16();
				float num76 = binaryReader.ReadSingle();
				float num77 = binaryReader.ReadSingle();
				double num51 = (double)num65 * num21 + (double)num66 * num22 + (double)num67 * num23 + (double)num68 * num24 + (double)num69 * num25 + (double)num70 * num26 + (double)num71 * num27 + (double)num72 * num12 + (double)num73 * num13 + (double)num74 * num14 + (double)num75 * num15 + (double)num76;
				double num52 = ((double)num65 * num38 + (double)num66 * num39 + (double)num67 * num40 + (double)num68 * num41 + (double)num69 * num42 + (double)num70 * num43 + (double)num71 * num44 + (double)num72 * num29 + (double)num73 * num30 + (double)num74 * num31 + (double)num75 * num32 + (double)num76 - num51) / 5.0;
				for (int num96 = 0; num96 < 6; num96++)
				{
					array[num96] += num2 * (double)num77 * Math.Sin(num51 + (double)num96 * num52);
				}
			}
			for (int num97 = 1; num97 <= 42; num97++)
			{
				int num65 = binaryReader.ReadInt16();
				int num66 = binaryReader.ReadInt16();
				int num67 = binaryReader.ReadInt16();
				int num68 = binaryReader.ReadInt16();
				int num69 = binaryReader.ReadInt16();
				int num70 = binaryReader.ReadInt16();
				int num71 = binaryReader.ReadInt16();
				int num72 = binaryReader.ReadInt16();
				int num73 = binaryReader.ReadInt16();
				int num74 = binaryReader.ReadInt16();
				int num75 = binaryReader.ReadInt16();
				float num76 = binaryReader.ReadSingle();
				float num77 = binaryReader.ReadSingle();
				double num51 = (double)num65 * num21 + (double)num66 * num22 + (double)num67 * num23 + (double)num68 * num24 + (double)num69 * num25 + (double)num70 * num26 + (double)num71 * num27 + (double)num72 * num12 + (double)num73 * num13 + (double)num74 * num14 + (double)num75 * num15 + (double)num76;
				double num52 = ((double)num65 * num38 + (double)num66 * num39 + (double)num67 * num40 + (double)num68 * num41 + (double)num69 * num42 + (double)num70 * num43 + (double)num71 * num44 + (double)num72 * num29 + (double)num73 * num30 + (double)num74 * num31 + (double)num75 * num32 + (double)num76 - num51) / 5.0;
				for (int num98 = 0; num98 < 6; num98++)
				{
					array2[num98] += num2 * (double)num77 * Math.Sin(num51 + (double)num98 * num52);
				}
			}
			for (int num99 = 1; num99 <= 57; num99++)
			{
				int num65 = binaryReader.ReadInt16();
				int num66 = binaryReader.ReadInt16();
				int num67 = binaryReader.ReadInt16();
				int num68 = binaryReader.ReadInt16();
				int num69 = binaryReader.ReadInt16();
				int num70 = binaryReader.ReadInt16();
				int num71 = binaryReader.ReadInt16();
				int num72 = binaryReader.ReadInt16();
				int num73 = binaryReader.ReadInt16();
				int num74 = binaryReader.ReadInt16();
				int num75 = binaryReader.ReadInt16();
				float num76 = binaryReader.ReadSingle();
				float num77 = binaryReader.ReadSingle();
				double num51 = (double)num65 * num21 + (double)num66 * num22 + (double)num67 * num23 + (double)num68 * num24 + (double)num69 * num25 + (double)num70 * num26 + (double)num71 * num27 + (double)num72 * num12 + (double)num73 * num13 + (double)num74 * num14 + (double)num75 * num15 + (double)num76;
				double num52 = ((double)num65 * num38 + (double)num66 * num39 + (double)num67 * num40 + (double)num68 * num41 + (double)num69 * num42 + (double)num70 * num43 + (double)num71 * num44 + (double)num72 * num29 + (double)num73 * num30 + (double)num74 * num31 + (double)num75 * num32 + (double)num76 - num51) / 5.0;
				for (int num100 = 0; num100 < 6; num100++)
				{
					array3[num100] += num2 * (double)num77 * Math.Sin(num51 + (double)num100 * num52);
				}
			}
			for (int num101 = 1; num101 <= 75; num101++)
			{
				int num102 = binaryReader.ReadInt16();
				int num103 = binaryReader.ReadInt16();
				int num104 = binaryReader.ReadInt16();
				int num105 = binaryReader.ReadInt16();
				int num106 = binaryReader.ReadInt16();
				float num107 = binaryReader.ReadSingle();
				float num108 = binaryReader.ReadSingle();
				double num51 = (double)num102 * num16 + (double)num103 * num12 + (double)num105 * num14 + (double)num104 * num13 + (double)num106 * num15 + (double)num107;
				double num52 = ((double)num102 * num33 + (double)num103 * num29 + (double)num105 * num31 + (double)num104 * num30 + (double)num106 * num32 + (double)num107 - num51) / 5.0;
				for (int num109 = 0; num109 < 6; num109++)
				{
					array[num109] += (double)num108 * Math.Sin(num51 + (double)num109 * num52);
				}
			}
			for (int num110 = 1; num110 <= 64; num110++)
			{
				int num102 = binaryReader.ReadInt16();
				int num103 = binaryReader.ReadInt16();
				int num104 = binaryReader.ReadInt16();
				int num105 = binaryReader.ReadInt16();
				int num106 = binaryReader.ReadInt16();
				float num107 = binaryReader.ReadSingle();
				float num108 = binaryReader.ReadSingle();
				double num51 = (double)num102 * num16 + (double)num103 * num12 + (double)num105 * num14 + (double)num104 * num13 + (double)num106 * num15 + (double)num107;
				double num52 = ((double)num102 * num33 + (double)num103 * num29 + (double)num105 * num31 + (double)num104 * num30 + (double)num106 * num32 + (double)num107 - num51) / 5.0;
				for (int num111 = 0; num111 < 6; num111++)
				{
					array2[num111] += (double)num108 * Math.Sin(num51 + (double)num111 * num52);
				}
			}
			for (int num112 = 1; num112 <= 62; num112++)
			{
				int num102 = binaryReader.ReadInt16();
				int num103 = binaryReader.ReadInt16();
				int num104 = binaryReader.ReadInt16();
				int num105 = binaryReader.ReadInt16();
				int num106 = binaryReader.ReadInt16();
				float num107 = binaryReader.ReadSingle();
				float num108 = binaryReader.ReadSingle();
				double num51 = (double)num102 * num16 + (double)num103 * num12 + (double)num105 * num14 + (double)num104 * num13 + (double)num106 * num15 + (double)num107;
				double num52 = ((double)num102 * num33 + (double)num103 * num29 + (double)num105 * num31 + (double)num104 * num30 + (double)num106 * num32 + (double)num107 - num51) / 5.0;
				for (int num113 = 0; num113 < 6; num113++)
				{
					array3[num113] += (double)num108 * Math.Sin(num51 + (double)num113 * num52);
				}
			}
			for (int num114 = 1; num114 <= 1; num114++)
			{
				int num102 = binaryReader.ReadInt16();
				int num103 = binaryReader.ReadInt16();
				int num104 = binaryReader.ReadInt16();
				int num105 = binaryReader.ReadInt16();
				int num106 = binaryReader.ReadInt16();
				float num107 = binaryReader.ReadSingle();
				float num108 = binaryReader.ReadSingle();
				double num51 = (double)num102 * num16 + (double)num103 * num12 + (double)num105 * num14 + (double)num104 * num13 + (double)num106 * num15 + (double)num107;
				double num52 = ((double)num102 * num33 + (double)num103 * num29 + (double)num105 * num31 + (double)num104 * num30 + (double)num106 * num32 + (double)num107 - num51) / 5.0;
				for (int num115 = 0; num115 < 6; num115++)
				{
					array[num115] += num2 * (double)num108 * Math.Sin(num51 + (double)num115 * num52);
				}
			}
			for (int num116 = 1; num116 <= 1; num116++)
			{
				int num102 = binaryReader.ReadInt16();
				int num103 = binaryReader.ReadInt16();
				int num104 = binaryReader.ReadInt16();
				int num105 = binaryReader.ReadInt16();
				int num106 = binaryReader.ReadInt16();
				float num107 = binaryReader.ReadSingle();
				float num108 = binaryReader.ReadSingle();
				double num51 = (double)num102 * num16 + (double)num103 * num12 + (double)num105 * num14 + (double)num104 * num13 + (double)num106 * num15 + (double)num107;
				double num52 = ((double)num102 * num33 + (double)num103 * num29 + (double)num105 * num31 + (double)num104 * num30 + (double)num106 * num32 + (double)num107 - num51) / 5.0;
				for (int num117 = 0; num117 < 6; num117++)
				{
					array2[num117] += num2 * (double)num108 * Math.Sin(num51 + (double)num117 * num52);
				}
			}
			for (int num118 = 1; num118 <= 0; num118++)
			{
				int num102 = binaryReader.ReadInt16();
				int num103 = binaryReader.ReadInt16();
				int num104 = binaryReader.ReadInt16();
				int num105 = binaryReader.ReadInt16();
				int num106 = binaryReader.ReadInt16();
				float num107 = binaryReader.ReadSingle();
				float num108 = binaryReader.ReadSingle();
				double num51 = (double)num102 * num16 + (double)num103 * num12 + (double)num105 * num14 + (double)num104 * num13 + (double)num106 * num15 + (double)num107;
				double num52 = ((double)num102 * num33 + (double)num103 * num29 + (double)num105 * num31 + (double)num104 * num30 + (double)num106 * num32 + (double)num107 - num51) / 5.0;
				for (int num119 = 0; num119 < 6; num119++)
				{
					array3[num119] += num2 * (double)num108 * Math.Sin(num51 + (double)num119 * num52);
				}
			}
			for (int num120 = 1; num120 <= 1; num120++)
			{
				int num102 = binaryReader.ReadInt16();
				int num103 = binaryReader.ReadInt16();
				int num104 = binaryReader.ReadInt16();
				int num105 = binaryReader.ReadInt16();
				int num106 = binaryReader.ReadInt16();
				float num107 = binaryReader.ReadSingle();
				float num108 = binaryReader.ReadSingle();
				double num51 = (double)num102 * num16 + (double)num103 * num12 + (double)num105 * num14 + (double)num104 * num13 + (double)num106 * num15 + (double)num107;
				double num52 = ((double)num102 * num33 + (double)num103 * num29 + (double)num105 * num31 + (double)num104 * num30 + (double)num106 * num32 + (double)num107 - num51) / 5.0;
				for (int num121 = 0; num121 < 6; num121++)
				{
					array[num121] += (double)num108 * Math.Sin(num51 + (double)num121 * num52);
				}
			}
			for (int num122 = 1; num122 <= 0; num122++)
			{
				int num102 = binaryReader.ReadInt16();
				int num103 = binaryReader.ReadInt16();
				int num104 = binaryReader.ReadInt16();
				int num105 = binaryReader.ReadInt16();
				int num106 = binaryReader.ReadInt16();
				float num107 = binaryReader.ReadSingle();
				float num108 = binaryReader.ReadSingle();
				double num51 = (double)num102 * num16 + (double)num103 * num12 + (double)num105 * num14 + (double)num104 * num13 + (double)num106 * num15 + (double)num107;
				double num52 = ((double)num102 * num33 + (double)num103 * num29 + (double)num105 * num31 + (double)num104 * num30 + (double)num106 * num32 + (double)num107 - num51) / 5.0;
				for (int num123 = 0; num123 < 6; num123++)
				{
					array2[num123] += (double)num108 * Math.Sin(num51 + (double)num123 * num52);
				}
			}
			for (int num124 = 1; num124 <= 0; num124++)
			{
				int num102 = binaryReader.ReadInt16();
				int num103 = binaryReader.ReadInt16();
				int num104 = binaryReader.ReadInt16();
				int num105 = binaryReader.ReadInt16();
				int num106 = binaryReader.ReadInt16();
				float num107 = binaryReader.ReadSingle();
				float num108 = binaryReader.ReadSingle();
				double num51 = (double)num102 * num16 + (double)num103 * num12 + (double)num105 * num14 + (double)num104 * num13 + (double)num106 * num15 + (double)num107;
				double num52 = ((double)num102 * num33 + (double)num103 * num29 + (double)num105 * num31 + (double)num104 * num30 + (double)num106 * num32 + (double)num107 - num51) / 5.0;
				for (int num125 = 0; num125 < 6; num125++)
				{
					array3[num125] += (double)num108 * Math.Sin(num51 + (double)num125 * num52);
				}
			}
			for (int num126 = 1; num126 <= 1; num126++)
			{
				int num102 = binaryReader.ReadInt16();
				int num103 = binaryReader.ReadInt16();
				int num104 = binaryReader.ReadInt16();
				int num105 = binaryReader.ReadInt16();
				int num106 = binaryReader.ReadInt16();
				float num107 = binaryReader.ReadSingle();
				float num108 = binaryReader.ReadSingle();
				double num51 = (double)num102 * num16 + (double)num103 * num12 + (double)num105 * num14 + (double)num104 * num13 + (double)num106 * num15 + (double)num107;
				double num52 = ((double)num102 * num33 + (double)num103 * num29 + (double)num105 * num31 + (double)num104 * num30 + (double)num106 * num32 + (double)num107 - num51) / 5.0;
				for (int num127 = 0; num127 < 6; num127++)
				{
					array[num127] += num2 * (double)num108 * Math.Sin(num51 + (double)num127 * num52);
				}
			}
			for (int num128 = 1; num128 <= 0; num128++)
			{
				int num102 = binaryReader.ReadInt16();
				int num103 = binaryReader.ReadInt16();
				int num104 = binaryReader.ReadInt16();
				int num105 = binaryReader.ReadInt16();
				int num106 = binaryReader.ReadInt16();
				float num107 = binaryReader.ReadSingle();
				float num108 = binaryReader.ReadSingle();
				double num51 = (double)num102 * num16 + (double)num103 * num12 + (double)num105 * num14 + (double)num104 * num13 + (double)num106 * num15 + (double)num107;
				double num52 = ((double)num102 * num33 + (double)num103 * num29 + (double)num105 * num31 + (double)num104 * num30 + (double)num106 * num32 + (double)num107 - num51) / 5.0;
				for (int num129 = 0; num129 < 6; num129++)
				{
					array2[num129] += num2 * (double)num108 * Math.Sin(num51 + (double)num129 * num52);
				}
			}
			for (int num130 = 1; num130 <= 2; num130++)
			{
				int num102 = binaryReader.ReadInt16();
				int num103 = binaryReader.ReadInt16();
				int num104 = binaryReader.ReadInt16();
				int num105 = binaryReader.ReadInt16();
				int num106 = binaryReader.ReadInt16();
				float num107 = binaryReader.ReadSingle();
				float num108 = binaryReader.ReadSingle();
				double num51 = (double)num102 * num16 + (double)num103 * num12 + (double)num105 * num14 + (double)num104 * num13 + (double)num106 * num15 + (double)num107;
				double num52 = ((double)num102 * num33 + (double)num103 * num29 + (double)num105 * num31 + (double)num104 * num30 + (double)num106 * num32 + (double)num107 - num51) / 5.0;
				for (int num131 = 0; num131 < 6; num131++)
				{
					array3[num131] += num2 * (double)num108 * Math.Sin(num51 + (double)num131 * num52);
				}
			}
			for (int num132 = 1; num132 <= 1; num132++)
			{
				int num102 = binaryReader.ReadInt16();
				int num103 = binaryReader.ReadInt16();
				int num104 = binaryReader.ReadInt16();
				int num105 = binaryReader.ReadInt16();
				int num106 = binaryReader.ReadInt16();
				float num107 = binaryReader.ReadSingle();
				float num108 = binaryReader.ReadSingle();
				double num51 = (double)num102 * num16 + (double)num103 * num12 + (double)num105 * num14 + (double)num104 * num13 + (double)num106 * num15 + (double)num107;
				double num52 = ((double)num102 * num33 + (double)num103 * num29 + (double)num105 * num31 + (double)num104 * num30 + (double)num106 * num32 + (double)num107 - num51) / 5.0;
				for (int num133 = 0; num133 < 6; num133++)
				{
					array[num133] += (double)num108 * Math.Sin(num51 + (double)num133 * num52);
				}
			}
			for (int num134 = 1; num134 <= 0; num134++)
			{
				int num102 = binaryReader.ReadInt16();
				int num103 = binaryReader.ReadInt16();
				int num104 = binaryReader.ReadInt16();
				int num105 = binaryReader.ReadInt16();
				int num106 = binaryReader.ReadInt16();
				float num107 = binaryReader.ReadSingle();
				float num108 = binaryReader.ReadSingle();
				double num51 = (double)num102 * num16 + (double)num103 * num12 + (double)num105 * num14 + (double)num104 * num13 + (double)num106 * num15 + (double)num107;
				double num52 = ((double)num102 * num33 + (double)num103 * num29 + (double)num105 * num31 + (double)num104 * num30 + (double)num106 * num32 + (double)num107 - num51) / 5.0;
				for (int num135 = 0; num135 < 6; num135++)
				{
					array2[num135] += (double)num108 * Math.Sin(num51 + (double)num135 * num52);
				}
			}
			for (int num136 = 1; num136 <= 1; num136++)
			{
				int num102 = binaryReader.ReadInt16();
				int num103 = binaryReader.ReadInt16();
				int num104 = binaryReader.ReadInt16();
				int num105 = binaryReader.ReadInt16();
				int num106 = binaryReader.ReadInt16();
				float num107 = binaryReader.ReadSingle();
				float num108 = binaryReader.ReadSingle();
				double num51 = (double)num102 * num16 + (double)num103 * num12 + (double)num105 * num14 + (double)num104 * num13 + (double)num106 * num15 + (double)num107;
				double num52 = ((double)num102 * num33 + (double)num103 * num29 + (double)num105 * num31 + (double)num104 * num30 + (double)num106 * num32 + (double)num107 - num51) / 5.0;
				for (int num137 = 0; num137 < 6; num137++)
				{
					array3[num137] += (double)num108 * Math.Sin(num51 + (double)num137 * num52);
				}
			}
			for (int num138 = 1; num138 <= 2; num138++)
			{
				int num102 = binaryReader.ReadInt16();
				int num103 = binaryReader.ReadInt16();
				int num104 = binaryReader.ReadInt16();
				int num105 = binaryReader.ReadInt16();
				int num106 = binaryReader.ReadInt16();
				float num107 = binaryReader.ReadSingle();
				float num108 = binaryReader.ReadSingle();
				double num51 = (double)num102 * num16 + (double)num103 * num12 + (double)num105 * num14 + (double)num104 * num13 + (double)num106 * num15 + (double)num107;
				double num52 = ((double)num102 * num33 + (double)num103 * num29 + (double)num105 * num31 + (double)num104 * num30 + (double)num106 * num32 + (double)num107 - num51) / 5.0;
				for (int num139 = 0; num139 < 6; num139++)
				{
					array[num139] += (double)num108 * Math.Sin(num51 + (double)num139 * num52);
				}
			}
			for (int num140 = 1; num140 <= 0; num140++)
			{
				int num102 = binaryReader.ReadInt16();
				int num103 = binaryReader.ReadInt16();
				int num104 = binaryReader.ReadInt16();
				int num105 = binaryReader.ReadInt16();
				int num106 = binaryReader.ReadInt16();
				float num107 = binaryReader.ReadSingle();
				float num108 = binaryReader.ReadSingle();
				double num51 = (double)num102 * num16 + (double)num103 * num12 + (double)num105 * num14 + (double)num104 * num13 + (double)num106 * num15 + (double)num107;
				double num52 = ((double)num102 * num33 + (double)num103 * num29 + (double)num105 * num31 + (double)num104 * num30 + (double)num106 * num32 + (double)num107 - num51) / 5.0;
				for (int num141 = 0; num141 < 6; num141++)
				{
					array2[num141] += (double)num108 * Math.Sin(num51 + (double)num141 * num52);
				}
			}
			for (int num142 = 1; num142 <= 2; num142++)
			{
				int num102 = binaryReader.ReadInt16();
				int num103 = binaryReader.ReadInt16();
				int num104 = binaryReader.ReadInt16();
				int num105 = binaryReader.ReadInt16();
				int num106 = binaryReader.ReadInt16();
				float num107 = binaryReader.ReadSingle();
				float num108 = binaryReader.ReadSingle();
				double num51 = (double)num102 * num16 + (double)num103 * num12 + (double)num105 * num14 + (double)num104 * num13 + (double)num106 * num15 + (double)num107;
				double num52 = ((double)num102 * num33 + (double)num103 * num29 + (double)num105 * num31 + (double)num104 * num30 + (double)num106 * num32 + (double)num107 - num51) / 5.0;
				for (int num143 = 0; num143 < 6; num143++)
				{
					array3[num143] += (double)num108 * Math.Sin(num51 + (double)num143 * num52);
				}
			}
			for (int num144 = 1; num144 <= 5; num144++)
			{
				int num102 = binaryReader.ReadInt16();
				int num103 = binaryReader.ReadInt16();
				int num104 = binaryReader.ReadInt16();
				int num105 = binaryReader.ReadInt16();
				int num106 = binaryReader.ReadInt16();
				float num107 = binaryReader.ReadSingle();
				float num108 = binaryReader.ReadSingle();
				double num51 = (double)num102 * num16 + (double)num103 * num12 + (double)num105 * num14 + (double)num104 * num13 + (double)num106 * num15 + (double)num107;
				double num52 = ((double)num102 * num33 + (double)num103 * num29 + (double)num105 * num31 + (double)num104 * num30 + (double)num106 * num32 + (double)num107 - num51) / 5.0;
				for (int num145 = 0; num145 < 6; num145++)
				{
					array[num145] += num9 * (double)num108 * Math.Sin(num51 + (double)num145 * num52);
				}
			}
			for (int num146 = 1; num146 <= 0; num146++)
			{
				int num102 = binaryReader.ReadInt16();
				int num103 = binaryReader.ReadInt16();
				int num104 = binaryReader.ReadInt16();
				int num105 = binaryReader.ReadInt16();
				int num106 = binaryReader.ReadInt16();
				float num107 = binaryReader.ReadSingle();
				float num108 = binaryReader.ReadSingle();
				double num51 = (double)num102 * num16 + (double)num103 * num12 + (double)num105 * num14 + (double)num104 * num13 + (double)num106 * num15 + (double)num107;
				double num52 = ((double)num102 * num33 + (double)num103 * num29 + (double)num105 * num31 + (double)num104 * num30 + (double)num106 * num32 + (double)num107 - num51) / 5.0;
				for (int num147 = 0; num147 < 6; num147++)
				{
					array2[num147] += num9 * (double)num108 * Math.Sin(num51 + (double)num147 * num52);
				}
			}
			for (int num148 = 1; num148 <= 4; num148++)
			{
				int num102 = binaryReader.ReadInt16();
				int num103 = binaryReader.ReadInt16();
				int num104 = binaryReader.ReadInt16();
				int num105 = binaryReader.ReadInt16();
				int num106 = binaryReader.ReadInt16();
				float num107 = binaryReader.ReadSingle();
				float num108 = binaryReader.ReadSingle();
				double num51 = (double)num102 * num16 + (double)num103 * num12 + (double)num105 * num14 + (double)num104 * num13 + (double)num106 * num15 + (double)num107;
				double num52 = ((double)num102 * num33 + (double)num103 * num29 + (double)num105 * num31 + (double)num104 * num30 + (double)num106 * num32 + (double)num107 - num51) / 5.0;
				for (int num149 = 0; num149 < 6; num149++)
				{
					array3[num149] += num9 * (double)num108 * Math.Sin(num51 + (double)num149 * num52);
				}
			}
			fileStream.Close();
			FileStream fileStream2 = new FileStream(AppPath + "\\Resource Files\\nutation.bin", FileMode.Open, FileAccess.Read);
			BinaryReader binaryReader2 = new BinaryReader(fileStream2);
			for (int num150 = 1; num150 < 64; num150++)
			{
				int num151 = binaryReader2.ReadInt16();
				int num152 = binaryReader2.ReadInt16();
				int num153 = binaryReader2.ReadInt16();
				int num154 = binaryReader2.ReadInt16();
				int num155 = binaryReader2.ReadInt16();
				double num156 = binaryReader2.ReadDouble();
				float num157 = binaryReader2.ReadSingle();
				double num158 = binaryReader2.ReadDouble();
				int num159 = binaryReader2.ReadInt16();
				double num51 = (double)num151 * num12 + (double)num152 * num13 + (double)num153 * num14 + (double)num154 * num15 + (double)num155 * num19;
				double num52 = ((double)num151 * num29 + (double)num152 * num30 + (double)num153 * num31 + (double)num154 * num32 + (double)num155 * num36 - num51) / 5.0;
				for (int num160 = 0; num160 < 6; num160++)
				{
					array4[num160] += (num156 + (double)num157 * num2) * Math.Sin(num51 + (double)num160 * num52);
					array5[num160] += (num158 + (double)num159 * num2) * Math.Cos(num51 + (double)num160 * num52);
				}
			}
			fileStream2.Close();
			double[] array6 = new double[6];
			double[] array7 = new double[6];
			double[] array8 = new double[6];
			for (int num161 = 0; num161 < 6; num161++)
			{
				double num162 = 6378.137 / array3[num161];
				array8[num161] = Math.Atan(num162 / Math.Sqrt(1.0 - num162 * num162)) * (180.0 / Math.PI);
				double num163 = (num18 + (double)((float)num161 / 5f) * (num35 - num18) + array[num161] / 3600.0 + array4[num161] / 36000000.0) / (180.0 / Math.PI) + -4.772990690523381E-07 + 0.0 / (num4 + (double)num161 * num6) / 3600.0 / (180.0 / Math.PI);
				double num164 = array2[num161] / 3600.0 / (180.0 / Math.PI) + 0.0 / num4 / 3600.0 / (180.0 / Math.PI);
				double num165 = num20 + (double)((float)num161 / 5f) * (num37 - num20) + array5[num161] / 36000000.0 / (180.0 / Math.PI);
				double num166 = Math.Cos(num164) * Math.Cos(num163);
				double num167 = Math.Cos(num164) * Math.Sin(num163) * Math.Cos(num165) - Math.Sin(num164) * Math.Sin(num165);
				double num168 = Math.Sin(num164) * Math.Cos(num165) + Math.Cos(num164) * Math.Sin(num165) * Math.Sin(num163);
				array6[num161] = Math.Atan2(num167, num166) * (180.0 / Math.PI);
				if (array6[num161] < 0.0)
				{
					array6[num161] += 360.0;
				}
				if (num161 > 0 && array6[num161] < array6[0])
				{
					array6[num161] += 360.0;
				}
				array7[num161] = Math.Atan(num168 / Math.Sqrt(num166 * num166 + num167 * num167)) * (180.0 / Math.PI);
			}
			FileStream fileStream3 = new FileStream(AppPath + "\\Resource Files\\Moon\\Moon_" + Convert.ToString(Year) + ".bin", FileMode.OpenOrCreate, FileAccess.Write);
			long num169 = fileStream3.Length / MoonFileRecordLength;
			long num170 = Convert.ToInt32(JD_TT - num);
			if (num170 - num169 > 1)
			{
				StreamWriter streamWriter = new StreamWriter(fileStream3);
				for (long num171 = num169 + 1; num171 < num170; num171++)
				{
					fileStream3.Seek(num170 * MoonFileRecordLength, SeekOrigin.Begin);
					streamWriter.Write("".PadRight(MoonFileRecordLength, '\0'));
				}
			}
			BinaryWriter binaryWriter = new BinaryWriter(fileStream3);
			fileStream3.Seek(num170 * MoonFileRecordLength, SeekOrigin.Begin);
			Make_DailyMoonSeries(array6, 0, ref MSeries);
			for (int num172 = 0; num172 < 6; num172++)
			{
				binaryWriter.Write(MSeries[num172]);
			}
			Make_DailyMoonSeries(array7, 1, ref MSeries);
			for (int num173 = 0; num173 < 6; num173++)
			{
				binaryWriter.Write(MSeries[num173]);
			}
			Make_DailyMoonSeries(array8, 2, ref MSeries);
			for (int num174 = 0; num174 < 5; num174++)
			{
				binaryWriter.Write(MSeries[num174]);
			}
			short value = 87;
			short value2 = Convert.ToInt16(0.0 / num4 * 10000.0);
			short value3 = Convert.ToInt16(0.0 / num4 * 10000.0);
			binaryWriter.Write(value2);
			binaryWriter.Write(value3);
			binaryWriter.Write(value);
			fileStream3.Close();
		}

		private static void Make_DailyMoonSeries(double[] Arg, int Element, ref int[] MSeries)
		{
			double[,] array = new double[6, 7];
			double[] array2 = new double[7];
			int num = 0;
			if (Element == 2)
			{
				num = 1;
			}
			for (int i = 1; i <= 5 - num; i++)
			{
				for (int j = 1; j <= 5 - num; j++)
				{
					array[i, j] = Math.Pow(0.2 * (double)(i + num), j);
				}
				array[i, 6 - num] = Arg[i + num] - Arg[0];
			}
			for (int i = 1; i <= 5 - num; i++)
			{
				for (int j = i; j <= 5 - num; j++)
				{
					double num2 = array[j, i];
					for (int k = i; k <= 6 - num; k++)
					{
						array[j, k] /= num2;
						if (j > i)
						{
							array[j, k] -= array[i, k];
						}
					}
				}
			}
			for (int k = 5 - num; k >= 1; k--)
			{
				array2[k] = array[k, 6 - num];
				for (int num3 = 5 - num; num3 >= k; num3--)
				{
					if (num3 != k)
					{
						array2[k] -= array2[num3] * array[k, num3];
					}
				}
			}
			array2[0] = Arg[0];
			for (int i = 0; i <= 5 - num; i++)
			{
				if (Element < 2)
				{
					if (Element == 0 && i == 0)
					{
						MSeries[i] = Convert.ToInt32((array2[i] - 180.0) * 10000000.0);
					}
					else
					{
						MSeries[i] = Convert.ToInt32(array2[i] * 10000000.0);
					}
				}
				else
				{
					MSeries[i] = Convert.ToInt32(array2[i] * 100000000.0);
				}
			}
		}

		public static bool MoonDataExists(int Year, int DayInYear)
		{
			bool result = true;
			if (!File.Exists(AppPath + "\\Resource Files\\Moon\\Moon_" + Convert.ToString(Year) + ".bin"))
			{
				result = false;
			}
			else
			{
				FileStream fileStream = new FileStream(AppPath + "\\Resource Files\\Moon\\Moon_" + Convert.ToString(Year) + ".bin", FileMode.Open, FileAccess.Read);
				if (fileStream.Length < MoonFileRecordLength * (DayInYear + 1))
				{
					result = false;
				}
				else
				{
					BinaryReader binaryReader = new BinaryReader(fileStream);
					fileStream.Seek(MoonFileRecordLength * DayInYear, SeekOrigin.Begin);
					int num = binaryReader.ReadInt32();
					if (num < -1800000000 || num > 1800000000)
					{
						result = false;
					}
					int num2 = binaryReader.ReadInt32();
					if (num2 < 100000000 || num2 > 180000000)
					{
						result = false;
					}
				}
				fileStream.Close();
			}
			return result;
		}

		public static void MoonSeries_Retrieve(double JD_at_0Hrs, out double[] RA, out double[] Dec, out double[] Parallax, out float LongCorrection_arcsec1e4, out float LatCorrection_arcsec1e4, out int Source)
		{
			double day = 0.0;
			RA = new double[6];
			Dec = new double[6];
			Parallax = new double[6];
			int Year = 0;
			int Month = 0;
			double num = Math.Floor(JD_at_0Hrs) + 0.5;
			Date_from_JD(num, out Year, out Month, out day);
			double num2 = JD_from_Date(Year, 1, 1.0);
			int num3 = Convert.ToInt32(num - num2);
			if (!MoonDataExists(Year, num3))
			{
				Moon(num);
			}
			FileStream fileStream = new FileStream(AppPath + "\\Resource Files\\Moon\\Moon_" + Convert.ToString(Year) + ".bin", FileMode.Open, FileAccess.Read);
			BinaryReader binaryReader = new BinaryReader(fileStream);
			fileStream.Seek(MoonFileRecordLength * num3, SeekOrigin.Begin);
			for (int i = 0; i < 6; i++)
			{
				RA[i] = (double)binaryReader.ReadInt32() / 10000000.0;
			}
			RA[0] += 180.0;
			for (int j = 0; j < 6; j++)
			{
				Dec[j] = (double)binaryReader.ReadInt32() / 10000000.0;
			}
			for (int k = 0; k < 5; k++)
			{
				Parallax[k] = (double)binaryReader.ReadInt32() / 100000000.0;
			}
			LongCorrection_arcsec1e4 = binaryReader.ReadInt16();
			LatCorrection_arcsec1e4 = binaryReader.ReadInt16();
			Source = binaryReader.ReadInt16();
			fileStream.Close();
		}

		public static void MoonPositionFromSeries(double DayFraction, double[] RA, double[] Dec, double[] Par, out double RAMoon, out double DecMoon, out double ParMoon)
		{
			RAMoon = RA[0] + (RA[1] + (RA[2] + (RA[3] + (RA[4] + RA[5] * DayFraction) * DayFraction) * DayFraction) * DayFraction) * DayFraction;
			DecMoon = Dec[0] + (Dec[1] + (Dec[2] + (Dec[3] + (Dec[4] + Dec[5] * DayFraction) * DayFraction) * DayFraction) * DayFraction) * DayFraction;
			ParMoon = Par[0] + (Par[1] + (Par[2] + (Par[3] + (Par[4] + Par[5] * DayFraction) * DayFraction) * DayFraction) * DayFraction) * DayFraction;
		}

		public static void GeocentricMoon(double jdUT, out double RA, out double Dec, out double PiMoon, out double l, out double b, out double C)
		{
			TopocentricMoon(jdUT, 0.0, 0.0, -10000.0, Altitude_is_MSL: true, 0, out RA, out Dec, out var MoonRadius_Radians, out var _, out l, out b, out C, out var _);
			PiMoon = Math.Asin(Math.Sin(MoonRadius_Radians) / 0.2725076);
		}

		public static bool MoonPosition_Using_JPLDE_or_Series(double JD_TT, out double RAMoon, out double DecMoon, out double LunarDistance, out int Version)
		{
			double TrueEcliptic = (RAMoon = (DecMoon = 0.0));
			LunarDistance = 1.0;
			double x = 0.0;
			double y = 0.0;
			double z = 0.0;
			Version = 0;
			bool result = false;
			if (DE_EphemerisFileExists | DE_LongEphemerisFileExists)
			{
				result = JPL_DE.Lunar_Position(JD_TT, out x, out y, out z, out Version);
				XYZ_to_RA_Dec(x, y, z, out RAMoon, out DecMoon);
				LunarDistance = Math.Sqrt(x * x + y * y + z * z);
				Precession(2000, JD_TT, use2006values_Not1976: false, ref RAMoon, ref DecMoon, 0.0, 0.0);
				Nutation(JD_TT, out var Longitude_arcsec, out var Obliquity_arcsec, out TrueEcliptic);
				double num = MeanEclipticOfDate(JD_TT, Use1976Value: true);
				RAMoon += (Longitude_arcsec * (Math.Cos(num) + Math.Sin(num) * Math.Sin(RAMoon) * Math.Tan(DecMoon)) - Obliquity_arcsec * Math.Cos(RAMoon) * Math.Tan(DecMoon)) / 3600.0 / (180.0 / Math.PI);
				DecMoon += (Longitude_arcsec * Math.Sin(num) * Math.Cos(RAMoon) + Obliquity_arcsec * Math.Sin(RAMoon)) / 3600.0 / (180.0 / Math.PI);
			}
			else
			{
				double num2 = Math.Floor(JD_TT) + 0.5;
				double num3 = JD_TT - num2;
				if (num3 < 0.0)
				{
					num3 += 1.0;
					num2 -= 1.0;
				}
				if (num3 > 1.0)
				{
					num3 -= 1.0;
					num2 += 1.0;
				}
				Date_from_JD(num2, out var Year, out var _, out var _);
				double num4 = JD_from_Date(Year, 1, 1.0);
				int num5 = Convert.ToInt32(num2 - num4);
				if (!MoonDataExists(Year, num5))
				{
					Moon(num2);
				}
				FileStream fileStream = new FileStream(AppPath + "\\Resource Files\\Moon\\Moon_" + Convert.ToString(Year) + ".bin", FileMode.Open, FileAccess.Read);
				BinaryReader binaryReader = new BinaryReader(fileStream);
				fileStream.Seek(MoonFileRecordLength * num5, SeekOrigin.Begin);
				int num6 = binaryReader.ReadInt32();
				int num7 = binaryReader.ReadInt32();
				int num8 = binaryReader.ReadInt32();
				int num9 = binaryReader.ReadInt32();
				int num10 = binaryReader.ReadInt32();
				int num11 = binaryReader.ReadInt32();
				int num12 = binaryReader.ReadInt32();
				int num13 = binaryReader.ReadInt32();
				int num14 = binaryReader.ReadInt32();
				int num15 = binaryReader.ReadInt32();
				int num16 = binaryReader.ReadInt32();
				int num17 = binaryReader.ReadInt32();
				int num18 = binaryReader.ReadInt32();
				int num19 = binaryReader.ReadInt32();
				int num20 = binaryReader.ReadInt32();
				int num21 = binaryReader.ReadInt32();
				int num22 = binaryReader.ReadInt32();
				binaryReader.ReadInt16();
				binaryReader.ReadInt16();
				fileStream.Close();
				RAMoon = Math.PI + ((double)num6 + num3 * (double)num7 + num3 * num3 * (double)num8 + num3 * num3 * num3 * (double)num9 + num3 * num3 * num3 * num3 * (double)num10 + num3 * num3 * num3 * num3 * num3 * (double)num11) / 10000000.0 / (180.0 / Math.PI);
				DecMoon = ((double)num12 + num3 * (double)num13 + num3 * num3 * (double)num14 + num3 * num3 * num3 * (double)num15 + num3 * num3 * num3 * num3 * (double)num16 + num3 * num3 * num3 * num3 * num3 * (double)num17) / 10000000.0 / (180.0 / Math.PI);
				double a = ((double)num18 + num3 * (double)num19 + num3 * num3 * (double)num20 + num3 * num3 * num3 * (double)num21 + num3 * num3 * num3 * num3 * (double)num22) / 100000000.0 / (180.0 / Math.PI);
				LunarDistance = 6378.137 / Math.Sin(a);
			}
			return result;
		}

		public static void TopocentricMoon(double jdUT, double Longitude, double Latitude, double Elevation_metres, bool Altitude_is_MSL, int DatumNumber, out double RA, out double Dec, out double MoonRadius_Radians, out double MoonScale, out double l, out double b, out double C, out double MoonAlt_Deg)
		{
			double num = 0.0;
			double num2 = 0.0;
			double num3 = 0.0;
			double num4 = 0.0;
			double num5 = 0.0;
			double num6 = 0.0;
			EphemerisBasis(jdUT);
			double num7;
			RA = (Dec = (num7 = 0.0));
			MoonAlt_Deg = -100.0;
			double UT1UTC;
			double x;
			double y;
			double num8 = jdUT + delta_T(jdUT, out UT1UTC, out x, out y) / 86400.0;
			ConvertLatLong_forEOP(Longitude, Latitude, x, y, out var EOPLong, out var EOPLat);
			Longitude = EOPLong;
			Latitude = EOPLat;
			double RAMoon;
			double DecMoon;
			double d;
			if (Elevation_metres > -10000.0)
			{
				GetGeodetic(Latitude, Elevation_metres, Altitude_is_MSL, out var pCos, out var pSin, DatumNumber, ref Longitude);
				double num9 = SiderealTime_deg(jdUT + UT1UTC / 86400.0, Apparent: true) / (180.0 / Math.PI) + Longitude;
				double num10 = 1.0;
				for (int i = 0; i < 2; i++)
				{
					double num11;
					for (int j = 0; j < 2; j++)
					{
						MoonPosition_Using_JPLDE_or_Series(num8 + num6, out RAMoon, out DecMoon, out var LunarDistance, out var _);
						num7 = Math.Asin(6378.137 / LunarDistance);
						num4 = num9 - RAMoon;
						num = Math.Cos(DecMoon) * Math.Sin(num4);
						num2 = Math.Cos(DecMoon) * Math.Cos(num4) - num10 * pCos * Math.Sin(num7);
						num3 = Math.Sin(DecMoon) - num10 * pSin * Math.Sin(num7);
						Dec = Math.Atan(num3 / Math.Sqrt(num * num + num2 * num2));
						RA = num9 - Math.Atan2(num, num2);
						num11 = Math.Sin(Dec) * pSin + Math.Cos(Dec) * Math.Cos(num4) * pCos;
						if (num11 >= 1.0)
						{
							num11 = 0.99999;
						}
						num6 = 0.02127517497454856 * num11 / 86400.0;
					}
					if (num10 != 1.0)
					{
						break;
					}
					num11 = Math.Sin(Dec) * pSin + Math.Cos(Dec) * Math.Cos(num4) * pCos;
					num5 = Math.Asin(num11);
					if (num11 > 0.5)
					{
						break;
					}
					double num12 = Refraction_deg(num5 * (180.0 / Math.PI), 1016.0, 0.0) / (180.0 / Math.PI);
					num10 = 1.000292 * Math.Cos(num5 + num12) / Math.Cos(num5);
				}
				d = 0.2725076 * Math.Sin(num7 / Math.Sqrt(num * num + num2 * num2 + num3 * num3));
				_ = 6378.137 / Math.Sin(num7 / Math.Sqrt(num * num + num2 * num2 + num3 * num3));
				MoonAlt_Deg = num5 * (180.0 / Math.PI);
			}
			else
			{
				MoonPosition_Using_JPLDE_or_Series(num8, out RAMoon, out DecMoon, out var LunarDistance2, out var _);
				RA = RAMoon;
				Dec = DecMoon;
				d = 0.2725076 * (6378.137 / LunarDistance2);
			}
			MoonRadius_Radians = Math.Asin(d);
			MoonScale = 221.1778 * MoonRadius_Radians;
			while (RA > Math.PI * 2.0)
			{
				RA -= Math.PI * 2.0;
			}
			while (RA < 0.0)
			{
				RA += Math.PI * 2.0;
			}
			Librations(jdUT, RA, Dec, out l, out b, out C);
		}

		public static void QuickMoon(double JD_TT, out double RA, out double Dec, out double Parallax, out double MoonLongitude, out double MoonLatitude)
		{
			double num = (JD_TT - 2451545.0) / 36525.0;
			double num2 = (297.8502042 + 445267.1115168 * num - 0.00163 * num * num + 1.832E-06 * num * num * num) / (180.0 / Math.PI);
			double num3 = (93.2720993 + 483202.0175273 * num - 0.0034064 * num * num - 2.83E-07 * num * num * num) / (180.0 / Math.PI);
			double num4 = (357.5291092 + 35999.0502909 * num - 0.000156 * num * num - 4.08E-08 * num * num * num) / (180.0 / Math.PI);
			double num5 = (134.9634114 + 477198.8676313 * num + 0.0089937 * num * num + 1.435E-05 * num * num * num) / (180.0 / Math.PI);
			double num6 = (218.3164591 + 481267.88127722 * num - 0.0016356 * num * num + 1.832E-06 * num * num * num) % 360.0 / (180.0 / Math.PI);
			double num7 = MeanEclipticOfDate(JD_TT, Use1976Value: true);
			double num8 = 14.0 * Math.Sin(4.0 * num2) + 2369.0 * Math.Sin(2.0 * num2) + 192.0 * Math.Sin(num5 + 2.0 * num2) + 22639.0 * Math.Sin(num5) - 4586.0 * Math.Sin(num5 - 2.0 * num2) - 38.0 * Math.Sin(num5 - 4.0 * num2) - 24.0 * Math.Sin(num4 + 2.0 * num2) - 668.0 * Math.Sin(num4) - 165.0 * Math.Sin(num4 - 2.0 * num2) - 125.0 * Math.Sin(num2) + 14.0 * Math.Sin(2.0 * num5 + 2.0 * num2) + 769.0 * Math.Sin(2.0 * num5) - 211.0 * Math.Sin(2.0 * num5 - 2.0 * num2) - 30.0 * Math.Sin(2.0 * num5 - 4.0 * num2) - 110.0 * Math.Sin(num5 + num4) - 206.0 * Math.Sin(num5 + num4 - 2.0 * num2) + 15.0 * Math.Sin(num5 - num4 + 2.0 * num2) + 148.0 * Math.Sin(num5 - num4) + 28.0 * Math.Sin(num5 - num4 - 2.0 * num2) - 411.0 * Math.Sin(2.0 * num3) - 55.0 * Math.Sin(2.0 * num3 - 2.0 * num2) - 55.0 * Math.Sin(2.0 * num3 - 2.0 * num2) + 18.0 * Math.Sin(num5 - num2) - 18.0 * Math.Sin(num5 + num2) + 36.0 * Math.Sin(3.0 * num5) - 13.0 * Math.Sin(3.0 * num5 - 2.0 * num2) - 45.0 * Math.Sin(num5 + 2.0 * num3) + 39.0 * Math.Sin(num5 - 2.0 * num3);
			double num9 = num6 + num8 / 3600.0 / (180.0 / Math.PI);
			double num10 = 117.0 * Math.Sin(num3 + 2.0 * num2) + 18461.0 * Math.Sin(num3) - 624.0 * Math.Sin(num3 - 2.0 * num2) + 1010.0 * Math.Sin(num5 + num3) - 167.0 * Math.Sin(num5 + num3 - 2.0 * num2) + 199.0 * Math.Sin(num3 - num5 + 2.0 * num2) - 1000.0 * Math.Sin(num3 - num5) - 30.0 * Math.Sin(num4 + num3 - 2.0 * num2) + 62.0 * Math.Sin(2.0 * num5 + num3) - 15.0 * Math.Sin(2.0 * num5 + num3 - 2.0 * num2) - 32.0 * Math.Sin(-2.0 * num5 + num3);
			num10 = num10 / 3600.0 / (180.0 / Math.PI);
			Parallax = (3422.5 + 0.26 * Math.Cos(4.0 * num2) + 28.23 * Math.Cos(2.0 * num2) + 3.09 * Math.Cos(num5 + 2.0 * num2) + 186.54 * Math.Cos(num5) + 34.31 * Math.Cos(num5 - 2.0 * num2) + 0.6 * Math.Cos(num5 - 4.0 * num2) - 0.3 * Math.Cos(num4 + 2.0 * num2) - 0.4 * Math.Cos(num4) + 1.92 * Math.Cos(num4 - 2.0 * num2) - 0.98 * Math.Cos(num2) + 0.28 * Math.Cos(2.0 * num5 + 2.0 * num2) + 10.17 * Math.Cos(2.0 * num5) - 0.3 * Math.Cos(2.0 * num5 - 2.0 * num2) + 0.37 * Math.Cos(2.0 * num5 - 4.0 * num2) - 0.95 * Math.Cos(num5 + num4) + 1.44 * Math.Cos(num5 + num4 - 2.0 * num2) + 1.15 * Math.Cos(num5 - num4) + 0.62 * Math.Cos(3.0 * num5) - 0.71 * Math.Cos(num5 - 2.0 * num3)) / 3600.0 / (180.0 / Math.PI);
			double num11 = Math.Cos(num10) * Math.Sin(num9) * Math.Cos(num7) - Math.Sin(num10) * Math.Sin(num7);
			double num12 = Math.Cos(num10) * Math.Cos(num9);
			double num13 = Math.Sin(num10) * Math.Cos(num7) + Math.Cos(num10) * Math.Sin(num7) * Math.Sin(num9);
			RA = Math.Atan2(num11, num12);
			if (RA < 0.0)
			{
				RA += Math.PI * 2.0;
			}
			Dec = Math.Atan(num13 / Math.Sqrt(num11 * num11 + num12 * num12));
			MoonLongitude = num9;
			MoonLatitude = num10;
		}

		public static void Librations(double JD_TT, double MoonRA, double MoonDec, out double l_deg, out double b_deg, out double C_deg)
		{
			Librations(JD_TT, MoonRA, MoonDec, 0.0, GetEarthSelenographic: true, GetSunSelenographic: false, 0.0, 0.0, 1.0, out l_deg, out b_deg, out C_deg, out var _, out var _, out var _, out var _);
		}

		public static void Librations(double JD_TT, double MoonRA_Apparent, double MoonDec_Apparent, double PiMoon_Apparent, bool GetEarthSelenographic, bool GetSunSelenographic, double RASun, double DecSun, double RSun, out double l_deg, out double b_deg, out double C_deg, out double lSun_deg, out double bSun_deg, out double CSun_deg, out double SunMinusMoonLongitude)
		{
			double Obliquity_arcsec = (l_deg = (b_deg = (C_deg = (lSun_deg = (bSun_deg = (CSun_deg = (SunMinusMoonLongitude = 0.0)))))));
			double num = (JD_TT - 2451545.0) / 36525.0;
			double num2 = NormaliseDegrees(297.8502042 + 445267.1115168 * num - 0.0016335 * num * num + num * num * num / 546300.0);
			double num3 = NormaliseDegrees(93.2720993 + 483202.0175273 * num - 0.0034064 * num * num - num * num * num / 6550000.0);
			double num4 = NormaliseDegrees(357.5291092 + 35999.0502909 * num - 0.000156 * num * num - num * num * num / 2280000.0);
			double num5 = NormaliseDegrees(134.9634114 + 477198.8676313 * num + 0.0089937 * num * num + num * num * num / 73725.0);
			double num6 = NormaliseDegrees(NormaliseDegrees(218.3164591 + 481267.88134236 * num - 0.0013298 * num * num + num * num * num / 546300.0) - num3) / (180.0 / Math.PI);
			num3 = NormaliseDegrees(num3) / (180.0 / Math.PI);
			num2 /= 180.0 / Math.PI;
			num4 /= 180.0 / Math.PI;
			num5 /= 180.0 / Math.PI;
			double a = (119.75 + 131.849 * num) / (180.0 / Math.PI);
			double a2 = (72.56 + 20.186 * num) / (180.0 / Math.PI);
			double num7 = 1.0 - 0.002516 * num - 7.4E-06 * num * num;
			double num8 = -0.02752 * Math.Cos(num5) - 0.02245 * Math.Sin(num3) + 0.00684 * Math.Cos(num5 - 2.0 * num3) - 0.00293 * Math.Cos(2.0 * num3) - 0.00085 * Math.Cos(2.0 * num3 - 2.0 * num2) - 0.00054 * Math.Cos(num5 - 2.0 * num2) - 0.0002 * Math.Sin(num5 + num3) - 0.0002 * Math.Cos(num5 + 2.0 * num3) - 0.0002 * Math.Cos(num5 - num3) + 0.00014 * Math.Cos(num5 + 2.0 * num3 - 2.0 * num2);
			double num9 = -0.02816 * Math.Sin(num5) + 0.02244 * Math.Cos(num3) - 0.00682 * Math.Sin(num5 - 2.0 * num3) - 0.00279 * Math.Sin(2.0 * num3) - 0.00083 * Math.Sin(2.0 * num3 - 2.0 * num2) + 0.00069 * Math.Sin(num5 - 2.0 * num2) + 0.0004 * Math.Cos(num5 + num3) - 0.00025 * Math.Sin(2.0 * num5) - 0.00023 * Math.Sin(num5 + 2.0 * num3) + 0.0002 * Math.Cos(num5 - num3) + 0.00019 * Math.Sin(num5 - num3) + 0.00013 * Math.Sin(num5 + 2.0 * num3 - 2.0 * num2) - 0.0001 * Math.Cos(num5 - 3.0 * num3);
			double num10 = 0.0252 * num7 * Math.Sin(num4) + 0.00473 * Math.Sin(2.0 * num5 - 2.0 * num3) - 0.00467 * Math.Sin(num5) + 0.00396 * Math.Sin(a) + 0.00276 * Math.Sin(2.0 * num5 - 2.0 * num2) + 0.00196 * Math.Sin(num6) - 0.00183 * Math.Cos(num5 - num3) + 0.00115 * Math.Sin(num5 - 2.0 * num2) - 0.00096 * Math.Sin(num5 - num2) + 0.00046 * Math.Sin(2.0 * num3 - 2.0 * num2) - 0.00039 * Math.Sin(num5 - num3) - 0.00032 * Math.Sin(num5 - num4 - num2) + 0.00027 * Math.Sin(2.0 * num5 - num4 - 2.0 * num2) + 0.00023 * Math.Sin(a2) - 0.00014 * Math.Sin(2.0 * num2) + 0.00014 * Math.Cos(2.0 * num5 - 2.0 * num3) - 0.00012 * Math.Sin(num5 - 2.0 * num3) - 0.00012 * Math.Sin(2.0 * num5) + 0.00011 * Math.Sin(2.0 * num5 - 2.0 * num4 - 2.0 * num2);
			Nutation(JD_TT, out var Longitude_arcsec, out Obliquity_arcsec, out var TrueEcliptic);
			double num11 = Math.Cos(MoonDec_Apparent) * Math.Sin(MoonRA_Apparent) * Math.Cos(TrueEcliptic) + Math.Sin(MoonDec_Apparent) * Math.Sin(TrueEcliptic);
			double num12 = Math.Cos(MoonDec_Apparent) * Math.Cos(MoonRA_Apparent);
			double num13 = Math.Sin(MoonDec_Apparent) * Math.Cos(TrueEcliptic) - Math.Cos(MoonDec_Apparent) * Math.Sin(TrueEcliptic) * Math.Sin(MoonRA_Apparent);
			double num14 = Math.Atan2(num11, num12);
			double num15 = Math.Atan(num13 / Math.Sqrt(num11 * num11 + num12 * num12));
			if (GetEarthSelenographic)
			{
				double num16 = num14 - num6 - Longitude_arcsec / 3600.0 / (180.0 / Math.PI);
				num11 = Math.Cos(num16) * Math.Cos(num15);
				num12 = Math.Sin(num16) * Math.Cos(num15) * 0.99963767 - Math.Sin(num15) * 0.026916998;
				double d = (0.0 - Math.Sin(num16)) * Math.Cos(num15) * 0.026916998 - Math.Sin(num15) * 0.99963767;
				double num17 = Math.Atan2(num12, num11);
				double num18 = Math.Asin(d);
				for (l_deg = (num17 - num3) * (180.0 / Math.PI) - num10 + (num8 * Math.Cos(num17) + num9 * Math.Sin(num17)) * Math.Tan(num18); l_deg > 90.0; l_deg -= 360.0)
				{
				}
				while (l_deg < -90.0)
				{
					l_deg += 360.0;
				}
				b_deg = num18 * (180.0 / Math.PI) + num9 * Math.Cos(num17) - num8 * Math.Sin(num17);
				double num19 = num6 + (Longitude_arcsec / 3600.0 + num9 / 0.026916998) / (180.0 / Math.PI);
				double num20 = Math.Sin((1.5424167 + num8) / (180.0 / Math.PI)) * Math.Sin(num19);
				double num21 = Math.Sin((1.5424167 + num8) / (180.0 / Math.PI)) * Math.Cos(num19) * Math.Cos(TrueEcliptic) - Math.Cos((1.5424167 + num8) / (180.0 / Math.PI)) * Math.Sin(TrueEcliptic);
				double num22 = Math.Atan2(num20, num21);
				C_deg = Math.Asin(Math.Sqrt(num20 * num20 + num21 * num21) * Math.Cos(MoonRA_Apparent - num22) / Math.Cos(num18)) * (180.0 / Math.PI);
				if (C_deg < 0.0)
				{
					C_deg += 360.0;
				}
			}
			if (GetSunSelenographic)
			{
				num11 = Math.Cos(DecSun) * Math.Sin(RASun) * Math.Cos(TrueEcliptic) + Math.Sin(DecSun) * Math.Sin(TrueEcliptic);
				num12 = Math.Cos(DecSun) * Math.Cos(RASun);
				double num23 = Math.Atan2(num11, num12);
				SunMinusMoonLongitude = num23 - num14;
				double num24 = num23 + Math.PI + 4.2643E-05 / PiMoon_Apparent / RSun * Math.Sin(num23 - num14) * Math.Cos(num15);
				double num25 = 4.2643E-05 / PiMoon_Apparent / RSun * num15;
				double num26 = num24 - num6 - Longitude_arcsec / 3600.0 / (180.0 / Math.PI);
				num11 = Math.Cos(num26) * Math.Cos(num25);
				num12 = Math.Sin(num26) * Math.Cos(num25) * 0.99963767 - Math.Sin(num25) * 0.026916998;
				double d2 = (0.0 - Math.Sin(num26)) * Math.Cos(num25) * 0.026916998 - Math.Sin(num25) * 0.99963767;
				double num17 = Math.Atan2(num12, num11);
				double num18 = Math.Asin(d2);
				for (lSun_deg = (num17 - num3) * (180.0 / Math.PI) - num10 + (num8 * Math.Cos(num17) + num9 * Math.Sin(num17)) * Math.Tan(num18); lSun_deg > 90.0; lSun_deg -= 360.0)
				{
				}
				while (lSun_deg < -90.0)
				{
					lSun_deg += 360.0;
				}
				bSun_deg = num18 * (180.0 / Math.PI) + num9 * Math.Cos(num17) - num8 * Math.Sin(num17);
				double num19 = num6 + (Longitude_arcsec / 3600.0 + num9 / 0.026916998) / (180.0 / Math.PI);
				double num20 = Math.Sin((1.5424167 + num8) / (180.0 / Math.PI)) * Math.Sin(num19);
				double num21 = Math.Sin((1.5424167 + num8) / (180.0 / Math.PI)) * Math.Cos(num19) * Math.Cos(TrueEcliptic) - Math.Cos((1.5424167 + num8) / (180.0 / Math.PI)) * Math.Sin(TrueEcliptic);
				double num22 = Math.Atan2(num20, num21);
				num11 = Math.Cos(num25) * Math.Sin(num24) * Math.Cos(TrueEcliptic) - Math.Sin(num25) * Math.Sin(TrueEcliptic);
				num12 = Math.Cos(num25) * Math.Cos(num24);
				double num27 = Math.Atan2(num11, num12);
				CSun_deg = Math.Asin(Math.Sqrt(num20 * num20 + num21 * num21) * Math.Cos(num27 - num22) / Math.Cos(num18)) * (180.0 / Math.PI);
				if (CSun_deg < 0.0)
				{
					CSun_deg += 360.0;
				}
			}
		}

		public static void Librations_P_D(double L_deg, double B_deg, double AA_deg, out double P_deg, out double D_deg)
		{
			P_deg = AA_deg + 0.0175 * L_deg * B_deg * Math.Cos(AA_deg / (180.0 / Math.PI)) * Math.Cos(AA_deg / (180.0 / Math.PI)) + 0.0044 * (B_deg * B_deg - L_deg * L_deg) * Math.Sin(2.0 * AA_deg / (180.0 / Math.PI)) - 0.0045 * (B_deg * Math.Sin(AA_deg / (180.0 / Math.PI)) + L_deg * Math.Cos(AA_deg / (180.0 / Math.PI)));
			D_deg = L_deg * Math.Sin(AA_deg / (180.0 / Math.PI)) - B_deg * Math.Cos(AA_deg / (180.0 / Math.PI)) + 0.26;
		}

		public static void Convert_PD_toAA(double L_deg, double B_deg, double MoonRatioToMean, double P_deg, double D_deg, out double AA_deg, out double HeightAboveLimb, out double Dlimb_deg)
		{
			double num = 0.0 - Math.Sin(L_deg / (180.0 / Math.PI));
			double num2 = Math.Cos(L_deg / (180.0 / Math.PI));
			double num3 = 0.0 - Math.Sin(B_deg / (180.0 / Math.PI));
			double num4 = Math.Cos(B_deg / (180.0 / Math.PI));
			double num5 = 384398.55 / MoonRatioToMean;
			double num6 = Math.PI / 2.0 - Math.Asin(1738.0908063412 / num5);
			double num7 = 648000.0 / Math.PI * Math.Asin(1738.0908063412 / num5);
			double num8 = 1737.4 * Math.Cos(D_deg / (180.0 / Math.PI)) * Math.Cos(P_deg / (180.0 / Math.PI));
			double num9 = 1737.4 * Math.Cos(D_deg / (180.0 / Math.PI)) * Math.Sin(P_deg / (180.0 / Math.PI));
			double num10 = 1737.4 * Math.Sin(D_deg / (180.0 / Math.PI));
			double num11 = num8;
			double num12 = num9 * num2 - num10 * num;
			double num13 = num9 * num + num10 * num2;
			double num14 = num11 * num4 + num13 * num3;
			double num15 = num12;
			double num16 = (0.0 - num11) * num3 + num13 * num4;
			double x = num5 - num16;
			double y = Math.Sqrt(num14 * num14 + num15 * num15);
			Dlimb_deg = (num6 - Math.Atan2(y, num16)) * (180.0 / Math.PI);
			HeightAboveLimb = Math.Atan2(y, x) * (648000.0 / Math.PI) - num7;
			AA_deg = Math.Atan2(num15, num14) * (180.0 / Math.PI);
			if (AA_deg < 0.0)
			{
				AA_deg += 360.0;
			}
		}

		internal static double LOLA_LimbHeight_ActualDistance(double AxisAngle_deg, double Ldeg, double Bdeg, double Scale)
		{
			double SlopeBefore_Deg;
			double SlopeAfter_Deg;
			double P_Limb_deg;
			double D_Limb_deg;
			return LOLAHiRes.LimbHeight_Slope(AxisAngle_deg, Ldeg, Bdeg, Scale, IncludeSlope: false, WideSlope: false, out SlopeBefore_Deg, out SlopeAfter_Deg, out P_Limb_deg, out D_Limb_deg);
		}

		public static bool GetGeoidDistances(double Long1, double Lat1, double Long2, double Lat2, out double DistanceKm)
		{
			double AZ;
			double AZ2;
			return GetGeoidDistances(Long1, Lat1, Long2, Lat2, out DistanceKm, out AZ, out AZ2);
		}

		public static bool GetGeoidDistances(double Long1, double Lat1, double Long2, double Lat2, out double DistanceKm, out double AZ1, out double AZ2)
		{
			bool result = false;
			AZ1 = (AZ2 = 0.0);
			DistanceKm = 0.0;
			double num = 6378.137;
			double num2 = 0.0033528106647474805;
			double num3 = (1.0 - num2) * num;
			double num4 = Long2 - Long1;
			if (num4 > Math.PI)
			{
				num4 = Math.PI * 2.0 - num4;
			}
			if (num4 < -Math.PI)
			{
				num4 = Math.PI * 2.0 + num4;
			}
			double num5 = (1.0 - num2) * Math.Tan(Lat1);
			double num6 = (1.0 - num2) * Math.Tan(Lat2);
			double num7 = 1.0 / Math.Sqrt(1.0 + num5 * num5);
			double num8 = num5 * num7;
			double num9 = 1.0 / Math.Sqrt(1.0 + num6 * num6);
			double num10 = num6 * num9;
			int num11 = 0;
			double num12 = num4;
			double num13;
			double num14;
			double num16;
			double num17;
			double num18;
			double num19;
			double num21;
			double num22;
			do
			{
				num13 = Math.Sin(num12);
				num14 = Math.Cos(num12);
				double num15 = num9 * num13;
				num16 = num7 * num10 - num8 * num9 * num14;
				num17 = Math.Sqrt(num15 * num15 + num16 * num16);
				if (num17 == 0.0)
				{
					return true;
				}
				num18 = num8 * num10 + num7 * num9 * num14;
				num19 = Math.Atan2(num17, num18);
				double num20 = num7 * num9 * num13 / num17;
				num21 = 1.0 - num20 * num20;
				num22 = ((num21 == 0.0) ? 0.0 : (num18 - 2.0 * num8 * num10 / num21));
				double num23 = num2 / 16.0 * num21 * (4.0 + num2 * (4.0 - 3.0 * num21));
				double num24 = num12;
				num12 = num4 + (1.0 - num23) * num2 * num20 * (num19 + num23 * num17 * (num22 + num23 * num18 * (-1.0 + 2.0 * num22)));
				if (Math.Abs(num12 - num24) < 2E-10)
				{
					result = true;
					break;
				}
				num11++;
			}
			while (num11 < 100);
			double num25 = num21 * (num * num - num3 * num3) / (num3 * num3);
			double num26 = 1.0 + num25 / 16384.0 * (4096.0 + num25 * (-768.0 + num25 * (320.0 - 175.0 * num25)));
			double num27 = num25 / 1024.0 * (256.0 + num25 * (-128.0 + num25 * (74.0 - 47.0 * num25)));
			double num28 = num27 * num17 * (num22 + num27 / 4.0 * (num18 * (-1.0 + 2.0 * num22) - num27 / 6.0 * num22 * (-3.0 + 4.0 * num17 * num17) * (-3.0 + 4.0 * num22 * num22)));
			DistanceKm = num3 * num26 * (num19 - num28);
			AZ1 = Math.Atan2(num9 * num13, num16);
			AZ2 = Math.Atan2(num7 * num13, (0.0 - num8) * num9 + num7 * num10 * num14) - Math.PI;
			if (AZ1 < 0.0)
			{
				AZ1 += Math.PI * 2.0;
			}
			if (AZ2 < 0.0)
			{
				AZ2 += Math.PI * 2.0;
			}
			return result;
		}

		public static void GetGeoidLocation(double LongStart, double LatStart, double DistanceKM, double Azimuth, out double LongEnd, out double LatEnd)
		{
			double num = 6378.137;
			double num2 = 0.0033528106647474805;
			double num3 = (1.0 - num2) * num;
			double num4 = Math.Sin(Azimuth);
			double num5 = Math.Cos(Azimuth);
			double num6 = (1.0 - num2) * Math.Tan(LatStart);
			double num7 = 1.0 / Math.Sqrt(1.0 + num6 * num6);
			double num8 = num6 * num7;
			double num9 = Math.Atan2(num6, num5);
			double num10 = num7 * num4;
			double num11 = 1.0 - num10 * num10;
			double num12 = num11 * (num * num - num3 * num3) / (num3 * num3);
			double num13 = 1.0 + num12 / 16384.0 * (4096.0 + num12 * (-768.0 + num12 * (320.0 - 175.0 * num12)));
			double num14 = num12 / 1024.0 * (256.0 + num12 * (-128.0 + num12 * (74.0 - 47.0 * num12)));
			double num15 = DistanceKM / (num3 * num13);
			int num16 = 0;
			double num17;
			double num18;
			double num19;
			double num21;
			do
			{
				num17 = Math.Cos(2.0 * num9 + num15);
				num18 = Math.Sin(num15);
				num19 = Math.Cos(num15);
				double num20 = num14 * num18 * (num17 + num14 / 4.0 * (num19 * (-1.0 + 2.0 * num17 * num17) - num14 / 6.0 * num17 * (-3.0 + 4.0 * num18 * num18) * (-3.0 + 4.0 * num17 * num17)));
				num21 = num15;
				num15 = DistanceKM / (num3 * num13) + num20;
				num16++;
			}
			while (num16 <= 100 && Math.Abs(num21 - num15) > 1E-12);
			double num22 = num8 * num18 - num7 * num19 * num5;
			LatEnd = Math.Atan2(num8 * num19 + num7 * num18 * num5, (1.0 - num2) * Math.Sqrt(num10 * num10 + num22 * num22));
			double num23 = Math.Atan2(num18 * num4, num7 * num19 - num8 * num18 * num5);
			double num24 = num2 / 16.0 * num11 * (4.0 + num2 * (4.0 - 3.0 * num11));
			LongEnd = LongStart + num23 - (1.0 - num24) * num2 * num10 * (num15 + num24 * num18 * (num17 + num24 * num19 * (-1.0 + 2.0 * num17 * num17)));
		}

		public static void GetDatumParameters(int DatumFlag, out double X, out double Y, out double Z, out double f, out double EarthRadius, out string DatumName)
		{
			switch (DatumFlag)
			{
			case 0:
				f = 0.0033528106647474805;
				EarthRadius = 6378137.0;
				X = 0.0;
				Y = 0.0;
				Z = 0.0;
				DatumName = "WGS84";
				break;
			case 10:
				f = 0.0033528106647474805;
				EarthRadius = 6378137.0;
				X = 0.0;
				Y = 0.0;
				Z = 0.0;
				DatumName = "WGS84 - GoogleEarth";
				break;
			case 12:
				f = 1.0 / 297.0;
				EarthRadius = 6378388.0;
				X = 0.0;
				Y = 0.0;
				Z = 0.0;
				DatumName = "Christmas Isl Astro 1967";
				break;
			case 13:
				f = 1.0 / 297.0;
				EarthRadius = 6378388.0;
				X = -134.0;
				Y = 229.0;
				Z = -29.0;
				DatumName = "Chua Astro (Brazil)";
				break;
			case 14:
				f = 1.0 / 297.0;
				EarthRadius = 6378388.0;
				X = -206.0;
				Y = 172.0;
				Z = -6.0;
				DatumName = "Corrego Alegre (Brazil)";
				break;
			case 15:
				f = 1.0 / 297.0;
				EarthRadius = 6378388.0;
				X = 211.0;
				Y = 147.0;
				Z = 111.0;
				DatumName = "Easter Island Astro 1967";
				break;
			case 16:
				f = 1.0 / 297.0;
				EarthRadius = 6378388.0;
				X = -87.0;
				Y = -98.0;
				Z = -121.0;
				DatumName = "ED/ED1950/ED1979";
				break;
			case 17:
				f = 1.0 / 297.0;
				EarthRadius = 6378388.0;
				X = -104.0;
				Y = 167.0;
				Z = -38.0;
				DatumName = "Graciosa Island (Azores)";
				break;
			case 18:
				f = 1.0 / 297.0;
				EarthRadius = 6378388.0;
				X = 230.0;
				Y = -199.0;
				Z = -752.0;
				DatumName = "Gizo, Provisional DOS";
				break;
			case 19:
				f = 0.0033900753040885176;
				EarthRadius = 6378206.4;
				X = -100.0;
				Y = -248.0;
				Z = 259.0;
				DatumName = "Guam";
				break;
			case 20:
				f = 1.0 / 297.0;
				EarthRadius = 6378388.0;
				X = 0.0;
				Y = 0.0;
				Z = 0.0;
				DatumName = "Heard Astro 1969";
				break;
			case 21:
				f = 0.0033900753040885176;
				EarthRadius = 6378206.4;
				X = 0.0;
				Y = 0.0;
				Z = 0.0;
				DatumName = "Iben Astro, Navy 1947 (Truk)";
				break;
			case 22:
				f = 0.003324449296662885;
				EarthRadius = 6377276.345;
				X = 295.0;
				Y = 736.0;
				Z = 257.0;
				DatumName = "Indian";
				break;
			case 23:
				f = 0.0033900753040885176;
				EarthRadius = 6378206.4;
				X = 0.0;
				Y = 0.0;
				Z = 0.0;
				DatumName = "Isla Socorro Astro";
				break;
			case 24:
				f = 1.0 / 297.0;
				EarthRadius = 6378388.0;
				X = 189.0;
				Y = -79.0;
				Z = -202.0;
				DatumName = "Johnston Island 1961";
				break;
			case 25:
				f = 1.0 / 297.0;
				EarthRadius = 6378388.0;
				X = 647.0;
				Y = 1777.0;
				Z = 1124.0;
				DatumName = "Kusaie Astro 1962, 1965";
				break;
			case 26:
				f = 0.0033900753040885176;
				EarthRadius = 6378206.4;
				X = -133.0;
				Y = -77.0;
				Z = -51.0;
				DatumName = "Luzon 1911 (Philippines)";
				break;
			case 27:
				f = 1.0 / 297.0;
				EarthRadius = 6378388.0;
				X = 403.0;
				Y = -81.0;
				Z = 277.0;
				DatumName = "Midway Astro 1961";
				break;
			case 28:
				f = 1.0 / 297.0;
				EarthRadius = 6378388.0;
				X = 84.0;
				Y = -22.0;
				Z = 209.0;
				DatumName = "New Zealand 1949";
				break;
			case 29:
				f = 0.0033900753040885176;
				EarthRadius = 6378206.4;
				X = -8.0;
				Y = 160.0;
				Z = 176.0;
				DatumName = "NAD27/NAD1927";
				break;
			case 30:
				f = 0.0033900753040885176;
				EarthRadius = 6378206.4;
				X = -2.0;
				Y = 151.0;
				Z = 181.0;
				DatumName = "Cape Canaveral";
				break;
			case 31:
				f = 0.0033900753040885176;
				EarthRadius = 6378206.4;
				X = -2.0;
				Y = 151.0;
				Z = 181.0;
				DatumName = "White Sands*";
				break;
			case 32:
				f = 0.003342773182174806;
				EarthRadius = 6377397.155;
				X = 0.0;
				Y = 0.0;
				Z = 0.0;
				DatumName = "Old Bavarian*";
				break;
			case 33:
				f = 0.0033900753040885176;
				EarthRadius = 6378206.4;
				X = 61.0;
				Y = -285.0;
				Z = -181.0;
				DatumName = "Old Hawaiian";
				break;
			case 34:
				f = 0.0033408506414970775;
				EarthRadius = 6377563.396;
				X = 375.0;
				Y = -111.0;
				Z = 431.0;
				DatumName = "ORDN1936/GB1936";
				break;
			case 35:
				f = 1.0 / 297.0;
				EarthRadius = 6378388.0;
				X = -307.0;
				Y = -92.0;
				Z = 127.0;
				DatumName = "Pico de las Nieves (Canaries)";
				break;
			case 36:
				f = 1.0 / 297.0;
				EarthRadius = 6378388.0;
				X = 185.0;
				Y = 165.0;
				Z = 42.0;
				DatumName = "Pitcairn Island Astro";
				break;
			case 37:
				f = 0.003342773182174806;
				EarthRadius = 6377397.155;
				X = 591.0;
				Y = 81.0;
				Z = 396.0;
				DatumName = "Potsdam";
				break;
			case 38:
				f = 1.0 / 297.0;
				EarthRadius = 6378388.0;
				X = -288.0;
				Y = 175.0;
				Z = -376.0;
				DatumName = "Provisional South American 1956";
				break;
			case 39:
				f = 1.0 / 297.0;
				EarthRadius = 6378388.0;
				X = 16.0;
				Y = 196.0;
				Z = 93.0;
				DatumName = "Provisional South Chile 1963";
				break;
			case 40:
				f = 0.003352329869259135;
				EarthRadius = 6378245.0;
				X = 28.0;
				Y = -130.0;
				Z = -95.0;
				DatumName = "Pulkovo 1942";
				break;
			case 41:
				f = 0.003352891869237217;
				EarthRadius = 6378160.0;
				X = -57.0;
				Y = 1.0;
				Z = -41.0;
				DatumName = "SAM1969/SA69";
				break;
			case 42:
				f = 0.003407561378699334;
				EarthRadius = 6378249.145;
				X = 41.0;
				Y = -220.0;
				Z = -134.0;
				DatumName = "Southeast Island (Mahe)";
				break;
			case 43:
				f = 1.0 / 297.0;
				EarthRadius = 6378388.0;
				X = -794.0;
				Y = 119.0;
				Z = -298.0;
				DatumName = "South Georgia Astro";
				break;
			case 44:
				f = 1.0 / 297.0;
				EarthRadius = 6378388.0;
				X = 0.0;
				Y = 0.0;
				Z = 0.0;
				DatumName = "Swallow Islands (Solomons)";
				break;
			case 45:
				f = 1.0 / 297.0;
				EarthRadius = 6378388.0;
				X = -189.0;
				Y = -242.0;
				Z = -91.0;
				DatumName = "Tananarive";
				break;
			case 46:
				f = 0.003342773182174806;
				EarthRadius = 6377397.155;
				X = -148.0;
				Y = 507.0;
				Z = 685.0;
				DatumName = "Tokyo/TD";
				break;
			case 47:
				f = 1.0 / 297.0;
				EarthRadius = 6378388.0;
				X = -632.0;
				Y = 438.0;
				Z = -609.0;
				DatumName = "Tristan Astro 1968";
				break;
			case 48:
				f = 0.003407561378699334;
				EarthRadius = 6378249.145;
				X = 51.0;
				Y = 391.0;
				Z = -36.0;
				DatumName = "Viti Levu 1916 (Fiji)";
				break;
			case 49:
				f = 1.0 / 297.0;
				EarthRadius = 6378388.0;
				X = 276.0;
				Y = -57.0;
				Z = 149.0;
				DatumName = "Wake island Astro 1952";
				break;
			case 50:
				f = 0.003407561378699334;
				EarthRadius = 6378249.145;
				X = 0.0;
				Y = 0.0;
				Z = 0.0;
				DatumName = "Yof Astro 1967 (Dakar)*";
				break;
			case 51:
				f = 1.0 / 297.0;
				EarthRadius = 6378388.0;
				X = -104.0;
				Y = -129.0;
				Z = 239.0;
				DatumName = "Palmer Astro 1969 (Antarctica)";
				break;
			case 52:
				f = 1.0 / 297.0;
				EarthRadius = 6378388.0;
				X = -127.0;
				Y = -769.0;
				Z = 472.0;
				DatumName = "Efate (New Hebrides)";
				break;
			case 53:
				f = 1.0 / 297.0;
				EarthRadius = 6378388.0;
				X = 124.0;
				Y = -234.0;
				Z = -25.0;
				DatumName = "Marcus Island 1965";
				break;
			case 54:
				f = 1.0 / 297.0;
				EarthRadius = 6378388.0;
				X = 298.0;
				Y = -304.0;
				Z = -375.0;
				DatumName = "Canton Astro 1966";
				break;
			case 56:
				f = 0.0033900753040885176;
				EarthRadius = 6378206.4;
				X = 531.0;
				Y = 516.0;
				Z = 216.0;
				DatumName = "Yap Island*";
				break;
			case 58:
				f = 1.0 / 297.0;
				EarthRadius = 6378388.0;
				X = -288.0;
				Y = 175.0;
				Z = -376.0;
				DatumName = "Kourou (French Guiana)";
				break;
			case 59:
				f = 0.0033408506414970775;
				EarthRadius = 6377563.396;
				X = 0.0;
				Y = 0.0;
				Z = 0.0;
				DatumName = "Ordnance Survey of Great Britain 1970 *";
				break;
			case 60:
				f = 1.0 / 297.0;
				EarthRadius = 6378388.0;
				X = 164.0;
				Y = 138.0;
				Z = -189.0;
				DatumName = "Qornoq (Greenland)";
				break;
			case 61:
				f = 0.003407561378699334;
				EarthRadius = 6378249.145;
				X = -166.0;
				Y = -15.0;
				Z = 204.0;
				DatumName = "Adindan (Ethiopia)";
				break;
			case 62:
				f = 0.0033900753040885176;
				EarthRadius = 6378206.4;
				X = -115.0;
				Y = 118.0;
				Z = 426.0;
				DatumName = "American Samoa 1962";
				break;
			case 63:
				f = 0.003407561378699334;
				EarthRadius = 6378249.145;
				X = -136.0;
				Y = -108.0;
				Z = -292.0;
				DatumName = "Arc-Cape/Cape (South Africa)";
				break;
			case 64:
				f = 1.0 / 297.0;
				EarthRadius = 6378388.0;
				X = -148.0;
				Y = 136.0;
				Z = 90.0;
				DatumName = "Campo Inchauspe (Argentine)";
				break;
			case 65:
				f = 1.0 / 297.0;
				EarthRadius = 6378388.0;
				X = -205.0;
				Y = 107.0;
				Z = 53.0;
				DatumName = "Ascension Island 1958";
				break;
			case 66:
				f = 0.003352891869237217;
				EarthRadius = 6378160.0;
				X = -133.0;
				Y = -48.0;
				Z = 148.0;
				DatumName = "Australian Geodetic/AU1966";
				break;
			case 67:
				f = 0.0033900753040885176;
				EarthRadius = 6378206.4;
				X = -73.0;
				Y = -213.0;
				Z = 296.0;
				DatumName = "Bermuda 1957";
				break;
			case 68:
				f = 0.003342773182174806;
				EarthRadius = 6377397.155;
				X = 0.0;
				Y = 0.0;
				Z = 0.0;
				DatumName = "Berne 1898*";
				break;
			case 69:
				f = 1.0 / 297.0;
				EarthRadius = 6378388.0;
				X = 0.0;
				Y = 0.0;
				Z = 0.0;
				DatumName = "Betio Island 1966*";
				break;
			case 70:
				f = 1.0 / 297.0;
				EarthRadius = 6378388.0;
				X = -104.0;
				Y = -129.0;
				Z = 239.0;
				DatumName = "Camp Area Astro 1961-62 USGS";
				break;
			case 71:
				f = 0.003342773182174806;
				EarthRadius = 6377397.155;
				X = -377.0;
				Y = 681.0;
				Z = -50.0;
				DatumName = "Batavia (Java)";
				break;
			case 72:
				f = 1.0 / 297.0;
				EarthRadius = 6378388.0;
				X = -103.0;
				Y = -106.0;
				Z = -141.0;
				DatumName = "Palestine (Israel, Jordan)";
				break;
			case 73:
				f = 0.003342773182174806;
				EarthRadius = 6377397.155;
				X = 682.0;
				Y = -203.0;
				Z = 480.0;
				DatumName = "Hermannskogel (Austria, Czech, Yugoslavia)";
				break;
			case 74:
				f = 0.003324449296662885;
				EarthRadius = 6377276.345;
				X = -97.0;
				Y = 787.0;
				Z = 86.0;
				DatumName = "Kandawala (Sri Lanka)";
				break;
			case 80:
				f = 0.0033528106647474805;
				EarthRadius = 6378137.0;
				X = 0.0;
				Y = 0.0;
				Z = 0.0;
				DatumName = "ETRS89 (European Terrestial Reference System)";
				break;
			case 81:
				f = 0.003342773182174806;
				EarthRadius = 6377397.155;
				X = 593.0;
				Y = 26.0;
				Z = 479.0;
				DatumName = "Amersfoort 1885 (Netherlands)";
				break;
			case 82:
				f = 0.0033528106647474805;
				EarthRadius = 6378137.0;
				X = 0.0;
				Y = 0.0;
				Z = 0.0;
				DatumName = "NAD83/NAD1983";
				break;
			case 84:
				f = 0.0033528106647474805;
				EarthRadius = 6378137.0;
				X = 0.0;
				Y = 0.0;
				Z = 0.0;
				DatumName = "WGS84";
				break;
			case 85:
				f = 0.0033528106647474805;
				EarthRadius = 6378137.0;
				X = 0.0;
				Y = 0.0;
				Z = 0.0;
				DatumName = "JGD2000";
				break;
			case 86:
				f = 0.0033528106647474805;
				EarthRadius = 6378137.0;
				X = 0.0;
				Y = 0.0;
				Z = 0.0;
				DatumName = "GDA94";
				break;
			case 87:
				f = 0.0033528106647474805;
				EarthRadius = 6378137.0;
				X = 0.0;
				Y = 0.0;
				Z = 0.0;
				DatumName = "NZGD2000";
				break;
			case 88:
				f = 0.0033528106647474805;
				EarthRadius = 6378137.0;
				X = 0.0;
				Y = 0.0;
				Z = 0.0;
				DatumName = "NGRF2000";
				break;
			case 89:
				f = 0.0033528106647474805;
				EarthRadius = 6378137.0;
				X = 0.0;
				Y = 0.0;
				Z = 0.0;
				DatumName = "KDG2000";
				break;
			case 90:
				f = 0.0033528106647474805;
				EarthRadius = 6378137.0;
				X = 0.0;
				Y = 0.0;
				Z = 0.0;
				DatumName = "Hartebeesthoek94";
				break;
			case 91:
				f = 0.0033528106647474805;
				EarthRadius = 6378137.0;
				X = 0.0;
				Y = 0.0;
				Z = 0.0;
				DatumName = "TWD94";
				break;
			default:
				f = 0.0033528106647474805;
				EarthRadius = 6378137.0;
				X = 0.0;
				Y = 0.0;
				Z = 0.0;
				DatumName = "No datum";
				break;
			}
		}

		public static void GetDatumToWGS84Corrections(int DatumFlag, double Longitude, double Latitude, double Altitude_metres, out double DLongitude_arcsec, out double DLatitude_arcsec)
		{
			GetDatumParameters(DatumFlag, out var X, out var Y, out var Z, out var f, out var EarthRadius, out var _);
			double num = 0.0033528106647474805 - f;
			double num2 = Math.Sqrt(2.0 * f - f * f);
			double num3 = 6378137.0 - EarthRadius;
			double num4 = 1.0 - f;
			double num5 = Math.Sin(Latitude) * Math.Cos(Latitude);
			double num6 = EarthRadius * (1.0 - num2 * num2) / Math.Pow(1.0 - num2 * Math.Sin(Latitude) * num2 * Math.Sin(Latitude), 1.5);
			double num7 = EarthRadius / Math.Pow(1.0 - num2 * Math.Sin(Latitude) * num2 * Math.Sin(Latitude), 0.5);
			DLatitude_arcsec = ((0.0 - X) * Math.Sin(Latitude) * Math.Cos(Longitude) - Y * Math.Sin(Latitude) * Math.Sin(Longitude) + Z * Math.Cos(Latitude) + num3 * num7 * num2 * num2 * num5 / EarthRadius + num * (num6 / num4 + num7 * num4) * num5) / (num6 + Altitude_metres) / 4.8481E-06;
			DLongitude_arcsec = ((0.0 - X) * Math.Sin(Longitude) + Y * Math.Cos(Longitude)) / (num7 + Altitude_metres) / Math.Cos(Latitude) / 4.8481E-06;
		}

		public static void GetGeodetic(double Latitude, double Altitude_metres, bool Altitude_is_MSL, out double pCos, out double pSin, int DatumNumber, ref double ObserverLongitude)
		{
			GetDatumToWGS84Corrections(DatumNumber, ObserverLongitude, Latitude, Altitude_metres, out var DLongitude_arcsec, out var DLatitude_arcsec);
			ObserverLongitude += DLongitude_arcsec / 3600.0 / (180.0 / Math.PI);
			Latitude += DLatitude_arcsec / 3600.0 / (180.0 / Math.PI);
			double num = 0.0;
			if (Altitude_is_MSL)
			{
				num = GeoidHeight(ObserverLongitude * (180.0 / Math.PI), Latitude * (180.0 / Math.PI));
			}
			double num2 = 1.0 / Math.Sqrt(Math.Cos(Latitude) * Math.Cos(Latitude) + 0.9933056200098587 * Math.Sin(Latitude) * Math.Sin(Latitude));
			double num3 = 0.9933056200098587 * num2;
			pCos = Math.Cos(Latitude) * (num2 + (Altitude_metres + num) / 6378137.0);
			pSin = Math.Sin(Latitude) * (num3 + (Altitude_metres + num) / 6378137.0);
		}

		public static void Get_pSin_pCos(double Longitude, double Latitude, double SiteAltitude_Meters_Above_MSL, out double pCos, out double pSin)
		{
			double num = GeoidHeight(Longitude * (180.0 / Math.PI), Latitude * (180.0 / Math.PI));
			double num2 = 1.0 / Math.Sqrt(Math.Cos(Latitude) * Math.Cos(Latitude) + 0.9933056200098587 * Math.Sin(Latitude) * Math.Sin(Latitude));
			double num3 = 0.9933056200098587 * num2;
			pCos = Math.Cos(Latitude) * (num2 + (SiteAltitude_Meters_Above_MSL + num) / 6378137.0);
			pSin = Math.Sin(Latitude) * (num3 + (SiteAltitude_Meters_Above_MSL + num) / 6378137.0);
		}

		public static int RGOdatum_to_ILOCdatum(int DatumNumber, double Longitude_deg, double Latitude_deg)
		{
			if (DatumNumber < 1)
			{
				return DatumNumber;
			}
			if (DatumNumber > 9)
			{
				return DatumNumber;
			}
			if (DatumNumber == 1)
			{
				if (Longitude_deg > 66.0 && Longitude_deg < 112.0 && Latitude_deg > 5.0 && Latitude_deg < 38.0)
				{
					return 22;
				}
				if (Longitude_deg > -11.0 && Longitude_deg < 2.0 && Latitude_deg > 50.0 && Latitude_deg < 60.0)
				{
					return 16;
				}
				if (Longitude_deg > 12.0 && Longitude_deg < 23.0 && Latitude_deg > 41.0 && Latitude_deg < 51.0)
				{
					return 16;
				}
				if (Longitude_deg > 3.0 && Longitude_deg < 81.0 && Latitude_deg > 50.0 && Latitude_deg < 54.0)
				{
					return 16;
				}
				if (Longitude_deg > -4.0 && Longitude_deg < 15.0 && Latitude_deg > 40.0 && Latitude_deg < 55.0)
				{
					return 16;
				}
				if (Longitude_deg > -180.0 && Longitude_deg < -135.0 && Latitude_deg > 40.0 && Latitude_deg < 55.0)
				{
					return 29;
				}
				if (Longitude_deg > 170.0 && Longitude_deg < 180.0 && Latitude_deg > 40.0 && Latitude_deg < 55.0)
				{
					return 29;
				}
				if (Longitude_deg > 113.0 && Longitude_deg < 150.0 && Latitude_deg > 21.0 && Latitude_deg < 53.0)
				{
					return 46;
				}
				if (Longitude_deg > 16.0 && Longitude_deg < 30.0 && Latitude_deg > 50.0 && Latitude_deg < 60.0)
				{
					return 16;
				}
				if (Longitude_deg > 24.0 && Longitude_deg < 30.0 && Latitude_deg > 48.0 && Latitude_deg < 54.0)
				{
					return 16;
				}
				if (Longitude_deg > 34.0 && Longitude_deg < 39.0 && Latitude_deg > 29.0 && Latitude_deg < 34.0)
				{
					return 72;
				}
				if (Longitude_deg > -90.0 && Longitude_deg < -77.0 && Latitude_deg > 8.0 && Latitude_deg < 12.0)
				{
					return 30;
				}
				if (Longitude_deg > -84.0 && Longitude_deg < -60.0 && Latitude_deg > -22.0 && Latitude_deg < 12.0)
				{
					return 38;
				}
				if (Longitude_deg > -76.0 && Longitude_deg < -68.0 && Latitude_deg > -56.0 && Latitude_deg < -17.0)
				{
					return 39;
				}
				if (Longitude_deg > -73.0 && Longitude_deg < -54.0 && Latitude_deg > -52.0 && Latitude_deg < -22.0)
				{
					return 64;
				}
				if (Longitude_deg > -60.0 && Longitude_deg < -32.0 && Latitude_deg > -30.0 && Latitude_deg < 4.0)
				{
					return 14;
				}
				if (Longitude_deg > -11.0 && Longitude_deg < 31.0 && Latitude_deg > 25.0 && Latitude_deg < 72.0)
				{
					return 16;
				}
				if (Longitude_deg > 30.0 && Longitude_deg < 38.0 && Latitude_deg > 25.0 && Latitude_deg < 44.0)
				{
					return 16;
				}
				if (Longitude_deg > 38.0 && Longitude_deg < 50.0 && Latitude_deg > 25.0 && Latitude_deg < 40.0)
				{
					return 16;
				}
				if (Longitude_deg > 50.0 && Longitude_deg < 70.0 && Latitude_deg > 24.0 && Latitude_deg < 42.0)
				{
					return 16;
				}
				if (Longitude_deg > -30.0 && Longitude_deg < -22.0 && Latitude_deg > 34.0 && Latitude_deg < 41.0)
				{
					return 17;
				}
				if (Longitude_deg > 117.0 && Longitude_deg < 128.0 && Latitude_deg > 5.0 && Latitude_deg < 21.0)
				{
					return 26;
				}
				if (Longitude_deg > 166.0 && Longitude_deg < 180.0 && Latitude_deg > -48.0 && Latitude_deg < -32.0)
				{
					return 28;
				}
				if (Longitude_deg > -172.0 && Longitude_deg < -45.0 && Latitude_deg > 55.0 && Latitude_deg < 84.0)
				{
					return 29;
				}
				if (Longitude_deg > -98.0 && Longitude_deg < -50.0 && Latitude_deg > 12.0 && Latitude_deg < 55.0)
				{
					return 30;
				}
				if (Longitude_deg > -135.0 && Longitude_deg < -98.0 && Latitude_deg > 12.0 && Latitude_deg < 55.0)
				{
					return 31;
				}
				if (Longitude_deg > -180.0 && Longitude_deg < -150.0 && Latitude_deg > 15.0 && Latitude_deg < 30.0)
				{
					return 33;
				}
				if (Longitude_deg > -19.0 && Longitude_deg < -13.0 && Latitude_deg > 27.0 && Latitude_deg < 30.0)
				{
					return 35;
				}
				if (Longitude_deg > -180.0 && Longitude_deg < -172.0 && Latitude_deg > 38.0 && Latitude_deg < 78.0)
				{
					return 40;
				}
				if (Longitude_deg > 30.0 && Longitude_deg < 180.0 && Latitude_deg > 38.0 && Latitude_deg < 78.0)
				{
					return 40;
				}
				if (Longitude_deg > -84.0 && Longitude_deg < -32.0 && Latitude_deg > -57.0 && Latitude_deg < 12.0)
				{
					return 41;
				}
				if (Longitude_deg > 175.0 && Longitude_deg < 180.0 && Latitude_deg > -20.0 && Latitude_deg < -12.0)
				{
					return 48;
				}
				if (Longitude_deg > -180.0 && Longitude_deg < -175.0 && Latitude_deg > -20.0 && Latitude_deg < -12.0)
				{
					return 48;
				}
				if (Longitude_deg > 152.0 && Longitude_deg < 156.0 && Latitude_deg > 22.0 && Latitude_deg < 26.0)
				{
					return 53;
				}
				if (Longitude_deg > 136.0 && Longitude_deg < 140.0 && Latitude_deg > 8.0 && Latitude_deg < 12.0)
				{
					return 56;
				}
				if (Longitude_deg > 28.0 && Longitude_deg < 35.0 && Latitude_deg > 0.0 && Latitude_deg < 13.0)
				{
					return 63;
				}
				if (Longitude_deg > -20.0 && Longitude_deg < 55.0 && Latitude_deg > 0.0 && Latitude_deg < 28.0)
				{
					return 61;
				}
				if (Longitude_deg > 8.0 && Longitude_deg < 42.0 && Latitude_deg > -40.0 && Latitude_deg < 0.0)
				{
					return 63;
				}
				if (Longitude_deg > 112.0 && Longitude_deg < 157.0 && Latitude_deg > -45.0 && Latitude_deg < -1.0)
				{
					return 66;
				}
				if (Longitude_deg > 104.0 && Longitude_deg < 128.0 && Latitude_deg > -11.0 && Latitude_deg < -6.0)
				{
					return 71;
				}
				if (Longitude_deg > 78.0 && Longitude_deg < 84.0 && Latitude_deg > 4.0 && Latitude_deg < 10.0)
				{
					return 74;
				}
				return 84;
			}
			if (Longitude_deg > 66.0 && Longitude_deg < 112.0 && Latitude_deg > 5.0 && Latitude_deg < 38.0)
			{
				return 22;
			}
			if (Longitude_deg > -11.0 && Longitude_deg < 2.0 && Latitude_deg > 50.0 && Latitude_deg < 60.0)
			{
				return 34;
			}
			if (Longitude_deg > 12.0 && Longitude_deg < 23.0 && Latitude_deg > 41.0 && Latitude_deg < 51.0)
			{
				return 73;
			}
			if (Longitude_deg > 3.0 && Longitude_deg < 81.0 && Latitude_deg > 50.0 && Latitude_deg < 54.0)
			{
				return 81;
			}
			if (Longitude_deg > -4.0 && Longitude_deg < 15.0 && Latitude_deg > 40.0 && Latitude_deg < 55.0)
			{
				return 37;
			}
			if (Longitude_deg > -180.0 && Longitude_deg < -135.0 && Latitude_deg > 40.0 && Latitude_deg < 55.0)
			{
				return 29;
			}
			if (Longitude_deg > 170.0 && Longitude_deg < 180.0 && Latitude_deg > 40.0 && Latitude_deg < 55.0)
			{
				return 29;
			}
			if (Longitude_deg > 113.0 && Longitude_deg < 150.0 && Latitude_deg > 21.0 && Latitude_deg < 53.0)
			{
				return 46;
			}
			if (Longitude_deg > 16.0 && Longitude_deg < 30.0 && Latitude_deg > 50.0 && Latitude_deg < 60.0)
			{
				return 40;
			}
			if (Longitude_deg > 24.0 && Longitude_deg < 30.0 && Latitude_deg > 48.0 && Latitude_deg < 54.0)
			{
				return 40;
			}
			if (Longitude_deg > 34.0 && Longitude_deg < 39.0 && Latitude_deg > 29.0 && Latitude_deg < 34.0)
			{
				return 72;
			}
			if (Longitude_deg > -90.0 && Longitude_deg < -77.0 && Latitude_deg > 8.0 && Latitude_deg < 12.0)
			{
				return 30;
			}
			if (Longitude_deg > -84.0 && Longitude_deg < -60.0 && Latitude_deg > -22.0 && Latitude_deg < 12.0)
			{
				return 38;
			}
			if (Longitude_deg > -76.0 && Longitude_deg < -68.0 && Latitude_deg > -56.0 && Latitude_deg < -17.0)
			{
				return 39;
			}
			if (Longitude_deg > -73.0 && Longitude_deg < -54.0 && Latitude_deg > -52.0 && Latitude_deg < -22.0)
			{
				return 64;
			}
			if (Longitude_deg > -60.0 && Longitude_deg < -32.0 && Latitude_deg > -30.0 && Latitude_deg < 4.0)
			{
				return 14;
			}
			if (Longitude_deg > -11.0 && Longitude_deg < 31.0 && Latitude_deg > 25.0 && Latitude_deg < 72.0)
			{
				return 16;
			}
			if (Longitude_deg > 30.0 && Longitude_deg < 38.0 && Latitude_deg > 25.0 && Latitude_deg < 44.0)
			{
				return 16;
			}
			if (Longitude_deg > 38.0 && Longitude_deg < 50.0 && Latitude_deg > 25.0 && Latitude_deg < 40.0)
			{
				return 16;
			}
			if (Longitude_deg > 50.0 && Longitude_deg < 70.0 && Latitude_deg > 24.0 && Latitude_deg < 42.0)
			{
				return 16;
			}
			if (Longitude_deg > -30.0 && Longitude_deg < -22.0 && Latitude_deg > 34.0 && Latitude_deg < 41.0)
			{
				return 17;
			}
			if (Longitude_deg > 117.0 && Longitude_deg < 128.0 && Latitude_deg > 5.0 && Latitude_deg < 21.0)
			{
				return 26;
			}
			if (Longitude_deg > 166.0 && Longitude_deg < 180.0 && Latitude_deg > -48.0 && Latitude_deg < -32.0)
			{
				return 28;
			}
			if (Longitude_deg > -172.0 && Longitude_deg < -45.0 && Latitude_deg > 55.0 && Latitude_deg < 84.0)
			{
				return 29;
			}
			if (Longitude_deg > -98.0 && Longitude_deg < -50.0 && Latitude_deg > 12.0 && Latitude_deg < 55.0)
			{
				return 30;
			}
			if (Longitude_deg > -135.0 && Longitude_deg < -98.0 && Latitude_deg > 12.0 && Latitude_deg < 55.0)
			{
				return 31;
			}
			if (Longitude_deg > -180.0 && Longitude_deg < -150.0 && Latitude_deg > 15.0 && Latitude_deg < 30.0)
			{
				return 33;
			}
			if (Longitude_deg > -19.0 && Longitude_deg < -13.0 && Latitude_deg > 27.0 && Latitude_deg < 30.0)
			{
				return 35;
			}
			if (Longitude_deg > -180.0 && Longitude_deg < -172.0 && Latitude_deg > 38.0 && Latitude_deg < 78.0)
			{
				return 40;
			}
			if (Longitude_deg > 30.0 && Longitude_deg < 180.0 && Latitude_deg > 38.0 && Latitude_deg < 78.0)
			{
				return 40;
			}
			if (Longitude_deg > -84.0 && Longitude_deg < -32.0 && Latitude_deg > -57.0 && Latitude_deg < 12.0)
			{
				return 41;
			}
			if (Longitude_deg > 175.0 && Longitude_deg < 180.0 && Latitude_deg > -20.0 && Latitude_deg < -12.0)
			{
				return 48;
			}
			if (Longitude_deg > -180.0 && Longitude_deg < -175.0 && Latitude_deg > -20.0 && Latitude_deg < -12.0)
			{
				return 48;
			}
			if (Longitude_deg > 152.0 && Longitude_deg < 156.0 && Latitude_deg > 22.0 && Latitude_deg < 26.0)
			{
				return 53;
			}
			if (Longitude_deg > 136.0 && Longitude_deg < 140.0 && Latitude_deg > 8.0 && Latitude_deg < 12.0)
			{
				return 56;
			}
			if (Longitude_deg > 28.0 && Longitude_deg < 35.0 && Latitude_deg > 0.0 && Latitude_deg < 13.0)
			{
				return 63;
			}
			if (Longitude_deg > -20.0 && Longitude_deg < 55.0 && Latitude_deg > 0.0 && Latitude_deg < 28.0)
			{
				return 61;
			}
			if (Longitude_deg > 8.0 && Longitude_deg < 42.0 && Latitude_deg > -40.0 && Latitude_deg < 0.0)
			{
				return 63;
			}
			if (Longitude_deg > 112.0 && Longitude_deg < 157.0 && Latitude_deg > -45.0 && Latitude_deg < -1.0)
			{
				return 66;
			}
			if (Longitude_deg > 104.0 && Longitude_deg < 128.0 && Latitude_deg > -11.0 && Latitude_deg < -6.0)
			{
				return 71;
			}
			if (Longitude_deg > 78.0 && Longitude_deg < 84.0 && Latitude_deg > 4.0 && Latitude_deg < 10.0)
			{
				return 74;
			}
			return 84;
		}

		internal static double GeoidHeight(double Longitude_deg, double Latitude_deg)
		{
			if (!GeoHeightsIsFilled)
			{
				DateTime dateTime = new DateTime(2016, 12, 20);
				string path = AppPath + "\\Resource Files\\GeoidHeights.bin";
				if (InternetIsAvailable() && File.GetLastWriteTime(path) < dateTime)
				{
					if (File.Exists(AppPath + "\\Resource Files\\GeoidHeights.bup"))
					{
						File.Delete(AppPath + "\\Resource Files\\GeoidHeights.bup");
					}
					File.Move(AppPath + "\\Resource Files\\GeoidHeights.bin", AppPath + "\\Resource Files\\GeoidHeights.bup");
					http.DownloadHTTP(Settings.Default.OccultServer, "geoidheights.zip", AppPath + "\\Resource Files\\", unzip: true, gunzip: false, ShowMessages: true);
				}
				GeoHeights = File.ReadAllBytes(AppPath + "\\Resource Files\\GeoidHeights.bin");
				GeoHeightsIsFilled = true;
			}
			while (Longitude_deg < -180.0)
			{
				Longitude_deg += 360.0;
			}
			while (Longitude_deg > 180.0)
			{
				Longitude_deg -= 360.0;
			}
			if (Longitude_deg < -180.0)
			{
				Longitude_deg += 1.0;
			}
			if (Longitude_deg > 180.0)
			{
				Longitude_deg -= 1.0;
			}
			int num = (int)Math.Floor(Longitude_deg);
			int num2 = (int)Math.Floor(Latitude_deg);
			double singleGeoidHeightPoint = GetSingleGeoidHeightPoint(num, num2);
			double singleGeoidHeightPoint2 = GetSingleGeoidHeightPoint(num + 1, num2);
			double singleGeoidHeightPoint3 = GetSingleGeoidHeightPoint(num, num2 + 1);
			double singleGeoidHeightPoint4 = GetSingleGeoidHeightPoint(num + 1, num2 + 1);
			double num3 = singleGeoidHeightPoint * ((double)(num + 1) - Longitude_deg) + singleGeoidHeightPoint2 * (Longitude_deg - (double)num);
			double num4 = singleGeoidHeightPoint3 * ((double)(num + 1) - Longitude_deg) + singleGeoidHeightPoint4 * (Longitude_deg - (double)num);
			return num3 * ((double)(num2 + 1) - Latitude_deg) + num4 * (Latitude_deg - (double)num2);
		}

		private static double GetSingleGeoidHeightPoint(int Longitude_deg, int Latitude_deg)
		{
			if (Longitude_deg > 179)
			{
				Longitude_deg = -180;
			}
			if (Longitude_deg < -180)
			{
				Longitude_deg = -180;
			}
			if (Latitude_deg > 90)
			{
				Latitude_deg = 90;
			}
			if (Latitude_deg < -90)
			{
				Latitude_deg = -90;
			}
			int num = (90 - Latitude_deg) * 360 + 180 + Longitude_deg;
			double num2 = (int)GeoHeights[num];
			if (num2 > 127.0)
			{
				num2 -= 256.0;
			}
			return num2;
		}

		public static void NumericIntegrate(double IntegrateTo_Date, ref double OsculatingDate, ref double EpochMeanAnomaly, ref double a, ref double q, ref double e, ref double i, ref double node, ref double perihelion, bool saveflag, int IDNumber, string IDName, double H0, double G, double logR_Coeff, double DiameterMax, string DiaSource, double PEU, int NumOfRings, int NumOfMoons, string OrbitSource, string OrbitDate, string AsteroidClass, double ErrorMajor, double ErrorMinor, double ErrorPA, List<AsteroidElements> ElementsAt1Day)
		{
			double num = 0.0;
			double[,] scheme = new double[30, 8];
			double[,] xyz = new double[3, 10];
			double[] array = new double[3];
			double[] array2 = new double[3];
			double[] attraction = new double[3];
			double[] radiusvector = new double[10];
			bool plutoFlag = IDNumber == 134340;
			double num2 = 1.0;
			if (IntegrateTo_Date < OsculatingDate)
			{
				num2 = -1.0;
			}
			double num3 = 0.0;
			q = a * (1.0 - e);
			double num4 = 0.01720209895 * Math.Pow(a, -1.5);
			double num5 = a * Math.Sqrt(1.0 - e * e);
			double num6 = a * (Math.Cos(perihelion) * Math.Cos(node) - Math.Sin(perihelion) * Math.Sin(node) * Math.Cos(i));
			double num7 = Math.Sin(perihelion) * Math.Cos(node) * Math.Cos(i) + Math.Cos(perihelion) * Math.Sin(node);
			double num8 = a * (cosEcliptic2000[EclipticID] * num7 - sinEcliptic2000[EclipticID] * Math.Sin(perihelion) * Math.Sin(i));
			double num9 = a * (sinEcliptic2000[EclipticID] * num7 + cosEcliptic2000[EclipticID] * Math.Sin(perihelion) * Math.Sin(i));
			double num10 = num5 * ((0.0 - Math.Sin(perihelion)) * Math.Cos(node) - Math.Cos(perihelion) * Math.Sin(node) * Math.Cos(i));
			double num11 = Math.Cos(perihelion) * Math.Cos(node) * Math.Cos(i) - Math.Sin(perihelion) * Math.Sin(node);
			double num12 = num5 * (cosEcliptic2000[EclipticID] * num11 - sinEcliptic2000[EclipticID] * Math.Cos(perihelion) * Math.Sin(i));
			double num13 = num5 * (sinEcliptic2000[EclipticID] * num11 + cosEcliptic2000[EclipticID] * Math.Cos(perihelion) * Math.Sin(i));
			for (int j = -3; j < 4; j++)
			{
				num3 = (double)j * num2;
				double num14 = EpochMeanAnomaly + num4 * num3;
				double TrueAnomaly = 0.0;
				double num15 = 0.0;
				double num16 = 0.0;
				double EccentricAnomaly;
				if (e == 0.0)
				{
					EccentricAnomaly = num14;
				}
				else if (e == 1.0)
				{
					num16 = num3;
					num15 = 0.01720209895 * num16 / Math.Pow(q, 1.5);
					Kepler(num15, 1.0, out TrueAnomaly, out EccentricAnomaly);
				}
				else
				{
					num4 = 0.01720209895 * Math.Pow(Math.Abs(q / (1.0 - e)), -1.5);
					for (num15 = EpochMeanAnomaly + num4 * (OsculatingDate + num3 - OsculatingDate); num15 > Math.PI; num15 -= Math.PI * 2.0)
					{
					}
					for (; num15 < -Math.PI; num15 += Math.PI * 2.0)
					{
					}
					Kepler(num15, e, out TrueAnomaly, out EccentricAnomaly);
				}
				xyz[0, 0] = num6 * (Math.Cos(EccentricAnomaly) - e) + num10 * Math.Sin(EccentricAnomaly);
				xyz[1, 0] = num8 * (Math.Cos(EccentricAnomaly) - e) + num12 * Math.Sin(EccentricAnomaly);
				xyz[2, 0] = num9 * (Math.Cos(EccentricAnomaly) - e) + num13 * Math.Sin(EccentricAnomaly);
				radiusvector[0] = Math.Sqrt(xyz[0, 0] * xyz[0, 0] + xyz[1, 0] * xyz[1, 0] + xyz[2, 0] * xyz[2, 0]);
				array2[0] = (num10 * Math.Cos(EccentricAnomaly) - num6 * Math.Sin(EccentricAnomaly)) / radiusvector[0] / Math.Sqrt(a);
				array2[1] = (num12 * Math.Cos(EccentricAnomaly) - num8 * Math.Sin(EccentricAnomaly)) / radiusvector[0] / Math.Sqrt(a);
				array2[2] = (num13 * Math.Cos(EccentricAnomaly) - num9 * Math.Sin(EccentricAnomaly)) / radiusvector[0] / Math.Sqrt(a);
				if (num3 == 0.0)
				{
					for (int k = 0; k <= 2; k++)
					{
						scheme[10 * k + 9, 4] = xyz[k, 0] * 10000000.0;
						scheme[10 * k + 8, 4] = array2[k] * num2 * 172020.9895;
					}
				}
				Attractions(OsculatingDate + num3, num3, ref scheme, ref xyz, ref attraction, ref radiusvector, num2, initialflag: true, plutoFlag);
				for (int k = 0; k <= 2; k++)
				{
					int num17 = 10 * k + 2;
					scheme[num17, 3 - Convert.ToInt32(num3 / num2)] = attraction[k];
				}
			}
			InitialDifferences(ref scheme);
			InitialSums(ref scheme);
			for (int l = 0; l < 7; l++)
			{
				int num18 = l - 1;
				if (num18 < 0)
				{
					num18 = 0;
				}
				for (int k = 0; k < 3; k++)
				{
					int num17 = 10 * k;
					array[k] = scheme[num17, l + 1] + scheme[num17 + 2, l] / 12.0 - scheme[num17 + 4, num18] / 240.0 + 0.0005125661375661375 * scheme[num17 + 6, 1];
					array[k] *= 1E-07;
				}
				double num19 = array[0] * array[0] + array[1] * array[1] + array[2] * array[2];
				double num20 = num2 * num2 / 100.0 * 295912.208 / num19 / Math.Sqrt(num19);
				for (int k = 0; k < 3; k++)
				{
					int num17 = 10 * k + 9;
					scheme[num17 - l, 7] = (0.0 - num20) * array[k] - scheme[num17 - l, 7];
				}
			}
			for (int k = 0; k <= 20; k += 10)
			{
				for (int num17 = 0; num17 < 7; num17++)
				{
					scheme[k + 2, num17] += scheme[k + 9 - num17, 7];
				}
			}
			InitialDifferences(ref scheme);
			InitialSums(ref scheme);
			if (saveflag)
			{
				ElementsAt1Day.Clear();
			}
			num3 += num2;
			do
			{
				MoveScheme(ref scheme);
				EstimateNext(ref scheme, num2);
				for (int k = 0; k < 3; k++)
				{
					xyz[k, 0] = scheme[10 * k + 9, 4];
				}
				radiusvector[0] = Math.Sqrt(xyz[0, 0] * xyz[0, 0] + xyz[1, 0] * xyz[1, 0] + xyz[2, 0] * xyz[2, 0]);
				Attractions(OsculatingDate + num3, num3, ref scheme, ref xyz, ref attraction, ref radiusvector, num2, initialflag: false, plutoFlag);
				for (int k = 0; k < 3; k++)
				{
					int num17 = 10 * k + 2;
					scheme[num17, 0] = attraction[k];
				}
				NewDifferences(ref scheme);
				if (saveflag)
				{
					num = OsculatingDate + num3 - 3.0 * num2;
					Elements_from_Scheme(ref scheme, ref xyz, ref radiusvector, num2, ref EpochMeanAnomaly, ref num, ref a, ref e, ref i, ref node, ref perihelion, saveflag, IDNumber, IDName, H0, G, logR_Coeff, DiameterMax, DiaSource, PEU, NumOfRings, NumOfMoons, OrbitSource, OrbitDate, AsteroidClass, ErrorMajor, ErrorMinor, ErrorPA, ElementsAt1Day);
				}
				if (num2 > 0.0)
				{
					if (OsculatingDate + num3 - 3.0 * num2 >= IntegrateTo_Date)
					{
						break;
					}
				}
				else if (OsculatingDate + num3 - 3.0 * num2 <= IntegrateTo_Date)
				{
					break;
				}
				num3 += num2;
			}
			while (num2 != 0.0);
			num = OsculatingDate + num3 - 3.0 * num2;
			Elements_from_Scheme(ref scheme, ref xyz, ref radiusvector, num2, ref EpochMeanAnomaly, ref num, ref a, ref e, ref i, ref node, ref perihelion, saveflag: false, IDNumber, IDName, H0, G, logR_Coeff, DiameterMax, DiaSource, PEU, NumOfRings, NumOfMoons, OrbitSource, OrbitDate, AsteroidClass, ErrorMajor, ErrorMinor, ErrorPA, ElementsAt1Day);
			OsculatingDate = num;
		}

		private static void Attractions(double jdate, double DaysFromEpoch, ref double[,] scheme, ref double[,] xyz, ref double[] attraction, ref double[] radiusvector, double interval, bool initialflag, bool PlutoFlag)
		{
			double[] array = new double[3];
			double[] array2 = new double[10] { 295912.2083, 0.04912, 0.72435, 0.89968, 0.09549, 282.5369, 84.59502, 12.88829, 15.29346, 0.00223 };
			double num = interval * interval / 100.0;
			GetPlanetCoords(jdate, ref xyz, ref radiusvector);
			for (int i = 0; i < 3; i++)
			{
				attraction[i] = 0.0;
			}
			double num2 = (0.0 - num) * array2[0] / Math.Pow(radiusvector[0], 3.0);
			for (int j = 0; j < 3; j++)
			{
				attraction[j] = num2 * xyz[j, 0];
				if (initialflag)
				{
					scheme[10 * j + 6 + Convert.ToInt16(DaysFromEpoch / interval), 7] = attraction[j];
				}
			}
			int num3 = 9;
			if (PlutoFlag)
			{
				num3 = 8;
			}
			for (int k = 1; k <= num3; k++)
			{
				double num4 = 0.0;
				for (int l = 0; l < 3; l++)
				{
					array[l] = xyz[l, 0] - xyz[l, k];
					num4 += array[l] * array[l];
				}
				num2 = num4 * Math.Sqrt(num4);
				double num5 = Math.Pow(radiusvector[k], 3.0);
				for (int m = 0; m < 3; m++)
				{
					attraction[m] += (0.0 - num) * array2[k] * (array[m] / num2 + xyz[m, k] / num5);
				}
			}
		}

		private static void Elements_from_Scheme(ref double[,] scheme, ref double[,] xyz, ref double[] radiusvector, double interval, ref double MeanAnomaly, ref double epochdate, ref double a, ref double e, ref double i, ref double node, ref double perihelion, bool saveflag, int IDNumber, string IDName, double H0, double G, double logR_Coeff, double DiameterMax, string DiaSource, double PEU, int NumOfRings, int NumOfMoons, string OrbitSource, string OrbitDate, string AsteroidClass, double ErrorMajor, double ErrorMinor, double ErrorPA, List<AsteroidElements> ElementsAt1Day)
		{
			int Year = 0;
			int Month = 0;
			double num = 0.0;
			double num2 = 0.0;
			double num3 = 0.0;
			double day = 0.0;
			double[] array = new double[3];
			double[] array2 = new double[3];
			double[] array3 = new double[3];
			double[] array4 = new double[3];
			for (int j = 0; j < 3; j++)
			{
				int num4 = 10 * j;
				array[j] = array[j] + scheme[num4, 4] + scheme[num4 + 2, 3] / 12.0 - scheme[num4 + 4, 2] / 240.0 + scheme[num4 + 6, 1] / 1950.968 - scheme[num4 + 8, 0] / 12556.4;
				array2[j] = array2[j] + scheme[num4 + 1, 3] - scheme[num4 + 2, 3] / 2.0 - (scheme[num4 + 3, 2] + scheme[num4 + 3, 3]) / 24.0;
				array2[j] = array2[j] + (scheme[num4 + 5, 1] + scheme[num4 + 5, 2]) * 11.0 / 1440.0 - (scheme[num4 + 7, 0] + scheme[num4 + 7, 1]) / 633.3;
				array2[j] = array2[j] / interval / 172020.9895;
				array[j] *= 1E-07;
				num += array[j] * array[j];
				num3 += array2[j] * array2[j];
				num2 += array[j] * array2[j];
			}
			int num13;
			double y;
			double num12;
			try
			{
				num = Math.Sqrt(num);
				a = 1.0 / (2.0 / num - num3);
				_ = 172020.9895 * Math.Pow(a, -1.5) * (180.0 / Math.PI) / 10000000.0;
				double num5 = num2 / Math.Sqrt(a);
				double num6 = num * num3 - 1.0;
				e = Math.Sqrt(num5 * num5 + num6 * num6);
				double num7 = Math.Atan2(num5, num6);
				MeanAnomaly = num7 - e * Math.Sin(num7);
				if (MeanAnomaly < 0.0)
				{
					MeanAnomaly += Math.PI * 2.0;
				}
				double num8 = Math.Sqrt(num * num * num3 - num2 * num2);
				for (int j = 0; j < 3; j++)
				{
					array3[j] = array[j] / num * Math.Cos(num7) - array2[j] * Math.Sqrt(a) * Math.Sin(num7);
					array4[j] = (array[j] / num * Math.Sin(num7) + array2[j] * Math.Sqrt(a) * (Math.Cos(num7) - e)) / Math.Sqrt(1.0 - e * e);
				}
				double num9 = (array[1] * array2[2] - array[2] * array2[1]) / num8;
				double num10 = (array[2] * array2[0] - array[0] * array2[2]) / num8;
				double num11 = (array[0] * array2[1] - array[1] * array2[0]) / num8;
				num12 = (0.0 - num10) * cosEcliptic2000[EclipticID] - num11 * sinEcliptic2000[EclipticID];
				node = Math.Atan2(num9, num12);
				if (node < 0.0)
				{
					node += Math.PI * 2.0;
				}
				y = Math.Sqrt(num9 * num9 + num12 * num12);
				i = Math.Atan2(y, num11 * cosEcliptic2000[EclipticID] - num10 * sinEcliptic2000[EclipticID]);
				num13 = Math.Sign(Math.Cos(i));
			}
			catch (Exception innerException)
			{
				throw new OccultException("Error searching for events for " + IDNumber + ", " + IDName, innerException);
			}
			y = (double)num13 * array3[1] * cosEcliptic2000[EclipticID] + (double)num13 * array3[2] * sinEcliptic2000[EclipticID] - array4[0];
			num12 = (double)num13 * array4[1] * cosEcliptic2000[EclipticID] + (double)num13 * array4[2] * sinEcliptic2000[EclipticID] + array3[0];
			for (perihelion = Math.Atan2(y, num12) - (double)num13 * node; perihelion < 0.0; perihelion += Math.PI * 2.0)
			{
			}
			while (perihelion > Math.PI * 2.0)
			{
				perihelion -= Math.PI * 2.0;
			}
			if (saveflag)
			{
				AsteroidElements asteroidElements = new AsteroidElements();
				Date_from_JD(epochdate, out Year, out Month, out day);
				asteroidElements.IDNumber = IDNumber;
				asteroidElements.IDName = IDName;
				asteroidElements.Meananomaly = MeanAnomaly * (180.0 / Math.PI);
				asteroidElements.EpochYear = Year;
				asteroidElements.EpochMonth = Month;
				asteroidElements.EpochDay = day;
				asteroidElements.Perihelion = perihelion * (180.0 / Math.PI);
				asteroidElements.Node = node * (180.0 / Math.PI);
				asteroidElements.I = i * (180.0 / Math.PI);
				asteroidElements.E = e;
				asteroidElements.A = a;
				asteroidElements.q = a * (1.0 - e);
				asteroidElements.H0 = H0;
				asteroidElements.G_phaseCoeff = G;
				asteroidElements.LogR_Coeff = logR_Coeff;
				asteroidElements.Diameter_Mean = DiameterMax;
				asteroidElements.Dia_Source = DiaSource;
				asteroidElements.PeakEphemUncert = PEU;
				asteroidElements.Num_Rings = NumOfRings;
				asteroidElements.Num_Moons = NumOfMoons;
				asteroidElements.OrbitSource = OrbitSource + "+INTG:";
				asteroidElements.OrbitDate = OrbitDate;
				asteroidElements.AsteroidClass = AsteroidClass;
				asteroidElements.ErrorMajor = ErrorMajor;
				asteroidElements.ErrorMinor = ErrorMinor;
				asteroidElements.ErrorPA = ErrorPA;
				ElementsAt1Day.Add(asteroidElements);
			}
		}

		private static void EstimateNext(ref double[,] scheme, double interval)
		{
			for (int i = 0; i <= 20; i += 10)
			{
				scheme[i + 8, 0] = scheme[i + 8, 1];
				for (int num = 7; num >= 2; num--)
				{
					scheme[i + num, 0] = scheme[i + num, 1] + scheme[i + num + 1, 0];
				}
				scheme[i + 9, 4] = scheme[i, 1] + scheme[i + 2, 0] / 12.0;
				scheme[i + 9, 4] = scheme[i + 9, 4] - (scheme[i + 4, 0] + scheme[i + 5, 0] + scheme[i + 6, 0] + scheme[i + 7, 0] + scheme[i + 8, 0]) / 240.0;
				scheme[i + 9, 4] = (scheme[i + 9, 4] + (scheme[i + 6, 0] + 2.0 * scheme[i + 7, 0] + 3.0 * scheme[i + 8, 0]) / 1950.968) * 1E-07;
				scheme[i + 8, 4] = (scheme[i + 1, 0] + scheme[i + 2, 0] / 2.0 - scheme[i + 3, 0] / 24.0 + (scheme[i + 5, 0] + scheme[i + 5, 0]) * 11.0 / 1440.0) / interval / 172020.9895;
			}
		}

		private static void InitialDifferences(ref double[,] scheme)
		{
			for (int i = 0; i <= 20; i += 10)
			{
				for (int j = 3; j < 9; j++)
				{
					int num = i + j;
					for (int num2 = 8 - j; num2 >= 0; num2--)
					{
						scheme[num, num2] = scheme[num - 1, num2] - scheme[num - 1, num2 + 1];
					}
				}
			}
		}

		private static void InitialSums(ref double[,] scheme)
		{
			for (int i = 0; i <= 20; i += 10)
			{
				scheme[i + 1, 3] = scheme[i + 8, 4] + scheme[i + 2, 3] / 2.0 + (scheme[i + 3, 2] + scheme[i + 3, 3]) / 24.0;
				scheme[i + 1, 3] = scheme[i + 1, 3] - (scheme[i + 5, 1] + scheme[i + 5, 2]) * 11.0 / 1440.0;
				scheme[i + 1, 3] = scheme[i + 1, 3] + (scheme[i + 7, 0] + scheme[i + 7, 1]) / 633.29438;
				for (int num = 2; num >= 0; num--)
				{
					scheme[i + 1, num] = scheme[i + 1, num + 1] + scheme[i + 2, num];
				}
				for (int num = 4; num <= 6; num++)
				{
					scheme[i + 1, num] = scheme[i + 1, num - 1] - scheme[i + 2, num - 1];
				}
			}
			for (int i = 0; i <= 20; i += 10)
			{
				scheme[i, 4] = scheme[i + 9, 4] - scheme[i + 2, 3] / 12.0 + scheme[i + 4, 2] / 240.0 - scheme[i + 6, 1] / 1950.968 + scheme[i + 8, 0] / 12556.4;
				for (int num = 3; num >= 0; num--)
				{
					scheme[i, num] = scheme[i, num + 1] + scheme[i + 1, num];
				}
				for (int num = 5; num <= 7; num++)
				{
					scheme[i, num] = scheme[i, num - 1] - scheme[i + 1, num - 1];
				}
			}
		}

		private static void MoveScheme(ref double[,] scheme)
		{
			for (int i = 0; i < 30; i += 10)
			{
				for (int num = 8; num >= 0; num--)
				{
					int num2 = 9 - num;
					int num3 = i + num;
					for (int num4 = 7; num4 > 0; num4--)
					{
						if (num4 <= num2)
						{
							scheme[num3, num4] = scheme[num3, num4 - 1];
						}
					}
				}
			}
		}

		private static void NewDifferences(ref double[,] scheme)
		{
			for (int i = 0; i < 30; i += 10)
			{
				for (int j = 3; j < 10; j++)
				{
					int num = i + j;
					scheme[num, 0] = scheme[num - 1, 0] - scheme[num - 1, 1];
				}
				for (int num2 = 1; num2 >= 0; num2--)
				{
					int num = i + num2;
					scheme[num, 0] = scheme[num, 1] + scheme[num + 1, 0];
				}
			}
		}

		private static void GetPlanetCoords(double jdate, ref double[,] xyz, ref double[] radiusvector)
		{
			double num = Math.Floor(jdate / 10000.0);
			string text = Convert.ToString(num);
			double value = 120.0 * Math.Floor(jdate - num * 10000.0);
			FileStream fileStream = new FileStream(AppPath + "\\Resource Files\\" + text + "0000_XYZ.bin", FileMode.Open, FileAccess.Read);
			BinaryReader binaryReader = new BinaryReader(fileStream);
			fileStream.Seek(Convert.ToInt64(value), SeekOrigin.Begin);
			for (int i = 1; i < 10; i++)
			{
				for (int j = 0; j < 3; j++)
				{
					if (i == 3)
					{
						xyz[j, i] = binaryReader.ReadDouble();
					}
					else
					{
						xyz[j, i] = binaryReader.ReadSingle();
					}
				}
			}
			binaryReader.Close();
			for (int k = 1; k < 10; k++)
			{
				radiusvector[k] = Math.Sqrt(xyz[0, k] * xyz[0, k] + xyz[1, k] * xyz[1, k] + xyz[2, k] * xyz[2, k]);
			}
		}

		internal static double Interpolate_Bessel(double T, double t0, double deltaT, double x_1, double x0, double x1, double x2)
		{
			double num = (T - t0) / deltaT;
			double num2 = 0.5 * num * (num - 1.0) / 2.0;
			return x0 + num * (x1 - x0) + num2 * (x2 - x1 - x0 + x_1);
		}

		internal static void PolynomialFit(double x0, double x1, double x2, out double T0, out double dT, out double d2T)
		{
			T0 = x0;
			dT = x1 - T0;
			d2T = x2 - T0;
			d2T = (d2T - 2.0 * dT) / 2.0;
			dT -= d2T;
		}

		internal static void PolynomialFit(double x_1, double x0, double x1, double x2, out double T0, out double dT, out double d2T, out double d3T)
		{
			double num = 0.0;
			T0 = (dT = (d2T = (d3T = 0.0)));
			T0 = x0;
			double num2 = x_1 - T0;
			double num3 = x1 - T0;
			double num4 = x2 - T0;
			d2T = (num2 + num3) / 2.0;
			double num5 = num3 - d2T;
			num = (num4 - 4.0 * d2T) / 2.0;
			d3T = (num - num5) / 3.0;
			dT = num5 - d3T;
		}

		internal static void PolynomialFit(double x_2, double x0, double x1, double x2, double x3, out double T0, out double dT, out double d2T, out double d3T, out double d4T)
		{
			T0 = (dT = (d2T = (d3T = (d4T = 0.0))));
			T0 = x0;
			double num = x_2 - x0;
			double num2 = x1 - x0;
			double num3 = x2 - x0;
			double num4 = x3 - x0;
			double num5 = (num3 + num) / 8.0;
			double num6 = (num3 - num) / 4.0;
			double num7 = num + 2.0 * num2;
			double num8 = num4 - 3.0 * num2;
			double num9 = (num8 + 4.0 * num7) / 30.0;
			d4T = num9 - num5;
			d2T = num5 - 4.0 * d4T;
			double num10 = (num8 - num7) / 30.0;
			d3T = num10 - 2.0 * d4T;
			dT = num6 - 4.0 * d3T;
		}

		public static void Display_IR_AsteroidDiameter(int Asteroid, bool ShowInForm, out string Diameters)
		{
			//IL_0073: Unknown result type (might be due to invalid IL or missing references)
			Get_SingleAsteroid_Diameter_and_UncertaintyInfo(Asteroid, out var Diameter, out var DiameterUncertainty, out var Source);
			int num = 0;
			Diameters = "";
			string path = AppPath + "/Resource Files/AsteroidDiametersAll.bin";
			if (!File.Exists(path))
			{
				http.DownloadHTTP(Settings.Default.OccultServer, "asteroiddiametersall.zip", AppPath + "\\Resource Files\\", unzip: true, gunzip: false, ShowMessages: false);
			}
			if (!File.Exists(path))
			{
				MessageBox.Show("IR Satellite diameters cannot be displayed", "No file of asteroid diameters", (MessageBoxButtons)0, (MessageBoxIcon)16, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
				return;
			}
			using (FileStream fileStream = new FileStream(path, FileMode.Open, FileAccess.Read))
			{
				int num2;
				int num3 = (num2 = (int)fileStream.Length / 9 - 1);
				num = 0;
				using BinaryReader binaryReader = new BinaryReader(fileStream);
				int num4 = Asteroid - 1;
				int num5;
				if (Asteroid == 1)
				{
					num5 = 0;
				}
				else
				{
					do
					{
						num5 = (num2 + num) / 2;
						if (num5 < 0 || num5 > num3)
						{
							break;
						}
						fileStream.Seek(num5 * 9, SeekOrigin.Begin);
						int num6 = binaryReader.ReadInt32();
						if (num6 == num4)
						{
							break;
						}
						if (num6 > Asteroid)
						{
							num2 = num5 - 1;
						}
						else
						{
							num = num5 + 1;
						}
					}
					while (num <= num2);
					num5 -= 10;
					if (num5 < 0)
					{
						num5 = 0;
					}
				}
				Diameters = "Satellite IR diameters for asteroid (" + Asteroid + ")\r\n";
				string[] array = new string[6] { "Mission", "NEOWISE", "AcuA", "IRAS", "MSX", "TNO-centaurs" };
				int num7 = 0;
				int num8 = 0;
				int num9 = 0;
				double num10 = 0.0;
				double num11 = 0.0;
				double num12 = 0.0;
				do
				{
					fileStream.Seek(num5 * 9, SeekOrigin.Begin);
					int num6 = binaryReader.ReadInt32();
					if (num6 == Asteroid)
					{
						num8 = binaryReader.ReadInt16();
						num9 = binaryReader.ReadByte();
						int num13 = binaryReader.ReadByte();
						double num14 = Convert.ToDouble(binaryReader.ReadByte()) / 500.0;
						string text = string.Format(":  albedo {0,2:f3}", num14);
						if (num14 > 0.49)
						{
							text = $": albedo - invalid";
						}
						Diameters = Diameters + "\r\n" + array[num13].PadLeft(9) + string.Format(":{0,6:f1} ±{1,4:f1}km", (double)num8 / 10.0, (double)num9 / 2.5) + text;
						num11 = Math.Pow((double)num9 / 2.5, 2.0);
						num10 += (double)(num8 / 10) / num11;
						num12 += 1.0 / num11;
						num7++;
					}
					else if (num6 > Asteroid)
					{
						break;
					}
					num5++;
				}
				while (num5 < fileStream.Length / 9);
				if (num7 == 0)
				{
					Diameters += "   No NEOWISE, AcuA or IRAS diameters";
				}
				else
				{
					Diameters = Diameters + "\r\n\r\nWeighted diameter:" + string.Format("{0,6:f1} ± {1,4:f1} km", Diameter, DiameterUncertainty) + "\r\n[including recommended increase in uncertainty]";
				}
			}
			string path2 = AppPath + "/Resource Files/AsteroidDias_Indiv.bin";
			if (!File.Exists(path2))
			{
				http.DownloadHTTP(Settings.Default.OccultServer, "AlbedoFile.zip", AppPath + "\\Resource Files\\", unzip: true, gunzip: false, ShowMessages: false);
			}
			if (File.Exists(path2))
			{
				using FileStream fileStream2 = new FileStream(path2, FileMode.Open, FileAccess.Read);
				int num15 = 9;
				int num16;
				int num17 = (num16 = (int)fileStream2.Length / num15 - 1);
				num = 0;
				using BinaryReader binaryReader2 = new BinaryReader(fileStream2);
				int num18 = Asteroid - 1;
				int num5;
				if (Asteroid == 1)
				{
					num5 = 0;
				}
				else
				{
					do
					{
						num5 = (num16 + num) / 2;
						if (num5 < 0 || num5 > num17)
						{
							break;
						}
						fileStream2.Seek(num5 * num15, SeekOrigin.Begin);
						int num6 = binaryReader2.ReadInt32();
						if (num6 == num18)
						{
							break;
						}
						if (num6 > Asteroid)
						{
							num16 = num5 - 1;
						}
						else
						{
							num = num5 + 1;
						}
					}
					while (num <= num16);
					num5 -= 10;
					if (num5 < 0)
					{
						num5 = 0;
					}
				}
				double num19 = 0.0;
				double num20 = 0.0;
				double num21 = 0.0;
				int num22 = 0;
				string[] array2 = new string[3] { "", "  NEOWISE", "  AcuA" };
				Diameters = Diameters + "\r\n" + "".PadRight(46, '_') + "\r\nIndividual diameter and albedo measurements";
				do
				{
					fileStream2.Seek(num5 * num15, SeekOrigin.Begin);
					int num6 = binaryReader2.ReadInt32();
					if (num6 == Asteroid)
					{
						num19 = (double)binaryReader2.ReadInt16() / 10.0;
						num20 = (double)(int)binaryReader2.ReadByte() / 2.5;
						num21 = (double)(int)binaryReader2.ReadByte() / 500.0;
						int num23 = binaryReader2.ReadByte();
						Diameters = Diameters + "\r\n" + string.Format(" {0,6:f1} ± {1,4:f1}km:  albedo {2,2:f3}:", num19, num20, num21) + array2[num23];
						num22++;
					}
					else if (num6 > Asteroid)
					{
						break;
					}
					num5++;
				}
				while (num5 < fileStream2.Length / num15);
				if (num22 == 0)
				{
					Diameters += "\r\n   No NEOWISE or AcuA data";
				}
			}
			if (!ShowInForm)
			{
				return;
			}
			if (Elements.MainAsteroids.AstElements.Count < 1)
			{
				Elements.MainAsteroids.Fill_AllAsteroids();
			}
			int asteroidRecord_fromNumber = Elements.MainAsteroids.GetAsteroidRecord_fromNumber(Asteroid);
			if (asteroidRecord_fromNumber >= 0)
			{
				string text2 = "0";
				double h = Elements.MainAsteroids.AstElements[asteroidRecord_fromNumber].H0;
				string text3 = " ";
				double MeanDiameter = 0.0;
				Diameters = Diameters + "\r\n" + "".PadRight(46, '_') + "\r\n" + "Estimated diameter, absolute mag. H0 = ".PadLeft(10) + string.Format("{0,1:f2}", h);
				double DiameterUncertainty2;
				bool SatelliteMeasure;
				for (int num24 = 40; num24 >= 1; num24--)
				{
					double num25 = 0.01 * (double)num24;
					text3 = " ";
					if (num24 == 15)
					{
						text3 = "*";
					}
					GetAsteroidDiameter(0, h, num25, out MeanDiameter, out DiameterUncertainty2, out Source, out SatelliteMeasure);
					text2 = "0";
					if (MeanDiameter < 2.0)
					{
						text2 = "2";
					}
					else if (MeanDiameter < 10.0)
					{
						text2 = "1";
					}
					Diameters = Diameters + "\r\n " + text3 + string.Format(" albedo {0,4:f2} => diameter = {1,1:f" + text2 + "} km ", num25, MeanDiameter) + text3;
					if (num24 > 11)
					{
						num24 -= 4;
					}
				}
				GetAsteroidDiameter(0, h, 0.057, out MeanDiameter, out DiameterUncertainty2, out Source, out SatelliteMeasure);
				text2 = "0";
				if (MeanDiameter < 2.0)
				{
					text2 = "2";
				}
				else if (MeanDiameter < 10.0)
				{
					text2 = "1";
				}
				Diameters = Diameters + "\r\n " + text3 + string.Format("Trojans {0,4:f2} => diameter = {1,1:f" + text2 + "} km ", 0.057, MeanDiameter) + text3;
			}
			try
			{
				((Control)DataBoxDia).Show();
			}
			catch
			{
				DataBoxDia = new DisplayData();
				((Control)DataBoxDia).Show();
				((Form)DataBoxDia).set_Location(Settings.Default.LocationIRDiameter);
			}
			((ToolStripItem)DataBoxDia.cmdCancel).set_Visible(false);
			((ToolStripItem)DataBoxDia.cmdOK).set_Visible(false);
			((ToolStripItem)DataBoxDia.helpToolStripMenuItem).set_Visible(true);
			DataBoxDia.HelpTopic = 0;
			((Control)DataBoxDia.txtBox).set_Text(Diameters);
			((Control)DataBoxDia).set_Width(380);
			((Control)DataBoxDia).set_Height(500);
			((Control)DataBoxDia).set_Text("Diameters from IR satellites");
			((TextBoxBase)DataBoxDia.txtBox).set_SelectionStart(0);
			((TextBoxBase)DataBoxDia.txtBox).Select(0, 0);
			((Control)DataBoxDia).Focus();
		}

		public static bool Get_Individual_AsteroidDiameter(int Asteroid, out double AllSats_Dia, out double AllSats_Uncert, out double NEOWISE_Diameter, out double NEOWISE_Uncert, out double Akari_Dia, out double Akari_Uncert, out double IRAS_Dia, out double IRAS_Uncert)
		{
			int num = 0;
			AllSats_Dia = (AllSats_Uncert = (IRAS_Dia = (IRAS_Uncert = (Akari_Dia = (Akari_Uncert = (NEOWISE_Diameter = (NEOWISE_Uncert = 0.0)))))));
			string path = AppPath + "/Resource Files/AsteroidDiametersAll.bin";
			if (!File.Exists(path))
			{
				http.DownloadHTTP(Settings.Default.OccultServer, "asteroiddiametersall.zip", AppPath + "\\Resource Files\\", unzip: true, gunzip: false, ShowMessages: false);
			}
			if (!File.Exists(path))
			{
				return false;
			}
			Get_SingleAsteroid_Diameter_and_UncertaintyInfo(Asteroid, out AllSats_Dia, out AllSats_Uncert, out var _);
			using FileStream fileStream = new FileStream(path, FileMode.Open, FileAccess.Read);
			int num2;
			int num3 = (num2 = (int)fileStream.Length / 9 - 1);
			num = 0;
			using BinaryReader binaryReader = new BinaryReader(fileStream);
			int num4 = Asteroid - 1;
			int num5;
			if (Asteroid == 1)
			{
				num5 = 0;
			}
			else
			{
				do
				{
					num5 = (num2 + num) / 2;
					if (num5 < 0 || num5 > num3)
					{
						break;
					}
					fileStream.Seek(num5 * 9, SeekOrigin.Begin);
					int num6 = binaryReader.ReadInt32();
					if (num6 == num4)
					{
						break;
					}
					if (num6 > Asteroid)
					{
						num2 = num5 - 1;
					}
					else
					{
						num = num5 + 1;
					}
				}
				while (num <= num2);
				num5 -= 10;
				if (num5 < 0)
				{
					num5 = 0;
				}
			}
			string text = "Satellite IR diameters for asteroid (" + Asteroid + ")\r\n";
			string[] array = new string[6] { "Mission visit", "NEOWISE", "AcuA", "IRAS", "MSX", "TNO-centaurs" };
			int num7 = 0;
			double num8 = 0.0;
			double num9 = 0.0;
			double num10 = 0.0;
			do
			{
				fileStream.Seek(num5 * 9, SeekOrigin.Begin);
				int num6 = binaryReader.ReadInt32();
				if (num6 == Asteroid)
				{
					int num11 = binaryReader.ReadInt16();
					int num12 = binaryReader.ReadByte();
					int num13 = binaryReader.ReadByte();
					if (num13 == 1)
					{
						NEOWISE_Diameter = (double)num11 / 10.0;
						NEOWISE_Uncert = (double)num12 / 2.5 + 0.2;
					}
					if (num13 == 2)
					{
						Akari_Dia = (double)num11 / 10.0;
						Akari_Uncert = (double)num12 / 2.5 + 0.2;
					}
					if (num13 == 3)
					{
						IRAS_Dia = (double)num11 / 10.0;
						IRAS_Uncert = (double)num12 / 2.5 + 0.2;
					}
					text = text + "\r\n" + array[num13].PadLeft(14) + string.Format("{0,6:f1} ± {1,4:f1}km", (double)num11 / 10.0, (double)num12 / 2.5 + 0.2);
					num9 = Math.Pow((double)num12 / 2.5 + 0.2, 2.0);
					num8 += (double)(num11 / 10) / num9;
					num10 += 1.0 / num9;
					num7++;
				}
				else if (num6 > Asteroid)
				{
					break;
				}
				num5++;
			}
			while (num5 < fileStream.Length / 9);
			if (num7 == 0)
			{
				text += "\r\nNo satellite IR diameters";
			}
			else
			{
				text = text + "\r\n\r\nWeighted diameter = " + string.Format("{0,6:f1} ±{1,1:f1}  km", num8 / num10, 1.0 / Math.Sqrt(num10));
			}
			return true;
		}

		internal static DialogResult InputDialog(string FormTitle, ref string input)
		{
			//IL_000e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0013: Unknown result type (might be due to invalid IL or missing references)
			//IL_001a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0021: Unknown result type (might be due to invalid IL or missing references)
			//IL_0028: Unknown result type (might be due to invalid IL or missing references)
			//IL_002d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0044: Unknown result type (might be due to invalid IL or missing references)
			//IL_0051: Unknown result type (might be due to invalid IL or missing references)
			//IL_005a: Expected O, but got Unknown
			//IL_005a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0066: Unknown result type (might be due to invalid IL or missing references)
			//IL_006c: Expected O, but got Unknown
			//IL_00b2: Unknown result type (might be due to invalid IL or missing references)
			//IL_00be: Unknown result type (might be due to invalid IL or missing references)
			//IL_00c4: Expected O, but got Unknown
			//IL_0107: Unknown result type (might be due to invalid IL or missing references)
			//IL_0113: Unknown result type (might be due to invalid IL or missing references)
			//IL_011a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0121: Unknown result type (might be due to invalid IL or missing references)
			Size clientSize = new Size(500, 70);
			Form val = new Form();
			val.set_FormBorderStyle((FormBorderStyle)3);
			val.set_ClientSize(clientSize);
			((Control)val).set_Text(FormTitle);
			TextBox val2 = new TextBox();
			((Control)val2).set_Size(new Size(clientSize.Width - 10, 23));
			((Control)val2).set_Location(new Point(5, 5));
			((Control)val2).set_Text(input);
			TextBox val3 = val2;
			((Control)val).get_Controls().Add((Control)(object)val3);
			Button val4 = new Button();
			val4.set_DialogResult((DialogResult)1);
			((Control)val4).set_Name("okButton");
			((Control)val4).set_Size(new Size(75, 23));
			((Control)val4).set_Text("&OK");
			((Control)val4).set_Location(new Point(clientSize.Width - 80 - 80, 39));
			((Control)val).get_Controls().Add((Control)(object)val4);
			Button val5 = new Button();
			val5.set_DialogResult((DialogResult)2);
			((Control)val5).set_Name("cancelButton");
			((Control)val5).set_Size(new Size(75, 23));
			((Control)val5).set_Text("&Cancel");
			((Control)val5).set_Location(new Point(clientSize.Width - 80, 39));
			((Control)val).get_Controls().Add((Control)(object)val5);
			val.set_AcceptButton((IButtonControl)(object)val4);
			val.set_CancelButton((IButtonControl)(object)val5);
			DialogResult result = val.ShowDialog();
			input = ((Control)val3).get_Text();
			return result;
		}

		public static void DeleteSupercededFiles()
		{
			try
			{
				((Control)DSF).Show();
			}
			catch
			{
				((Control)new DeleteSupercededFiles()).Show();
			}
		}
	}
}
