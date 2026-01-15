using System;
using System.Collections;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Drawing.Printing;
using System.IO;
using System.IO.Compression;
using System.Reflection;
using System.Text;
using System.Windows.Forms;
using Occult.Lunar_Occultations;
using Occult.Properties;
using Occult.Star_Catalogues;
using VDTimer;

namespace Occult
{
	public class LunarOccultations
	{
		private const double Radian = 180.0 / Math.PI;

		private const double TwoPi = Math.PI * 2.0;

		private const double MoonRadius = 0.2725076;

		public const double km_at_1_AU_to_Arcsec = 0.0013787957192358201;

		public static string AppPath;

		internal static List<LunarOccultationElements> Elements = new List<LunarOccultationElements>();

		internal static LunarOccultationElements Bessel;

		internal static List<LunarPredictionLine> Prediction = new List<LunarPredictionLine>();

		internal static List<LunarPredictionLine> MultiPrediction = new List<LunarPredictionLine>();

		internal static LunarPredictionLine PredictionLine;

		internal static ArrayList GrazeHeader = new ArrayList();

		internal static ArrayList GrazeFooter = new ArrayList();

		internal static ArrayList GrazeSiteScan = new ArrayList();

		internal static List<LunarPredictionLine> GrazePredictionLines = new List<LunarPredictionLine>();

		internal static XZ_File_Management XZManage;

		internal static LunarAsteroidXYZ AsteroidXYZ;

		internal static Lunar_FindAnOccultation LunarFind;

		internal static LunarAutoGenerate AutoGenerate;

		internal static LunarGraze_MagLimits MagLimits;

		internal static LunarOccultation_GrazePathMap GrazeMap;

		public static LunarOccultationPrediction LunarPrediction;

		internal static LunarOccultationWorldView OccultationWorldMap;

		internal static MoonInStarField MoonStarField;

		internal static MoonMap MoonMap;

		internal static StarCatalogueDetails StarCatDetails;

		internal static WorldMap_All WorldMapAll;

		internal static GrazeWeather GrazeWeather;

		internal static LunarOutputFilter OutputFilter;

		public static frmVDTimer VDTimer;

		private static double[] RAMoon = new double[29];

		private static double[] RAMoon2000 = new double[29];

		private static double[] DecMoon = new double[29];

		private static double[] DecMoon2000 = new double[29];

		private static double[] PiMoon2000 = new double[29];

		private static double[] MoonElongation = new double[29];

		private static double[] MoonLong = new double[29];

		private static double[] MoonLat = new double[29];

		private static double RAMoon_1 = 0.0;

		private static double DecMoon_1 = 0.0;

		private static double PiMoon_1 = 1.0;

		private static double RAMoon_2 = 0.0;

		private static double DecMoon_2 = 0.0;

		private static double PiMoon_2 = 1.0;

		internal static double[] PiMoon = new double[28];

		private static bool[] MoonUp = new bool[28];

		private static bool[] SunDown = new bool[28];

		private static double[] RASun = new double[3];

		private static double[] DecSun = new double[3];

		private static double[] DistSun = new double[3];

		internal static int[] XZIndex = new int[361];

		private static double MoonNode;

		private static double NutLongSec1;

		private static double NutLatSec;

		private static double TrueEcliptic;

		private static double StartOfPeriodJD;

		private static double CurrentJD;

		private static double gYear;

		private static double Sidereal;

		private static double deltaT;

		private static double EventPartDay;

		private static bool StartInPreviousDay = false;

		public static double AsteroidCount;

		internal static double VPS_at_Moon;

		internal static double VerticalProfileScale;

		internal static bool ValidIntegrationRange = true;

		internal static bool ListGrazesOnly = false;

		internal static double LastGrazeLatitude = 0.0;

		internal static double GrazeStartLongitude;

		internal static double GrazeEndLongitude;

		internal static double GrazeLongitudeStep;

		internal static double GrazeNominalAltitude;

		internal static int GrazeLimit;

		internal static int GrazeSelectedStar;

		internal static int MapSelectedStar;

		public static double[,] StartEndLongitude = new double[700, 2];

		public static double[,] StartEndLatitude = new double[700, 2];

		public static double[,] LimitLongitude = new double[700, 2];

		public static double[,] LimitLatitude = new double[700, 2];

		public static double[,] LimitAltitude = new double[700, 2];

		public static int[] StartEndCount = new int[2];

		public static int[] MaxCount = new int[3];

		public static int[] LimitCount = new int[5];

		private static int PlotAll_IndividualChartWidth;

		private static int PlotAll_IndividualChartHeight;

		internal static int MoonMapSelectedStar;

		internal static int MultiLocationStar;

		internal static string MultiLoc_Illum;

		internal static string MultiLoc_Elong;

		internal static string MultiLocDate;

		internal static ArrayList ShortD = new ArrayList();

		internal static ArrayList ShortR = new ArrayList();

		internal static ArrayList LongD = new ArrayList();

		internal static ArrayList LongR = new ArrayList();

		internal static ArrayList MultiSites = new ArrayList();

		public static void CloseAllLunarPredictionForms()
		{
			try
			{
				((Form)XZManage).Close();
				((Component)(object)XZManage).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)AsteroidXYZ).Close();
				((Component)(object)AsteroidXYZ).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)LunarFind).Close();
				((Component)(object)LunarFind).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)AutoGenerate).Close();
				((Component)(object)AutoGenerate).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)MagLimits).Close();
				((Component)(object)MagLimits).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)GrazeMap).Close();
				((Component)(object)GrazeMap).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)LunarPrediction).Close();
				((Component)(object)LunarPrediction).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)OccultationWorldMap).Close();
				((Component)(object)OccultationWorldMap).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)MoonStarField).Close();
				((Component)(object)MoonStarField).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)MoonMap).Close();
				((Component)(object)MoonMap).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)StarCatDetails).Close();
				((Component)(object)StarCatDetails).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)WorldMapAll).Close();
				((Component)(object)WorldMapAll).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)LunarProfile.GrazeProfile).Close();
				((Component)(object)LunarProfile.GrazeProfile).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)GrazeWeather).Close();
				((Component)(object)GrazeWeather).Dispose();
			}
			catch
			{
			}
		}

		public static void InitialiseStarCatIndexArray(string Catalogue)
		{
			StreamReader streamReader = new StreamReader(Catalogue.Replace(".dat", ".inx"));
			for (int i = 0; i <= 360; i++)
			{
				XZIndex[i] = Convert.ToInt32(streamReader.ReadLine()) - 1;
			}
			streamReader.Close();
		}

		public static void InitialiseSearch()
		{
			Elements.Clear();
			JPL_DE.InitialiseDE_Ephemeris();
		}

		public static void NewDate(double JD, int StartHour, bool SiteSpecific, double North_deg, double South_deg, double East_deg, double West_deg)
		{
			double[] RA = new double[6];
			double[] Dec = new double[6];
			double[] Parallax = new double[6];
			double num = JD;
			float LongCorrection_arcsec1e = 0f;
			float LatCorrection_arcsec1e = 0f;
			int Source = 0;
			CurrentJD = JD;
			gYear = (CurrentJD - 2451545.0) / 365.25;
			StartOfPeriodJD = JD + (double)StartHour / 24.0;
			StartInPreviousDay = StartHour < 0;
			if (StartHour < 0)
			{
				num -= 1.0;
				StartHour += 24;
			}
			Utilities.MoonSeries_Retrieve(num, out RA, out Dec, out Parallax, out LongCorrection_arcsec1e, out LatCorrection_arcsec1e, out Source);
			for (int i = -2; i <= 27; i++)
			{
				int num2 = StartHour + i;
				switch (i)
				{
				case -2:
					Utilities.MoonPositionFromSeries((double)num2 / 24.0, RA, Dec, Parallax, out RAMoon_2, out DecMoon_2, out PiMoon_2);
					RAMoon_2 /= 180.0 / Math.PI;
					DecMoon_2 /= 180.0 / Math.PI;
					PiMoon_2 /= 180.0 / Math.PI;
					continue;
				case -1:
					Utilities.MoonPositionFromSeries((double)num2 / 24.0, RA, Dec, Parallax, out RAMoon_1, out DecMoon_1, out PiMoon_1);
					RAMoon_1 /= 180.0 / Math.PI;
					DecMoon_1 /= 180.0 / Math.PI;
					PiMoon_1 /= 180.0 / Math.PI;
					continue;
				}
				if (num2 == 24)
				{
					num += 1.0;
					StartHour -= 24;
					num2 = StartHour + i;
					Utilities.MoonSeries_Retrieve(num, out RA, out Dec, out Parallax, out LongCorrection_arcsec1e, out LatCorrection_arcsec1e, out Source);
				}
				Utilities.MoonPositionFromSeries((double)num2 / 24.0, RA, Dec, Parallax, out RAMoon[i], out DecMoon[i], out PiMoon[i]);
				RAMoon[i] /= 180.0 / Math.PI;
				DecMoon[i] /= 180.0 / Math.PI;
				PiMoon[i] /= 180.0 / Math.PI;
				RAMoon2000[i] = RAMoon[i];
				DecMoon2000[i] = DecMoon[i];
				Utilities.PrecessStartToEnd(JD, 2451545.5, use2006values_Not1976: false, ref RAMoon2000[i], ref DecMoon2000[i]);
				if (i > 0)
				{
					if (RAMoon[i] - RAMoon[0] < 0.0)
					{
						RAMoon[i] += Math.PI * 2.0;
					}
					if (RAMoon2000[i] - RAMoon2000[0] < 0.0)
					{
						RAMoon2000[i] += Math.PI * 2.0;
					}
				}
			}
			double num3 = (num + (double)StartHour / 24.0 + 0.5 - 2451545.0) / 36525.0;
			MoonNode = (125.04435980001 - 1934.13618494 * num3 - 0.0047362 * num3 * num3) % 360.0 / (180.0 / Math.PI);
			Utilities.PlanetGeocentric(num + (double)StartHour / 24.0, 3, 1E-05, 0, out RASun[0], out DecSun[0], out DistSun[0]);
			Utilities.PlanetGeocentric(num + (double)StartHour / 24.0 + 0.5, 3, 1E-05, 0, out RASun[1], out DecSun[1], out DistSun[1]);
			Utilities.PlanetGeocentric(num + (double)StartHour / 24.0 + 1.0, 3, 1E-05, 0, out RASun[2], out DecSun[2], out DistSun[2]);
			if (RASun[1] < RASun[0])
			{
				RASun[1] += Math.PI * 2.0;
			}
			if (RASun[2] < RASun[0])
			{
				RASun[2] += Math.PI * 2.0;
			}
			Utilities.PolynomialFit(RASun[0], RASun[1], RASun[2], out var _, out var dT, out var d2T);
			Utilities.PolynomialFit(DecSun[0], DecSun[1], DecSun[2], out var _, out var dT2, out var d2T2);
			RASun[1] = dT / 12.0;
			RASun[2] = d2T / 144.0;
			DecSun[1] = dT2 / 12.0;
			DecSun[2] = d2T2 / 144.0;
			for (int j = 0; j <= 27; j++)
			{
				Utilities.Distance(RASun[0] + (double)j * RASun[1], DecSun[0] + (double)j * DecSun[1], RAMoon[j], DecMoon[j], out var Distance, out var _);
				MoonElongation[j] = Distance * (180.0 / Math.PI);
			}
			Sidereal = Utilities.SiderealTime_deg(num + (double)StartHour / 24.0, Apparent: true) / (180.0 / Math.PI);
			Utilities.Nutation(num + (double)StartHour / 24.0 + 0.5, out NutLongSec1, out NutLatSec, out TrueEcliptic);
			for (int k = 0; k <= 27; k++)
			{
				Utilities.RA_DEC_to_Long_Lat(RAMoon[k], DecMoon[k], TrueEcliptic, ref MoonLong[k], ref MoonLat[k]);
				if (k > 0 && MoonLong[k] < MoonLong[0])
				{
					MoonLong[k] += Math.PI * 2.0;
				}
			}
			if (SiteSpecific && East_deg - West_deg < 120.0)
			{
				for (int l = 0; l <= 27; l++)
				{
					MoonUp[l] = Utilities.ObjectUp_SunBelowCivil(num, StartHour + l, RAMoon[l], DecMoon[l], West_deg / (180.0 / Math.PI), North_deg / (180.0 / Math.PI), RequireSunDown: false);
					MoonUp[l] |= Utilities.ObjectUp_SunBelowCivil(num, StartHour + l, RAMoon[l], DecMoon[l], East_deg / (180.0 / Math.PI), North_deg / (180.0 / Math.PI), RequireSunDown: false);
					MoonUp[l] |= Utilities.ObjectUp_SunBelowCivil(num, StartHour + l, RAMoon[l], DecMoon[l], West_deg / (180.0 / Math.PI), South_deg / (180.0 / Math.PI), RequireSunDown: false);
					MoonUp[l] |= Utilities.ObjectUp_SunBelowCivil(num, StartHour + l, RAMoon[l], DecMoon[l], East_deg / (180.0 / Math.PI), South_deg / (180.0 / Math.PI), RequireSunDown: false);
					MoonUp[l] |= Utilities.ObjectUp_SunBelowCivil(num, StartHour + l, RAMoon[l], DecMoon[l], (East_deg + West_deg) / 2.0 / (180.0 / Math.PI), (North_deg + South_deg) / 2.0 / (180.0 / Math.PI), RequireSunDown: false);
					SunDown[l] = Utilities.ObjectUp_SunBelowCivil(num, StartHour + l, RAMoon[l], DecMoon[l], West_deg / (180.0 / Math.PI), North_deg / (180.0 / Math.PI), RequireSunDown: true, -14.0);
					SunDown[l] |= Utilities.ObjectUp_SunBelowCivil(num, StartHour + l, RAMoon[l], DecMoon[l], East_deg / (180.0 / Math.PI), North_deg / (180.0 / Math.PI), RequireSunDown: true, -14.0);
					SunDown[l] |= Utilities.ObjectUp_SunBelowCivil(num, StartHour + l, RAMoon[l], DecMoon[l], West_deg / (180.0 / Math.PI), South_deg / (180.0 / Math.PI), RequireSunDown: true, -14.0);
					SunDown[l] |= Utilities.ObjectUp_SunBelowCivil(num, StartHour + l, RAMoon[l], DecMoon[l], East_deg / (180.0 / Math.PI), South_deg / (180.0 / Math.PI), RequireSunDown: true, -14.0);
					SunDown[l] |= Utilities.ObjectUp_SunBelowCivil(num, StartHour + l, RAMoon[l], DecMoon[l], (East_deg + West_deg) / 2.0 / (180.0 / Math.PI), (North_deg + South_deg) / 2.0 / (180.0 / Math.PI), RequireSunDown: true, -14.0);
				}
			}
			else
			{
				for (int m = 0; m <= 27; m++)
				{
					MoonUp[m] = true;
					SunDown[m] = true;
				}
			}
			deltaT = Utilities.delta_T(num) / 3600.0;
			if (Settings.Default.Lunar_IncludeAsteroids)
			{
				ValidIntegrationRange = ValidateIntegrationRange(StartOfPeriodJD);
				try
				{
					((Control)LunarPrediction.labelIntegration).set_Visible(!ValidIntegrationRange);
				}
				catch
				{
				}
			}
		}

		internal static bool ValidateIntegrationRange(double StartOfPeriodJD)
		{
			//IL_00a1: Unknown result type (might be due to invalid IL or missing references)
			//IL_00d8: Unknown result type (might be due to invalid IL or missing references)
			ValidIntegrationRange = true;
			int num = 245;
			int num2 = (int)Math.Floor(StartOfPeriodJD / 10000.0);
			if (num2 < 245)
			{
				num = num2;
				num2 = 245;
			}
			for (int i = num; i <= num2; i++)
			{
				if (File.Exists(AppPath + "\\Resource Files\\" + Convert.ToString(i) + "0000_XYZ.bin"))
				{
					if (new FileInfo(AppPath + "\\Resource Files\\" + Convert.ToString(i) + "0000_XYZ.bin").Length != 1200000)
					{
						ValidIntegrationRange = false;
					}
				}
				else
				{
					ValidIntegrationRange = false;
				}
			}
			if (!ValidIntegrationRange)
			{
				try
				{
					((Form)AsteroidXYZ).ShowDialog();
				}
				catch
				{
					AsteroidXYZ = new LunarAsteroidXYZ();
					((Control)AsteroidXYZ).set_Text("Provide planetary coordinates to integrate the orbits of asteroids to " + Utilities.Date_from_JD(CurrentJD, 0));
					((Form)AsteroidXYZ).ShowDialog();
				}
			}
			ValidIntegrationRange = true;
			for (int j = num; j <= num2; j++)
			{
				if (File.Exists(AppPath + "\\Resource Files\\" + Convert.ToString(j) + "0000_XYZ.bin"))
				{
					if (new FileInfo(AppPath + "\\Resource Files\\" + Convert.ToString(j) + "0000_XYZ.bin").Length != 1200000)
					{
						ValidIntegrationRange = false;
					}
				}
				else
				{
					ValidIntegrationRange = false;
				}
			}
			return ValidIntegrationRange;
		}

		internal static double GetReadMagLimit(double TelescopeDiaCM, double MagLimitCorrection)
		{
			double num = 0.0;
			LunarPredictionLine PredictionLine = new LunarPredictionLine();
			PredictionLine.SunAlt = -90.0;
			PredictionLine.MoonAlt = 90.0;
			PredictionLine.CA = 90.0;
			Utilities.Distance(RASun[0], DecSun[0], RAMoon[0], DecMoon[0], out var Distance, out var _);
			PredictionLine.Elongation = Distance * (180.0 / Math.PI);
			PredictionLine.Illumination = 50.0 * (1.0 - Math.Cos(Distance));
			double num2 = visibility(ref PredictionLine, 10.0 * TelescopeDiaCM, MagLimitCorrection);
			if (num < num2)
			{
				num = num2;
			}
			Utilities.Distance(RASun[0] + 24.0 * RASun[1], DecSun[0] + 24.0 * DecSun[1], RAMoon[24], DecMoon[24], out var Distance2, out var _);
			PredictionLine.Elongation = Distance2 * (180.0 / Math.PI);
			PredictionLine.Illumination = 50.0 * (1.0 - Math.Cos(Distance2));
			num2 = visibility(ref PredictionLine, 10.0 * TelescopeDiaCM, MagLimitCorrection);
			if (num < num2)
			{
				num = num2;
			}
			return num;
		}

		public static void ScanXZFile(string CatalogueID, double NthLimit, double SthLimit, double ReadMagLimit, bool DoublesOnly, bool Auto)
		{
			bool lunarElements_FilterBySite = Settings.Default.LunarElements_FilterBySite;
			bool flag = false;
			double RA = 0.0;
			double Dec = 0.0;
			double num = 0.0;
			int num2 = 0;
			int num3 = 0;
			Elements.Clear();
			double num4 = 0.0;
			for (int i = 0; i < 25; i++)
			{
				if (MoonElongation[i] > 179.0)
				{
					if (MoonElongation[i] < num4)
					{
						break;
					}
					flag = true;
					RA = RASun[0] + (double)i * RASun[1] + Math.PI;
					Dec = 0.0 - (DecSun[0] + (double)i * DecSun[1]);
					Utilities.PrecessToJ2000(CurrentJD, use2006values_Not1976: false, ref RA, ref Dec);
					num4 = MoonElongation[i];
				}
			}
			if (Settings.Default.Lunar_IncludePlanets | Settings.Default.Lunar_IncludeAsteroids)
			{
				ScanForPlanets();
			}
			if (!Settings.Default.Lunar_IncludeStars)
			{
				return;
			}
			using (FileStream fileStream = new FileStream(CatalogueID, FileMode.Open, FileAccess.Read))
			{
				using BinaryReader readXZ = new BinaryReader(fileStream);
				int num5 = (int)Math.Floor(RAMoon2000[0] * (180.0 / Math.PI) - 3.0);
				int num6 = (int)Math.Floor(RAMoon2000[24] * (180.0 / Math.PI) + 2.0);
				if (num5 < 1)
				{
					num5 += 360;
				}
				if (num6 < 1)
				{
					num6 += 360;
				}
				if (num5 >= 360)
				{
					num5 -= 360;
				}
				if (num6 >= 360)
				{
					num6 -= 360;
				}
				int num7 = XZIndex[num5];
				int num8 = XZIndex[num6];
				int num9 = XZIndex[360];
				num2 = num8 - num7 + 1;
				if (Auto)
				{
					((Control)AutoGenerate.pBar).set_Visible(true);
				}
				else
				{
					((Control)LunarPrediction.pBar).set_Visible(true);
				}
				int num10 = num8;
				int num11 = 0;
				if (num7 >= num8)
				{
					num10 = num9;
					num11 = 1;
					num2 = num9 - num8 + num7 + 1;
				}
				if (Auto)
				{
					AutoGenerate.pBar.set_Minimum(0);
					AutoGenerate.pBar.set_Maximum(num2);
				}
				else
				{
					LunarPrediction.pBar.set_Minimum(0);
					LunarPrediction.pBar.set_Maximum(num2);
				}
				int num12 = num7;
				while (true)
				{
					if (Auto)
					{
						AutoGenerate.pBar.set_Value(num3);
					}
					else
					{
						LunarPrediction.pBar.set_Value(num3);
					}
					XZ80Q.ReadStarEntry(fileStream, readXZ, num12);
					if ((XZ80Q.Dec_rad > 1.0) | (XZ80Q.Dec_rad < -1.0))
					{
						num12++;
					}
					else
					{
						bool flag2 = false;
						if (flag)
						{
							double num13 = XZ80Q.RA_rad - RA;
							if (num13 > 4.0)
							{
								num13 -= Math.PI * 2.0;
							}
							if (num13 < -4.0)
							{
								num13 += Math.PI * 2.0;
							}
							if ((Math.Abs(num13) < 0.017) & (Math.Abs(XZ80Q.Dec_rad - Dec) < 0.017))
							{
								flag2 = true;
							}
						}
						if (((ReadMagLimit > XZ80Q.Mv) & (XZ80Q.Mv < 12.0)) || flag2)
						{
							double num14 = XZ80Q.RA_rad;
							while (num14 - RAMoon2000[0] > Math.PI)
							{
								num14 -= Math.PI * 2.0;
							}
							for (; num14 - RAMoon2000[0] < -Math.PI; num14 += Math.PI * 2.0)
							{
							}
							int j;
							for (j = 0; num14 - RAMoon2000[j] > 0.0 && j < 24; j++)
							{
							}
							int num15 = j - 1;
							int num16 = j + 1;
							if (num15 < 0)
							{
								num15 = 0;
							}
							if (num16 > 27)
							{
								num16 = 27;
							}
							if (j < 0)
							{
								goto IL_0f24;
							}
							if ((MoonUp[num15] | MoonUp[num16]) && (SunDown[num15] | SunDown[num16] | (!SunDown[j] & (XZ80Q.Mv < 6.0))) && !(Math.Abs((num14 - RAMoon2000[j]) / (RAMoon2000[j + 1] - RAMoon2000[j]) * (DecMoon2000[j + 1] - DecMoon2000[j]) + DecMoon2000[j] - XZ80Q.Dec_rad) > 1.5 * PiMoon[j] + 2E-05 * Math.Abs(2000.0 - gYear)))
							{
								double num17 = (num14 = XZ80Q.RA_rad);
								double Dec_end;
								double num18 = (Dec_end = XZ80Q.Dec_rad);
								num = XZ80Q.Parallax_Rad;
								Utilities.ApparentStarPosition(num17, num18, XZ80Q.PMRA_rad, XZ80Q.PMDec_rad, num, 2000, IncludeRelativisticBending: true, CurrentJD, use2006values_Not1976: true, out num14, out Dec_end);
								while (num14 - RAMoon[0] > Math.PI)
								{
									num14 -= Math.PI * 2.0;
								}
								for (; num14 - RAMoon[0] < -Math.PI; num14 += Math.PI * 2.0)
								{
								}
								j = (int)(24.0 * (num14 - RAMoon[0]) / (RAMoon[24] - RAMoon[0]));
								if (j >= 0 && j <= 24)
								{
									BesselianElements(j - 2, num14, Dec_end, out var X, out var Y, out var _);
									BesselianElements(j, num14, Dec_end, out var X2, out var Y2, out var DMoon2);
									BesselianElements(j + 1, num14, Dec_end, out var X3, out var Y3, out var DMoon3);
									BesselianElements(j + 2, num14, Dec_end, out var X4, out var Y4, out var DMoon4);
									BesselianElements(j + 3, num14, Dec_end, out var X5, out var Y5, out var _);
									Utilities.PolynomialFit(X, X2, X3, X4, X5, out var T, out var dT, out var d2T, out var d3T, out var d4T);
									Utilities.PolynomialFit(Y, Y2, Y3, Y4, Y5, out var T2, out var dT2, out var d2T2, out var d3T2, out var d4T2);
									Utilities.PolynomialFit(DMoon2, DMoon3, DMoon4, out var T3, out var dT3, out var d2T3);
									double num19 = Math.Atan(Math.Abs(dT / dT2));
									double num20 = Math.Sin(num19);
									double num21 = (T2 - dT2 * T / dT) * num20;
									double num22 = num21 + 0.2725076;
									double num23 = num21 - 0.2725076;
									double num24 = num20 * Math.Cos(Dec_end);
									if (!((Math.Abs(num22) > 1.0) & (Math.Abs(num23) > 1.0)))
									{
										double num25;
										double num26;
										if ((NthLimit > 60.0 && SthLimit < -60.0) || !lunarElements_FilterBySite)
										{
											num25 = 90.0;
											num26 = -90.0;
										}
										else if (!(Dec_end > 0.0))
										{
											num26 = ((num22 < 0.0 - num24) ? (Math.Acos(num22) * (180.0 / Math.PI) - Math.Asin(num24) * (180.0 / Math.PI) - 180.0) : ((!(num23 < 0.0 - num24)) ? ((Math.Asin(num24) - Math.Acos(num23)) * (180.0 / Math.PI)) : (-90.0)));
											num25 = ((num22 < num20) ? (Math.Asin(Math.Sin(num19 + Math.Acos(num22)) * Math.Cos(Dec_end)) * (180.0 / Math.PI)) : ((!(num22 < 1.0 || (num22 >= 1.0 && num23 < num20))) ? (Math.Asin(Math.Sin(num19 + Math.Acos(num23)) * Math.Cos(Dec_end)) * (180.0 / Math.PI)) : (90.0 + Dec_end * (180.0 / Math.PI))));
										}
										else
										{
											num25 = ((num23 > num24) ? ((Math.Asin(num24) + Math.Acos(num23)) * (180.0 / Math.PI)) : ((!(num22 > num24)) ? (180.0 - Math.Asin(num24) * (180.0 / Math.PI) - Math.Acos(num22) * (180.0 / Math.PI)) : 90.0));
											num26 = ((num23 > 0.0 - num20) ? (Math.Asin(Math.Sin(num19 - Math.Acos(num23)) * Math.Cos(Dec_end)) * (180.0 / Math.PI)) : ((!(num23 > -1.0 || (num23 <= -1.0 && num22 > 0.0 - num20))) ? (Math.Asin(Math.Sin(num19 - Math.Acos(num22)) * Math.Cos(Dec_end)) * (180.0 / Math.PI)) : (Dec_end * (180.0 / Math.PI) - 90.0)));
										}
										if (num25 >= SthLimit && num26 <= NthLimit)
										{
											double h = Sidereal + 1.00273791 * ((double)j - deltaT) / 12.0 * Math.PI - num14;
											double num27 = (double)j - T / dT;
											if (num27 >= 0.0 && num27 < 24.002)
											{
												Bessel = new LunarOccultationElements();
												Bessel.StarRA = num14;
												Bessel.StarDec = Dec_end;
												Bessel.StarRA2000 = num17;
												Bessel.StarDec2000 = num18;
												Bessel.DeltaDec = 0.0;
												Bessel.JDzero = StartOfPeriodJD;
												Bessel.T = j;
												Bessel.X = T;
												Bessel.dX = dT;
												Bessel.d2X = d2T;
												Bessel.d3X = d3T;
												Bessel.d4X = d4T;
												Bessel.Y = T2;
												Bessel.dY = dT2;
												Bessel.d2Y = d2T2;
												Bessel.d3Y = d3T2;
												Bessel.d4Y = d4T2;
												Bessel.DMoon = T3;
												Bessel.dDMoon = dT3;
												Bessel.d2DMoon = d2T3;
												Bessel.H = h;
												Bessel.DeltaH = 0.0;
												Bessel.ShadowIncreaseOnFundamentalPlane = 0.0;
												Bessel.SinF2 = 0.0;
												Bessel.XZNum = XZ80Q.XZ;
												Bessel.DoubleCode = XZ80Q.DoubleFlag;
												Bessel.StarVar = XZ80Q.VariableFlag;
												Bessel.StarSpectrum = XZ80Q.Spectrum;
												Bessel.StarCert = " ";
												Bessel.PlanetId = "";
												if (XZ80Q.ZC > 0)
												{
													Bessel.StarId = string.Format("{0,7:F0}", XZ80Q.ZC) + XZ80Q.DoubleFlag + XZ80Q.Spectrum;
												}
												else if (XZ80Q.SAO > 0)
												{
													Bessel.StarId = string.Format("{0,7:F0}", XZ80Q.SAO) + XZ80Q.DoubleFlag + XZ80Q.Spectrum;
												}
												else if (XZ80Q.XZ > 0)
												{
													Bessel.StarId = string.Format("X{0,6:F0}", XZ80Q.XZ) + XZ80Q.DoubleFlag + XZ80Q.Spectrum;
												}
												if (XZ80Q.ZC > 0 && ZCName(XZ80Q.ZC, out var Name))
												{
													Bessel.ZCName = Name;
												}
												if (XZ80Q.VariableFlag != " " && VariableStarInfo(XZ80Q.XZ, Bessel.JDzero + (double)Bessel.T / 24.0, out var VariableDetails))
												{
													Bessel.VariableDetails = "   " + Bessel.StarId.Substring(0, 7) + " = " + VariableDetails;
												}
												if ("KkPp".Contains(XZ80Q.DoubleFlag) && Kepler2.GetKepler2InfoForXZ(XZ80Q.XZ, out var ID, out var Cadence))
												{
													Bessel.Kepler2ID = ID;
													Bessel.Kepler2Cadence = Cadence;
												}
												if (XZ80Q.DoubleFlag != " ")
												{
													string[] array = new string[4] { "", "", "", "" };
													bool[] array2 = new bool[4];
													double[] array3 = new double[4];
													double[] array4 = new double[4];
													DoubleStars.GetXZDoubleMatches(XZ80Q.XZ, Bessel.JDzero, ForReductions: false, out var Count, array, array3, array4, array2, out var _, out var DiscovererName, out var NonInstantaneous, out var ObservationsWanted);
													Bessel.NonInstantaneous = NonInstantaneous;
													Bessel.ObservationsWanted = ObservationsWanted;
													Bessel.DoubleCount = Count;
													if (Count > 0)
													{
														Bessel.Doub1 = array[0];
														Bessel.PA1 = array3[0];
														Bessel.Sep1 = array4[0];
														Bessel.IsMean1 = array2[0];
														Bessel.DoubID1 = DiscovererName.Substring(0, 7);
													}
													if (Count > 1)
													{
														Bessel.Doub2 = array[1];
														Bessel.PA2 = array3[1];
														Bessel.Sep2 = array4[1];
														Bessel.IsMean2 = array2[1];
														Bessel.DoubID2 = DiscovererName.Substring(7, 7);
													}
													if (Count > 2)
													{
														Bessel.Doub3 = array[2];
														Bessel.PA3 = array3[2];
														Bessel.Sep3 = array4[2];
														Bessel.IsMean3 = array2[2];
														Bessel.DoubID3 = DiscovererName.Substring(14, 7);
													}
													if (Count > 3)
													{
														Bessel.Doub4 = array[3];
														Bessel.PA4 = array3[3];
														Bessel.Sep4 = array4[3];
														Bessel.IsMean4 = array2[3];
														Bessel.DoubID4 = DiscovererName.Substring(21, 7);
													}
												}
												Bessel.Mv = XZ80Q.Mv;
												Bessel.Mp = XZ80Q.Mp;
												Bessel.Mr = XZ80Q.Mr;
												Bessel.PlanetDiaArcSec = 0.0;
												Bessel.PlanetNum = 0;
												LunarOccultationElements bessel = Bessel;
												LunarOccultationElements bessel2 = Bessel;
												LunarOccultationElements bessel3 = Bessel;
												double num29 = (Bessel.EarthDec_deg = 0.0);
												double num31 = (bessel3.PAPole_deg = num29);
												double num34 = (bessel.PA_BrightLimb_deg = (bessel2.PhaseAngle_deg = num31));
												Bessel.Illumination = 1.0;
												Bessel.LightCurveAvailable = false;
												if (!DoublesOnly | Bessel.ObservationsWanted | (Bessel.Kepler2ID > 0))
												{
													Elements.Add(Bessel);
												}
											}
										}
									}
								}
							}
						}
						num12++;
						num3++;
					}
					goto IL_0f24;
					IL_0f24:
					if (num12 > num10)
					{
						if (num11 == 0)
						{
							break;
						}
						num11 = 0;
						num12 = 0;
						num10 = num8;
						if (num11 < 0)
						{
							break;
						}
					}
				}
			}
			if (Auto)
			{
				((Control)AutoGenerate.pBar).set_Visible(false);
			}
			else
			{
				((Control)LunarPrediction.pBar).set_Visible(false);
			}
		}

		internal static void BesselianElements(int t, double RA, double Dec, out double X, out double Y, out double DMoon)
		{
			switch (t)
			{
			case -2:
				X = Math.Cos(DecMoon_2) * Math.Sin(RAMoon_2 - RA) / Math.Sin(PiMoon_2);
				Y = (Math.Sin(DecMoon_2) * Math.Cos(Dec) - Math.Cos(DecMoon_2) * Math.Sin(Dec) * Math.Cos(RAMoon_2 - RA)) / Math.Sin(PiMoon_2);
				DMoon = 6378.137 / Math.Sin(PiMoon_2);
				break;
			case -1:
				X = Math.Cos(DecMoon_1) * Math.Sin(RAMoon_1 - RA) / Math.Sin(PiMoon_1);
				Y = (Math.Sin(DecMoon_1) * Math.Cos(Dec) - Math.Cos(DecMoon_1) * Math.Sin(Dec) * Math.Cos(RAMoon_1 - RA)) / Math.Sin(PiMoon_1);
				DMoon = 6378.137 / Math.Sin(PiMoon_1);
				break;
			default:
				X = Math.Cos(DecMoon[t]) * Math.Sin(RAMoon[t] - RA) / Math.Sin(PiMoon[t]);
				Y = (Math.Sin(DecMoon[t]) * Math.Cos(Dec) - Math.Cos(DecMoon[t]) * Math.Sin(Dec) * Math.Cos(RAMoon[t] - RA)) / Math.Sin(PiMoon[t]);
				DMoon = 6378.137 / Math.Sin(PiMoon[t]);
				break;
			}
		}

		internal static void PlanetBesselianElements(int t, double RAPlanet, double DecPlanet, double DistPlanet, out double X, out double Y, out double ShadowIncreaseOnFundamentalPlane, out double sinF2, out double DMoon, out double RA_ShadowAxis, out double Dec_ShadowAxis)
		{
			double a = 4.2635E-05 / DistPlanet;
			double num2;
			double num3;
			double num4;
			double num5;
			if (t == -1)
			{
				double num = Math.Sin(a) / Math.Sin(PiMoon_1);
				num2 = Math.Cos(DecPlanet) * Math.Cos(RAPlanet) - num * Math.Cos(DecMoon_1) * Math.Cos(RAMoon_1);
				num3 = Math.Cos(DecPlanet) * Math.Sin(RAPlanet) - num * Math.Cos(DecMoon_1) * Math.Sin(RAMoon_1);
				num4 = Math.Sin(DecPlanet) - num * Math.Sin(DecMoon_1);
				RA_ShadowAxis = Math.Atan2(num3, num2);
				if (RA_ShadowAxis < 0.0)
				{
					RA_ShadowAxis += Math.PI * 2.0;
				}
				Dec_ShadowAxis = Math.Atan(num4 / Math.Sqrt(num2 * num2 + num3 * num3));
				X = Math.Cos(DecMoon_1) * Math.Sin(RAMoon_1 - RA_ShadowAxis) / Math.Sin(PiMoon_1);
				Y = (Math.Sin(DecMoon_1) * Math.Cos(Dec_ShadowAxis) - Math.Cos(DecMoon_1) * Math.Sin(Dec_ShadowAxis) * Math.Cos(RAMoon_1 - RA_ShadowAxis)) / Math.Sin(PiMoon_1);
				num5 = (Math.Sin(DecMoon_1) * Math.Sin(Dec_ShadowAxis) + Math.Cos(DecMoon_1) * Math.Cos(Dec_ShadowAxis) * Math.Cos(RAMoon_1 - RA_ShadowAxis)) / Math.Sin(PiMoon_1);
				DMoon = 6378.137 / Math.Sin(PiMoon_1);
			}
			else
			{
				double num = Math.Sin(a) / Math.Sin(PiMoon[t]);
				num2 = Math.Cos(DecPlanet) * Math.Cos(RAPlanet) - num * Math.Cos(DecMoon[t]) * Math.Cos(RAMoon[t]);
				num3 = Math.Cos(DecPlanet) * Math.Sin(RAPlanet) - num * Math.Cos(DecMoon[t]) * Math.Sin(RAMoon[t]);
				num4 = Math.Sin(DecPlanet) - num * Math.Sin(DecMoon[t]);
				RA_ShadowAxis = Math.Atan2(num3, num2);
				if (RA_ShadowAxis < 0.0)
				{
					RA_ShadowAxis += Math.PI * 2.0;
				}
				Dec_ShadowAxis = Math.Atan(num4 / Math.Sqrt(num2 * num2 + num3 * num3));
				X = Math.Cos(DecMoon[t]) * Math.Sin(RAMoon[t] - RA_ShadowAxis) / Math.Sin(PiMoon[t]);
				Y = (Math.Sin(DecMoon[t]) * Math.Cos(Dec_ShadowAxis) - Math.Cos(DecMoon[t]) * Math.Sin(Dec_ShadowAxis) * Math.Cos(RAMoon[t] - RA_ShadowAxis)) / Math.Sin(PiMoon[t]);
				num5 = (Math.Sin(DecMoon[t]) * Math.Sin(Dec_ShadowAxis) + Math.Cos(DecMoon[t]) * Math.Cos(Dec_ShadowAxis) * Math.Cos(RAMoon[t] - RA_ShadowAxis)) / Math.Sin(PiMoon[t]);
				DMoon = 6378.137 / Math.Sin(PiMoon[t]);
			}
			double num6 = Math.Sqrt(num2 * num2 + num3 * num3 + num4 * num4);
			sinF2 = -0.2725076 * Math.Sin(a) / num6;
			double num7 = (0.0 - (num5 - 0.2725076 / sinF2)) * sinF2 / Math.Sqrt(1.0 - sinF2 * sinF2);
			ShadowIncreaseOnFundamentalPlane = num7 - 0.2725076;
		}

		internal static void ScanForPlanets()
		{
			double RA = 0.0;
			double Dec = 0.0;
			double RA2 = 0.0;
			double Dec2 = 0.0;
			int[,] array = new int[2, 6]
			{
				{ 1, 2, 3, 4, 0, 0 },
				{ 2, 3, 4, 5, 6, 8 }
			};
			double[] array2 = new double[7];
			double[] array3 = new double[7];
			double[] array4 = new double[7];
			double[] array5 = new double[7];
			double[] array6 = new double[7];
			double[] array7 = new double[7];
			double[] array8 = new double[7];
			double[] array9 = new double[7];
			double[] array10 = new double[4];
			double[] array11 = new double[4];
			double[] array12 = new double[3];
			double Diameter = 0.0;
			double Magnitude = 0.0;
			double OsculatingDate = 0.0;
			double num = 0.0;
			float[] array13 = new float[7];
			float[] array14 = new float[7];
			string[] array15 = new string[7];
			bool flag = false;
			double EpochMeanAnomaly = 0.0;
			double a = 0.0;
			double num2 = 0.0;
			double e = 0.0;
			double i = 0.0;
			double node = 0.0;
			double perihelion = 0.0;
			double num3 = 0.0;
			double num4 = 0.0;
			double num5 = 5.0;
			double integrateTo_Date = 0.0;
			int num6 = 0;
			double ecliptic = Utilities.MeanEclipticOfDate(StartOfPeriodJD, Use1976Value: true);
			double Longitude = 0.0;
			double Longitude2 = 0.0;
			double Latitude = 0.0;
			double Latitude2 = 0.0;
			double num7 = 0.0;
			double num8 = 0.0;
			double Longitude3 = 0.0;
			double Latitude3 = 0.0;
			double Longitude4 = 0.0;
			double Latitude4 = 0.0;
			AsteroidElements asteroidElements = new AsteroidElements();
			List<AsteroidElements> elementsAt1Day = new List<AsteroidElements>();
			List<AsteroidElements> astElements = Occult.Elements.UserAsteroids_Lunar.AstElements;
			if (astElements.Count < 1)
			{
				Occult.Elements.UserAsteroids_Lunar.Fill_AllAsteroids();
			}
			int num9 = 1;
			int num10 = 9 + astElements.Count;
			if ((StartOfPeriodJD < 2422324.5) | (StartOfPeriodJD > 2469807.5))
			{
				num10 = 9;
			}
			if (!Settings.Default.Lunar_IncludePlanets)
			{
				num9 = 10;
			}
			if (!Settings.Default.Lunar_IncludeAsteroids)
			{
				num10 = 9;
			}
			Utilities.RA_DEC_to_Long_Lat(RAMoon[0], DecMoon[0], ecliptic, ref Longitude, ref Latitude);
			Utilities.RA_DEC_to_Long_Lat(RAMoon[24], DecMoon[24], ecliptic, ref Longitude2, ref Latitude2);
			if (Longitude > Longitude2)
			{
				Longitude2 += Math.PI * 2.0;
			}
			for (int j = num9; j <= num10; j++)
			{
				double num11 = 0.03;
				if (j < 10)
				{
					num11 = 0.05 / Math.Sqrt(j);
				}
				if (j == 3)
				{
					continue;
				}
				flag = false;
				int result;
				double RadiusVector;
				double Elongation;
				double PhaseAngle;
				if (j < 10)
				{
					Utilities.QuickPlanet(StartOfPeriodJD, j, EquinoxOfDate: true, out RA, out Dec, out var GeocentricDistance);
					Utilities.QuickPlanet(StartOfPeriodJD + 1.0, j, EquinoxOfDate: true, out RA2, out Dec2, out GeocentricDistance);
				}
				else
				{
					asteroidElements = astElements[j - 10];
					num6 = asteroidElements.IDNumber;
					string iDName = asteroidElements.IDName;
					OsculatingDate = Utilities.JD_from_Date(asteroidElements.EpochYear, asteroidElements.EpochMonth, asteroidElements.EpochDay);
					EpochMeanAnomaly = asteroidElements.Meananomaly / (180.0 / Math.PI);
					perihelion = asteroidElements.Perihelion / (180.0 / Math.PI);
					node = asteroidElements.Node / (180.0 / Math.PI);
					i = asteroidElements.I / (180.0 / Math.PI);
					e = asteroidElements.E;
					a = asteroidElements.A;
					num2 = asteroidElements.q;
					num3 = asteroidElements.H0;
					num4 = asteroidElements.G_phaseCoeff;
					num5 = asteroidElements.LogR_Coeff;
					double diameter_Mean = asteroidElements.Diameter_Mean;
					string dia_Source = asteroidElements.Dia_Source;
					double peakEphemUncert = asteroidElements.PeakEphemUncert;
					if (num5 == 5.0 && ((iDName.Substring(1, 1) == "/") | (((iDName.Substring(0, 1) == "P") | (iDName.Substring(0, 1) == "C")) & int.TryParse(iDName.Substring(1, 1), out result))))
					{
						num5 = 10.0;
					}
					Utilities.PositionfromElements(StartOfPeriodJD, 0.0, 0.0, 0.0, num, OsculatingDate, EpochMeanAnomaly, a * (1.0 - e), e, perihelion, node, i, num3, num4, num5, 1E-05, out RA, out Dec, out RadiusVector, out array12[0], out Magnitude, out Elongation, out PhaseAngle);
					Utilities.PositionfromElements(StartOfPeriodJD + 1.0, 0.0, 0.0, 0.0, num, OsculatingDate, EpochMeanAnomaly, a * (1.0 - e), e, perihelion, node, i, num3, num4, num5, 1E-05, out RA, out Dec, out RadiusVector, out array12[0], out Magnitude, out Elongation, out PhaseAngle);
					Utilities.ApparentStarPosition(ref RA, ref Dec, 0.0, 0.0, 2000, StartOfPeriodJD, use2006values_Not1976: false);
					Utilities.RA_DEC_to_Long_Lat(RA, Dec, ecliptic, ref Longitude3, ref Latitude3);
					if (Longitude3 < Longitude - 0.07 || Longitude3 > Longitude2 + 0.07)
					{
						flag = true;
					}
					if (!flag)
					{
						num7 = (Longitude3 - Longitude) / (Longitude2 - Longitude);
						num8 = Latitude + num7 * (Latitude2 - Latitude);
						if (Math.Abs(Latitude3 - num8) > 0.08)
						{
							flag = true;
						}
					}
					if (!flag)
					{
						bool flag2;
						if (asteroidElements.Meananomaly == 0.0)
						{
							num = OsculatingDate;
							OsculatingDate = 0.0;
							flag2 = false;
						}
						else
						{
							num = 0.0;
							integrateTo_Date = Math.Floor(StartOfPeriodJD - 0.5) + 0.5;
							flag2 = true;
						}
						if ((iDName.EndsWith("^") | ((!ValidIntegrationRange | (Math.Abs(OsculatingDate - StartOfPeriodJD) > 18000.0)) && num == 0.0)) || !flag2)
						{
							flag = true;
						}
						else
						{
							Utilities.NumericIntegrate(integrateTo_Date, ref OsculatingDate, ref EpochMeanAnomaly, ref a, ref num2, ref e, ref i, ref node, ref perihelion, saveflag: false, num6, iDName, num3, num4, num5, diameter_Mean, dia_Source, peakEphemUncert, 0, 0, "", "", "", 0.0, 0.0, 0.0, elementsAt1Day);
							if (e < 0.97)
							{
								Utilities.PositionfromElements(StartOfPeriodJD, 0.0, 0.0, 0.0, num, OsculatingDate, EpochMeanAnomaly, a * (1.0 - e), e, perihelion, node, i, num3, num4, num5, 1E-05, out RA, out Dec, out RadiusVector, out array12[0], out Magnitude, out Elongation, out PhaseAngle);
							}
							else
							{
								Utilities.PositionfromElements(StartOfPeriodJD, 0.0, 0.0, 0.0, num, OsculatingDate, 0.0, a, e, perihelion, node, i, num3, num4, num5, 1E-05, out RA, out Dec, out RadiusVector, out array12[0], out Magnitude, out Elongation, out PhaseAngle);
							}
						}
					}
				}
				if (flag)
				{
					continue;
				}
				Utilities.RA_DEC_to_Long_Lat(RA, Dec, ecliptic, ref Longitude3, ref Latitude3);
				Utilities.RA_DEC_to_Long_Lat(RA2, Dec2, ecliptic, ref Longitude4, ref Latitude4);
				double num12 = Longitude3 - MoonLong[0];
				if (num12 < -Math.PI)
				{
					num12 += Math.PI * 2.0;
				}
				if (num12 > Math.PI)
				{
					num12 -= Math.PI * 2.0;
				}
				double num13 = Longitude4 - MoonLong[24];
				if (num13 < -Math.PI)
				{
					num13 += Math.PI * 2.0;
				}
				if (num13 > Math.PI)
				{
					num13 -= Math.PI * 2.0;
				}
				if (!((num12 + num11 > -0.01) & (num12 - num11 < MoonLong[24] - MoonLong[0] + 0.01)))
				{
					continue;
				}
				double num14 = 24.0 * num12 / (num12 - num13);
				if (!(num14 > -0.1 && num14 < 24.1))
				{
					continue;
				}
				int num15 = (int)num14;
				if (num15 < 0)
				{
					num15 = 0;
				}
				double num16 = 1.6;
				if (j > 9)
				{
					num16 = 4.0;
				}
				if (!(Math.Abs(Dec - DecMoon[num15]) * (180.0 / Math.PI) < num16))
				{
					continue;
				}
				double RA3;
				double Dec3;
				if (j < 10)
				{
					Utilities.PlanetGeocentric(StartOfPeriodJD + (double)(num15 - 1) / 24.0, j, 1E-09, 0, out array5[0], out array9[0], out array12[0]);
					Utilities.PlanetGeocentric(StartOfPeriodJD + (double)num15 / 24.0, j, 1E-09, 0, out array2[0], out array6[0], out array12[0], out Magnitude, out Diameter);
					Utilities.PlanetGeocentric(StartOfPeriodJD + (double)(num15 + 1) / 24.0, j, 1E-09, 0, out array3[0], out array7[0], out array12[1]);
					Utilities.PlanetGeocentric(StartOfPeriodJD + (double)(num15 + 2) / 24.0, j, 1E-09, 0, out array4[0], out array8[0], out array12[2]);
					Utilities.PlanetGeocentric(StartOfPeriodJD + (double)num15 / 24.0, j, 1E-08, 2, out RA3, out Dec3, out array12[0]);
					RA3 -= array2[0];
					Dec3 -= array6[0];
				}
				else
				{
					if (e < 0.97)
					{
						Utilities.PositionfromElements(StartOfPeriodJD + (double)(num15 - 1) / 24.0, 0.0, 0.0, 0.0, num, OsculatingDate, EpochMeanAnomaly, a * (1.0 - e), e, perihelion, node, i, num3, num4, num5, 1E-05, out array5[0], out array9[0], out RadiusVector, out array12[1], out Magnitude, out Elongation, out PhaseAngle);
					}
					else
					{
						Utilities.PositionfromElements(StartOfPeriodJD + (double)(num15 - 1) / 24.0, 0.0, 0.0, 0.0, num, OsculatingDate, 0.0, a, e, perihelion, node, i, num3, num4, num5, 1E-05, out array5[0], out array9[0], out RadiusVector, out array12[1], out Magnitude, out Elongation, out PhaseAngle);
					}
					Utilities.ApparentStarPosition(ref array5[0], ref array9[0], 0.0, 0.0, 2000, StartOfPeriodJD, use2006values_Not1976: false);
					if (e < 0.97)
					{
						Utilities.PositionfromElements(StartOfPeriodJD + (double)num15 / 24.0, 0.0, 0.0, 0.0, num, OsculatingDate, EpochMeanAnomaly, a * (1.0 - e), e, perihelion, node, i, num3, num4, num5, 1E-05, out array2[0], out array6[0], out RadiusVector, out array12[0], out Magnitude, out Elongation, out PhaseAngle);
					}
					else
					{
						Utilities.PositionfromElements(StartOfPeriodJD + (double)num15 / 24.0, 0.0, 0.0, 0.0, num, OsculatingDate, 0.0, a, e, perihelion, node, i, num3, num4, num5, 1E-05, out array2[0], out array6[0], out RadiusVector, out array12[0], out Magnitude, out Elongation, out PhaseAngle);
					}
					RA3 = array2[0];
					Dec3 = array6[0];
					Utilities.ApparentStarPosition(ref array2[0], ref array6[0], 0.0, 0.0, 2000, StartOfPeriodJD, use2006values_Not1976: false);
					RA3 -= array2[0];
					Dec3 -= array6[0];
					if (e < 0.97)
					{
						Utilities.PositionfromElements(StartOfPeriodJD + (double)(num15 + 1) / 24.0, 0.0, 0.0, 0.0, num, OsculatingDate, EpochMeanAnomaly, a * (1.0 - e), e, perihelion, node, i, num3, num4, num5, 1E-05, out array3[0], out array7[0], out RadiusVector, out array12[1], out Magnitude, out Elongation, out PhaseAngle);
					}
					else
					{
						Utilities.PositionfromElements(StartOfPeriodJD + (double)(num15 + 1) / 24.0, 0.0, 0.0, 0.0, num, OsculatingDate, 0.0, a, e, perihelion, node, i, num3, num4, num5, 1E-05, out array3[0], out array7[0], out RadiusVector, out array12[1], out Magnitude, out Elongation, out PhaseAngle);
					}
					Utilities.ApparentStarPosition(ref array3[0], ref array7[0], 0.0, 0.0, 2000, StartOfPeriodJD, use2006values_Not1976: false);
					if (e < 0.97)
					{
						Utilities.PositionfromElements(StartOfPeriodJD + (double)(num15 + 2) / 24.0, 0.0, 0.0, 0.0, num, OsculatingDate, EpochMeanAnomaly, a * (1.0 - e), e, perihelion, node, i, num3, num4, num5, 1E-05, out array4[0], out array8[0], out RadiusVector, out array12[2], out Magnitude, out Elongation, out PhaseAngle);
					}
					else
					{
						Utilities.PositionfromElements(StartOfPeriodJD + (double)(num15 + 2) / 24.0, 0.0, 0.0, 0.0, num, OsculatingDate, 0.0, a, e, perihelion, node, i, num3, num4, num5, 1E-05, out array4[0], out array8[0], out RadiusVector, out array12[2], out Magnitude, out Elongation, out PhaseAngle);
					}
					Utilities.ApparentStarPosition(ref array4[0], ref array8[0], 0.0, 0.0, 2000, StartOfPeriodJD, use2006values_Not1976: false);
				}
				array13[0] = (float)Magnitude;
				int num17 = 0;
				if (j == 5 || j == 6)
				{
					num17 = 4;
					if (j == 6)
					{
						num17 = 6;
					}
					for (int k = 1; k <= num17; k++)
					{
						int moonNumber = array[j - 5, k - 1];
						Satellites.SatelliteCoordinates(StartOfPeriodJD + (double)(num15 - 1) / 24.0, j, moonNumber, ref array15[k], ref array14[k], ref array5[k], ref array9[k], ref array13[k]);
						Utilities.ApparentStarPosition(ref array5[k], ref array9[k], 0.0, 0.0, 2000, StartOfPeriodJD, use2006values_Not1976: false);
						Satellites.SatelliteCoordinates(StartOfPeriodJD + (double)num15 / 24.0, j, moonNumber, ref array15[k], ref array14[k], ref array2[k], ref array6[k], ref array13[k]);
						Utilities.ApparentStarPosition(ref array2[k], ref array6[k], 0.0, 0.0, 2000, StartOfPeriodJD, use2006values_Not1976: false);
						Satellites.SatelliteCoordinates(StartOfPeriodJD + (double)(num15 + 1) / 24.0, j, moonNumber, ref array15[k], ref array14[k], ref array3[k], ref array7[k], ref array13[k]);
						Utilities.ApparentStarPosition(ref array3[k], ref array7[k], 0.0, 0.0, 2000, StartOfPeriodJD, use2006values_Not1976: false);
						Satellites.SatelliteCoordinates(StartOfPeriodJD + (double)(num15 + 2) / 24.0, j, moonNumber, ref array15[k], ref array14[k], ref array4[k], ref array8[k], ref array13[k]);
						Utilities.ApparentStarPosition(ref array4[k], ref array8[k], 0.0, 0.0, 2000, StartOfPeriodJD, use2006values_Not1976: false);
					}
				}
				_ = num15 / 24;
				for (int l = 0; l <= num17; l++)
				{
					PlanetBesselianElements(num15 - 1, array5[l], array9[l], array12[0], out var X, out var Y, out var ShadowIncreaseOnFundamentalPlane, out var sinF, out var _, out array10[0], out array11[0]);
					PlanetBesselianElements(num15, array2[l], array6[l], array12[0], out var X2, out var Y2, out var ShadowIncreaseOnFundamentalPlane2, out var sinF2, out var DMoon2, out array10[1], out array11[1]);
					PlanetBesselianElements(num15 + 1, array3[l], array7[l], array12[1], out var X3, out var Y3, out sinF, out ShadowIncreaseOnFundamentalPlane, out var DMoon3, out array10[2], out array11[2]);
					PlanetBesselianElements(num15 + 2, array4[l], array8[l], array12[2], out var X4, out var Y4, out ShadowIncreaseOnFundamentalPlane, out sinF, out var DMoon4, out array10[3], out array11[3]);
					Utilities.PolynomialFit(X, X2, X3, X4, out var T, out var dT, out var d2T, out var d3T);
					Utilities.PolynomialFit(Y, Y2, Y3, Y4, out var T2, out var dT2, out var d2T2, out var d3T2);
					Utilities.PolynomialFit(DMoon2, DMoon3, DMoon4, out var T3, out var dT3, out var d2T3);
					double num18 = Math.Sin(Math.Atan(Math.Abs(X3 / Y3)));
					double num19 = (Y2 - Y3 * X2 / X3) * num18;
					double value = num19 + 0.2725076;
					double value2 = num19 - 0.2725076;
					Math.Cos(Dec);
					if ((Math.Abs(value) > 1.0) & (Math.Abs(value2) > 1.0))
					{
						continue;
					}
					double h = Sidereal + 1.00273791 * ((double)num15 - deltaT) / 12.0 * Math.PI - array10[1];
					double num20 = (double)num15 - T / dT;
					if (!(num20 >= 0.0 && num20 < 24.002))
					{
						continue;
					}
					Bessel = new LunarOccultationElements();
					Bessel.StarRA = array10[1];
					Bessel.StarDec = array11[1];
					Bessel.StarRA2000 = array2[l] + RA3;
					Bessel.StarDec2000 = array6[l] + Dec3;
					Bessel.DeltaDec = array11[2] - array11[1];
					Bessel.JDzero = StartOfPeriodJD;
					Bessel.T = num15;
					Bessel.X = T;
					Bessel.dX = dT;
					Bessel.d2X = d2T;
					Bessel.d3X = d3T;
					Bessel.Y = T2;
					Bessel.dY = dT2;
					Bessel.d2Y = d2T2;
					Bessel.d3Y = d3T2;
					Bessel.DMoon = T3;
					Bessel.dDMoon = dT3;
					Bessel.d2DMoon = d2T3;
					Bessel.H = h;
					Bessel.DeltaH = array10[1] - array10[2];
					Bessel.ShadowIncreaseOnFundamentalPlane = ShadowIncreaseOnFundamentalPlane2;
					Bessel.SinF2 = sinF2;
					Bessel.XZNum = 0;
					Bessel.DoubleCode = " ";
					Bessel.StarVar = " ";
					Bessel.StarSpectrum = "  ";
					Bessel.StarCert = " ";
					Bessel.Mv = array13[l];
					Bessel.Mp = array13[l];
					Bessel.Mr = array13[l];
					LunarOccultationElements bessel = Bessel;
					LunarOccultationElements bessel2 = Bessel;
					LunarOccultationElements bessel3 = Bessel;
					double num22 = (Bessel.EarthDec_deg = 0.0);
					ShadowIncreaseOnFundamentalPlane = (bessel3.PAPole_deg = num22);
					sinF = (bessel.PA_BrightLimb_deg = (bessel2.PhaseAngle_deg = ShadowIncreaseOnFundamentalPlane));
					Bessel.Illumination = 1.0;
					Bessel.PlanetNum = (short)j;
					Bessel.LightCurveAvailable = false;
					if (j < 10)
					{
						if (l == 0)
						{
							Bessel.PlanetDiaArcSec = Diameter;
							Bessel.StarId = Utilities.Planets[j].PadRight(10);
							Bessel.PlanetId = "P  " + j + "000";
							Bessel.PlanetNum = (short)j;
							Utilities.PlanetGeocentric(StartOfPeriodJD + num20 / 24.0, j, out var _, out var _, out var _, out var PhaseAngle_deg, out var illumination, out var PAlimb_deg, out var Planetocentric_Latitude_deg, out var PAPole_deg);
							Bessel.PA_BrightLimb_deg = PAlimb_deg;
							Bessel.Illumination = illumination;
							Bessel.PhaseAngle_deg = PhaseAngle_deg;
							Bessel.PAPole_deg = PAPole_deg;
							Bessel.EarthDec_deg = Planetocentric_Latitude_deg;
						}
						else
						{
							Bessel.PlanetDiaArcSec = (double)array14[l] / array12[0] * 0.0013787957192358201;
							Bessel.StarId = array15[l];
							Bessel.PlanetId = "P  " + j + l.ToString().PadLeft(3, '0');
							int num26 = Bessel.StarId.IndexOf("(");
							if (num26 > 0)
							{
								Bessel.StarId = Bessel.StarId.Substring(0, num26);
							}
						}
					}
					else
					{
						Bessel.PlanetDiaArcSec = asteroidElements.Diameter_Mean / array12[0] * 0.0013787957192358201;
						Bessel.StarId = asteroidElements.IDName;
						LunarOccultationElements bessel4 = Bessel;
						result = asteroidElements.IDNumber;
						bessel4.PlanetId = "A" + result.ToString().PadLeft(6);
					}
					Bessel.ShadowIncreaseOnFundamentalPlane = ShadowIncreaseOnFundamentalPlane2;
					Bessel.SinF2 = sinF2;
					Elements.Add(Bessel);
				}
			}
		}

		internal static void OneOffStar(int XZnum, bool useCoordinates, double RAin, double Decin, double Magin, string ID)
		{
			double starRA;
			double RA;
			double starDec;
			double Dec;
			if (useCoordinates)
			{
				XZ80Q.CreateDummyEntry(RAin, Decin, Magin);
				starRA = (RA = RAin);
				starDec = (Dec = Decin);
				Utilities.ApparentStarPosition(ref RA, ref Dec, 0.0, 0.0, 2000, CurrentJD, use2006values_Not1976: false);
			}
			else
			{
				XZ80Q.Get_XZ_Star(XZnum);
				starRA = (RA = XZ80Q.RA_rad);
				starDec = (Dec = XZ80Q.Dec_rad);
				Utilities.ApparentStarPosition(ref RA, ref Dec, XZ80Q.PMRA_rad, XZ80Q.PMDec_rad, 2000, CurrentJD, use2006values_Not1976: false);
			}
			if (ID.Equals(null))
			{
				ID = " ".PadRight(10);
			}
			while (RA - RAMoon[0] > Math.PI)
			{
				RA -= Math.PI * 2.0;
			}
			for (; RA - RAMoon[0] < -Math.PI; RA += Math.PI * 2.0)
			{
			}
			int num = (int)(24.0 * (RA - RAMoon[0]) / (RAMoon[24] - RAMoon[0]));
			if (!(num >= 0 && num <= 24))
			{
				return;
			}
			BesselianElements(num, RA, Dec, out var X, out var Y, out var DMoon);
			BesselianElements(num + 1, RA, Dec, out var X2, out var Y2, out var DMoon2);
			X2 -= X;
			Y2 -= Y;
			DMoon2 -= DMoon;
			BesselianElements(num + 2, RA, Dec, out var X3, out var Y3, out var DMoon3);
			X3 -= X;
			Y3 -= Y;
			DMoon3 -= DMoon;
			X3 = (X3 - 2.0 * X2) / 2.0;
			Y3 = (Y3 - 2.0 * Y2) / 2.0;
			DMoon3 = (DMoon3 - 2.0 * DMoon2) / 2.0;
			X2 -= X3;
			Y2 -= Y3;
			DMoon2 -= DMoon3;
			double h = Sidereal + 1.00273791 * ((double)num - deltaT) / 12.0 * Math.PI - RA;
			if (!((double)num - X / X2 < 24.0))
			{
				return;
			}
			Bessel = new LunarOccultationElements();
			Bessel.StarRA = RA;
			Bessel.StarDec = Dec;
			Bessel.StarRA2000 = starRA;
			Bessel.StarDec2000 = starDec;
			Bessel.DeltaDec = 0.0;
			Bessel.JDzero = StartOfPeriodJD;
			Bessel.T = num;
			Bessel.X = X;
			Bessel.dX = X2;
			Bessel.d2X = X3;
			Bessel.Y = Y;
			Bessel.dY = Y2;
			Bessel.d2Y = Y3;
			Bessel.DMoon = DMoon;
			Bessel.dDMoon = DMoon2;
			Bessel.d2DMoon = DMoon3;
			Bessel.H = h;
			Bessel.DeltaH = 0.0;
			Bessel.ShadowIncreaseOnFundamentalPlane = 0.0;
			Bessel.SinF2 = 0.0;
			Bessel.XZNum = XZ80Q.XZ;
			Bessel.DoubleCode = XZ80Q.DoubleFlag;
			Bessel.StarVar = XZ80Q.VariableFlag;
			Bessel.StarSpectrum = XZ80Q.Spectrum;
			Bessel.StarCert = " ";
			Bessel.PlanetId = "";
			Bessel.LightCurveAvailable = false;
			if (XZ80Q.ZC > 0)
			{
				Bessel.StarId = string.Format("{0,7:F0}", XZ80Q.ZC) + XZ80Q.DoubleFlag + XZ80Q.Spectrum;
			}
			else if (XZ80Q.SAO > 0)
			{
				Bessel.StarId = string.Format("{0,7:F0}", XZ80Q.SAO) + XZ80Q.DoubleFlag + XZ80Q.Spectrum;
			}
			else if (XZ80Q.XZ > 0)
			{
				Bessel.StarId = string.Format("X{0,6:F0}", XZ80Q.XZ) + XZ80Q.DoubleFlag + XZ80Q.Spectrum;
			}
			else
			{
				Bessel.StarId = ID;
			}
			if (XZ80Q.ZC > 0 && ZCName(XZ80Q.ZC, out var Name))
			{
				Bessel.ZCName = Name;
			}
			if (XZ80Q.VariableFlag != " " && VariableStarInfo(XZ80Q.XZ, Bessel.JDzero + (double)Bessel.T / 24.0, out var VariableDetails))
			{
				Bessel.VariableDetails = "   " + Bessel.StarId.Substring(0, 7) + " = " + VariableDetails;
			}
			if (XZ80Q.DoubleFlag != " ")
			{
				string[] array = new string[4] { "", "", "", "" };
				bool[] array2 = new bool[4];
				double[] array3 = new double[4];
				double[] array4 = new double[4];
				DoubleStars.GetXZDoubleMatches(XZ80Q.XZ, Bessel.JDzero, ForReductions: false, out var Count, array, array3, array4, array2, out var _, out var DiscovererName, out var NonInstantaneous, out var ObservationsWanted);
				Bessel.NonInstantaneous = NonInstantaneous;
				Bessel.ObservationsWanted = ObservationsWanted;
				Bessel.DoubleCount = Count;
				if (Count > 0)
				{
					Bessel.Doub1 = array[0];
					Bessel.PA1 = array3[0];
					Bessel.Sep1 = array4[0];
					Bessel.IsMean1 = array2[0];
					Bessel.DoubID1 = DiscovererName.Substring(0, 7);
				}
				if (Count > 1)
				{
					Bessel.Doub2 = array[1];
					Bessel.PA2 = array3[1];
					Bessel.Sep2 = array4[1];
					Bessel.IsMean2 = array2[1];
					Bessel.DoubID2 = DiscovererName.Substring(7, 7);
				}
				if (Count > 2)
				{
					Bessel.Doub3 = array[2];
					Bessel.PA3 = array3[2];
					Bessel.Sep3 = array4[2];
					Bessel.IsMean3 = array2[2];
					Bessel.DoubID3 = DiscovererName.Substring(14, 7);
				}
				if (Count > 3)
				{
					Bessel.Doub4 = array[3];
					Bessel.PA4 = array3[3];
					Bessel.Sep4 = array4[3];
					Bessel.IsMean4 = array2[3];
					Bessel.DoubID4 = DiscovererName.Substring(21, 7);
				}
			}
			Bessel.Mv = XZ80Q.Mv;
			Bessel.Mp = XZ80Q.Mp;
			Bessel.Mr = XZ80Q.Mr;
			Bessel.PlanetDiaArcSec = 0.0;
			Elements.Add(Bessel);
		}

		internal static void OneOffPlanet(int Planet, int AsteroidNumber, bool useAsteroid)
		{
			double RA = 0.0;
			double Dec = 0.0;
			double[] array = new double[5];
			double[] array2 = new double[5];
			double[] array3 = new double[5];
			double[] array4 = new double[5];
			double[] array5 = new double[5];
			double[] array6 = new double[5];
			double[] array7 = new double[3];
			double[] array8 = new double[4];
			double[] array9 = new double[4];
			double Diameter = 0.0;
			double Magnitude = 0.0;
			double OsculatingDate = 0.0;
			double perihelionDate = 0.0;
			float[] array10 = new float[5];
			float[] array11 = new float[5];
			string[] array12 = new string[5];
			int num = 0;
			double EpochMeanAnomaly = 0.0;
			double a = 0.0;
			double num2 = 0.0;
			double e = 0.0;
			double i = 0.0;
			double node = 0.0;
			double perihelion = 0.0;
			double num3 = 0.0;
			double num4 = 0.0;
			double num5 = 5.0;
			AsteroidElements asteroidElements = new AsteroidElements();
			List<AsteroidElements> elementsAt1Day = new List<AsteroidElements>();
			if (Occult.Elements.UserAsteroids_Lunar.AstElements.Count < 1)
			{
				Occult.Elements.UserAsteroids_Lunar.Fill_AllAsteroids();
			}
			double RadiusVector;
			double Elongation;
			double PhaseAngle;
			if (useAsteroid)
			{
				int asteroidRecord_fromNumber = Occult.Elements.UserAsteroids_Lunar.GetAsteroidRecord_fromNumber(AsteroidNumber);
				if (asteroidRecord_fromNumber >= 0)
				{
					asteroidElements = Occult.Elements.UserAsteroids_Lunar.AstElements[asteroidRecord_fromNumber];
					num = asteroidElements.IDNumber;
					string iDName = asteroidElements.IDName;
					EpochMeanAnomaly = asteroidElements.Meananomaly / (180.0 / Math.PI);
					perihelion = asteroidElements.Perihelion / (180.0 / Math.PI);
					node = asteroidElements.Node / (180.0 / Math.PI);
					i = asteroidElements.I / (180.0 / Math.PI);
					e = asteroidElements.E;
					a = asteroidElements.A;
					num2 = asteroidElements.q;
					num3 = asteroidElements.H0;
					num4 = asteroidElements.G_phaseCoeff;
					num5 = asteroidElements.LogR_Coeff;
					double diameter_Mean = asteroidElements.Diameter_Mean;
					string dia_Source = asteroidElements.Dia_Source;
					double peakEphemUncert = asteroidElements.PeakEphemUncert;
					if (num5 == 5.0 && ((iDName.Substring(1, 1) == "/") | (((iDName.Substring(0, 1) == "P") | (iDName.Substring(0, 1) == "C")) & int.TryParse(iDName.Substring(1, 1), out var _))))
					{
						num5 = 10.0;
					}
					OsculatingDate = Utilities.JD_from_Date(asteroidElements.EpochYear, asteroidElements.EpochMonth, asteroidElements.EpochDay);
					bool flag;
					if (asteroidElements.Meananomaly == 0.0)
					{
						perihelionDate = OsculatingDate;
						OsculatingDate = 0.0;
						flag = false;
					}
					else
					{
						perihelionDate = 0.0;
						Math.Floor((StartOfPeriodJD - 0.5) / 10.0);
						flag = true;
					}
					if (ValidIntegrationRange && flag)
					{
						Utilities.NumericIntegrate(Math.Floor(StartOfPeriodJD) + 0.5, ref OsculatingDate, ref EpochMeanAnomaly, ref a, ref num2, ref e, ref i, ref node, ref perihelion, saveflag: false, num, iDName, num3, num4, num5, diameter_Mean, dia_Source, peakEphemUncert, 0, 0, "", "", "", 0.0, 0.0, 0.0, elementsAt1Day);
					}
					if (e < 0.97)
					{
						Utilities.PositionfromElements(StartOfPeriodJD, 0.0, 0.0, 0.0, perihelionDate, OsculatingDate, EpochMeanAnomaly, a * (1.0 - e), e, perihelion, node, i, num3, num4, num5, 1E-05, out RA, out Dec, out RadiusVector, out array7[0], out Magnitude, out Elongation, out PhaseAngle);
					}
					else
					{
						Utilities.PositionfromElements(StartOfPeriodJD, 0.0, 0.0, 0.0, perihelionDate, OsculatingDate, 0.0, a, e, perihelion, node, i, num3, num4, num5, 1E-05, out RA, out Dec, out RadiusVector, out array7[0], out Magnitude, out Elongation, out PhaseAngle);
					}
				}
			}
			else
			{
				Utilities.PlanetGeocentric(StartOfPeriodJD, Planet, 1E-06, 0, out RA, out Dec, out var _);
			}
			int num6 = (int)Math.Floor(24.0 * (RA - RAMoon[0]) / (RAMoon[24] - RAMoon[0]));
			if (num6 < 0)
			{
				num6 = 0;
			}
			if (num6 > 24)
			{
				num6 = 24;
			}
			double RA2;
			double Dec2;
			if (!useAsteroid)
			{
				Utilities.PlanetGeocentric(StartOfPeriodJD + (double)num6 / 24.0, Planet, 1E-07, 0, out array[0], out array4[0], out array7[0], out Magnitude, out Diameter);
				Utilities.PlanetGeocentric(StartOfPeriodJD + (double)(num6 + 1) / 24.0, Planet, 1E-07, 0, out array2[0], out array5[0], out array7[1]);
				Utilities.PlanetGeocentric(StartOfPeriodJD + (double)(num6 + 2) / 24.0, Planet, 1E-07, 0, out array3[0], out array6[0], out array7[2]);
				Utilities.PlanetGeocentric(StartOfPeriodJD + (double)num6 / 24.0, Planet, 1E-05, 2, out RA2, out Dec2, out array7[0]);
				RA2 -= array[0];
				Dec2 -= array4[0];
			}
			else
			{
				if (e < 0.97)
				{
					Utilities.PositionfromElements(StartOfPeriodJD + (double)num6 / 24.0, 0.0, 0.0, 0.0, perihelionDate, OsculatingDate, EpochMeanAnomaly, a * (1.0 - e), e, perihelion, node, i, num3, num4, num5, 1E-05, out array[0], out array4[0], out RadiusVector, out array7[0], out Magnitude, out Elongation, out PhaseAngle);
				}
				else
				{
					Utilities.PositionfromElements(StartOfPeriodJD + (double)num6 / 24.0, 0.0, 0.0, 0.0, perihelionDate, OsculatingDate, 0.0, a, e, perihelion, node, i, num3, num4, num5, 1E-05, out array[0], out array4[0], out RadiusVector, out array7[0], out Magnitude, out Elongation, out PhaseAngle);
				}
				RA2 = array[0];
				Dec2 = array4[0];
				Utilities.ApparentStarPosition(ref array[0], ref array4[0], 0.0, 0.0, 2000, StartOfPeriodJD, use2006values_Not1976: false);
				RA2 -= array[0];
				Dec2 -= array4[0];
				if (e < 0.97)
				{
					Utilities.PositionfromElements(StartOfPeriodJD + (double)(num6 + 1) / 24.0, 0.0, 0.0, 0.0, perihelionDate, OsculatingDate, EpochMeanAnomaly, a * (1.0 - e), e, perihelion, node, i, num3, num4, num5, 1E-05, out array2[0], out array5[0], out RadiusVector, out array7[1], out Magnitude, out Elongation, out PhaseAngle);
				}
				else
				{
					Utilities.PositionfromElements(StartOfPeriodJD + (double)(num6 + 1) / 24.0, 0.0, 0.0, 0.0, perihelionDate, OsculatingDate, 0.0, a, e, perihelion, node, i, num3, num4, num5, 1E-05, out array2[0], out array5[0], out RadiusVector, out array7[1], out Magnitude, out Elongation, out PhaseAngle);
				}
				Utilities.ApparentStarPosition(ref array2[0], ref array5[0], 0.0, 0.0, 2000, StartOfPeriodJD, use2006values_Not1976: false);
				if (e < 0.97)
				{
					Utilities.PositionfromElements(StartOfPeriodJD + (double)(num6 + 2) / 24.0, 0.0, 0.0, 0.0, perihelionDate, OsculatingDate, EpochMeanAnomaly, a * (1.0 - e), e, perihelion, node, i, num3, num4, num5, 1E-05, out array3[0], out array6[0], out RadiusVector, out array7[2], out Magnitude, out Elongation, out PhaseAngle);
				}
				else
				{
					Utilities.PositionfromElements(StartOfPeriodJD + (double)(num6 + 2) / 24.0, 0.0, 0.0, 0.0, perihelionDate, OsculatingDate, 0.0, a, e, perihelion, node, i, num3, num4, num5, 1E-05, out array3[0], out array6[0], out RadiusVector, out array7[2], out Magnitude, out Elongation, out PhaseAngle);
				}
				Utilities.ApparentStarPosition(ref array3[0], ref array6[0], 0.0, 0.0, 2000, StartOfPeriodJD, use2006values_Not1976: false);
			}
			array10[0] = (float)Magnitude;
			int num7 = 0;
			if (Planet == 5 || Planet == 6)
			{
				num7 = 4;
				if (Planet == 6)
				{
					num7 = 3;
				}
				for (int j = 1; j <= num7; j++)
				{
					int num8 = j;
					if (Planet == 6)
					{
						num8 += 4;
					}
					if (num8 == 7)
					{
						num8 = 8;
					}
					Satellites.SatelliteCoordinates(StartOfPeriodJD + (double)num6 / 24.0, Planet, num8, ref array12[j], ref array11[j], ref array[j], ref array4[j], ref array10[j]);
					Utilities.ApparentStarPosition(ref array[j], ref array4[j], 0.0, 0.0, 2000, StartOfPeriodJD, use2006values_Not1976: false);
					Satellites.SatelliteCoordinates(StartOfPeriodJD + (double)(num6 + 1) / 24.0, Planet, num8, ref array12[j], ref array11[j], ref array2[j], ref array5[j], ref array10[j]);
					Utilities.ApparentStarPosition(ref array2[j], ref array5[j], 0.0, 0.0, 2000, StartOfPeriodJD, use2006values_Not1976: false);
					Satellites.SatelliteCoordinates(StartOfPeriodJD + (double)(num6 + 2) / 24.0, Planet, num8, ref array12[j], ref array11[j], ref array3[j], ref array6[j], ref array10[j]);
					Utilities.ApparentStarPosition(ref array3[j], ref array6[j], 0.0, 0.0, 2000, StartOfPeriodJD, use2006values_Not1976: false);
				}
			}
			_ = num6 / 24;
			for (int k = 0; k <= num7; k++)
			{
				PlanetBesselianElements(num6, array[k], array4[k], array7[0], out var X, out var Y, out var ShadowIncreaseOnFundamentalPlane, out var sinF, out var DMoon, out array8[0], out array9[0]);
				PlanetBesselianElements(num6 + 1, array2[k], array5[k], array7[1], out var X2, out var Y2, out var ShadowIncreaseOnFundamentalPlane2, out var sinF2, out var DMoon2, out array8[1], out array9[1]);
				X2 -= X;
				Y2 -= Y;
				DMoon2 -= DMoon;
				PlanetBesselianElements(num6 + 2, array3[k], array6[k], array7[2], out var X3, out var Y3, out sinF2, out ShadowIncreaseOnFundamentalPlane2, out var DMoon3, out array8[2], out array9[2]);
				X3 -= X;
				Y3 -= Y;
				DMoon3 -= DMoon;
				X3 = (X3 - 2.0 * X2) / 2.0;
				Y3 = (Y3 - 2.0 * Y2) / 2.0;
				DMoon3 = (DMoon3 - 2.0 * DMoon2) / 2.0;
				X2 -= X3;
				Y2 -= Y3;
				DMoon2 -= DMoon3;
				double num9 = Math.Sin(Math.Atan(Math.Abs(X2 / Y2)));
				double num10 = (Y - Y2 * X / X2) * num9;
				double value = num10 + 0.2725076;
				double value2 = num10 - 0.2725076;
				Math.Cos(Dec);
				if ((Math.Abs(value) > 1.0) & (Math.Abs(value2) > 1.0))
				{
					continue;
				}
				double h = Sidereal + 1.00273791 * ((double)num6 - deltaT) / 12.0 * Math.PI - array8[0];
				if (!((double)num6 - X / X2 < 24.0))
				{
					continue;
				}
				Bessel = new LunarOccultationElements();
				Bessel.StarRA = array8[0];
				Bessel.StarDec = array9[0];
				Bessel.StarRA2000 = array[k] + RA2;
				Bessel.StarDec2000 = array4[k] + Dec2;
				Bessel.DeltaDec = array5[k] - array4[k];
				Bessel.JDzero = StartOfPeriodJD;
				Bessel.T = num6;
				Bessel.X = X;
				Bessel.dX = X2;
				Bessel.d2X = X3;
				Bessel.Y = Y;
				Bessel.dY = Y2;
				Bessel.d2Y = Y3;
				Bessel.DMoon = DMoon;
				Bessel.dDMoon = DMoon2;
				Bessel.d2DMoon = DMoon3;
				Bessel.H = h;
				Bessel.DeltaH = array8[0] - array8[1];
				Bessel.XZNum = 0;
				Bessel.DoubleCode = " ";
				Bessel.StarVar = " ";
				Bessel.StarSpectrum = "  ";
				Bessel.StarCert = " ";
				Bessel.LightCurveAvailable = false;
				Bessel.Mv = array10[k];
				Bessel.Mp = array10[k];
				Bessel.Mr = array10[k];
				if (!useAsteroid)
				{
					if (k == 0)
					{
						Bessel.PlanetDiaArcSec = Diameter;
						Bessel.StarId = Utilities.Planets[Planet].PadRight(10);
						Bessel.PlanetId = "P  " + Planet + "000";
					}
					else
					{
						Bessel.PlanetDiaArcSec = (double)array11[k] / array7[0] * 0.0013787957192358201;
						Bessel.StarId = array12[k];
						int num11 = 0;
						if (Planet == 6)
						{
							num11 = 4;
							if (k == 3)
							{
								num11 = 5;
							}
						}
						Bessel.PlanetId = "P  " + Planet + (k + num11).ToString().PadLeft(3, '0');
						int num12 = Bessel.StarId.IndexOf("(");
						if (num12 > 0)
						{
							Bessel.StarId = Bessel.StarId.Substring(0, num12);
						}
					}
				}
				else
				{
					Bessel.PlanetDiaArcSec = asteroidElements.Diameter_Mean / array7[0] * 0.0013787957192358201;
					Bessel.StarId = asteroidElements.IDName;
					Bessel.PlanetId = "A" + asteroidElements.IDNumber.ToString().PadLeft(6);
				}
				Bessel.ShadowIncreaseOnFundamentalPlane = ShadowIncreaseOnFundamentalPlane;
				Bessel.SinF2 = sinF;
				Elements.Add(Bessel);
			}
		}

		public static void ComputeEventForALocation(int ElementNumber, double Longitude, double Latitude_deg, double SiteAltitude, double pSinPhi, double pCosPhi, double Aperture_mm, double MagLimitCorrection, double TravelDistance, bool MultiSite, int Tag)
		{
			int num = 0;
			double num2 = 0.0;
			double num3 = 0.0;
			double num4 = 0.0;
			double num5 = 0.0;
			double num6 = 0.2725076;
			double[] array = new double[4];
			string[] array2 = new string[4] { "", "", "", "" };
			for (int i = -1; i <= 1; i += 2)
			{
				double num7 = 0.0;
				int num9;
				int num8;
				int num10 = (num9 = (num8 = 0));
				bool flag = true;
				PredictionLine = new LunarPredictionLine();
				LunarPredictionLine.IsGrazeLine = false;
				double num11;
				double num14;
				double num15;
				double num16;
				double num17;
				double num18;
				double num19;
				double num20;
				double num21;
				double num23;
				do
				{
					num11 = num7;
					double num12 = Elements[ElementNumber].X + Elements[ElementNumber].dX * num7 + Elements[ElementNumber].d2X * num7 * num7 + Elements[ElementNumber].d3X * num7 * num7 * num7 + Elements[ElementNumber].d4X * num7 * num7 * num7 * num7;
					double num13 = Elements[ElementNumber].Y + Elements[ElementNumber].dY * num7 + Elements[ElementNumber].d2Y * num7 * num7 + Elements[ElementNumber].d3Y * num7 * num7 * num7 + Elements[ElementNumber].d4Y * num7 * num7 * num7 * num7;
					num14 = Elements[ElementNumber].H + Longitude + 0.2625161707907961 * num7 + Elements[ElementNumber].DeltaH * num7;
					num15 = pCosPhi * Math.Sin(num14);
					num16 = pSinPhi * Math.Cos(Elements[ElementNumber].StarDec + num7 * Elements[ElementNumber].DeltaDec) - pCosPhi * Math.Cos(num14) * Math.Sin(Elements[ElementNumber].StarDec + num7 * Elements[ElementNumber].DeltaDec);
					num17 = num12 - num15;
					num18 = Elements[ElementNumber].dX + 2.0 * Elements[ElementNumber].d2X * num7 + 3.0 * Elements[ElementNumber].d3X * num7 * num7 + 4.0 * Elements[ElementNumber].d4X * num7 * num7 * num7 - (0.2625161707907961 + Elements[ElementNumber].DeltaH) * pCosPhi * Math.Cos(num14);
					num19 = num13 - num16;
					num20 = Elements[ElementNumber].dY + 2.0 * Elements[ElementNumber].d2Y * num7 + 3.0 * Elements[ElementNumber].d3Y * num7 * num7 + 4.0 * Elements[ElementNumber].d4Y * num7 * num7 * num7 - (0.2625161707907961 + Elements[ElementNumber].DeltaH) * num15 * Math.Sin(Elements[ElementNumber].StarDec + num7 * Elements[ElementNumber].DeltaDec);
					num21 = num18 * num18 + num20 * num20;
					double num22 = Math.Sqrt(num21);
					num23 = num17 * num18 + num19 * num20;
					double num24 = pSinPhi * Math.Sin(Elements[ElementNumber].StarDec) + pCosPhi * Math.Cos(Elements[ElementNumber].StarDec) * Math.Cos(num14);
					num6 = 0.2725076 + Elements[ElementNumber].ShadowIncreaseOnFundamentalPlane + Elements[ElementNumber].SinF2 * num24;
					num2 = (num17 * num20 - num19 * num18) / num22 / num6;
					num = 0;
					if (Math.Abs(num2) <= 0.996 && num10 > 0)
					{
						num9++;
					}
					if (Math.Abs(num2) > 0.996)
					{
						if (!(Math.Abs(num2) > 0.996 || (num10 > 4 && num9 > 4)))
						{
							continue;
						}
						num = 1;
						num10++;
						if (Math.Abs(num23 / num21) > 0.0001 && num10 < 10)
						{
							num7 = num11 - num23 / num21;
							num = 1;
							continue;
						}
						i = 1;
						if (Math.Abs(num2) < 1.004)
						{
							PredictionLine.EventPhase = "Gr";
						}
						if (Math.Abs(num2) >= 1.004)
						{
							PredictionLine.EventPhase = "M ";
						}
						num = 2;
					}
					else
					{
						num7 = num11 + num6 * (double)i * Math.Sqrt(1.0 - num2 * num2) / num22 - num23 / num21;
						num8++;
					}
				}
				while (((num == 1) | (Math.Abs(num11 - num7) > 1E-05)) && num8 < 30);
				if (Math.Abs(num2) > 1.2)
				{
					break;
				}
				EventPartDay = ((double)Elements[ElementNumber].T + num7) / 24.0;
				PredictionLine.JD = Elements[ElementNumber].JDzero + ((double)Elements[ElementNumber].T + num7 - deltaT) / 24.0;
				double num25 = Elements[ElementNumber].DMoon + Elements[ElementNumber].dDMoon * num7 + Elements[ElementNumber].d2DMoon * num7 * num7;
				if (num != 2)
				{
					if (i == -1)
					{
						PredictionLine.EventPhase = "D ";
					}
					else
					{
						PredictionLine.EventPhase = "R ";
					}
				}
				PredictionLine.Dec = Elements[ElementNumber].StarDec;
				double num26 = (0.0 - Math.Cos(PredictionLine.Dec)) * Math.Sin(num14);
				double num27 = Math.Sin(PredictionLine.Dec) * pCosPhi - Math.Cos(PredictionLine.Dec) * Math.Cos(num14) * pSinPhi;
				double num28 = (num3 = Math.Sin(PredictionLine.Dec) * pSinPhi + Math.Cos(PredictionLine.Dec) * Math.Cos(num14) * pCosPhi);
				PredictionLine.MoonAlt = 180.0 / Math.PI * Math.Atan(num28 / Math.Sqrt(num26 * num26 + num27 * num27));
				if (PredictionLine.MoonAlt < 0.0)
				{
					flag = false;
				}
				if (!flag)
				{
					continue;
				}
				PredictionLine.MoonAz = 180.0 / Math.PI * Math.Atan2(num26, num27);
				if (PredictionLine.MoonAz < 0.0)
				{
					PredictionLine.MoonAz += 360.0;
				}
				PredictionLine.PA = Math.Atan2(0.0 - num17, 0.0 - num19) * (180.0 / Math.PI);
				if (PredictionLine.PA < 0.0)
				{
					PredictionLine.PA += 360.0;
				}
				Physical(num == 2, ref PredictionLine, ElementNumber, EventPartDay, num14, pSinPhi, pCosPhi);
				double num29 = visibility(ref PredictionLine, Aperture_mm, MagLimitCorrection);
				if (Elements[ElementNumber].Mv >= num29)
				{
					flag = false;
				}
				if (!flag)
				{
					continue;
				}
				if (num29 - MagLimitCorrection - Elements[ElementNumber].Mv < 1.0)
				{
					PredictionLine.EventPhase = PredictionLine.EventPhase.ToLower();
				}
				PredictionLine.StarId = Elements[ElementNumber].StarId;
				PredictionLine.Mv = Elements[ElementNumber].Mv;
				PredictionLine.Mp = Elements[ElementNumber].Mp;
				PredictionLine.Mr = Elements[ElementNumber].Mr;
				PredictionLine.DoubleCode = Elements[ElementNumber].DoubleCode;
				PredictionLine.VariableCode = Elements[ElementNumber].StarVar;
				PredictionLine.Spectrum = Elements[ElementNumber].StarSpectrum;
				PredictionLine.RA = Elements[ElementNumber].StarRA;
				PredictionLine.RA2000 = Elements[ElementNumber].StarRA2000;
				PredictionLine.Dec2000 = Elements[ElementNumber].StarDec2000;
				PredictionLine.ElementRecordNumber = ElementNumber;
				if (Elements[ElementNumber].ZCNameExists)
				{
					PredictionLine.ZCName = Elements[ElementNumber].ZCName;
				}
				if (Elements[ElementNumber].VariableDetailsExist)
				{
					PredictionLine.VariableDetails = Elements[ElementNumber].VariableDetails;
				}
				double num30 = (0.0 - (num17 * num18 + num19 * num20)) / num6 / Math.Sqrt(num21);
				if (num30 > 1.0)
				{
					num30 = 1.0;
				}
				if (num30 < -1.0)
				{
					num30 = -1.0;
				}
				PredictionLine.CCT = 180.0 / Math.PI * Math.Acos(num30);
				if (num17 * num20 - num19 * num18 < 0.0)
				{
					PredictionLine.CCT = 0.0 - PredictionLine.CCT;
				}
				PredictionLine.MoonSpeedMsec = Math.Sqrt(num21) * 6378.137 / 3.6;
				PredictionLine.MoonDistanceKM = num25 - num3 * 6378.137;
				double num31 = num17 - 1.0 / 120.0 * num18;
				double num32 = num19 - 1.0 / 120.0 * num20;
				double num33 = num17 + 1.0 / 120.0 * num18;
				double num34 = num19 + 1.0 / 120.0 * num20;
				num4 = Math.Atan2(0.0 - num31, 0.0 - num32) * (180.0 / Math.PI);
				if (num4 < 0.0)
				{
					num4 += 360.0;
				}
				num5 = Math.Atan2(0.0 - num33, 0.0 - num34) * (180.0 / Math.PI);
				if (num5 < 0.0)
				{
					num5 += 360.0;
				}
				PredictionLine.dPA_minute = num5 - num4;
				if (PredictionLine.dPA_minute > 180.0)
				{
					PredictionLine.dPA_minute -= 360.0;
				}
				if (PredictionLine.dPA_minute < -180.0)
				{
					PredictionLine.dPA_minute += 360.0;
				}
				PredictionLine.RV = Math.Abs(Math.Sqrt(num33 * num33 + num34 * num34) - Math.Sqrt(num31 * num31 + num32 * num32)) / 0.2725076 / 60.0;
				PredictionLine.RV *= Math.Atan(1737.4 / PredictionLine.MoonDistanceKM) * (180.0 / Math.PI) * 3600.0;
				if (Elements[ElementNumber].DoubleDetailsExist)
				{
					array2[0] = (array2[1] = (array2[2] = (array2[3] = "")));
					if (PredictionLine.RV > 0.05)
					{
						string format = ", dT = {0:+0;-0}sec";
						string format2 = ", dT = {0:+0.#;-0.#}sec";
						string format3 = ", dT = {0:+0.##;-0.##;0.00}sec";
						if (Elements[ElementNumber].PA1 > 0.0)
						{
							array[0] = Elements[ElementNumber].Sep1 * Math.Cos((Elements[ElementNumber].PA1 - PredictionLine.PA) / (180.0 / Math.PI)) / PredictionLine.RV;
							if (Math.Abs(PredictionLine.CCT) > 90.0)
							{
								array[0] = 0.0 - array[0];
							}
							if (Math.Abs(array[0]) < 0.5)
							{
								array2[0] = string.Format(format3, array[0]);
							}
							else if (Math.Abs(array[0]) < 3.0)
							{
								array2[0] = string.Format(format2, array[0]);
							}
							else
							{
								array2[0] = string.Format(format, array[0]);
							}
						}
						if (Elements[ElementNumber].PA2 > 0.0)
						{
							array[1] = Elements[ElementNumber].Sep2 * Math.Cos((Elements[ElementNumber].PA2 - PredictionLine.PA) / (180.0 / Math.PI)) / PredictionLine.RV;
							if (Math.Abs(PredictionLine.CCT) > 90.0)
							{
								array[1] = 0.0 - array[1];
							}
							if (Math.Abs(array[1]) < 0.5)
							{
								array2[1] = string.Format(format3, array[1]);
							}
							else if (Math.Abs(array[1]) < 3.0)
							{
								array2[1] = string.Format(format2, array[1]);
							}
							else
							{
								array2[1] = string.Format(format, array[1]);
							}
						}
						if (Elements[ElementNumber].PA3 > 0.0)
						{
							array[2] = Elements[ElementNumber].Sep3 * Math.Cos((Elements[ElementNumber].PA3 - PredictionLine.PA) / (180.0 / Math.PI)) / PredictionLine.RV;
							if (Math.Abs(PredictionLine.CCT) > 90.0)
							{
								array[2] = 0.0 - array[2];
							}
							if (Math.Abs(array[2]) < 0.5)
							{
								array2[2] = string.Format(format3, array[2]);
							}
							else if (Math.Abs(array[2]) < 3.0)
							{
								array2[2] = string.Format(format2, array[2]);
							}
							else
							{
								array2[2] = string.Format(format, array[2]);
							}
						}
						if (Elements[ElementNumber].PA4 > 0.0)
						{
							array[3] = Elements[ElementNumber].Sep4 * Math.Cos((Elements[ElementNumber].PA4 - PredictionLine.PA) / (180.0 / Math.PI)) / PredictionLine.RV;
							if (Math.Abs(PredictionLine.CCT) > 90.0)
							{
								array[3] = 0.0 - array[3];
							}
							if (Math.Abs(array[3]) < 0.5)
							{
								array2[3] = string.Format(format3, array[3]);
							}
							else if (Math.Abs(array[3]) < 3.0)
							{
								array2[3] = string.Format(format2, array[3]);
							}
							else
							{
								array2[3] = string.Format(format, array[3]);
							}
						}
					}
					string doubleDetails = Elements[ElementNumber].DoubleCount switch
					{
						1 => "   " + PredictionLine.StarId.Substring(0, 7) + " is double: " + Elements[ElementNumber].Doub1 + array2[0], 
						2 => "   " + PredictionLine.StarId.Substring(0, 7) + " is triple: " + Elements[ElementNumber].Doub1 + array2[0] + " : " + Elements[ElementNumber].Doub2 + array2[1], 
						3 => "   " + PredictionLine.StarId.Substring(0, 7) + " is quadruple: " + Elements[ElementNumber].Doub1 + array2[0] + " : " + Elements[ElementNumber].Doub2 + array2[1] + " : " + Elements[ElementNumber].Doub3 + array2[2], 
						_ => "   " + PredictionLine.StarId.Substring(0, 7) + " is multiple: " + Elements[ElementNumber].Doub1 + array2[0] + " : " + Elements[ElementNumber].Doub2 + array2[1] + " : " + Elements[ElementNumber].Doub3 + array2[2] + " : " + Elements[ElementNumber].Doub4 + array2[3], 
					};
					PredictionLine.DoubleDetails = doubleDetails;
				}
				PredictionLine.LightCurveAvailable = Elements[ElementNumber].LightCurveAvailable;
				PredictionLine.XZnum = Elements[ElementNumber].XZNum;
				if (Elements[ElementNumber].ObservationsWanted)
				{
					if (Elements[ElementNumber].NonInstantaneous)
					{
						string text = "";
						if ("   OCcGc ---S  ".Contains(Elements[ElementNumber].DoubID1.Substring(0, 3)))
						{
							text = Elements[ElementNumber].DoubID1;
						}
						else if ("   OCcGC ---S  ".Contains(Elements[ElementNumber].DoubID2.Substring(0, 3)))
						{
							text = Elements[ElementNumber].DoubID2;
						}
						else if ("   OCcGC ---S  ".Contains(Elements[ElementNumber].DoubID3.Substring(0, 3)))
						{
							text = Elements[ElementNumber].DoubID3;
						}
						else if ("   OCcGC ---S  ".Contains(Elements[ElementNumber].DoubID4.Substring(0, 3)))
						{
							text = Elements[ElementNumber].DoubID4;
						}
						PredictionLine.DoubleObservationsWanted = "   " + PredictionLine.StarId.Substring(0, 7) + " has been reported as non-instantaneous (" + text + "). Observations are highly desired";
					}
					else
					{
						PredictionLine.DoubleObservationsWanted = "   " + PredictionLine.StarId.Substring(0, 7) + " is a close double. Observations are highly desired";
					}
				}
				PredictionLine.Durn = Elements[ElementNumber].PlanetDiaArcSec / PredictionLine.RV;
				PredictionLine.DiaPlanet = Elements[ElementNumber].PlanetDiaArcSec;
				PredictionLine.IllumPlanet = Elements[ElementNumber].Illumination;
				PredictionLine.LimbPlanet = Elements[ElementNumber].PA_BrightLimb_deg;
				PredictionLine.PhaseAngle = Elements[ElementNumber].PhaseAngle_deg;
				PredictionLine.PAPole_deg = Elements[ElementNumber].PAPole_deg;
				PredictionLine.EarthDec_deg = Elements[ElementNumber].EarthDec_deg;
				PredictionLine.Planet = Elements[ElementNumber].PlanetNum;
				PredictionLine.VA = PredictionLine.PA - Math.Atan2(num15, num16) * (180.0 / Math.PI);
				if (PredictionLine.VA < 0.0)
				{
					PredictionLine.VA += 360.0;
				}
				if (PredictionLine.VA >= 360.0)
				{
					PredictionLine.VA -= 360.0;
				}
				double RA = 0.0;
				double Dec = 0.0;
				double MoonRadius_Radians = 1.0;
				double MoonScale = 1.0;
				double l = 0.0;
				double b = 0.0;
				double C = 0.0;
				Utilities.TopocentricMoon(PredictionLine.JD, Longitude, Latitude_deg / (180.0 / Math.PI), SiteAltitude, Altitude_is_MSL: true, 84, out RA, out Dec, out MoonRadius_Radians, out MoonScale, out l, out b, out C, out var _);
				PredictionLine.LL = l;
				PredictionLine.LB = b;
				Utilities.Distance(RA, Dec, Elements[ElementNumber].StarRA, Elements[ElementNumber].StarDec, out var _, out var PA_atOrigin);
				PredictionLine.PA = PA_atOrigin * (180.0 / Math.PI);
				if (num != 2)
				{
					double num39;
					if (Utilities.LOLAFileExists)
					{
						double num35 = 0.0;
						double num36 = 0.0;
						int num37 = 0;
						double num38 = PredictionLine.dPA_minute / 60.0;
						do
						{
							num36 = num35;
							num39 = Utilities.LOLA_LimbHeight_ActualDistance(PredictionLine.PA - C + num38 * num35, PredictionLine.LL, PredictionLine.LB, MoonScale);
							num35 = num39 / PredictionLine.RV;
							if ((PredictionLine.CCT > -90.0) & (PredictionLine.CCT < 90.0))
							{
								num35 = 0.0 - num35;
							}
							num37++;
						}
						while (Math.Abs(num35 - num36) > 0.05 && num37 < 10);
					}
					else
					{
						num39 = 0.0;
					}
					if ((PredictionLine.CCT > -90.0) & (PredictionLine.CCT < 90.0))
					{
						PredictionLine.JD -= num39 / PredictionLine.RV / 86400.0;
					}
					else
					{
						PredictionLine.JD += num39 / PredictionLine.RV / 86400.0;
					}
				}
				PredictionLine.A = 1.047 / num23 * (num17 * pCosPhi * Math.Cos(num14) + num19 * num15 * Math.Sin(Elements[ElementNumber].StarDec));
				PredictionLine.B = -1.047 / num23 * (pSinPhi * (num17 * Math.Sin(num14) - num19 * Math.Sin(Elements[ElementNumber].StarDec) * Math.Cos(num14)) - pCosPhi * num19 * Math.Cos(Elements[ElementNumber].StarDec));
				if ((num == 2) | (Math.Abs(PredictionLine.A) > 9.9) | (Math.Abs(PredictionLine.B) > 9.9))
				{
					PredictionLine.A = 9.9;
					PredictionLine.B = 9.9;
				}
				if (!ListGrazesOnly)
				{
					if (MultiSite)
					{
						MultiPrediction.Add(PredictionLine);
					}
					else
					{
						Prediction.Add(PredictionLine);
					}
				}
				PredictionLine.Tag = Tag;
			}
			if (MultiSite)
			{
				return;
			}
			double num40 = Math.Abs((Elements[ElementNumber].X * Elements[ElementNumber].dY - Elements[ElementNumber].Y * Elements[ElementNumber].dX) / Math.Sqrt(Elements[ElementNumber].dX * Elements[ElementNumber].dX + Elements[ElementNumber].dY * Elements[ElementNumber].dY) - num6 * (double)Math.Sign(num2));
			double num41 = ((!(num40 < 0.99)) ? 8.06E-05 : (Math.Sqrt(1.0 - num40 * num40) / 1738.0));
			double num42 = num41 * TravelDistance;
			if (Elements[ElementNumber].Mv < 6.5 && (double)Settings.Default.Graze_TravelDistance_65 > TravelDistance)
			{
				num42 = num41 * (double)Settings.Default.Graze_TravelDistance_65;
			}
			if (Elements[ElementNumber].Mv < 4.5 && (double)Settings.Default.Graze_TravelDistance_45 > TravelDistance)
			{
				num42 = num41 * (double)Settings.Default.Graze_TravelDistance_45;
			}
			if (!(Math.Abs(Math.Abs(num2) - 1.0) < num42))
			{
				return;
			}
			double num43 = 1.0;
			PredictionLine = new LunarPredictionLine();
			double num44 = Longitude * (180.0 / Math.PI);
			int northOrSouthLimit = Math.Sign(num2);
			if ((PredictionLine.MoonAz > 180.0) & (PredictionLine.MoonAlt < 20.0))
			{
				num43 = -1.0;
			}
			if (GrazePoint(ElementNumber, northOrSouthLimit, 0, DoublePath: false, num44 / (180.0 / Math.PI), 0.0, FullGrazePrediction: true, StartWithLastLatitude: false, ref PredictionLine, out var Latitude_Deg))
			{
				PredictionLine.GrazeLongitude = num44;
				PredictionLine.GrazeLatitude = Latitude_Deg;
			}
			num44 += num43;
			if (GrazePoint(ElementNumber, northOrSouthLimit, 0, DoublePath: false, num44 / (180.0 / Math.PI), 0.0, FullGrazePrediction: false, StartWithLastLatitude: true, ref PredictionLine, out Latitude_Deg))
			{
				PredictionLine.GrazeLongitude2 = num44;
				PredictionLine.GrazeLatitude2 = Latitude_Deg;
			}
			PredictionLine.CA += 3.0;
			PredictionLine.StarId = Elements[ElementNumber].StarId;
			PredictionLine.Mv = Elements[ElementNumber].Mv;
			PredictionLine.Mr = Elements[ElementNumber].Mr;
			PredictionLine.VariableCode = Elements[ElementNumber].StarVar;
			PredictionLine.RA2000 = Elements[ElementNumber].StarRA2000;
			PredictionLine.Dec2000 = Elements[ElementNumber].StarDec2000;
			if (!(visibility(ref PredictionLine, Aperture_mm, MagLimitCorrection) > PredictionLine.Mv))
			{
				return;
			}
			PredictionLine.CA -= 3.0;
			PredictionLine.EventPhase = "Gr";
			PredictionLine.GrazeDeltaLat = num43 * (PredictionLine.GrazeLatitude2 - PredictionLine.GrazeLatitude);
			PredictionLine.GrazeAZ = Math.Atan((0.0 - PredictionLine.GrazeDeltaLat) / Math.Cos(Latitude_deg / (180.0 / Math.PI))) * (180.0 / Math.PI);
			if (PredictionLine.GrazeAZ < 0.0)
			{
				PredictionLine.GrazeAZ += 360.0;
			}
			PredictionLine.GrazeD = (PredictionLine.GrazeLatitude - Latitude_deg) * Math.Cos(PredictionLine.GrazeAZ / (180.0 / Math.PI)) * 111.0;
			if (PredictionLine.GrazeD < 0.0)
			{
				PredictionLine.GrazeD = 0.0 - PredictionLine.GrazeD;
				PredictionLine.GrazeAZ -= 180.0;
				if (PredictionLine.GrazeAZ < 0.0)
				{
					PredictionLine.GrazeAZ += 360.0;
				}
			}
			Prediction.Add(PredictionLine);
		}

		internal static bool Physical(bool IsGraze, ref LunarPredictionLine PredictionLine, int ElementNumber, double EventPartDay, double h1, double pSin, double pCos)
		{
			double num = 0.0;
			double num2 = 0.0;
			double num3 = 0.0;
			double num4 = 0.0;
			int i = 0;
			bool flag = true;
			if (double.IsNaN(PredictionLine.MoonAlt))
			{
				flag = false;
			}
			if (flag)
			{
				for (i = (int)Math.Floor(EventPartDay * 24.0); i < 0; i++)
				{
				}
				num = 24.0 * EventPartDay - (double)i;
				num2 = EventPartDay;
				num3 = (1.0 - num) * RAMoon[i] + num * RAMoon[i + 1] - PiMoon[i] * pCos * Math.Sin(h1) / Math.Cos(Elements[ElementNumber].StarDec);
				num4 = (1.0 - num) * DecMoon[i] + num * DecMoon[i + 1] - PiMoon[i] * (pSin * Math.Cos(Elements[ElementNumber].StarDec) - pCos * Math.Sin(Elements[ElementNumber].StarDec) * Math.Cos(h1));
				double num5 = Math.Cos(num4) * Math.Cos(num3);
				double num6 = Math.Cos(num4) * Math.Sin(num3) * Math.Cos(TrueEcliptic) + Math.Sin(num4) * Math.Sin(TrueEcliptic);
				double num7 = (0.0 - Math.Cos(num4)) * Math.Sin(num3) * Math.Sin(TrueEcliptic) + Math.Sin(num4) * Math.Cos(TrueEcliptic);
				double num8 = Math.Atan2(num6, num5);
				double num9 = Math.Atan(num7 / Math.Sqrt(num5 * num5 + num6 * num6)) * (180.0 / Math.PI);
				double num10 = num8 - MoonNode;
				double num11 = (PredictionLine.JD - 2451545.0) / 36525.0;
				double num12 = (218.3164591 + 481267.88134236 * num11 - 0.0013298 * num11 * num11) % 360.0;
				num12 /= 180.0 / Math.PI;
				PredictionLine.LB = -1.535 * Math.Sin(num10) - num9;
				PredictionLine.LL = (num8 - num12) * (180.0 / Math.PI) + 0.01 * Math.Sin(2.0 * num10) + 0.027 * Math.Cos(num10) * PredictionLine.LB;
				PredictionLine.LL %= 360.0;
				if (PredictionLine.LL > 180.0)
				{
					PredictionLine.LL -= 360.0;
				}
				if (PredictionLine.LL < -180.0)
				{
					PredictionLine.LL += 360.0;
				}
				double num13 = (0.0 - Math.Sin(TrueEcliptic)) * Math.Sin(MoonNode + NutLongSec1 / (180.0 / Math.PI) / 3600.0);
				double num14 = 0.02678 * Math.Cos(TrueEcliptic) - 0.999641 * Math.Sin(TrueEcliptic) * Math.Cos(MoonNode);
				num6 = Math.Atan2(num13, num14);
				PredictionLine.AA = PredictionLine.PA - 180.0 / Math.PI * Math.Asin(Math.Sqrt(num13 * num13 + num14 * num14) * Math.Cos(PredictionLine.LL / (180.0 / Math.PI) + num6 + num12 - MoonNode) / Math.Cos(Elements[ElementNumber].StarDec));
				if (PredictionLine.AA < 0.0)
				{
					PredictionLine.AA += 360.0;
				}
				if (PredictionLine.AA > 360.0)
				{
					PredictionLine.AA -= 360.0;
				}
			}
			if (flag)
			{
				double num15 = RASun[0] + 24.0 * num2 * RASun[1] + 576.0 * num2 * num2 * RASun[2];
				double num16 = DecSun[0] + 24.0 * num2 * DecSun[1] + 576.0 * num2 * num2 * DecSun[2];
				double num17 = num15 - num3;
				if (num17 > Math.PI)
				{
					num17 -= Math.PI * 2.0;
				}
				if (num17 < -Math.PI)
				{
					num17 += Math.PI * 2.0;
				}
				if (num17 < 0.0)
				{
					PredictionLine.WaxFlag = "+";
				}
				double num5 = Math.Cos(num16) * Math.Sin(num17);
				double num6 = Math.Sin(num16) * Math.Cos(num4) - Math.Cos(num16) * Math.Sin(num4) * Math.Cos(num17);
				double num18 = Math.Atan2(num5, num6) * (180.0 / Math.PI);
				PredictionLine.PABrightLimb = num18;
				double num14 = Math.Sin(num16) * Math.Sin(num4) + Math.Cos(num16) * Math.Cos(num4) * Math.Cos(num17);
				PredictionLine.Illumination = 100.0 * (0.5 - num14 / 2.0);
				PredictionLine.Elongation = Math.Atan2(Math.Sqrt(num5 * num5 + num6 * num6), num14) * (180.0 / Math.PI);
				if (PredictionLine.Elongation < 0.0)
				{
					PredictionLine.Elongation += 180.0;
				}
				PredictionLine.Cusp = " ";
				if (PredictionLine.Elongation > 178.0)
				{
					double num19 = num15 + Math.PI;
					double num20 = 0.0 - num16;
					double num21 = h1 + Elements[ElementNumber].StarRA - num19;
					num19 -= PiMoon[i] * pCos * Math.Sin(num21) / (Math.Cos(num20) - PiMoon[i] * pCos * Math.Cos(num21));
					if (num19 > Math.PI * 2.0)
					{
						num19 -= Math.PI * 2.0;
					}
					if (num19 < 0.0)
					{
						num19 += Math.PI * 2.0;
					}
					num20 -= PiMoon[i] * (pSin * Math.Cos(num20) - pCos * Math.Cos(num21) * Math.Sin(num20));
					double num22 = 1.02 * (0.998333 * PiMoon[i] - 0.004609 / DistSun[0]);
					Utilities.Distance(num19, num20, Elements[ElementNumber].StarRA, Elements[ElementNumber].StarDec, out var Distance, out var PA_atOrigin);
					Distance = 100.0 * Distance / num22;
					Utilities.Distance(num15 + Math.PI, 0.0 - num16, (1.0 - num) * RAMoon[i] + num * RAMoon[i + 1], (1.0 - num) * DecMoon[i] + num * DecMoon[i + 1], out var Distance2, out PA_atOrigin);
					double num23 = 0.2725076 * PiMoon[i];
					if (Distance2 + num23 <= num22)
					{
						PredictionLine.Illumination = 0.0;
					}
					else if (Distance2 - num23 >= num22)
					{
						PredictionLine.Illumination = 100.0;
					}
					else
					{
						double num24 = (Distance2 / num23 * (Distance2 / num23) - num22 / num23 * (num22 / num23) + 1.0) / 2.0 / Distance2 * num23;
						double num25 = Math.Atan(Math.Sqrt(1.0 - num24 * num24) / num24);
						if (num25 < 0.0)
						{
							num25 += Math.PI;
						}
						double num26 = Math.Sin(num25) * num23 / num22;
						double num27 = Math.Atan(num26 / Math.Sqrt(1.0 - num26 * num26));
						PredictionLine.Illumination = 100.0 * (Math.PI - num25 + Math.Sin(2.0 * num25) / 2.0 - num22 / num23 * (num22 / num23) * (num27 - Math.Sin(2.0 * num27) / 2.0)) / Math.PI;
					}
					if (PredictionLine.Illumination < 100.0)
					{
						PredictionLine.WaxFlag = "E";
						if (Distance < 103.5)
						{
							PredictionLine.Cusp = "U";
							PredictionLine.CA = Distance;
						}
					}
				}
				if (PredictionLine.Cusp != "U")
				{
					double num28;
					for (num28 = num18 - 90.0; num28 > 90.0; num28 -= 180.0)
					{
					}
					for (; num28 < -90.0; num28 += 180.0)
					{
					}
					PredictionLine.CA = (double)Math.Sign(num17) * (num28 - PredictionLine.PA);
					if (PredictionLine.CA > 180.0)
					{
						PredictionLine.CA -= 360.0;
					}
					if (PredictionLine.CA < -180.0)
					{
						PredictionLine.CA += 360.0;
					}
					PredictionLine.Cusp = "N";
					if (Math.Abs(PredictionLine.CA) > 90.0)
					{
						PredictionLine.CA = (double)Math.Sign(PredictionLine.CA) * (180.0 - Math.Abs(PredictionLine.CA));
						PredictionLine.Cusp = "S";
					}
				}
				if (flag)
				{
					double sunAlt = 180.0 / Math.PI * Math.Asin(Math.Sin(num16) * pSin + pCos * Math.Cos(h1 + Elements[ElementNumber].StarRA - num15) * Math.Cos(num16));
					PredictionLine.SunAlt = sunAlt;
				}
			}
			return flag;
		}

		internal static void GrazePrediction(bool GetStar, string SiteFile)
		{
			//IL_0029: Unknown result type (might be due to invalid IL or missing references)
			//IL_0190: Unknown result type (might be due to invalid IL or missing references)
			double num = 1.0;
			string text = "north";
			string text2 = "earlier";
			if (GetStar)
			{
				GrazeSelectedStar = -1;
				((Form)new LunarGrazeSelection(ShowGrazePanel: true, Multi: false)).ShowDialog();
			}
			int grazeSelectedStar = GrazeSelectedStar;
			if (grazeSelectedStar < 0)
			{
				return;
			}
			LunarPredictionLine.IsGrazeLine = true;
			int grazeLimit = GrazeLimit;
			double num2 = Math.Sqrt(Elements[grazeSelectedStar].dX * Elements[grazeSelectedStar].dX + Elements[grazeSelectedStar].dY * Elements[grazeSelectedStar].dY);
			double num3 = (Elements[grazeSelectedStar].Y * Elements[grazeSelectedStar].dX - Elements[grazeSelectedStar].X * Elements[grazeSelectedStar].dY) / num2 + (double)grazeLimit * 0.2725076;
			bool flag = false;
			double value = Elements[grazeSelectedStar].Y - Elements[grazeSelectedStar].X / Elements[grazeSelectedStar].dX * Elements[grazeSelectedStar].dY + (double)grazeLimit * 0.2725076;
			if (Math.Abs(value) > 0.9966 * Math.Cos(Elements[grazeSelectedStar].StarDec))
			{
				flag = true;
			}
			if (Math.Abs(num3) > 0.995)
			{
				MessageBox.Show("No graze occurs at this limit", "No Graze", (MessageBoxButtons)0, (MessageBoxIcon)48);
				return;
			}
			VPS_at_Moon = 1.864 / Math.Sqrt(1.0 - num3 * num3);
			VerticalProfileScale = VPS_at_Moon * PiMoon[12] / 0.01659;
			GrazeHeader.Clear();
			GrazeFooter.Clear();
			GrazePredictionLines.Clear();
			string starId = Elements[grazeSelectedStar].StarId;
			if (starId.Length > 7)
			{
				starId.Insert(7, " ");
			}
			GrazeHeader.Add("Grazing Occultation of " + starId + "      Magnitude " + string.Format("{0,3:F1}", Elements[grazeSelectedStar].Mv) + " [Red = " + string.Format("{0,3:F1}", Elements[grazeSelectedStar].Mr) + "]  " + Elements[grazeSelectedStar].StarVar);
			if (Elements[grazeSelectedStar].ZCName.Length > 1)
			{
				GrazeHeader.Add(Elements[grazeSelectedStar].ZCName);
			}
			GrazeHeader.Add("");
			GrazeHeader.Add("Nominal site altitude " + string.Format("{0,1}", GrazeNominalAltitude) + "m");
			GrazeHeader.Add("");
			if (Settings.Default.Grazes_DMScoords)
			{
				GrazeHeader.Add("E. Longit.   Latitude       U.T.    Sun  Moon   TanZ   PA    AA      CA");
			}
			else
			{
				GrazeHeader.Add("E. Longit.   Latitude      U.T.    Sun  Moon   TanZ   PA    AA      CA");
			}
			if (Settings.Default.Grazes_DMScoords)
			{
				GrazeHeader.Add("   o  '  \"    o  '   \"    h  m  s   Alt Alt Az          o     o      o");
			}
			else if (Settings.Default.Grazes_DDDcoords)
			{
				GrazeHeader.Add("    o          o         h  m  s   Alt Alt Az          o     o      o");
			}
			else
			{
				GrazeHeader.Add("   o   '      o   '      h  m  s   Alt Alt Az          o     o      o");
			}
			int num4 = 1;
			int num5 = 0;
			int num6 = -1;
			if (flag & (Math.Sign(Elements[grazeSelectedStar].dY) == Math.Sign(value)))
			{
				num4 = 0;
				num5 = 1;
				num6 = 1;
			}
			if (!flag)
			{
				num4 = 0;
				num5 = 0;
				num6 = 1;
			}
			for (int i = -1; i <= 1; i += 2)
			{
				if (Elements[grazeSelectedStar].PlanetDiaArcSec == 0.0)
				{
					i = 1;
				}
				int num7 = Math.Sign(num6);
				for (int j = num4; num7 * j <= num7 * num5; j += num6)
				{
					double num8 = GrazeStartLongitude;
					double num9 = GrazeEndLongitude;
					double num10 = GrazeLongitudeStep;
					if (j == 1)
					{
						num8 = GrazeEndLongitude;
						num9 = GrazeStartLongitude;
						num10 = 0.0 - GrazeLongitudeStep;
					}
					int num11 = Math.Sign(num10);
					for (double num12 = num8; (double)num11 * num12 <= (double)num11 * num9; num12 += num10)
					{
						LunarPredictionLine PLine = new LunarPredictionLine();
						double Latitude_Deg;
						bool num13 = GrazePoint(grazeSelectedStar, grazeLimit, i, j != 0, num12 / (180.0 / Math.PI), GrazeNominalAltitude, FullGrazePrediction: true, num12 != num8, ref PLine, out Latitude_Deg);
						PLine.GrazeInnerOuterLimit = i;
						if (num13)
						{
							GrazePredictionLines.Add(PLine);
						}
					}
				}
			}
			bool flag2 = false;
			for (int k = 0; k < GrazePredictionLines.Count - 1; k++)
			{
				if (!flag2 & (GrazePredictionLines[k].JD < GrazePredictionLines[k + 1].JD))
				{
					flag2 = true;
				}
				if ((GrazePredictionLines[k].JD > GrazePredictionLines[k + 1].JD) & (GrazePredictionLines[k].GrazeInnerOuterLimit == GrazePredictionLines[k + 1].GrazeInnerOuterLimit))
				{
					if (!flag2)
					{
						GrazePredictionLines.RemoveAt(k);
					}
					else
					{
						GrazePredictionLines.RemoveAt(k + 1);
					}
					k--;
				}
			}
			if (GrazePredictionLines.Count > 0)
			{
				int index = GrazePredictionLines.Count / 2;
				if (GrazePredictionLines[0].GrazeInnerOuterLimit == -1)
				{
					index = GrazePredictionLines.Count / 4;
				}
				Utilities.Librations_P_D(GrazePredictionLines[index].LL, GrazePredictionLines[index].LB, GrazePredictionLines[index].AA, out var P_deg, out var D_deg);
				double num14 = 180.0 / Math.PI * GrazePredictionLines[index].N * PiMoon[12];
				GrazeFooter.Add("");
				GrazeFooter.Add("Path coordinates are referred to WGS84 (as used by GPS), with the");
				GrazeFooter.Add("nominal site altitude being referenced to Mean Sea Level. The path");
				GrazeFooter.Add("is adjusted for the effects of refraction at low moon altitudes.");
				GrazeFooter.Add("");
				GrazeHeader.Insert(GrazeHeader.Count - 5, "Date: " + Utilities.DateTime_from_JD(GrazePredictionLines[0].JD) + ",  to  " + Utilities.DateTime_from_JD(GrazePredictionLines[GrazePredictionLines.Count - 1].JD));
				if (Elements[grazeSelectedStar].PlanetDiaArcSec > 0.0)
				{
					GrazeFooter.Add("Angular diameter of " + Elements[grazeSelectedStar].StarId.Trim() + string.Format(" = {0,1:f2} arcsec", Elements[grazeSelectedStar].PlanetDiaArcSec));
					GrazeFooter.Add("");
				}
				string Basis;
				int NumMeasures;
				bool InvalidDiameter;
				double num15 = Utilities.StarDiameter_CHARM2_CADARS(Elements[grazeSelectedStar].StarRA2000 * (180.0 / Math.PI) / 15.0, Elements[grazeSelectedStar].StarDec2000 * (180.0 / Math.PI), Elements[grazeSelectedStar].Mv, Elements[grazeSelectedStar].Mp, Elements[grazeSelectedStar].Mr, out Basis, out NumMeasures, out InvalidDiameter) * VerticalProfileScale * 1000.0;
				if (num15 > 1.0)
				{
					StringBuilder stringBuilder = new StringBuilder();
					stringBuilder.Append("Projected diameter of star  " + string.Format("{0,1:F0}", num15) + " meters  [" + Basis);
					if (NumMeasures > 0)
					{
						stringBuilder.Append(string.Format(", {0,1} measures]", NumMeasures));
					}
					else
					{
						stringBuilder.Append("]");
					}
					GrazeFooter.Add(stringBuilder.ToString());
					GrazeFooter.Add("");
				}
				if (Elements[grazeSelectedStar].VariableDetailsExist)
				{
					GrazeFooter.Add(Elements[grazeSelectedStar].StarId.Substring(0, 7).Trim() + " is variable: ");
					GrazeFooter.Add(Elements[grazeSelectedStar].VariableDetails);
					GrazeFooter.Add("");
				}
				LunarProfile.IsDouble = false;
				LunarProfile.DoublePA = 0.0;
				LunarProfile.DoubleSep = 0.0;
				if (Elements[grazeSelectedStar].DoubleDetailsExist)
				{
					switch (Elements[grazeSelectedStar].DoubleCount)
					{
					case 1:
						GrazeFooter.Add(Elements[grazeSelectedStar].StarId.Substring(0, 7).Trim() + " is double: ");
						break;
					case 2:
						GrazeFooter.Add(Elements[grazeSelectedStar].StarId.Substring(0, 7).Trim() + " is triple: ");
						break;
					case 3:
						GrazeFooter.Add(Elements[grazeSelectedStar].StarId.Substring(0, 7).Trim() + " is quadruple: ");
						break;
					default:
						GrazeFooter.Add(Elements[grazeSelectedStar].StarId.Substring(0, 7).Trim() + " is multiple: ");
						break;
					}
					for (int l = 0; l < Elements[grazeSelectedStar].DoubleCount; l++)
					{
						string text3 = "".PadRight(2);
						string text4 = "";
						string text5 = "";
						string text6 = "";
						string text7 = "";
						double num16;
						double num17;
						double num20;
						int num21;
						int num22;
						int num23;
						int length;
						int num24;
						switch (l)
						{
						case 0:
							LunarProfile.IsDouble = true;
							num16 = (LunarProfile.DoublePA = Elements[grazeSelectedStar].PA1);
							num17 = (LunarProfile.DoubleSep = Elements[grazeSelectedStar].Sep1);
							text3 += Elements[grazeSelectedStar].Doub1;
							text4 = Elements[grazeSelectedStar].Doub1;
							text7 = Elements[grazeSelectedStar].DoubID1;
							num = (LunarProfile.MeanRatio = 1.0);
							if (Elements[grazeSelectedStar].IsMean1)
							{
								GrazeFooter.Add("".PadRight(2) + "Graze prediction is based on the mean position of " + text3.TrimStart(Array.Empty<char>()));
								num = (LunarProfile.MeanRatio = MeanRatioFromDoubleLine(text4));
								num20 = num16 - GrazePredictionLines[index].PA;
								num21 = text4.IndexOf(" ");
								if (num21 > 0)
								{
									text4 = text4.Substring(0, num21).Trim();
								}
								num22 = text4.IndexOf(",");
								num23 = text4.IndexOf("-");
								length = text4.Length;
								num24 = text4.IndexOf(" ");
								if (text4.Substring(0, 2) == "**")
								{
									GrazeFooter.Add("".PadRight(2) + "This next pair is not confirmed");
									text5 = "the primary";
									text6 = "?";
								}
								else if (num24 == 0)
								{
									text5 = "primary";
									text6 = "companion";
								}
								else if (num22 > 0)
								{
									text5 = text4.Substring(0, num22);
									text6 = text4.Substring(num22 + 1);
								}
								else if (num23 > 0)
								{
									text5 = text4.Substring(0, num23);
									text6 = text4.Substring(num23 + 1);
								}
								else
								{
									text5 = text4.Substring(0, length - 1);
									text6 = text4.Substring(length - 1);
								}
								text = "south";
								if (Math.Cos(num20 / (180.0 / Math.PI)) * Math.Cos(GrazePredictionLines[index].PA / (180.0 / Math.PI)) > 0.0)
								{
									text = "north";
								}
								text2 = "later";
								if (Math.Sin(num20 / (180.0 / Math.PI)) * Math.Cos(GrazePredictionLines[index].PA / (180.0 / Math.PI)) > 0.0)
								{
									text2 = "earlier";
								}
								GrazeFooter.Add(" (" + text7.Trim() + ") " + string.Format("Graze path of {0,1} approximately {1,1:F1} km ", text5, Math.Abs(num17 * (1.0 - num) * VerticalProfileScale * Math.Cos(num20 / (180.0 / Math.PI)))) + text + string.Format(", and {0,1:f1} secs ", Math.Abs(num17 * (1.0 - num) / num14 * Math.Sin(num20 / (180.0 / Math.PI)))) + text2 + " compared to prediction");
								text = ((!(text == "south")) ? "south" : "north");
								text2 = ((!(text2 == "later")) ? "later" : "earlier");
								GrazeFooter.Add("".PadRight(2) + string.Format("Graze path of {0,1} approximately {1,1:F1} km ", text6, Math.Abs(num17 * num * VerticalProfileScale * Math.Cos(num20 / (180.0 / Math.PI)))) + text + string.Format(", and {0,1:f1} secs ", Math.Abs(num17 * num / num14 * Math.Sin(num20 / (180.0 / Math.PI)))) + text2 + " compared to prediction");
								GrazeFooter.Add("  - refer to graze profile for exact distances");
								GrazeFooter.Add("");
							}
							break;
						case 1:
							num16 = Elements[grazeSelectedStar].PA2;
							num17 = Elements[grazeSelectedStar].Sep2;
							text3 += Elements[grazeSelectedStar].Doub2;
							text4 = Elements[grazeSelectedStar].Doub2;
							text7 = Elements[grazeSelectedStar].DoubID2;
							break;
						case 2:
							num16 = Elements[grazeSelectedStar].PA3;
							num17 = Elements[grazeSelectedStar].Sep3;
							text3 += Elements[grazeSelectedStar].Doub3;
							text4 = Elements[grazeSelectedStar].Doub3;
							text7 = Elements[grazeSelectedStar].DoubID3;
							break;
						default:
							num16 = Elements[grazeSelectedStar].PA4;
							num17 = Elements[grazeSelectedStar].Sep4;
							text3 += Elements[grazeSelectedStar].Doub4;
							text4 = Elements[grazeSelectedStar].Doub4;
							text7 = Elements[grazeSelectedStar].DoubID4;
							break;
						}
						num20 = num16 - GrazePredictionLines[index].PA;
						num21 = text4.IndexOf(" ");
						if (num21 > 0)
						{
							text4 = text4.Substring(0, num21).Trim();
						}
						num22 = text4.IndexOf(",");
						num23 = text4.IndexOf("-");
						length = text4.Length;
						num24 = text4.IndexOf(" ");
						if (text4.PadRight(2).Substring(0, 2) == "**")
						{
							GrazeFooter.Add("".PadRight(2) + "This next pair is not confirmed");
							text5 = "the primary";
							text6 = "?";
						}
						else if (num24 == 0)
						{
							text5 = "primary";
							text6 = "companion";
						}
						else if (num22 > 0)
						{
							text5 = text4.Substring(0, num22);
							text6 = text4.Substring(num22 + 1);
						}
						else if (num23 > 0)
						{
							text5 = text4.Substring(0, num23);
							text6 = text4.Substring(num23 + 1);
						}
						else
						{
							text5 = text4.Substring(0, length - 1);
							text6 = text4.Substring(length - 1);
						}
						text = "north";
						if (Math.Cos(num20 / (180.0 / Math.PI)) * Math.Cos(GrazePredictionLines[index].PA / (180.0 / Math.PI)) > 0.0)
						{
							text = "south";
						}
						text2 = "earlier";
						if (Math.Sin(num20 / (180.0 / Math.PI)) * Math.Cos(GrazePredictionLines[index].PA / (180.0 / Math.PI)) > 0.0)
						{
							text2 = "later";
						}
						text3 = text3 + " (" + text7.Trim() + ") " + string.Format("Graze path of {0,1} approximately {1,1:F1} km ", text6, Math.Abs(num17 * VerticalProfileScale * Math.Cos(num20 / (180.0 / Math.PI)))) + text + string.Format(", and {0,1:f1} secs ", Math.Abs(num17 / num14 * Math.Sin(num20 / (180.0 / Math.PI)))) + text2 + " compared to " + text5;
						GrazeFooter.Add(text3);
						GrazeFooter.Add("  - refer to graze profile for exact distances");
					}
				}
				GrazeFooter.Add("");
				if (D_deg < -1.0 && (P_deg < 7.0 || (P_deg > 172.0 && P_deg < 188.0) || P_deg > 353.0))
				{
					GrazeFooter.Add("    C A S S I N I   R E G I O N   G R A Z E");
				}
				GrazeFooter.Add("".PadRight(16) + "Librations  Long " + $"{GrazePredictionLines[index].LL:+0.00;-0.00}" + "   Lat " + $"{GrazePredictionLines[index].LB:+0.00;-0.00}");
				GrazeFooter.Add("".PadRight(28) + "P " + $"{P_deg:+0.00;-0.00}" + "     D " + $"{D_deg:+0.00;-0.00}");
				GrazeFooter.Add("".PadRight(6) + "Illumination of moon  " + string.Format("{0,1:F0}%", GrazePredictionLines[index].Illumination) + GrazePredictionLines[index].WaxFlag);
				GrazeFooter.Add("".PadRight(8) + "Elongation of Moon  " + string.Format("{0,1:F0}", GrazePredictionLines[index].Elongation));
				GrazeFooter.Add("".PadRight(4) + "Vertical Profile Scale  approx. " + string.Format("{0,1:F2}", VPS_at_Moon) + " km/arcsec at mean distance of moon");
				GrazeFooter.Add("".PadRight(3) + "Horizontal Scale Factor  " + string.Format("{0,1:F2}", GrazePredictionLines[index].N * 3.5043) + " deg/min");
				GrazeFooter.Add("");
				LunarPredictionLine PredictionLine = GrazePredictionLines[index];
				double cA = GrazePredictionLines[index].CA;
				GrazeFooter.Add("");
				GrazeFooter.Add($"At longitude {PredictionLine.GrazeLongitude:F2}:");
				GrazeFooter.Add("Limiting Magnitudes for various telescope apertures (in cm)");
				GrazeFooter.Add("      CA\\Tdia   5    10    15    20    25    30    35");
				for (int m = 0; m <= 4; m++)
				{
					GrazePredictionLines[index].CA = cA + (double)(2 * m) - 4.0;
					StringBuilder stringBuilder2 = new StringBuilder();
					stringBuilder2.AppendFormat("{0,8:F1}", GrazePredictionLines[index].CA);
					stringBuilder2.Append("   ");
					for (int n = 1; n <= 7; n++)
					{
						stringBuilder2.AppendFormat("{0,6:F1}", visibility(ref PredictionLine, 50 * n, 0.0));
					}
					GrazeFooter.Add(stringBuilder2.ToString());
				}
				GrazePredictionLines[index].CA = cA;
				if (SiteFile != "")
				{
					SitesNearGraze(grazeSelectedStar, SiteFile);
				}
				GrazeFooter.Add("");
				GrazeFooter.Add("".PadRight(78, '-'));
				if (Elements[grazeSelectedStar].DoubleDetailsExist)
				{
					GrazeFooter.Add("Double star catalogue details");
					string[] array = Interferometric_Plus_WDS.Find_WDS_IF_Matches(Elements[grazeSelectedStar].StarRA2000 * (180.0 / Math.PI) / 15.0, Elements[grazeSelectedStar].StarDec2000 * (180.0 / Math.PI), HighPrecision: false, ShowResults: false).Replace("\r", " ").Split(new char[1] { '\n' });
					for (int num25 = 0; num25 <= array.GetUpperBound(0); num25++)
					{
						GrazeFooter.Add(array[num25]);
					}
					GrazeFooter.Add("".PadRight(78, '-'));
				}
			}
			else
			{
				GrazeHeader.Insert(GrazeHeader.Count - 5, "Date of conjunction: " + Utilities.Date_from_JD(Elements[grazeSelectedStar].JDzero, 0) + "    Nominal site altitude " + string.Format("{0,1}", GrazeNominalAltitude) + "m");
			}
		}

		internal static double MeanRatioFromDoubleLine(string Line)
		{
			double result = 0.0;
			double result2 = 0.0;
			string[] array = Line.Split();
			if (!double.TryParse(array[1], out result))
			{
				result = 0.0;
			}
			if (!double.TryParse(array[2], out result2))
			{
				result2 = 0.0;
			}
			if (result < -5.0)
			{
				return 0.0;
			}
			if (result2 < -5.0)
			{
				return 1.0;
			}
			double num = Math.Pow(10.0, result / -2.5);
			double num2 = Math.Pow(10.0, result2 / -2.5);
			double num3 = num + num2;
			return num / num3;
		}

		internal static void GetMagLimit(int LineNumber)
		{
			if (GrazePredictionLines.Count < 1)
			{
				return;
			}
			MagLimits_Show();
			MagLimits.lstMag.get_Items().Clear();
			LunarPredictionLine PredictionLine = GrazePredictionLines[LineNumber];
			double cA = GrazePredictionLines[LineNumber].CA;
			MagLimits.lstMag.get_Items().Add((object)("Graze of  " + GrazePredictionLines[LineNumber].StarId.PadRight(7).Substring(0, 7).Trim() + ",  Magnitude " + $"{GrazePredictionLines[LineNumber].Mv:F1}" + $" [R {GrazePredictionLines[LineNumber].Mr:F1}]" + ",  on " + Utilities.Date_from_JD(Math.Floor(GrazePredictionLines[LineNumber].JD - 0.5) + 0.5, 0)));
			MagLimits.lstMag.get_Items().Add((object)$"At longitude {PredictionLine.GrazeLongitude:F2}:");
			MagLimits.lstMag.get_Items().Add((object)"");
			MagLimits.lstMag.get_Items().Add((object)"Limiting Magnitudes for various telescope apertures (in cm)");
			MagLimits.lstMag.get_Items().Add((object)"      CA\\Tdia   5    10    15    20    25    30    35");
			for (int i = 0; i <= 4; i++)
			{
				GrazePredictionLines[LineNumber].CA = cA + (double)(2 * i) - 4.0;
				StringBuilder stringBuilder = new StringBuilder();
				stringBuilder.AppendFormat("{0,8:F1}", GrazePredictionLines[LineNumber].CA);
				stringBuilder.Append("   ");
				for (int j = 1; j <= 7; j++)
				{
					stringBuilder.AppendFormat("{0,6:F1}", visibility(ref PredictionLine, 50 * j, 0.0));
				}
				MagLimits.lstMag.get_Items().Add((object)stringBuilder.ToString());
			}
			GrazePredictionLines[LineNumber].CA = cA;
		}

		internal static bool GrazePoint(int ElementNumber, int NorthOrSouthLimit, int NorthOrSouthPlanetLimb, bool DoublePath, double Longitude, double SiteAltitude, bool FullGrazePrediction, bool StartWithLastLatitude, ref LunarPredictionLine PLine, out double Latitude_Deg)
		{
			double num = 0.0;
			double num2 = 0.0;
			double num3 = 0.0;
			double num4 = 1.0;
			Latitude_Deg = 0.0;
			double x = Elements[ElementNumber].X;
			double y = Elements[ElementNumber].Y;
			double dX = Elements[ElementNumber].dX;
			double dY = Elements[ElementNumber].dY;
			double d = dX * dX + dY * dY;
			double num5 = Math.Sqrt(d);
			double num6 = 0.2725076 + Elements[ElementNumber].ShadowIncreaseOnFundamentalPlane + (double)NorthOrSouthPlanetLimb * Elements[ElementNumber].PlanetDiaArcSec / 2.0 / 3600.0 / (180.0 / Math.PI) / PiMoon[12];
			double value = (double)NorthOrSouthLimit * num6 - (x * dY - y * dX) / num5;
			if (Math.Abs(value) > 1.0)
			{
				return false;
			}
			double num7 = 0.0;
			int num8 = 0;
			double num9 = 0.0;
			if (DoublePath)
			{
				num9 = 88 * Math.Sign(Elements[ElementNumber].Y);
			}
			if (StartWithLastLatitude)
			{
				num9 = LastGrazeLatitude;
			}
			double num12;
			double num13;
			double num15;
			do
			{
				if (Math.Abs(num9) == 90.0)
				{
					num9 -= 0.01 * (double)Math.Sign(num9);
				}
				while (Math.Abs(num9) > 90.0)
				{
					num9 = (double)(180 * Math.Sign(num9)) - num9;
				}
				double num10 = Utilities.GeoidHeight(Longitude * (180.0 / Math.PI), num9);
				double num11 = Math.Atan(Utilities.sqrt_1LessEarthEllipticitySqrd * Math.Tan(num9 / (180.0 / Math.PI)));
				num12 = Math.Cos(num11) * (1.0 + (SiteAltitude + num10) / 6378140.0) * num4;
				num13 = Utilities.sqrt_1LessEarthEllipticitySqrd * Math.Sin(num11) * (1.0 + (SiteAltitude + num10) / 6378140.0) * num4;
				int num14 = 0;
				double num18;
				double num19;
				do
				{
					num15 = Elements[ElementNumber].H + 0.2625161707907961 * num7 + Longitude + Elements[ElementNumber].DeltaH * num7;
					double num16 = num12 * Math.Sin(num15);
					double num17 = num13 * Math.Cos(Elements[ElementNumber].StarDec + num7 * Elements[ElementNumber].DeltaDec) - num12 * Math.Cos(num15) * Math.Sin(Elements[ElementNumber].StarDec + num7 * Elements[ElementNumber].DeltaDec);
					num18 = num13 * Math.Sin(Elements[ElementNumber].StarDec + num7 * Elements[ElementNumber].DeltaDec) + num12 * Math.Cos(num15) * Math.Cos(Elements[ElementNumber].StarDec + num7 * Elements[ElementNumber].DeltaDec);
					x = Elements[ElementNumber].X + Elements[ElementNumber].dX * num7 + Elements[ElementNumber].d2X * num7 * num7 + Elements[ElementNumber].d3X * num7 * num7 * num7 + Elements[ElementNumber].d4X * num7 * num7 * num7 * num7 - num16;
					y = Elements[ElementNumber].Y + Elements[ElementNumber].dY * num7 + Elements[ElementNumber].d2Y * num7 * num7 + Elements[ElementNumber].d3Y * num7 * num7 * num7 + Elements[ElementNumber].d4Y * num7 * num7 * num7 * num7 - num17;
					dX = Elements[ElementNumber].dX + 2.0 * Elements[ElementNumber].d2X * num7 + 3.0 * Elements[ElementNumber].d3X * num7 * num7 + 4.0 * Elements[ElementNumber].d4X * num7 * num7 * num7 - (0.2625161707907961 + Elements[ElementNumber].DeltaH) * num12 * Math.Cos(num15);
					dY = Elements[ElementNumber].dY + 2.0 * Elements[ElementNumber].d2Y * num7 + 3.0 * Elements[ElementNumber].d3Y * num7 * num7 + 4.0 * Elements[ElementNumber].d4Y * num7 * num7 * num7 - (0.2625161707907961 + Elements[ElementNumber].DeltaH) * num12 * Math.Sin(num15) * Math.Sin(Elements[ElementNumber].StarDec + num7 * Elements[ElementNumber].DeltaDec);
					d = dX * dX + dY * dY;
					num5 = Math.Sqrt(d);
					num19 = (x * dX + y * dY) / d;
					num7 -= num19;
					num14++;
				}
				while (Math.Abs(num19) > 0.0001 && num14 < 20);
				num6 = 0.2725076 + Elements[ElementNumber].ShadowIncreaseOnFundamentalPlane + Elements[ElementNumber].SinF2 * num18 + (double)NorthOrSouthPlanetLimb * Elements[ElementNumber].PlanetDiaArcSec / 2.0 / 3600.0 / (180.0 / Math.PI) / PiMoon[12];
				value = (double)NorthOrSouthLimit - (x * dY - y * dX) / num6 / num5;
				if (num8 == 0)
				{
					num8 = 1;
					num3 = value;
					num = num9;
					num9 += 1.0;
				}
				else
				{
					num8++;
				}
				if (num8 > 1 && value == num3)
				{
					num8 = 20;
				}
				if (num8 > 2 && num18 < -0.1)
				{
					num8 = 20;
				}
				if (num8 > 1 && num8 < 20)
				{
					num2 = (num9 - num) / (num3 - value) * value;
					num = num9;
					num3 = value;
					num9 += num2;
					double num20 = (0.0 - Math.Cos(Elements[ElementNumber].StarDec)) * Math.Sin(num15);
					double num21 = Math.Sin(Elements[ElementNumber].StarDec) * num12 - Math.Cos(Elements[ElementNumber].StarDec) * Math.Cos(num15) * num13;
					double num22 = Math.Sin(Elements[ElementNumber].StarDec) * num13 + Math.Cos(Elements[ElementNumber].StarDec) * Math.Cos(num15) * num12;
					PLine.MoonAlt = 180.0 / Math.PI * Math.Atan(num22 / Math.Sqrt(num20 * num20 + num21 * num21));
					PLine.MoonAz = 180.0 / Math.PI * Math.Atan2(num20, num21);
					if (PLine.MoonAz < 0.0)
					{
						PLine.MoonAz += 360.0;
					}
					num4 = ((PLine.MoonAlt < 0.0) ? 1.000292 : ((!(PLine.MoonAlt > 20.0)) ? (1.000292 * Math.Cos((PLine.MoonAlt + Utilities.Refraction_deg(PLine.MoonAlt, 1016.0, 0.0)) / (180.0 / Math.PI)) / Math.Cos(PLine.MoonAlt / (180.0 / Math.PI))) : 1.0));
				}
			}
			while ((num8 < 2) | (Math.Abs(num2) > 0.0005 && num8 < 20));
			if (num8 >= 20)
			{
				return false;
			}
			if (double.IsNaN(PLine.MoonAlt))
			{
				return false;
			}
			LastGrazeLatitude = num9;
			Latitude_Deg = num9;
			if (FullGrazePrediction)
			{
				if (PLine.MoonAlt < 0.0)
				{
					return false;
				}
				PLine.StarId = Elements[ElementNumber].StarId.Trim();
				PLine.ElementRecordNumber = ElementNumber;
				PLine.Mv = Elements[ElementNumber].Mv;
				PLine.Mr = Elements[ElementNumber].Mr;
				PLine.JD = Elements[ElementNumber].JDzero + ((double)Elements[ElementNumber].T + num7 - deltaT) / 24.0;
				PLine.GrazeLongitude = Longitude * (180.0 / Math.PI);
				PLine.GrazeLatitude = Latitude_Deg;
				PLine.N = num5;
				PLine.PA = Math.Atan2(0.0 - x, 0.0 - y) * (180.0 / Math.PI);
				if (PLine.PA < 0.0)
				{
					PLine.PA += 360.0;
				}
				Utilities.TopocentricMoon(PLine.JD, Longitude, Latitude_Deg / (180.0 / Math.PI), SiteAltitude, Altitude_is_MSL: true, 84, out var RA, out var Dec, out var _, out var _, out var _, out var _, out var _, out var _);
				Utilities.Distance(RA, Dec, Elements[ElementNumber].StarRA, Elements[ElementNumber].StarDec, out var _, out var PA_atOrigin);
				PLine.PA = PA_atOrigin * (180.0 / Math.PI);
				Physical(IsGraze: true, ref PLine, ElementNumber, ((double)Elements[ElementNumber].T + num7) / 24.0, num15, num13, num12);
			}
			return true;
		}

		public static void SitesNearGraze(int ElementNumber, string SiteFile)
		{
			int num = 0;
			double num2 = 0.0;
			Sites sites = new Sites();
			GrazeSiteScan.Clear();
			using StreamReader streamReader = new StreamReader(AppPath + "\\Sites\\" + SiteFile);
			while (!streamReader.EndOfStream)
			{
				sites.Read_SiteFile(streamReader.ReadLine());
				double num3 = sites.Longitude / (180.0 / Math.PI);
				double latitude = sites.Latitude;
				double pSin = sites.pSin;
				double pCos = sites.pCos;
				double telesDia_mm = sites.ApertureCM * 10f;
				double magLimitCorrn = sites.MagCorrection;
				double grazeTravelDist = sites.GrazeTravelDist;
				double num4 = 0.0;
				int num5 = 0;
				bool flag = true;
				PredictionLine = new LunarPredictionLine();
				double num6;
				double num9;
				double num13;
				double num15;
				do
				{
					num6 = num4;
					double num7 = Elements[ElementNumber].X + Elements[ElementNumber].dX * num4 + Elements[ElementNumber].d2X * num4 * num4 + Elements[ElementNumber].d3X * num4 * num4 * num4 + Elements[ElementNumber].d4X * num4 * num4 * num4 * num4;
					double num8 = Elements[ElementNumber].Y + Elements[ElementNumber].dY * num4 + Elements[ElementNumber].d2Y * num4 * num4 + Elements[ElementNumber].d3Y * num4 * num4 * num4 + Elements[ElementNumber].d4Y * num4 * num4 * num4 * num4;
					num9 = Elements[ElementNumber].H + num3 + 0.2625161707907961 * num4 + Elements[ElementNumber].DeltaH * num4;
					double num10 = pCos * Math.Sin(num9);
					double num11 = pCos * Math.Cos(num9);
					double num12 = pSin * Math.Cos(Elements[ElementNumber].StarDec + num4 * Elements[ElementNumber].DeltaDec) - num11 * Math.Sin(Elements[ElementNumber].StarDec + num4 * Elements[ElementNumber].DeltaDec);
					num13 = num7 - num10;
					double num14 = Elements[ElementNumber].dX + 2.0 * Elements[ElementNumber].d2X * num4 + 3.0 * Elements[ElementNumber].d3X * num4 * num4 + 4.0 * Elements[ElementNumber].d4X * num4 * num4 * num4 - (0.2625161707907961 + Elements[ElementNumber].DeltaH) * num11;
					num15 = num8 - num12;
					double num16 = Elements[ElementNumber].dY + 2.0 * Elements[ElementNumber].d2Y * num4 + 3.0 * Elements[ElementNumber].d3Y * num4 * num4 + 4.0 * Elements[ElementNumber].d4Y * num4 * num4 * num4 - (0.2625161707907961 + Elements[ElementNumber].DeltaH) * num10 * Math.Sin(Elements[ElementNumber].StarDec + num4 * Elements[ElementNumber].DeltaDec);
					double num17 = num14 * num14 + num16 * num16;
					double num18 = Math.Sqrt(num17);
					double num19 = num13 * num14 + num15 * num16;
					double num20 = Math.Sin(Elements[ElementNumber].StarDec) * pSin + Math.Cos(Elements[ElementNumber].StarDec) * Math.Cos(num9) * pCos;
					if (num20 > 1.0)
					{
						num20 = 1.0;
					}
					double num21 = 0.2725076 + Bessel.ShadowIncreaseOnFundamentalPlane + Bessel.SinF2 * num20;
					num2 = (num13 * num16 - num15 * num14) / num18 / num21;
					if ((Math.Abs(num2) > 1.3) | (Math.Abs(num2) < 0.7))
					{
						break;
					}
					num4 = num6 - num19 / num17;
					num5++;
				}
				while (Math.Abs(num6 - num4) > 0.0001 && num5 < 30);
				EventPartDay = ((double)Elements[ElementNumber].T + num4) / 24.0;
				PredictionLine.JD = Elements[ElementNumber].JDzero + ((double)Elements[ElementNumber].T + num4 - deltaT) / 24.0;
				PredictionLine.Dec = Elements[ElementNumber].StarDec;
				double num22 = (0.0 - Math.Cos(PredictionLine.Dec)) * Math.Sin(num9);
				double num23 = Math.Sin(PredictionLine.Dec) * pCos - Math.Cos(PredictionLine.Dec) * Math.Cos(num9) * pSin;
				double num24 = Math.Sin(PredictionLine.Dec) * pSin + Math.Cos(PredictionLine.Dec) * Math.Cos(num9) * pCos;
				PredictionLine.MoonAlt = 180.0 / Math.PI * Math.Atan(num24 / Math.Sqrt(num22 * num22 + num23 * num23));
				if (PredictionLine.MoonAlt < 0.0)
				{
					flag = false;
				}
				if (flag)
				{
					PredictionLine.MoonAz = 180.0 / Math.PI * Math.Atan2(num22, num23);
					if (PredictionLine.MoonAz < 0.0)
					{
						PredictionLine.MoonAz += 360.0;
					}
					PredictionLine.PA = Math.Atan2(0.0 - num13, 0.0 - num15) * (180.0 / Math.PI);
					if (PredictionLine.PA < 0.0)
					{
						PredictionLine.PA += 360.0;
					}
					Physical(num == 2, ref PredictionLine, ElementNumber, EventPartDay, num9, pSin, pCos);
					double num25 = visibility(ref PredictionLine, telesDia_mm, magLimitCorrn);
					if (Elements[ElementNumber].Mv >= num25)
					{
						flag = false;
					}
				}
				if (!flag)
				{
					continue;
				}
				double num26 = Math.Abs((Elements[ElementNumber].X * Elements[ElementNumber].dY - Elements[ElementNumber].Y * Elements[ElementNumber].dX) / Math.Sqrt(Elements[ElementNumber].dX * Elements[ElementNumber].dX + Elements[ElementNumber].dY * Elements[ElementNumber].dY) - 0.2725076 * (double)Math.Sign(num2));
				double num27 = ((!(num26 < 0.99)) ? 8.06E-05 : (Math.Sqrt(1.0 - num26 * num26) / 1738.0));
				double num28 = num27 * grazeTravelDist;
				if (Elements[ElementNumber].Mv < 6.5 && (double)Settings.Default.Graze_TravelDistance_65 > grazeTravelDist)
				{
					num28 = num27 * (double)Settings.Default.Graze_TravelDistance_65;
				}
				if (Elements[ElementNumber].Mv < 4.5 && (double)Settings.Default.Graze_TravelDistance_45 > grazeTravelDist)
				{
					num28 = num27 * (double)Settings.Default.Graze_TravelDistance_45;
				}
				if (!(Math.Abs(Math.Abs(num2) - 1.0) < num28))
				{
					continue;
				}
				double num29 = 1.0;
				PredictionLine = new LunarPredictionLine();
				double num30 = num3 * (180.0 / Math.PI);
				int northOrSouthLimit = Math.Sign(num2);
				if ((PredictionLine.MoonAz > 180.0) & (PredictionLine.MoonAlt < 20.0))
				{
					num29 = -1.0;
				}
				if (GrazePoint(ElementNumber, northOrSouthLimit, 0, DoublePath: false, num30 / (180.0 / Math.PI), 0.0, FullGrazePrediction: true, StartWithLastLatitude: false, ref PredictionLine, out var Latitude_Deg))
				{
					PredictionLine.GrazeLongitude = num30;
					PredictionLine.GrazeLatitude = Latitude_Deg;
				}
				num30 += num29;
				if (GrazePoint(ElementNumber, northOrSouthLimit, 0, DoublePath: false, num30 / (180.0 / Math.PI), 0.0, FullGrazePrediction: false, StartWithLastLatitude: true, ref PredictionLine, out Latitude_Deg))
				{
					PredictionLine.GrazeLongitude2 = num30;
					PredictionLine.GrazeLatitude2 = Latitude_Deg;
				}
				PredictionLine.CA += 3.0;
				PredictionLine.StarId = Elements[ElementNumber].StarId;
				PredictionLine.Mv = Elements[ElementNumber].Mv;
				PredictionLine.Mr = Elements[ElementNumber].Mr;
				PredictionLine.VariableCode = Elements[ElementNumber].StarVar;
				if (visibility(ref PredictionLine, telesDia_mm, magLimitCorrn) > PredictionLine.Mv)
				{
					PredictionLine.CA -= 3.0;
					PredictionLine.EventPhase = "Gr";
					PredictionLine.GrazeDeltaLat = num29 * (PredictionLine.GrazeLatitude2 - PredictionLine.GrazeLatitude);
					PredictionLine.GrazeAZ = Math.Atan((0.0 - PredictionLine.GrazeDeltaLat) / Math.Cos(latitude / (180.0 / Math.PI))) * (180.0 / Math.PI);
					if (PredictionLine.GrazeAZ < 0.0)
					{
						PredictionLine.GrazeAZ += 360.0;
					}
					PredictionLine.GrazeD = (PredictionLine.GrazeLatitude - latitude) * Math.Cos(PredictionLine.GrazeAZ / (180.0 / Math.PI)) * 111.0;
					StringBuilder stringBuilder = new StringBuilder();
					stringBuilder.Append(sites.Name.PadRight(32));
					stringBuilder.AppendFormat("{0,7:F1}", sites.Longitude);
					stringBuilder.AppendFormat("{0,7:F1}", sites.Latitude);
					stringBuilder.AppendFormat("{0,6:F0}km", Math.Abs(PredictionLine.GrazeD));
					stringBuilder.Append("  " + PredictionLine.EventTime());
					GrazeSiteScan.Add(stringBuilder.ToString());
				}
			}
		}

		internal static double visibility(ref LunarPredictionLine PredictionLine, double TelesDia_mm, double MagLimitCorrn)
		{
			double num = 0.0;
			double num2 = 0.0;
			double TerminatorDistance = 0.0;
			double illumination = PredictionLine.Illumination;
			double elongation = PredictionLine.Elongation;
			num = PredictionLine.SunAlt;
			double moonAlt = PredictionLine.MoonAlt;
			double num3 = PredictionLine.CA;
			double num4 = 90.0 - moonAlt;
			double num5 = 0.7 * TelesDia_mm;
			double num6 = 0.0;
			double num7;
			if (num4 >= 92.0)
			{
				num7 = -99.0;
				return num7 + MagLimitCorrn;
			}
			int num8 = 0;
			double num9 = num3;
			if (illumination < 90.0 && elongation > 150.0)
			{
				num7 = 10.0 + TelesDia_mm / 80.0;
				PredictionLine.LimbDistance = 0.0;
				PredictionLine.MountainDistance = 0.0;
				return num7 + MagLimitCorrn;
			}
			Terminator(num3, elongation, out var TerminatorDistance2, out var MountainDistance);
			PredictionLine.LimbDistance = TerminatorDistance2;
			PredictionLine.MountainDistance = MountainDistance;
			if (!(illumination < -0.5) && !(illumination > 10.0 && num3 < 0.0) && !(illumination <= 10.0 && num3 >= 0.0))
			{
				if (illumination <= 10.0)
				{
					double elongation_deg = 180.0 - elongation;
					Terminator(0.0 - num3, elongation_deg, out TerminatorDistance, out var _);
					if (TerminatorDistance < 3.0)
					{
						num8 = 1;
					}
				}
				else
				{
					if (MountainDistance <= 3.0)
					{
						num3 = -1.0;
					}
					if (TerminatorDistance2 > 1.0)
					{
						num8 = 1;
					}
				}
			}
			while (true)
			{
				double num10 = Math.Cos(num4 / (180.0 / Math.PI));
				double num11 = 1.0 / (num10 + 0.025 * Math.Exp(-11.0 * num10));
				double num12 = Math.Sqrt(2.89 * num11 * Math.Pow(1.0, 2.0) + Math.Pow(454.0 / TelesDia_mm, 2.0));
				double num13 = Math.Pow(10.0, 0.12 * num11);
				double num14 = 1.41;
				double num15 = 1.25;
				double num16 = 4.0;
				double num17 = Math.Pow(TelesDia_mm / (num5 * num16), 2.0);
				if (num17 < 1.0)
				{
					num17 = 1.0;
				}
				double num18 = Math.Pow(num16 / TelesDia_mm, 2.0);
				num10 = 1.0 - Math.Exp(-0.026 * num16 * num16);
				double num19 = 1.0 - Math.Exp(-0.026 * Math.Pow(TelesDia_mm / num5, 2.0));
				double num20 = Math.Pow(TelesDia_mm / (num16 * num5), 2.0) * num10 / num19;
				if (num20 > 1.0)
				{
					num20 = 1.0;
				}
				double num21 = (1.0 + 0.03 * Math.Pow(num5 * num12 / 100.0, 2.0)) / 1.0 / 1.0;
				double num22 = num13 * num14 * num15 * num17 * num18 * num20 * num21;
				num14 = num5 * num5 * num14 * num15 * num17 * num18 * num20;
				double num23 = (180.0 - elongation) / (180.0 / Math.PI);
				double num24 = 1.49 * num23 + 0.043 * Math.Pow(num23, 4.0) - 12.73;
				double num25 = Math.Pow(10.0, -0.4 * (num24 + 16.57));
				double num26 = 5670000000000.0 * num25 / (num13 * illumination);
				double num27 = 3.14159 - num23;
				double num28 = 1.49 * num27 + 0.043 * Math.Pow(num27, 4.0) - 12.73;
				double num29 = 11000000.0 * Math.Pow(10.0, -0.4 * (num28 + 16.57)) / num13;
				num10 = Math.Pow(Math.Cos(num3 / (180.0 / Math.PI)), 2.0) + Math.Pow(1.0 - illumination / 100.0 + Math.Sin(num3 / (180.0 / Math.PI)), 2.0);
				num19 = 1.0 - 0.4 * Math.Exp((0.0 - num3) / 30.0);
				if (num19 < 0.6)
				{
					num19 = 0.6;
				}
				double num30 = 0.25 * Math.Sqrt(num10) * num19;
				double num31 = 62500000.0 * num25 * (num13 - 1.0) / Math.Pow(num30 * num13, 2.0);
				double num32 = 46300000.0 * num25 * 0.005 / (num30 * num30 * num13);
				double num33 = 11300000.0 * num25 / (num30 * num30 * num30 * TelesDia_mm * num13);
				double num34 = 260000000.0 * num25 * Math.Exp(-1.0 * Math.Pow(num30 / 0.4, 2.0)) / num13;
				double num35 = num31 + num32 + num33 + num34;
				num10 = 1.0 - 0.96 * Math.Pow(Math.Sin(num4 / (180.0 / Math.PI)), 2.0);
				double num36 = 180.0 * (0.4 + 0.6 / Math.Sqrt(num10)) / num13;
				num10 = Math.Pow(10.0, num23 / 1.571 - 1.1);
				if (num10 < 1.0)
				{
					num10 = 1.0;
				}
				double num37 = num;
				if (num37 > 10.0)
				{
					num37 = 10.0;
				}
				double num38 = num10 * Math.Pow(10.0, 8.45 + 0.4 * num37) * (num13 - 1.0) / num13;
				double num39 = Math.Pow(10.0, 5.36) * (1.06 + Math.Pow(Math.Cos(num23), 2.0));
				double num40 = Math.Pow(10.0, 1.65 + 1.43 * num23);
				num10 = -0.12 / Math.Sqrt(1.0 - 0.96 * Math.Pow(Math.Sin((90.0 - num) / (180.0 / Math.PI)), 2.0));
				double num41 = 11700.0 * (num39 + num40) * Math.Pow(10.0, num10) * (1.0 - Math.Pow(10.0, num10));
				double num42 = num36 + num38 + 0.0;
				if (num41 < num38)
				{
					num42 = num36 + num41 + 0.0;
				}
				double num43 = num42 + num29 + num35;
				if (num3 < 0.0)
				{
					num43 = num42 + num26;
				}
				double num44 = Math.Pow(num43 / num14, -0.29);
				if (num44 > 100.0)
				{
					num44 = 100.0;
				}
				double num45 = 380.0 * Math.Pow(10.0, 0.3 * num44);
				double num46 = 999.0;
				if (num44 < 1.0)
				{
					num46 = 42.0 * Math.Pow(10.0, 8.28 * num44);
				}
				double num47 = 900.0;
				if (num45 < 900.0)
				{
					num47 = num45;
				}
				if (num46 < num47)
				{
					num47 = num46;
				}
				num47 /= 1.0;
				num10 = 1.85 * (num25 * 0.000929 * 0.5 / num13) / Math.Pow(1.122, num5 * num30);
				num19 = Math.Exp(-0.16 * Math.Pow(1E-06 * num43 / num14 + num10, 0.4));
				double num48 = 6.78463 * num19;
				if (num48 < 2.0)
				{
					num48 = 2.0;
				}
				num16 = 7.0 * Math.Exp(-0.5 * Math.Pow(0.3, 2.0)) * num19;
				if (num16 < 2.0)
				{
					num16 = 2.0;
				}
				num17 = Math.Pow(num48 / (num5 * num16), 2.0);
				if (num17 < 1.0)
				{
					num17 = 1.0;
				}
				num18 = Math.Pow(num48 / TelesDia_mm, 2.0);
				num10 = 1.0 - Math.Exp(-0.026 * num48 * num48);
				num19 = 1.0 - Math.Exp(-0.026 * Math.Pow(TelesDia_mm / num5, 2.0));
				num20 = Math.Pow(TelesDia_mm / (num48 * num5), 2.0) * num10 / num19;
				if (num20 > 1.0)
				{
					num20 = 1.0;
				}
				num21 = (1.0 + 0.03 * Math.Pow(num5 * num12 / num47, 2.0)) / Math.Pow(1.0, 2.0);
				double num49 = Math.Pow(10.0, -0.26);
				if (num43 > 1480.0)
				{
					num49 = 1.0;
				}
				num22 = num13 * num14 * num15 * num17 * num18 * num20 * num21 * num49;
				num14 = num5 * num5 * num14 * num15 * num17 * num18 * num20 * num49;
				double num50 = num43 / num14;
				double num51 = 4.46E-09 * Math.Pow(1.0 + Math.Sqrt(1.26E-06 * num50), 2.0);
				double num52 = 1.59E-10 * Math.Pow(1.0 + Math.Sqrt(0.0126 * num50), 2.0);
				double num53 = num51;
				if (num52 < num51)
				{
					num53 = num52;
				}
				num7 = -16.57 - 2.5 * Math.Log10(num53 * num22);
				num7 += 0.48;
				if (num6 > 0.0 && num >= -6.0)
				{
					num7 += 1.0;
				}
				if (illumination < 2.5)
				{
					num7 -= 1.0;
				}
				if (illumination < 1.5)
				{
					num7 -= 2.0;
				}
				if (illumination >= 3.5 && illumination < 4.5)
				{
					num7 += 0.5;
				}
				if (illumination >= 4.5 && illumination < 6.5)
				{
					num7 += 1.0;
				}
				if (illumination >= 6.5 && illumination < 8.5)
				{
					num7 += 0.5;
				}
				if (num3 < 0.0)
				{
					num7 -= 1.0;
				}
				if (num3 < 0.0 && num6 > 0.0 && num < -6.0)
				{
					num7 += 0.5;
				}
				num3 = num9;
				if (num8 < 1 || num8 > 1)
				{
					break;
				}
				num2 = num7;
				if (illumination <= 10.0)
				{
					num3 = 1.0;
				}
				num8++;
			}
			if (num8 < 1)
			{
				return num7 + MagLimitCorrn;
			}
			double num54 = num7;
			if (illumination <= 10.0)
			{
				num7 = num2 + (num54 - num2) * (2.0 - TerminatorDistance) / 2.0;
				num3 = num9;
				return num7 + MagLimitCorrn;
			}
			if (TerminatorDistance2 > 3.0)
			{
				num7 = num2 + 0.7 * (num54 - num2) + 0.3 * (num54 - num2) * MountainDistance / 3.0;
				return num7 + MagLimitCorrn;
			}
			num7 = num2 + 0.7 * (num54 - num2) * (TerminatorDistance2 - 1.0) / 2.0;
			return num7 + MagLimitCorrn;
		}

		internal static void Terminator(double CuspAngle_deg, double Elongation_deg, out double TerminatorDistance, out double MountainDistance)
		{
			double[] array = new double[91]
			{
				90.0, 90.0, 90.0, 90.0, 90.0, 90.0, 90.0, 73.95, 57.2, 48.32,
				42.21, 37.62, 34.0, 31.06, 28.6, 26.51, 24.72, 23.16, 21.78, 20.55,
				19.46, 18.48, 17.59, 16.78, 16.05, 15.36, 14.74, 14.16, 13.62, 13.13,
				12.66, 12.22, 11.82, 11.43, 11.08, 10.73, 10.4, 10.11, 9.82, 9.53,
				9.28, 9.04, 8.79, 8.56, 8.34, 8.15, 7.96, 7.75, 7.56, 7.38,
				7.22, 7.07, 6.91, 6.75, 6.6, 6.45, 6.32, 6.2, 6.08, 5.96,
				5.82, 5.7, 5.58, 5.47, 5.37, 5.27, 5.17, 5.08, 5.0, 4.89,
				4.79, 4.7, 4.61, 4.52, 4.44, 4.36, 4.29, 4.21, 4.14, 4.08,
				4.01, 3.94, 3.86, 3.78, 3.71, 3.64, 3.58, 3.51, 3.45, 3.39,
				3.34
			};
			Elongation_deg = Math.Abs(Elongation_deg);
			if (CuspAngle_deg < 0.0)
			{
				TerminatorDistance = 0.0;
				MountainDistance = 0.0;
			}
			else if (Elongation_deg <= 90.0)
			{
				TerminatorDistance = 933.0 * Math.Sin(CuspAngle_deg / (180.0 / Math.PI));
				double num = array[(int)Elongation_deg];
				MountainDistance = 933.0 * Math.Sin((CuspAngle_deg - num) / (180.0 / Math.PI));
				if (MountainDistance < 0.0)
				{
					MountainDistance = 0.0;
				}
			}
			else
			{
				TerminatorDistance_GibbousMoon(CuspAngle_deg, Elongation_deg, Mountains: false, out TerminatorDistance);
				TerminatorDistance_GibbousMoon(CuspAngle_deg, Elongation_deg, Mountains: true, out MountainDistance);
			}
		}

		private static void TerminatorDistance_GibbousMoon(double CA, double Elong, bool Mountains, out double Dist)
		{
			double num = 0.0 - Math.Cos(Elong / (180.0 / Math.PI));
			double num2 = 0.0;
			double num3 = 0.0;
			double num4 = 0.0;
			double num5 = 0.0;
			double num6 = 0.0;
			int num7 = 0;
			double num8;
			if (Mountains)
			{
				num8 = Math.Sqrt(0.0034549790759998977) * Math.Sin(Elong / (180.0 / Math.PI));
				double num9 = Math.Sin(CA / (180.0 / Math.PI)) * num + num8;
				double num10 = Math.Cos(CA / (180.0 / Math.PI));
				if (num9 * num9 + num10 * num10 >= 1.0)
				{
					Dist = 0.0;
					return;
				}
			}
			else
			{
				num8 = 0.0;
			}
			double num11 = Math.Sin(CA / (180.0 / Math.PI));
			double num12 = Math.Cos(CA / (180.0 / Math.PI));
			num4 = ((CA < 10.0) ? 0.1 : ((CA < 20.0) ? 0.2 : ((CA < 30.0) ? 0.3 : ((!(CA < 40.0)) ? 1.0 : 0.5))));
			double num13 = CA - num4;
			if (num13 < 0.0)
			{
				num13 = 0.0;
			}
			if (num13 > 87.0)
			{
				num13 = 87.0;
			}
			double num14;
			double num15;
			do
			{
				double num9 = Math.Sin(num13 / (180.0 / Math.PI)) * num + num8;
				double num10 = Math.Cos(num13 / (180.0 / Math.PI));
				num14 = num11 - num9;
				num15 = num12 - num10;
				num2 = Math.Sqrt(num14 * num14 + num15 * num15);
				if (num3 - num2 < 0.0 && num7 > 1)
				{
					break;
				}
				num13 += num4;
				num3 = num2;
				num5 = num14;
				num6 = num15;
				num7++;
			}
			while (num13 < 90.0);
			double num16 = num14 - num5;
			double num17 = num15 - num6;
			double num18 = Math.Sqrt(num16 * num16 + num17 * num17);
			double num19 = (num14 * num17 - num15 * num16) / num18;
			Dist = 933.0 * num19;
		}

		internal static void MultiLocationPredictions(string SiteFile, bool GetStar)
		{
			//IL_0014: Unknown result type (might be due to invalid IL or missing references)
			int num = 0;
			int num2 = 1;
			if (GetStar)
			{
				MultiLocationStar = -1;
				((Form)new LunarGrazeSelection(ShowGrazePanel: false, Multi: true)).ShowDialog();
			}
			int multiLocationStar = MultiLocationStar;
			if (multiLocationStar < 0)
			{
				return;
			}
			List<Sites> list = new List<Sites>();
			using (StreamReader streamReader = new StreamReader(AppPath + "\\sites\\" + SiteFile))
			{
				do
				{
					Sites sites = new Sites();
					sites.Read_SiteFile(streamReader.ReadLine());
					list.Add(sites);
				}
				while (!streamReader.EndOfStream);
			}
			ShortD.Clear();
			ShortR.Clear();
			LongD.Clear();
			LongR.Clear();
			MultiSites.Clear();
			MultiLoc_Illum = (MultiLoc_Elong = "");
			MultiPrediction.Clear();
			((Control)LunarPrediction.pBar).set_Visible(true);
			LunarPrediction.pBar.set_Minimum(0);
			LunarPrediction.pBar.set_Maximum(list.Count);
			for (int i = 0; i < list.Count; i++)
			{
				LunarPrediction.pBar.set_Value(i);
				Application.DoEvents();
				bool flag = false;
				ComputeEventForALocation(multiLocationStar, list[i].Longitude / (180.0 / Math.PI), list[i].Latitude, list[i].Altitude, list[i].pSin, list[i].pCos, list[i].ApertureCM * 40f, 0.0, 0.0, MultiSite: true, num2);
				for (int j = num; j < MultiPrediction.Count; j++)
				{
					if (MultiLoc_Illum.Length < 1)
					{
						MultiLoc_Illum = $"{MultiPrediction[j].Illumination:F0}" + MultiPrediction[j].WaxFlag;
						MultiLoc_Elong = $"{MultiPrediction[j].Elongation:F0}";
						MultiLocDate = MultiPrediction[j].EventDate();
					}
					if (MultiPrediction[j].EventPhase.ToUpper() == "D " && (!Settings.Default.LunarMultiSitePrediction_SunAlt | (MultiPrediction[j].SunAlt < -6.0)))
					{
						ShortD.Add(string.Format("{0,2:F0} ", num2) + list[i].Name.PadRight(32) + MultiPrediction[j].ShortEventPrediction());
						LongD.Add(string.Format("{0,2:F0} ", num2) + list[i].Name.PadRight(32) + MultiPrediction[j].LongEventPrediction());
						flag = true;
					}
					if (MultiPrediction[j].EventPhase.ToUpper() == "R " && (!Settings.Default.LunarMultiSitePrediction_SunAlt | (MultiPrediction[j].SunAlt < -6.0)))
					{
						ShortR.Add(string.Format("{0,2:F0} ", num2) + list[i].Name.PadRight(32) + MultiPrediction[j].ShortEventPrediction());
						LongR.Add(string.Format("{0,2:F0} ", num2) + list[i].Name.PadRight(32) + MultiPrediction[j].LongEventPrediction());
						flag = true;
					}
				}
				if (flag)
				{
					MultiSites.Add(string.Format("{0,2:F0} ", num2) + list[i].Name.PadRight(32) + Utilities.DEGtoDMS(list[i].Longitude, 4, 1, MinutesOnly: true) + "    " + Utilities.DEGtoDMS(list[i].Latitude, 3, 1, MinutesOnly: true));
					num2++;
				}
				num = MultiPrediction.Count;
			}
			((Control)LunarPrediction.pBar).set_Visible(false);
		}

		internal static bool ZCName(int ZC, out string Name)
		{
			bool result = false;
			char[] array = new char[45];
			FileStream fileStream = new FileStream(AppPath + "\\Resource Files\\ZCNames.dat", FileMode.Open, FileAccess.Read);
			BinaryReader binaryReader = new BinaryReader(fileStream);
			long num = fileStream.Length / 45 - 1;
			long num2 = 0L;
			Name = "";
			do
			{
				long num3 = (num + num2) / 2;
				fileStream.Seek(num3 * 45, SeekOrigin.Begin);
				array = binaryReader.ReadChars(45);
				long num4 = int.Parse(new string(array, 0, 4));
				if (num4 == ZC)
				{
					Name = new string(array, 4, 41);
					Name = $"   R{ZC:####}" + " = " + Name.Trim();
					result = true;
					break;
				}
				if (num <= num2)
				{
					break;
				}
				if (num4 > ZC)
				{
					num = num3 - 1;
				}
				else
				{
					num2 = num3 + 1;
				}
			}
			while (num2 <= num);
			fileStream.Close();
			return result;
		}

		internal static string EventID(int ElementRecord)
		{
			string result = "";
			if ((ElementRecord >= 0) & (ElementRecord < Elements.Count))
			{
				string text = Elements[ElementRecord].StarId.PadRight(7).Substring(0, 7).Trim();
				if (int.TryParse(text, out var _))
				{
					text = ((text.Length >= 5) ? ("SAO" + text) : ("ZC" + text));
				}
				result = text + " " + Utilities.Date_from_JD(Elements[ElementRecord].JDzero + (double)Elements[ElementRecord].T / 24.0, 0);
			}
			return result;
		}

		internal static bool VariableStarInfo(int XZNum, double VarJD, out string VariableDetails)
		{
			string text = "";
			double num = 0.0;
			bool flag = false;
			char[] array = new char[94];
			FileStream fileStream = new FileStream(AppPath + "\\Resource Files\\XZVariables.dat", FileMode.Open, FileAccess.Read);
			BinaryReader binaryReader = new BinaryReader(fileStream);
			long num2 = fileStream.Length / 94 - 1;
			long num3 = 0L;
			VariableDetails = "";
			do
			{
				long num4 = (num2 + num3) / 2;
				fileStream.Seek(num4 * 94, SeekOrigin.Begin);
				array = binaryReader.ReadChars(94);
				int num5 = int.Parse(new string(array, 0, 6));
				if (num5 == XZNum)
				{
					text = new string(array, 7, 87);
					flag = true;
					break;
				}
				if (num2 <= num3)
				{
					break;
				}
				if (num5 > XZNum)
				{
					num2 = num4 - 1;
				}
				else
				{
					num3 = num4 + 1;
				}
			}
			while (num3 <= num2);
			fileStream.Close();
			if (flag)
			{
				StringBuilder stringBuilder = new StringBuilder();
				stringBuilder.Append(text.Substring(0, 29).Trim());
				string text2 = text.Substring(41, 6).Trim();
				if (text2.Length > 0)
				{
					stringBuilder.Append(", " + text2);
				}
				text2 = text.Substring(47, 6);
				if (text2.Trim().Length > 0)
				{
					if (text2.Substring(0, 1) == "(")
					{
						stringBuilder.Append(", range " + text2.Substring(1).Trim());
					}
					else
					{
						stringBuilder.Append(" to " + text2.Trim());
					}
				}
				stringBuilder.Append(", " + text.Substring(53, 3).Trim());
				string text3 = text.Substring(30, 11).Trim();
				if (text3.Length > 0)
				{
					stringBuilder.Append(", Type " + text3);
				}
				text2 = text.Substring(69, 14).Trim();
				string text4 = text.Substring(56, 12).Trim();
				if (text2.Length > 0)
				{
					stringBuilder.Append(", Period " + text2 + " days");
					if (text4.Length > 0)
					{
						num = double.Parse(text2);
						double num6 = double.Parse(text4);
						double num7 = (VarJD - num6) / num % 1.0;
						if (num7 < 0.0)
						{
							num7 += 1.0;
						}
						stringBuilder.AppendFormat(", Phase {0,4:P0}", num7);
					}
				}
				VariableDetails = stringBuilder.ToString();
			}
			return flag;
		}

		internal static void WorldMapPrediction()
		{
			//IL_000d: Unknown result type (might be due to invalid IL or missing references)
			MapSelectedStar = -1;
			((Form)new LunarGrazeSelection(ShowGrazePanel: false, Multi: false)).ShowDialog();
			if (MapSelectedStar >= 0)
			{
				ShowOccultationWorld();
			}
		}

		public static void ShowOccultationWorld()
		{
			try
			{
				((Control)OccultationWorldMap).Show();
			}
			catch
			{
				OccultationWorldMap = new LunarOccultationWorldView();
				((Control)OccultationWorldMap).Show();
			}
			((Control)OccultationWorldMap).Focus();
			OccultationWorldMap.DrawMap();
		}

		public static void PrintWorldGraphic()
		{
			//IL_0006: Unknown result type (might be due to invalid IL or missing references)
			//IL_000b: Unknown result type (might be due to invalid IL or missing references)
			//IL_001e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0025: Unknown result type (might be due to invalid IL or missing references)
			//IL_002b: Invalid comparison between Unknown and I4
			PrintDocument printDocument = new PrintDocument();
			PrintDialog val = new PrintDialog();
			val.set_UseEXDialog(true);
			printDocument.DefaultPageSettings.Landscape = true;
			val.set_Document(printDocument);
			if ((int)((CommonDialog)val).ShowDialog() == 1)
			{
				printDocument.PrintPage += PrintOccultationWorld;
				printDocument.Print();
			}
		}

		public static void PrintPreviewOccultationWorldGraphic()
		{
			//IL_0000: Unknown result type (might be due to invalid IL or missing references)
			//IL_0006: Expected O, but got Unknown
			//IL_000c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			//IL_002b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0032: Unknown result type (might be due to invalid IL or missing references)
			//IL_0038: Invalid comparison between Unknown and I4
			//IL_004d: Unknown result type (might be due to invalid IL or missing references)
			PrintPreviewDialog val = new PrintPreviewDialog();
			PrintDocument printDocument = new PrintDocument();
			PrintDialog val2 = new PrintDialog();
			val2.set_UseEXDialog(true);
			printDocument.DefaultPageSettings.Landscape = true;
			val.set_Document(printDocument);
			val2.set_Document(printDocument);
			if ((int)((CommonDialog)val2).ShowDialog() == 1)
			{
				printDocument.PrintPage += PrintOccultationWorld;
				((Form)val).ShowDialog();
			}
		}

		private static void PrintOccultationWorld(object sender, PrintPageEventArgs e)
		{
			Graphics graphics = e.Graphics;
			int chartHeight = (int)(0.92 * (double)e.PageBounds.Height);
			int chartWidth = (int)(0.92 * (double)e.PageBounds.Width);
			graphics.Clear(Color.White);
			DrawOccultationWorld(graphics, chartWidth, chartHeight, Printer: true, BWFlag: true, MultiPlot: false, 0);
		}

		public static void PlotOccultationWorld(bool BWFlag)
		{
			int width = ((Control)OccultationWorldMap.picWorld).get_Width();
			int height = ((Control)OccultationWorldMap.picWorld).get_Height();
			Bitmap image = new Bitmap(width, height);
			Graphics graphics = Graphics.FromImage(image);
			if (BWFlag)
			{
				graphics.Clear(Color.White);
			}
			else
			{
				graphics.Clear(Color.Black);
			}
			DrawOccultationWorld(graphics, width, height, Printer: false, BWFlag, MultiPlot: false, 0);
			OccultationWorldMap.picWorld.set_Image((Image)image);
			graphics.Dispose();
		}

		internal static void DrawOccultationWorld(Graphics formGraphics, int ChartWidth, int ChartHeight, bool Printer, bool BWFlag, bool MultiPlot, int MultiPlotCount)
		{
			float x = 0f;
			float y = 0f;
			float x2 = 0f;
			float y2 = 0f;
			float x3 = 0f;
			if ((Elements.Count < 1) | (MapSelectedStar >= Elements.Count))
			{
				return;
			}
			new LunarPredictionLine();
			if (Settings.Default.GraphicsSmoothed)
			{
				formGraphics.SmoothingMode = SmoothingMode.AntiAlias;
			}
			float num = (float)(180.0 - Elements[MapSelectedStar].H * (180.0 / Math.PI) - 15.0 * Elements[MapSelectedStar].X / Elements[MapSelectedStar].dX) % 360f;
			double num2 = Elements[MapSelectedStar].JDzero + ((double)Elements[MapSelectedStar].T - Elements[MapSelectedStar].X / Elements[MapSelectedStar].dX - deltaT) / 24.0;
			double num3 = Math.Floor(num2 - 0.5) + 0.5;
			string text = Utilities.DEGtoDMS(24.0 * (num2 - num3), 2, 1, MinutesOnly: true);
			string text2;
			if (MultiPlot)
			{
				text2 = "Occultation  of  " + Elements[MapSelectedStar].StarId.Trim() + ", " + $"{Elements[MapSelectedStar].Mv:F1}" + ", on " + Utilities.Date_from_JD(num3, 0);
				text = "UT = " + text.Substring(0, 2) + "h" + text.Substring(2) + "m";
			}
			else
			{
				text2 = "Occultation  of  " + Elements[MapSelectedStar].StarId.Trim() + ",      Magnitude " + $"{Elements[MapSelectedStar].Mv:F1}" + ",   on  " + Utilities.Date_from_JD(num3, 0);
				text = "UT of conjunction = " + text.Substring(0, 2) + "h" + text.Substring(2) + "m";
			}
			double num4 = ChartWidth / 5;
			Maps.PlotMercatorWorld(formGraphics, ChartWidth, ChartHeight, num, BWFlag, MultiPlot, MultiPlotCount);
			Font font = ((!MultiPlot) ? new Font("Times New Roman", 14f, FontStyle.Regular) : new Font("Times New Roman", 10f, FontStyle.Regular));
			Font font2 = new Font("Times New Roman", 10f, FontStyle.Regular);
			Font font3 = new Font("Times New Roman", 7f, FontStyle.Regular);
			Pen pen4;
			Pen pen3;
			Pen pen2;
			Pen pen;
			Brush brush;
			Brush brush2;
			if (BWFlag || Printer)
			{
				pen4 = (pen3 = (pen2 = (pen = Pens.Black)));
				brush = Brushes.Black;
				brush2 = Brushes.Black;
			}
			else
			{
				pen4 = Pens.White;
				pen3 = new Pen(Brushes.Blue, 1f);
				pen2 = Pens.Red;
				pen = Pens.Cyan;
				brush = Brushes.Yellow;
				brush2 = Brushes.Red;
			}
			Maps.MercatorXY(num + 180f, 90f, ref x2, ref y2);
			float width = formGraphics.MeasureString(text2, font).Width;
			float height = formGraphics.MeasureString(text2, font).Height;
			formGraphics.DrawString(text2, font, brush, x2 - width / 2f, y2 - 1.5f * height);
			Maps.MercatorXY(num + 180f, -90f, ref x2, ref y2);
			width = formGraphics.MeasureString("|", font2).Width;
			height = formGraphics.MeasureString("|", font2).Height;
			formGraphics.DrawString("|", font2, brush, x2 - width / 2f, y2 + height);
			width = formGraphics.MeasureString(text, font2).Width;
			formGraphics.DrawString(text, font2, brush, x2 - width / 2f, y2 + 2f * height);
			Maps.MercatorXY(num + 359.99f, -90f, ref x3, ref y2);
			x3 -= x2;
			formGraphics.DrawString("Occult " + Assembly.GetExecutingAssembly().GetName(copiedName: false).Version, font3, brush2, ChartWidth * (MultiPlotCount % 3) + 2, y2 + 2f * height + 4f);
			Compute_StartEnd_at_RiseSet_Curves();
			float num5 = 0f;
			float y3 = 0f;
			for (int i = 0; i <= 1; i++)
			{
				for (int j = 0; j < StartEndCount[i]; j++)
				{
					Maps.MercatorXY((float)StartEndLongitude[j, i] % 360f, (float)StartEndLatitude[j, i], ref x, ref y);
					if (j > 0 && (double)Math.Abs(x - num5) < num4)
					{
						formGraphics.DrawLine(pen, num5, y3, x, y);
					}
					num5 = x;
					y3 = y;
				}
			}
			Compute_Limit_Lines();
			for (int i = 0; i <= 1; i++)
			{
				for (int k = 0; k < LimitCount[i]; k++)
				{
					Maps.MercatorXY((float)LimitLongitude[k, i] % 360f, (float)LimitLatitude[k, i], ref x, ref y);
					if (k > 0 && (double)Math.Abs(x - num5) < num4)
					{
						if (LimitAltitude[k, i] > 0.0)
						{
							if (k % 2 == 0)
							{
								formGraphics.DrawLine(pen2, x, y, x + 1f, y);
							}
						}
						else if (LimitAltitude[k, i] > -10.0)
						{
							formGraphics.DrawLine(pen3, num5, y3, x, y);
						}
						else
						{
							formGraphics.DrawLine(pen4, num5, y3, x, y);
						}
					}
					num5 = x;
					y3 = y;
				}
			}
		}

		private static void MapOverBorder(Graphics formGraphics, Pen RiseSetPen, float MapCenterX, float MaximumX, float Xp0, float Xp1, float Yp0, float Yp1)
		{
			float num = Xp0;
			float num2 = Xp1;
			if (num < MapCenterX)
			{
				num += 2f * MaximumX;
			}
			else
			{
				num2 += 2f * MaximumX;
			}
		}

		public static void Compute_StartEnd_at_RiseSet_Curves()
		{
			double[,] array = new double[350, 2];
			double[,] array2 = new double[350, 2];
			int[] array3 = new int[2];
			int[] array4 = new int[2];
			double num = 0.0;
			double num2 = 0.0;
			int num3 = 0;
			_ = new float[4];
			_ = new float[4];
			double num4 = (-1.6 - Elements[MapSelectedStar].X) / Elements[MapSelectedStar].dX - 0.1;
			double num5 = (1.6 - Elements[MapSelectedStar].X) / Elements[MapSelectedStar].dX + 0.1;
			num3 = 0;
			int num6 = 0;
			array3[0] = (array3[1] = 0);
			array4[0] = (array4[1] = -1);
			for (int i = 0; i <= 1; i++)
			{
				for (double num7 = num4; num7 <= num5; num7 += 0.02)
				{
					double num8 = Elements[MapSelectedStar].H + 0.2625161707907961 * num7 + Elements[MapSelectedStar].DeltaH * num7;
					double num9 = Elements[MapSelectedStar].X + Elements[MapSelectedStar].dX * num7 + Elements[MapSelectedStar].d2X * num7 * num7;
					double num10 = Elements[MapSelectedStar].Y + Elements[MapSelectedStar].dY * num7 + Elements[MapSelectedStar].d2Y * num7 * num7;
					double num11 = 0.2725076;
					double num12 = Elements[MapSelectedStar].StarDec + Elements[MapSelectedStar].DeltaDec * num7;
					double num13 = Math.Sqrt(num9 * num9 + num10 * num10);
					if (num13 == 0.0)
					{
						num13 = 1E-09;
					}
					double num14 = 0.998;
					for (int j = 0; j <= 1; j++)
					{
						double num15 = (num13 * num13 + num14 * num14 - num11 * num11) / 2.0 / num13 / num14;
						if (Math.Abs(num15) >= 1.0 && j == 1)
						{
							if ((num3 == 2) & (array4[i] == -1))
							{
								array4[i] = array3[i];
							}
							if (num3 == 2)
							{
								num6 = 1;
							}
						}
						else
						{
							if (num3 == 0 && j == 1)
							{
								num3 = 1;
							}
							double num16 = ((!(Math.Abs(num15) >= 1.0)) ? Math.Acos(num15) : 0.0);
							double num17 = Math.Atan2(num9, num10);
							num = ((i != 0) ? (num17 - num16) : (num17 + num16));
							double num18 = Math.Sin(num);
							double num19 = (0.0 - Math.Cos(num)) * Math.Sin(num12) / num14;
							double num20 = Math.Cos(num) * Math.Cos(num12) / num14;
							double num21;
							for (num21 = (Math.Atan2(num18, num19) - num8) * (180.0 / Math.PI); num21 > 360.0; num21 -= 360.0)
							{
							}
							for (; num21 < 0.0; num21 += 360.0)
							{
							}
							num2 = 180.0 / Math.PI * Math.Atan(num20 / Math.Sqrt(num18 * num18 + num19 * num19) / Utilities.sqrt_1LessEarthEllipticitySqrd);
							if (j > 0)
							{
								array[array3[i], i] = num21;
								array2[array3[i], i] = num2;
								array3[i]++;
							}
						}
						num14 = 0.998327 + 0.001676 * Math.Cos(2.0 * num2 / (180.0 / Math.PI));
					}
					if (num3 == 1)
					{
						num3 = 2;
					}
					else if (num6 == 1)
					{
						num6 = 2;
					}
					if (num6 == 2)
					{
						num3 = (num6 = 0);
					}
				}
			}
			int num22 = 0;
			int num23 = 0;
			StartEndCount[0] = 0;
			StartEndCount[1] = 0;
			bool flag;
			int num24;
			int num25;
			if ((array4[0] > 0) & (array4[0] < array3[0]))
			{
				num24 = array4[0] - 1;
				num25 = array4[1] - 1;
				flag = true;
			}
			else
			{
				num24 = array3[0] - 1;
				num25 = array3[1] - 1;
				flag = false;
			}
			for (int k = num22; k <= num24; k++)
			{
				StartEndLongitude[StartEndCount[0], 0] = array[k, 0];
				StartEndLatitude[StartEndCount[0], 0] = array2[k, 0];
				StartEndCount[0]++;
			}
			for (int num26 = num25; num26 >= num23; num26--)
			{
				StartEndLongitude[StartEndCount[0], 0] = array[num26, 1];
				StartEndLatitude[StartEndCount[0], 0] = array2[num26, 1];
				StartEndCount[0]++;
			}
			StartEndLongitude[StartEndCount[0], 0] = array[0, 0];
			StartEndLatitude[StartEndCount[0], 0] = array2[0, 0];
			StartEndCount[0]++;
			if (flag)
			{
				num22 = array4[0];
				num24 = array3[0] - 1;
				num25 = array3[1] - 1;
				num23 = array4[1];
				array3[1] = 0;
				for (int l = num22; l <= num24; l++)
				{
					StartEndLongitude[StartEndCount[1], 1] = array[l, 0];
					StartEndLatitude[StartEndCount[1], 1] = array2[l, 0];
					StartEndCount[1]++;
				}
				for (int num27 = num25; num27 >= num23; num27--)
				{
					StartEndLongitude[StartEndCount[1], 1] = array[num27, 1];
					StartEndLatitude[StartEndCount[1], 1] = array2[num27, 1];
					StartEndCount[1]++;
				}
				StartEndLongitude[StartEndCount[1], 1] = array[num22, 0];
				StartEndLatitude[StartEndCount[1], 1] = array2[num22, 0];
				StartEndCount[1]++;
			}
		}

		public static void Compute_Limit_Lines()
		{
			double[,] array = new double[361, 2];
			double[,] array2 = new double[361, 2];
			double[,] array3 = new double[361, 2];
			double[,] array4 = new double[361, 2];
			double Latitude_Deg = 0.0;
			int num = (int)(180.0 - Elements[MapSelectedStar].H * (180.0 / Math.PI) - 15.0 * Elements[MapSelectedStar].X / Elements[MapSelectedStar].dX) % 360;
			LunarPredictionLine PLine = new LunarPredictionLine();
			LimitCount[0] = (LimitCount[1] = 0);
			int num2 = 0;
			for (int i = 0; i <= 1; i++)
			{
				int northOrSouthLimit = 2 * i - 1;
				int num3 = 0;
				if (!GrazePoint(MapSelectedStar, northOrSouthLimit, 0, DoublePath: false, (double)((180 + num) % 360) / (180.0 / Math.PI), 0.0, FullGrazePrediction: true, StartWithLastLatitude: false, ref PLine, out Latitude_Deg))
				{
					num3 = 180;
				}
				int num4 = 0;
				for (int j = 0; j < 360; j++)
				{
					for (int k = 0; k <= 1; k++)
					{
						bool num5 = GrazePoint(MapSelectedStar, northOrSouthLimit, 0, k == 0, (double)((j + num + num3) % 360) / (180.0 / Math.PI), 0.0, FullGrazePrediction: true, j > 0, ref PLine, out Latitude_Deg);
						array[num4, k] = (j + num + num3) % 360;
						if (num5)
						{
							array2[num4, k] = Latitude_Deg;
							array3[num4, k] = PLine.SunAlt;
							array4[num4, k] = PLine.JD;
							if (k == 0)
							{
								num2 = num4;
							}
						}
						else
						{
							array2[num4, k] = (array4[num4, k] = -99.0);
						}
					}
					num4++;
				}
				int num6 = 1;
				if (num2 > 4 && array4[num2 - 3, 0] < array4[num2 - 4, 0])
				{
					num6 = -1;
				}
				LimitCount[i] = 0;
				for (int num7 = 359; num7 >= 0; num7--)
				{
					if (array2[num7, 1] > -90.0 && ((Math.Abs(array2[num7, 0] - array2[num7, 1]) > 0.1) & ((double)num6 * array4[num7, 1] < (double)num6 * array4[num7, 0])))
					{
						LimitLongitude[LimitCount[i], i] = array[num7, 1];
						LimitLatitude[LimitCount[i], i] = array2[num7, 1];
						LimitAltitude[LimitCount[i], i] = array3[num7, 1];
						LimitCount[i]++;
					}
				}
				for (int l = 0; l <= 359; l++)
				{
					if (array2[l, 0] > -90.0)
					{
						LimitLongitude[LimitCount[i], i] = array[l, 0];
						LimitLatitude[LimitCount[i], i] = array2[l, 0];
						LimitAltitude[LimitCount[i], i] = array3[l, 0];
						LimitCount[i]++;
					}
				}
				for (int num8 = 359; num8 >= 0; num8--)
				{
					if (array2[num8, 1] > -90.0 && ((Math.Abs(array2[num8, 0] - array2[num8, 1]) > 0.1) & ((double)num6 * array4[num8, 1] > (double)num6 * array4[num8, 0])))
					{
						LimitLongitude[LimitCount[i], i] = array[num8, 1];
						LimitLatitude[LimitCount[i], i] = array2[num8, 1];
						LimitAltitude[LimitCount[i], i] = array3[num8, 1];
						LimitCount[i]++;
					}
				}
			}
		}

		internal static void PlotAll_Prepare()
		{
			WorldMapAll_Show();
			PlotAll_IndividualChartWidth = ((Control)WorldMapAll.picPlotAll).get_Width() / 3;
			PlotAll_IndividualChartHeight = 2 * PlotAll_IndividualChartWidth / 3;
			int num = LunarFind.lstEvents.get_Items().get_Count() / 3 + 1;
			int num2 = PlotAll_IndividualChartHeight * num + 20;
			if (num2 < ((Control)WorldMapAll).get_Height() - 50)
			{
				num2 = ((Control)WorldMapAll).get_Height() - 50;
			}
			((Control)WorldMapAll.panelAll).set_Height(((Control)WorldMapAll).get_Height() - 70);
			((Control)WorldMapAll.picPlotAll).set_Height(num2);
		}

		internal static void PlotAll_Plot(int MultiPlotIndex, bool BWFlag)
		{
			float x = 0f;
			float y = 0f;
			int plotAll_IndividualChartWidth = PlotAll_IndividualChartWidth;
			int plotAll_IndividualChartHeight = PlotAll_IndividualChartHeight;
			Bitmap image = ((MultiPlotIndex != 0) ? new Bitmap(WorldMapAll.picPlotAll.get_Image()) : new Bitmap(((Control)WorldMapAll.picPlotAll).get_Width(), ((Control)WorldMapAll.picPlotAll).get_Height()));
			Graphics graphics = Graphics.FromImage(image);
			if (Settings.Default.GraphicsSmoothed)
			{
				graphics.SmoothingMode = SmoothingMode.AntiAlias;
			}
			if (MultiPlotIndex == 0)
			{
				if (BWFlag)
				{
					graphics.Clear(Color.White);
				}
				else
				{
					graphics.Clear(Color.Black);
				}
				if (Elements.Count > 0)
				{
					WorldMapAll.SaveFileLabel = "Occultations of " + Elements[0].StarId;
				}
			}
			if (Elements.Count > 0)
			{
				DrawOccultationWorld(graphics, plotAll_IndividualChartWidth, plotAll_IndividualChartHeight, Printer: false, BWFlag, MultiPlot: true, MultiPlotIndex);
			}
			else
			{
				int num = MultiPlotIndex / 3;
				int num2 = MultiPlotIndex % 3;
				Font font = new Font("Times New Roman", 10f);
				Maps.MercatorXY(0f, 90f, ref x, ref y);
				for (y -= (float)plotAll_IndividualChartHeight; y < (float)(num * plotAll_IndividualChartHeight); y += (float)plotAll_IndividualChartHeight)
				{
				}
				string text = LunarFind.lstEvents.get_Items().get_Item(((ListControl)LunarFind.lstEvents).get_SelectedIndex()).ToString();
				text = "Possible occultation of " + LunarFind.ObjectID.Trim() + ", " + text.Substring(0, text.Length - 2);
				float width = graphics.MeasureString(text, font).Width;
				float height = graphics.MeasureString(text, font).Height;
				graphics.DrawString(text, font, Brushes.Yellow, ((float)num2 + 0.5f) * (float)plotAll_IndividualChartWidth - width / 2f, y - 1.5f * height);
				text = "No Occultation";
				width = graphics.MeasureString(text, font).Width;
				graphics.DrawString(text, font, Brushes.Yellow, ((float)num2 + 0.5f) * (float)plotAll_IndividualChartWidth - width / 2f, (float)((double)num + 0.5) * (float)plotAll_IndividualChartHeight);
			}
			WorldMapAll.picPlotAll.set_Image((Image)image);
			graphics.Dispose();
			((Control)WorldMapAll).Focus();
		}

		internal static void CreateGoogleEarthKMZFile_ForGrazes(string OutFile, string Label, bool AutoOpenFile, bool View, string SiteFile)
		{
			int[] array = new int[5] { 2, 3, 3, 1, 1 };
			int num = 0;
			int num2 = 0;
			double num3 = 0.0;
			double num4 = 0.0;
			if (GrazePredictionLines.Count < 1 || !GoogleEarth.Create_New_GoogleEarthKMZ_File(OutFile, Label, AutoOpenFile, out var CreatedFile))
			{
				return;
			}
			GoogleEarth.Write_Tags_For_New_GoogleEarthKMZ_Path(array[1]);
			num2 = GrazePredictionLines[0].GrazeInnerOuterLimit;
			for (int i = 0; i < GrazePredictionLines.Count; i++)
			{
				num = GrazePredictionLines[i].GrazeInnerOuterLimit;
				if (num != num2)
				{
					GoogleEarth.Write_Tags_At_End_of_Path_GoogleEarthKMZ();
					GoogleEarth.Write_Tags_For_New_GoogleEarthKMZ_Path(array[3]);
					num2 = num;
				}
				if (GrazePredictionLines[i].MoonAlt >= 0.0)
				{
					num3 = GrazePredictionLines[i].GrazeLongitude;
					if (num3 > 180.0)
					{
						num3 -= 360.0;
					}
					if (num3 < -180.0)
					{
						num3 += 360.0;
					}
					num4 = GrazePredictionLines[i].GrazeLatitude;
					GoogleEarth.Write_Path_Coordinate_GoogleEarthKMZ(num3, num4, 0.0);
				}
			}
			GoogleEarth.Write_Tags_At_End_of_Path_GoogleEarthKMZ();
			CreatedFile = GoogleEarth.Close_GoogleEarthKMZ_File(CreatedFile, ConvertToKMZ: true);
			if (View)
			{
				GoogleEarth.DisplayGoogleMap(CreatedFile);
			}
		}

		internal static void CreateGoogleMapHTMFile_ForGrazes(string OutFile, string Label, bool AutoOpenFile)
		{
			double num = 0.0;
			int num2 = 0;
			int num3 = 0;
			int num4 = GrazePredictionLines.Count - 1;
			if (num4 < 0 || !GoogleEarth.Create_New_GoogleMap_File(OutFile, Label, PositiveTimeIncrement: true, IncludeDirectionArrow: false, AutoOpenFile))
			{
				return;
			}
			GoogleEarth.Write_Tags_For_New_GoogleMap_Polyline_Path();
			for (int i = 0; i <= num4; i++)
			{
				num2 = GrazePredictionLines[i].GrazeInnerOuterLimit;
				if (num2 != num3)
				{
					GoogleEarth.Write_Tags_At_End_of_Polyline_Path_GoogleMap(-1);
					GoogleEarth.Write_Tags_For_New_GoogleMap_Polyline_Path();
					num3 = num2;
				}
				if (GrazePredictionLines[i].MoonAlt >= 0.0)
				{
					double grazeLongitude = GrazePredictionLines[i].GrazeLongitude;
					num = GrazePredictionLines[i].GrazeLatitude;
					GoogleEarth.Write_PolyLine_Path_Coordinate_GoogleMap(grazeLongitude, num, -1);
				}
			}
			GoogleEarth.Write_Tags_At_End_of_Polyline_Path_GoogleMap(-1);
			GoogleEarth.Write_Tags_For_New_GoogleMap_PolyPoint_Path();
			for (int j = 0; j <= num4 && !((GrazePredictionLines[0].GrazeInnerOuterLimit == -1) & (GrazePredictionLines[j].GrazeInnerOuterLimit == 1)); j++)
			{
				if (GrazePredictionLines[j].MoonAlt >= 0.0)
				{
					double grazeLongitude2 = GrazePredictionLines[j].GrazeLongitude;
					num = GrazePredictionLines[j].GrazeLatitude;
					GoogleEarth.Write_PolyPoint_Path_Coordinate_GoogleMap(grazeLongitude2, num);
				}
			}
			GoogleEarth.Write_Tags_At_End_of_Polypoint_Path_GoogleMap(0);
			double centreLongitude = 0.0;
			double centreLatitude = 0.0;
			if (num4 > 1)
			{
				centreLongitude = GrazePredictionLines[num4 / 2].GrazeLongitude;
				centreLatitude = GrazePredictionLines[num4 / 2].GrazeLatitude;
			}
			GoogleEarth.Close_GoogleMap_File(centreLongitude, centreLatitude);
		}

		internal static void CreateGoogleEarthKMZFile_ForWorld(string OutFile, string Label, bool AutoOpenFile, bool View, string SiteFile)
		{
			_ = new int[5] { 2, 3, 3, 1, 1 };
			double num = 0.0;
			Compute_StartEnd_at_RiseSet_Curves();
			Compute_Limit_Lines();
			string CreatedFile;
			if (View)
			{
				if (!GoogleEarth.Create_New_GoogleEarthKMZ_File(AppPath + "\\Predictions\\LunarOccultationRegion.KML", Label, AutoOpenFile: true, out CreatedFile))
				{
					return;
				}
			}
			else if (!GoogleEarth.Create_New_GoogleEarthKMZ_File(OutFile, Label, AutoOpenFile, out CreatedFile))
			{
				return;
			}
			for (int i = 0; i <= 1; i++)
			{
				if (StartEndCount[i] <= 0)
				{
					continue;
				}
				GoogleEarth.Write_Tags_For_New_GoogleEarthKMZ_Path(2);
				for (int j = 0; j < StartEndCount[i]; j++)
				{
					num = StartEndLongitude[j, i];
					if (num > 180.0)
					{
						num -= 360.0;
					}
					GoogleEarth.Write_Path_Coordinate_GoogleEarthKMZ(num, StartEndLatitude[j, i], 0.0);
				}
				GoogleEarth.Write_Tags_At_End_of_Path_GoogleEarthKMZ();
			}
			for (int i = 0; i <= 1; i++)
			{
				if (LimitCount[i] <= 0)
				{
					continue;
				}
				GoogleEarth.Write_Tags_For_New_GoogleEarthKMZ_Path(2);
				for (int k = 0; k < LimitCount[i]; k++)
				{
					num = LimitLongitude[k, i];
					if (num > 180.0)
					{
						num -= 360.0;
					}
					GoogleEarth.Write_Path_Coordinate_GoogleEarthKMZ(num, LimitLatitude[k, i], 0.0);
				}
				GoogleEarth.Write_Tags_At_End_of_Path_GoogleEarthKMZ();
			}
			CreatedFile = GoogleEarth.Close_GoogleEarthKMZ_File(CreatedFile, ConvertToKMZ: true);
			if (View)
			{
				GoogleEarth.DisplayGoogleMap(CreatedFile);
			}
		}

		internal static void CreateGoogleMapHTMFile_ForWorld(string OutFile, string Label, bool AutoOpenFile)
		{
			double num = 0.0;
			if (!GoogleEarth.Create_New_GoogleMap_File(OutFile, Label, PositiveTimeIncrement: true, IncludeDirectionArrow: false, AutoOpenFile))
			{
				return;
			}
			for (int i = 0; i <= 1; i++)
			{
				if (StartEndCount[i] <= 0)
				{
					continue;
				}
				GoogleEarth.Write_Tags_For_New_GoogleMap_Polyline_Path();
				for (int j = 0; j < StartEndCount[i]; j++)
				{
					num = StartEndLongitude[j, i];
					if (num > 180.0)
					{
						num -= 360.0;
					}
					GoogleEarth.Write_PolyLine_Path_Coordinate_GoogleMap(num, StartEndLatitude[j, i], -1);
				}
				GoogleEarth.Write_Tags_At_End_of_Polyline_Path_GoogleMap(-1);
			}
			for (int i = 0; i <= 1; i++)
			{
				if (LimitCount[i] <= 0)
				{
					continue;
				}
				GoogleEarth.Write_Tags_For_New_GoogleMap_Polyline_Path();
				for (int k = 0; k < LimitCount[i]; k++)
				{
					num = LimitLongitude[k, i];
					if (num > 180.0)
					{
						num -= 360.0;
					}
					GoogleEarth.Write_PolyLine_Path_Coordinate_GoogleMap(num, LimitLatitude[k, i], -1);
				}
				GoogleEarth.Write_Tags_At_End_of_Polyline_Path_GoogleMap(-1);
			}
			double num2 = 0.0;
			double num3 = 0.0;
			int num4 = 0;
			for (int i = 0; i <= 1; i++)
			{
				if (LimitCount[i] > 0)
				{
					num2 += LimitLongitude[LimitCount[i] / 2, i];
					num3 += LimitLatitude[LimitCount[i] / 2, i];
					num4++;
				}
			}
			if (num4 > 1)
			{
				num2 /= (double)num4;
				num3 /= (double)num4;
			}
			GoogleEarth.Close_GoogleMap_File(num2, num3);
		}

		internal static void CreateCMXFile_ForGrazes(string OutFile)
		{
			double num = 0.0;
			int num2 = GrazePredictionLines.Count - 1;
			if (num2 < 0 || !GoogleEarth.Create_New_CMX_File(OutFile, out var SavedFileName))
			{
				return;
			}
			for (int i = 0; i <= num2; i++)
			{
				if (GrazePredictionLines[i].MoonAlt >= 0.0)
				{
					double grazeLongitude = GrazePredictionLines[i].GrazeLongitude;
					num = GrazePredictionLines[i].GrazeLatitude;
					GoogleEarth.Add_point_to_CMX_Line(grazeLongitude, num);
				}
			}
			GoogleEarth.Write_CMX_Line_Block(-1, LastBlock: true);
			GoogleEarth.Close_CMX_File();
			string text = Path.GetDirectoryName(SavedFileName) + "\\" + Path.GetFileNameWithoutExtension(SavedFileName) + "_cmx.zip";
			if (File.Exists(text))
			{
				File.Delete(text);
			}
			using ZipArchive destination = ZipFile.Open(text, ZipArchiveMode.Create);
			destination.CreateEntryFromFile(SavedFileName, new FileInfo(SavedFileName).Name);
		}

		internal static void CreateGENFile_ForGrazes(string OutFile)
		{
			double num = 0.0;
			int num2 = GrazePredictionLines.Count - 1;
			if (num2 < 0 || !GoogleEarth.Create_New_GEN_File(OutFile))
			{
				return;
			}
			for (int i = 0; i <= num2; i++)
			{
				if (GrazePredictionLines[i].MoonAlt >= 0.0)
				{
					double grazeLongitude = GrazePredictionLines[i].GrazeLongitude;
					num = GrazePredictionLines[i].GrazeLatitude;
					GoogleEarth.Add_point_to_GEN_Line(grazeLongitude, num);
				}
			}
			GoogleEarth.Write_GEN_Line_Block(1);
			GoogleEarth.Close_GEN_File();
		}

		internal static void CreateGPXFile_ForGrazes(string OutFile)
		{
			double num = 0.0;
			int num2 = GrazePredictionLines.Count - 1;
			if (num2 < 0 || !GoogleEarth.Create_New_GPX_File(OutFile))
			{
				return;
			}
			for (int i = 0; i <= num2; i++)
			{
				if (GrazePredictionLines[i].MoonAlt >= 0.0)
				{
					double grazeLongitude = GrazePredictionLines[i].GrazeLongitude;
					num = GrazePredictionLines[i].GrazeLatitude;
					GoogleEarth.Add_point_to_GPX_Line(grazeLongitude, num);
				}
			}
			GoogleEarth.Write_GPX_Line_Block();
			GoogleEarth.Close_GPX_File();
		}

		internal static void CreateMIFFile_ForGrazes(string OutFile)
		{
			double num = 0.0;
			int num2 = GrazePredictionLines.Count - 1;
			if (num2 < 0 || !GoogleEarth.Create_New_MIF_File(OutFile))
			{
				return;
			}
			for (int i = 0; i <= num2; i++)
			{
				if (GrazePredictionLines[i].MoonAlt >= 0.0)
				{
					double grazeLongitude = GrazePredictionLines[i].GrazeLongitude;
					num = GrazePredictionLines[i].GrazeLatitude;
					GoogleEarth.Add_point_to_MIF_Line(grazeLongitude, num);
				}
			}
			GoogleEarth.Write_MIF_Line_Block(-1);
			GoogleEarth.Close_MIF_File();
		}

		internal static void CreatePLTFile_ForGrazes(string OutFile)
		{
			double num = 0.0;
			int num2 = GrazePredictionLines.Count - 1;
			int num3 = 1;
			if (num2 < 0 || !GoogleEarth.Create_New_PLT_File(OutFile))
			{
				return;
			}
			for (int i = 0; i <= num2; i++)
			{
				if (GrazePredictionLines[i].MoonAlt >= 0.0)
				{
					double grazeLongitude = GrazePredictionLines[i].GrazeLongitude;
					num = GrazePredictionLines[i].GrazeLatitude;
					GoogleEarth.Add_point_to_PLT_Line(grazeLongitude, num, num3);
					if (num3 == 1)
					{
						num3 = 0;
					}
				}
			}
			GoogleEarth.Write_PLT_Line_Block();
			GoogleEarth.Close_PLT_File();
		}

		internal static void CreateTXT_DeLorme_File_ForGrazes(string OutFile)
		{
			double num = 0.0;
			int num2 = GrazePredictionLines.Count - 1;
			if (num2 < 0 || !GoogleEarth.Create_New_DeLorme_File(OutFile))
			{
				return;
			}
			for (int i = 0; i <= num2; i++)
			{
				if (GrazePredictionLines[i].MoonAlt >= 0.0)
				{
					double grazeLongitude = GrazePredictionLines[i].GrazeLongitude;
					num = GrazePredictionLines[i].GrazeLatitude;
					GoogleEarth.Add_point_to_DeLorme_Line(grazeLongitude, num);
				}
			}
			GoogleEarth.Write_DeLorme_Line_Block();
			GoogleEarth.Close_DeLorme_File();
		}

		public static void LunarPredictionsShowForm()
		{
			//IL_0028: Unknown result type (might be due to invalid IL or missing references)
			//IL_002e: Invalid comparison between Unknown and I4
			try
			{
				((Control)LunarPrediction).Show();
			}
			catch
			{
				LunarPrediction = new LunarOccultationPrediction();
				((Control)LunarPrediction).Show();
			}
			if ((int)((Form)LunarPrediction).get_WindowState() == 1)
			{
				((Form)LunarPrediction).set_WindowState((FormWindowState)0);
			}
			((Control)LunarPrediction).Focus();
		}

		internal static void SetPredictionDatesExternally(double JD)
		{
			try
			{
				((Control)LunarPrediction).Show();
			}
			catch
			{
				LunarPrediction = new LunarOccultationPrediction();
				((Control)LunarPrediction).Show();
			}
			LunarPrediction.SetDatesExternally(JD);
		}

		public static void XZManageShowForm(bool CreateLunarK2Report)
		{
			//IL_0094: Unknown result type (might be due to invalid IL or missing references)
			//IL_00ac: Unknown result type (might be due to invalid IL or missing references)
			if (CreateLunarK2Report)
			{
				try
				{
					((Control)XZManage).Show();
				}
				catch
				{
					XZManage = new XZ_File_Management();
					((Control)XZManage).Show();
				}
				GroupBox grpEdit = XZManage.grpEdit;
				GroupBox grpReIndex = XZManage.grpReIndex;
				Button cmdCreateKepler2Cats = XZManage.cmdCreateKepler2Cats;
				bool flag;
				((Control)XZManage.cmdKepler2InAsteroidals).set_Enabled(flag = false);
				bool flag2;
				((Control)cmdCreateKepler2Cats).set_Enabled(flag2 = flag);
				bool enabled;
				((Control)grpReIndex).set_Enabled(enabled = flag2);
				((Control)grpEdit).set_Enabled(enabled);
				XZManage.CreateHistoricLunarsInKepler2();
				((Form)XZManage).Close();
				((Component)(object)XZManage).Dispose();
			}
			else
			{
				try
				{
					((Form)XZManage).ShowDialog();
				}
				catch
				{
					XZManage = new XZ_File_Management();
					((Form)XZManage).ShowDialog();
				}
				((Control)XZManage).Focus();
			}
		}

		public static void StarCatalogueShowForm()
		{
			try
			{
				((Control)StarCatDetails).Show();
			}
			catch
			{
				StarCatDetails = new StarCatalogueDetails();
				((Control)StarCatDetails).Show();
			}
			StarCatDetails.InsertToday();
			((Control)StarCatDetails.txtStar).Focus();
		}

		public static void MoonMapShowForm()
		{
			try
			{
				((Control)MoonMap).Show();
			}
			catch
			{
				MoonMap = new MoonMap();
				((Control)MoonMap).Show();
			}
			((Control)MoonMap).Focus();
			MoonMap.DrawMoon();
		}

		public static void MoonStarFieldShow()
		{
			try
			{
				((Control)MoonStarField).Show();
			}
			catch
			{
				MoonStarField = new MoonInStarField();
				((Control)MoonStarField).Show();
			}
		}

		public static void GrazeMapShowForm()
		{
			try
			{
				((Control)GrazeMap).Show();
			}
			catch
			{
				GrazeMap = new LunarOccultation_GrazePathMap();
				((Control)GrazeMap).Show();
			}
			GrazeMap.SetPlotBounds();
			GrazeMap.DrawPath();
			((Control)GrazeMap).Focus();
		}

		public static bool Lunar_FindShowForm()
		{
			//IL_0036: Unknown result type (might be due to invalid IL or missing references)
			//IL_003c: Invalid comparison between Unknown and I4
			try
			{
				((Control)LunarFind).Show();
			}
			catch
			{
				if (!File.Exists(AppPath + "\\Resource Files\\ZC.dat"))
				{
					if ((int)MessageBox.Show("To run 'Find an Occultation', you must have the ZC catalogue\r\n\r\nUse   'Create catalogue subsets'   in the next form to create the ZC catalogue", "No ZC catalogue", (MessageBoxButtons)1, (MessageBoxIcon)48, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152) == 2)
					{
						return false;
					}
					XZManageShowForm(CreateLunarK2Report: false);
					if (!File.Exists(AppPath + "\\Resource Files\\ZC.dat"))
					{
						return false;
					}
				}
				LunarFind = new Lunar_FindAnOccultation();
				((Control)LunarFind).Show();
			}
			return true;
		}

		public static void WorldMapAll_Show()
		{
			try
			{
				((Control)WorldMapAll).Show();
			}
			catch
			{
				WorldMapAll = new WorldMap_All();
				((Control)WorldMapAll).Show();
			}
			((Control)WorldMapAll).Focus();
		}

		public static void LunarAutoGenerate_Show()
		{
			//IL_0005: Unknown result type (might be due to invalid IL or missing references)
			//IL_001d: Unknown result type (might be due to invalid IL or missing references)
			try
			{
				((Form)AutoGenerate).ShowDialog();
			}
			catch
			{
				AutoGenerate = new LunarAutoGenerate();
				((Form)AutoGenerate).ShowDialog();
			}
			((Control)AutoGenerate).Focus();
		}

		public static void MagLimits_Show()
		{
			try
			{
				((Control)MagLimits).Show();
			}
			catch
			{
				MagLimits = new LunarGraze_MagLimits();
				((Control)MagLimits).Show();
			}
			((Control)MagLimits).Focus();
		}

		public static void ShowGrazeWeather()
		{
			try
			{
				((Control)GrazeWeather).Show();
			}
			catch
			{
				GrazeWeather = new GrazeWeather();
				((Control)GrazeWeather).Show();
			}
			((Control)GrazeWeather).Focus();
		}

		internal static void ShowOutputFilter()
		{
			//IL_000f: Unknown result type (might be due to invalid IL or missing references)
			try
			{
				OutputFilter = new LunarOutputFilter();
				((Form)OutputFilter).ShowDialog();
			}
			catch
			{
			}
		}

		public static void Show_RecordingTimer()
		{
			try
			{
				((Control)VDTimer).Show();
			}
			catch
			{
				VDTimer = new frmVDTimer();
				((Control)VDTimer).Show();
			}
		}
	}
}
