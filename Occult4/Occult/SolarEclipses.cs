using System;
using System.Collections;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Drawing.Printing;
using System.IO;
using System.Reflection;
using System.Text;
using System.Windows.Forms;
using Occult.Eclipses;
using Occult.Mapping;
using Occult.Properties;

namespace Occult
{
	public class SolarEclipses
	{
		public static string AppPath;

		private const double Radian = 180.0 / Math.PI;

		private const double TwoPi = Math.PI * 2.0;

		public static Lunar_Eclipses LunarEclipse;

		public static SolarEclipseWorldView WorldMap;

		public static SolarEclipseLocalMap LocalMap;

		public static SolarEclipseLocalPrediction SingleLocation;

		public static SolarEclipseMultiLocations MultiLocation;

		public static SolarEclipsePathCoordinates PathCoordinates;

		public static ClosePlanetaryConjunctions ClosePlanetaryConjunctions;

		public static double[,] StartEndLongitude = new double[1400, 2];

		public static double[,] StartEndLatitude = new double[1400, 2];

		public static double[,] MaxLongitude = new double[1400, 2];

		public static double[,] MaxLatitude = new double[1400, 2];

		public static double[,] LimitLongitude = new double[1400, 5];

		public static double[,] LimitLatitude = new double[1400, 5];

		public static int[] StartEndCount = new int[2];

		public static int[] MaxCount = new int[3];

		public static int[] LimitCount = new int[5];

		public static string SolarEclipseDateLabel;

		public static string SolarEclipseLabel;

		public static double hour;

		public static double deltaThours;

		public static double UT1_UTC;

		public static double tF1;

		public static double tF2;

		public static double JD_atMidTime;

		public static double[] X = new double[3];

		public static double[] Y = new double[3];

		public static double[] Z = new double[3];

		public static double[] L1 = new double[3];

		public static double[] L2 = new double[3];

		public static double[] Mu = new double[3];

		public static double[] D = new double[3];

		private static double Gamma = 0.0;

		internal static bool IsNonEclipse = false;

		public const double MoonRadius = 0.2725076;

		public const double MoonRadiusReducedForTotality = 0.272281;

		internal static string deltaT_string = "";

		internal static string deltaTUncertainty_string = "";

		internal static bool UseUTC = false;

		internal static int deltaTUncertainty;

		internal static string[] ElongationsLocal = new string[9];

		internal static string ElongationsWorldMap = "";

		public static double MidLongitude_deg;

		public static double MidLatitude_deg;

		public static double Range_deg;

		private static double TestDeltaX = 300.0;

		private static int EphemerisSource = 0;

		internal static double[] ContactTimes = new double[9];

		internal static double[] diffA = new double[6];

		internal static double[] diffB = new double[6];

		internal static double[] diffC = new double[6];

		internal static double[] Altit = new double[6];

		internal static double[] PA = new double[6];

		internal static string[] DayFlag = new string[9] { " ", " ", " ", " ", " ", " ", " ", " ", " " };

		private static bool[] ValidLocalTime = new bool[9];

		public static ArrayList Coordinates;

		internal static List<EclipseContactTimes> MultiEvent;

		internal static double LocalLongitude;

		internal static double LocalLatitude;

		internal static double LocalAltitude;

		internal static string LocalName;

		internal static int CurrentYear = 0;

		internal static int CurrentMonth = 0;

		internal static double CurrentDay = 0.0;

		internal static double[] MinLimbHeights = new double[360];

		internal static double[] MinLimbHeightsX10 = new double[3600];

		internal static double[] MaxLimbHeights = new double[360];

		internal static double[] MaxLimbHeightsX10 = new double[3600];

		internal static double CurrentSiteLong = -400.0;

		internal static double CurrentSiteLat = -100.0;

		internal static double MaxRad_R = 1.0;

		internal static double MaxRad_x = 0.0;

		internal static double MaxRad_y = 0.0;

		internal static double MinRad_R = 1.0;

		internal static double MinRad_x = 0.0;

		internal static double MinRad_y = 0.0;

		internal static void CloseAllSolarEclipses()
		{
			try
			{
				((Form)WorldMap).Close();
				((Component)(object)WorldMap).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)LocalMap).Close();
				((Component)(object)LocalMap).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)SingleLocation).Close();
				((Component)(object)SingleLocation).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)MultiLocation).Close();
				((Component)(object)MultiLocation).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)PathCoordinates).Close();
				((Component)(object)PathCoordinates).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)ClosePlanetaryConjunctions).Close();
				((Component)(object)ClosePlanetaryConjunctions).Dispose();
			}
			catch
			{
			}
		}

		public static void SolarEclipseBessellianElements(double JD_TT)
		{
			SolarEclipseBessellianElements(JD_TT, IncludeMaps: true);
		}

		public static void SolarEclipseBessellianElements(double JD_TT, bool IncludeMaps)
		{
			double RAMoon = 0.0;
			double DecMoon = 0.0;
			double ParMoon = 0.0;
			double RA = 0.0;
			double Dec = 0.0;
			double GeocentricDist = 0.0;
			double RAMoon2 = 0.0;
			double DecMoon2 = 0.0;
			double ParMoon2 = 0.0;
			double RA2 = 0.0;
			double Dec2 = 0.0;
			double GeocentricDist2 = 0.0;
			double[] RA3 = new double[6];
			double[] Dec3 = new double[6];
			double[] Parallax = new double[6];
			double[] array = new double[3];
			double[] array2 = new double[3];
			double[] array3 = new double[3];
			double[] array4 = new double[3];
			double[] array5 = new double[3];
			double[] array6 = new double[3];
			double[] array7 = new double[3];
			double[] array8 = new double[3];
			double[] array9 = new double[3];
			double[] array10 = new double[3];
			double[] array11 = new double[3];
			double[] array12 = new double[3];
			double[] array13 = new double[3];
			float LongCorrection_arcsec1e = 0f;
			float LatCorrection_arcsec1e = 0f;
			bool flag = false;
			for (int i = 0; i <= 1; i++)
			{
				Utilities.MoonSeries_Retrieve(JD_TT, out RA3, out Dec3, out Parallax, out LongCorrection_arcsec1e, out LatCorrection_arcsec1e, out EphemerisSource);
				Utilities.MoonPositionFromSeries(0.0, RA3, Dec3, Parallax, out RAMoon, out DecMoon, out ParMoon);
				Utilities.PlanetGeocentric(JD_TT, 3, 0.0, 0, out RA, out Dec, out GeocentricDist);
				Utilities.MoonPositionFromSeries(1.0, RA3, Dec3, Parallax, out RAMoon2, out DecMoon2, out ParMoon2);
				Utilities.PlanetGeocentric(JD_TT + 1.0, 3, 0.0, 0, out RA2, out Dec2, out GeocentricDist2);
				double num = RAMoon - RA * (180.0 / Math.PI);
				if (num > 180.0)
				{
					num -= 360.0;
				}
				if (num < -180.0)
				{
					num += 360.0;
				}
				double num2 = RAMoon2 - RA2 * (180.0 / Math.PI);
				if (num2 > 180.0)
				{
					num2 -= 360.0;
				}
				if (num2 < -180.0)
				{
					num2 += 360.0;
				}
				hour = (int)(num / (num - num2) * 24.0);
				if (hour < 0.0)
				{
					JD_TT -= 1.0;
					continue;
				}
				if (!(hour >= 24.0))
				{
					break;
				}
				JD_TT += 1.0;
			}
			Utilities.Date_from_JD(JD_TT, out CurrentYear, out CurrentMonth, out CurrentDay);
			double num3 = Utilities.delta_T(CurrentYear, CurrentMonth, CurrentDay, out UT1_UTC);
			deltaThours = num3 / 3600.0;
			Utilities.deltaT_Uncertainty(CurrentYear, out deltaTUncertainty, out deltaTUncertainty_string);
			if (Math.Abs(deltaTUncertainty) < 3600)
			{
				UseUTC = true;
			}
			else
			{
				UseUTC = false;
			}
			if (Math.Abs(num3) > 1800.0)
			{
				deltaT_string = string.Format("delta T = {0,5:F3} hrs", num3 / 3600.0);
			}
			else if (Math.Abs(num3) > 100.0)
			{
				deltaT_string = string.Format("delta T = {0,5:F0} secs", num3);
			}
			else if ((CurrentYear > 1971) & ((CurrentYear < DateTime.Now.AddYears(1).Year) | ((CurrentYear < DateTime.Now.AddYears(10).Year) & (UT1_UTC != 0.0))))
			{
				deltaT_string = string.Format("TT - UTC = {0,5:F2} secs, UT1-UTC = {1,1:f2}", num3, UT1_UTC);
			}
			else
			{
				deltaT_string = string.Format("delta T = {0,5:F1} secs", num3);
			}
			for (int j = 0; j <= 2; j++)
			{
				Utilities.MoonPositionFromSeries((hour + (double)j - 1.0) / 24.0, RA3, Dec3, Parallax, out array4[j], out array5[j], out array6[j]);
				Utilities.PlanetGeocentric(JD_TT + (hour + (double)j - 1.0) / 24.0, 3, 0.0, 0, out array[j], out array2[j], out array3[j]);
				array4[j] /= 180.0 / Math.PI;
				array5[j] /= 180.0 / Math.PI;
				array6[j] /= 180.0 / Math.PI;
				double num4 = Math.Sin(8.794143836182533 / array3[j] / 3600.0 / (180.0 / Math.PI)) / Math.Sin(array6[j]);
				double num5 = Math.Cos(array2[j]) * Math.Cos(array[j]) - num4 * Math.Cos(array5[j]) * Math.Cos(array4[j]);
				double num6 = Math.Cos(array2[j]) * Math.Sin(array[j]) - num4 * Math.Cos(array5[j]) * Math.Sin(array4[j]);
				double num7 = Math.Sin(array2[j]) - num4 * Math.Sin(array5[j]);
				double num8 = Math.Atan2(num6, num5);
				array10[j] = Math.Atan(num7 / Math.Sqrt(num5 * num5 + num6 * num6));
				double num9 = Math.Sqrt(num5 * num5 + num6 * num6 + num7 * num7);
				array7[j] = Math.Cos(array5[j]) * Math.Sin(array4[j] - num8) / Math.Sin(array6[j]);
				array8[j] = (Math.Sin(array5[j]) * Math.Cos(array10[j]) - Math.Cos(array5[j]) * Math.Sin(array10[j]) * Math.Cos(array4[j] - num8)) / Math.Sin(array6[j]);
				array9[j] = (Math.Sin(array5[j]) * Math.Sin(array10[j]) + Math.Cos(array5[j]) * Math.Cos(array10[j]) * Math.Cos(array4[j] - num8)) / Math.Sin(array6[j]);
				array11[j] = (Utilities.SiderealTime_deg(JD_TT, Apparent: true) + 15.041067 * (hour + (double)j - 1.0)) / (180.0 / Math.PI) - num8;
				array11[j] += 15.041067 * (0.0 - deltaThours + UT1_UTC / 3600.0) / (180.0 / Math.PI);
				while (array11[j] < 0.0)
				{
					array11[j] += Math.PI * 2.0;
				}
				while (array11[j] > Math.PI * 2.0)
				{
					array11[j] -= Math.PI * 2.0;
				}
				if (j > 0 && array11[j] < array11[0])
				{
					array11[j] += Math.PI * 2.0;
				}
				double num10 = 0.004664026 / num9 / array3[0];
				double num11 = 0.004640776 / num9 / array3[0];
				tF1 = num10 / Math.Sqrt(1.0 - num10 * num10);
				tF2 = num11 / Math.Sqrt(1.0 - num11 * num11);
				array12[j] = (array9[j] + 0.2725076 / num10) * tF1;
				if (j == 0)
				{
					flag = 0.272281 / num11 > array9[j];
				}
				if (flag)
				{
					array13[j] = (array9[j] - 0.272281 / num11) * tF2;
				}
				else
				{
					array13[j] = (array9[j] - 0.2725076 / num11) * tF2;
				}
			}
			X[0] = array7[0];
			X[2] = (array7[2] - 2.0 * array7[1] + array7[0]) / 2.0;
			X[1] = array7[1] - array7[0] - X[2];
			Y[0] = array8[0];
			Y[2] = (array8[2] - 2.0 * array8[1] + array8[0]) / 2.0;
			Y[1] = array8[1] - array8[0] - Y[2];
			Z[0] = array9[0];
			Z[2] = (array9[2] - 2.0 * array9[1] + array9[0]) / 2.0;
			Z[1] = array9[1] - array9[0] - Z[2];
			L1[0] = array12[0];
			L1[2] = (array12[2] - 2.0 * array12[1] + array12[0]) / 2.0;
			L1[1] = array12[1] - array12[0] - L1[2];
			L2[0] = array13[0];
			L2[2] = (array13[2] - 2.0 * array13[1] + array13[0]) / 2.0;
			L2[1] = array13[1] - array13[0] - L2[2];
			Mu[0] = array11[0];
			Mu[2] = (array11[2] - 2.0 * array11[1] + array11[0]) / 2.0;
			Mu[1] = array11[1] - array11[0] - Mu[2];
			D[0] = array10[0];
			D[2] = (array10[2] - 2.0 * array10[1] + array10[0]) / 2.0;
			D[1] = array10[1] - array10[0] - D[2];
			if (IncludeMaps)
			{
				Compute_StartEnd_at_RiseSet_Curves();
				Compute_Max_at_RiseSet_Curves();
				Compute_Limit_Lines();
			}
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.AppendFormat("{0,1:F0} ", CurrentYear);
			stringBuilder.Append(Utilities.ShortMonths[CurrentMonth]);
			stringBuilder.AppendFormat(" {0,2:F0} ", CurrentDay);
			if (Math.Abs(num3) > 300.0)
			{
				stringBuilder.Append(" (TT)");
			}
			SolarEclipseDateLabel = stringBuilder.ToString();
			double num12 = X[1] * X[1] + Y[1] * Y[1];
			double num13 = (0.0 - (X[0] * X[1] + Y[0] * Y[1])) / num12;
			double num14 = X[0] + X[1] * num13;
			double num15 = Y[0] + Y[1] * num13;
			double num16 = 1.0 - num14 * num14 - num15 * num15;
			double num17 = ((!(num16 < 0.0)) ? Math.Sqrt(num16) : 0.0);
			double num18 = L2[0] + L2[1] * num13;
			double num19 = num18 - num17 * tF2;
			Gamma = Math.Sqrt(num14 * num14 + num15 * num15) * (double)Math.Sign(num15);
			double num20 = L1[0] + L1[1] * num13;
			stringBuilder = new StringBuilder();
			if (Math.Abs(Gamma) > 0.997 + num20)
			{
				stringBuilder.Append("Non-eclipse  of  ");
				IsNonEclipse = true;
			}
			else
			{
				IsNonEclipse = false;
				if (Math.Abs(Gamma) > 0.997)
				{
					stringBuilder.Append("Partial  ");
				}
				if (num18 < 0.0)
				{
					stringBuilder.Append("Total");
				}
				else if (num19 > 0.0)
				{
					stringBuilder.Append("Annular");
				}
				else
				{
					stringBuilder.Append("Annular-Total");
				}
				stringBuilder.Append("  Eclipse  of  ");
			}
			stringBuilder.Append(SolarEclipseDateLabel);
			SolarEclipseLabel = stringBuilder.ToString().Trim();
			JD_atMidTime = JD_TT + (hour - 1.0 + num13) / 24.0;
			Utilities.Planet_SolarElongations(JD_atMidTime, out ElongationsLocal);
			ElongationsWorldMap = "Elongations       Mercury " + ElongationsLocal[1] + "    Venus " + ElongationsLocal[2] + "    Mars " + ElongationsLocal[4] + "    Jupiter " + ElongationsLocal[5] + "    Saturn " + ElongationsLocal[6];
			if (IncludeMaps)
			{
				PlotWorld(Settings.Default.BWFlag);
			}
			CurrentSiteLong = -400.0;
			CurrentSiteLat = -100.0;
		}

		internal static void ListBesselianElements()
		{
			//IL_0265: Unknown result type (might be due to invalid IL or missing references)
			MoonSeriesViewer moonSeriesViewer = new MoonSeriesViewer();
			moonSeriesViewer.ShowElements(new string[21]
			{
				"Besselian elements for the solar eclipse of",
				SolarEclipseDateLabel,
				"  ",
				string.Format("Zero time for series is {0,1:F0}h TT", hour - 1.0),
				deltaT_string,
				"  ",
				string.Format("    X {0,10:f6}        Y {1,10:f6}", X[0], Y[0]),
				string.Format("   dX {0,10:f6}       dY {1,10:f6}", X[1], Y[1]),
				string.Format("  d2X {0,10:f6}      d2Y {1,10:f6}", X[2], Y[2]),
				"  ",
				string.Format("   L1 {0,10:f6}       L2 {1,10:f6}", L1[0], L2[0]),
				string.Format("  dL1 {0,10:f6}      dL2 {1,10:f6}", L1[1], L2[1]),
				string.Format(" d2L1 {0,10:f6}     d2L2 {1,10:f6}", L1[2], L2[2]),
				"  ",
				string.Format("    D {0,10:f6}       Mu {1,10:f6}", D[0] * (180.0 / Math.PI), Mu[0] * (180.0 / Math.PI)),
				string.Format("   dD {0,10:f6}      dMu {1,10:f6}", D[1] * (180.0 / Math.PI), Mu[1] * (180.0 / Math.PI)),
				string.Format("  d2D {0,10:f6}     d2Mu {1,10:f6}", Mu[2] * (180.0 / Math.PI), Mu[2] * (180.0 / Math.PI)),
				"  ",
				string.Format("tan(f1) {0,8:f6}", tF1),
				string.Format("tan(f2) {0,8:f6}", tF2),
				string.Format("  Gamma {0,8:f6}", Gamma)
			});
			((Control)moonSeriesViewer).set_Text("Solar eclipse - Besselian elements viewer");
			moonSeriesViewer.HelpKeyword = "Solar eclipse elements";
			((Form)moonSeriesViewer).ShowDialog();
		}

		private static void PathLimits(int LimitID, bool FromPole, double Longitude_deg, out bool Valid, out double Latitude_deg, out double z, out double Tcentral, out double Duration, out double Magnitude)
		{
			double num = 0.0;
			double num2 = 0.0;
			double num3 = 0.0005;
			Latitude_deg = 0.0;
			if (FromPole)
			{
				Latitude_deg = 88 * Math.Sign(Y[0]);
			}
			int num4 = 0;
			double num5 = (Tcentral = (Duration = (Magnitude = (z = 0.0))));
			Valid = true;
			double num10;
			double num24;
			double num28;
			do
			{
				int num6 = 0;
				if (Math.Abs(Latitude_deg) == 90.0)
				{
					Latitude_deg -= 0.001 * (double)Math.Sign(Latitude_deg);
				}
				if (Math.Abs(Latitude_deg) > 90.0)
				{
					Latitude_deg = (double)(180 * Math.Sign(Latitude_deg)) - Latitude_deg;
				}
				double num7 = Math.Atan(Utilities.sqrt_1LessEarthEllipticitySqrd * Math.Tan(Latitude_deg / (180.0 / Math.PI)));
				double num8 = Math.Cos(num7);
				double num9 = Utilities.sqrt_1LessEarthEllipticitySqrd * Math.Sin(num7);
				double num19;
				double num20;
				double num21;
				double num22;
				do
				{
					num10 = num5;
					double num11 = X[0] + X[1] * num10 + X[2] * num10 * num10;
					double num12 = Y[0] + Y[1] * num10 + Y[2] * num10 * num10;
					double num13 = D[0] + D[1] * num10 + D[2] * num10 * num10;
					double num14 = Math.Sin(num13);
					double num15 = Math.Cos(num13);
					double num16 = Mu[0] + Mu[1] * num10 + Longitude_deg / (180.0 / Math.PI);
					double num17 = num8 * Math.Sin(num16);
					double num18 = num8 * Math.Cos(num16);
					num19 = num11 - num17;
					num20 = X[1] + 2.0 * X[2] * num10 - Mu[1] * num18;
					num21 = num12 - num9 * num15 + num18 * num14;
					z = num9 * num14 + num18 * num15;
					num22 = Y[1] + 2.0 * Y[2] * num10 - Mu[1] * num17 * num14 - z * D[1];
					double num23 = num20 * num20 + num22 * num22;
					num24 = Math.Sqrt(num23);
					double num25 = (0.0 - (num19 * num20 + num21 * num22)) / num23;
					if (Math.Abs(num25) > 0.1)
					{
						num25 = 0.1 * (double)Math.Sign(num25);
					}
					num5 = num10 + num25;
					num6++;
				}
				while (num6 <= 20 && Math.Abs(num5 - num10) > 0.0001);
				double value = ((LimitID == 1) ? 0.0 : ((!(LimitID == 2 || LimitID == 3)) ? (L1[0] + L1[1] * num10 + L1[2] * num10 * num10 - z * tF1) : (L2[0] + L2[1] * num10 + L2[2] * num10 * num10 - z * tF2)));
				double num26 = Math.Abs(value);
				if (LimitID == 3 || LimitID == 5)
				{
					num26 = 0.0 - num26;
				}
				double num27 = num26 - (num19 * num22 - num21 * num20) / num24;
				if (num4 == 0)
				{
					num4 = 1;
					num = num27;
					num2 = Latitude_deg;
					Latitude_deg += 1.0;
					num28 = 1.0;
					continue;
				}
				num4++;
				if (num27 == num || num4 > 12)
				{
					Latitude_deg = 100.0;
					break;
				}
				num28 = (Latitude_deg - num2) / (num - num27) * num27;
				if (Math.Abs(num28) > 20.0)
				{
					num28 = 20 * Math.Sign(num28);
				}
				num2 = Latitude_deg;
				num = num27;
				Latitude_deg += num28;
			}
			while (Math.Abs(num28) > num3);
			Valid = true;
			if (z < 0.0)
			{
				Valid = false;
			}
			if (Math.Abs(Latitude_deg) > 90.0)
			{
				Valid = false;
			}
			Tcentral = hour + num5 - 1.0 - deltaThours;
			if (Valid)
			{
				double value = L2[0] + L2[1] * num10 + L2[2] * num10 * num10 - z * tF2;
				Duration = value / num24 * 7200.0;
				value = L1[0] + L1[1] * num10 + L1[2] * num10 * num10 - z * tF1;
				Magnitude = 0.5464 / (2.0 * value - 0.5464);
			}
		}

		public static void CalculateLocalCircumstances(double Longitude, double pCos, double pSin, out double Magnitude, out double Magnitude_DiameterRatio, out double Delta, out double LL2, out double n, out double SemiDurn, out string Rise_Magnitude, out string Set_Magnitude)
		{
			Rise_Magnitude = (Set_Magnitude = "");
			Magnitude = (Magnitude_DiameterRatio = 0.0);
			Delta = (LL2 = (n = (SemiDurn = 1.0)));
			for (int i = 0; i < 5; i++)
			{
				ValidLocalTime[i] = true;
			}
			for (int j = -1; j <= 1; j += 2)
			{
				double num = 3 * j;
				int num2 = 0;
				double num10;
				double num20;
				double num21;
				do
				{
					double num3 = num;
					double num4 = X[0] + X[1] * num3 + X[2] * num3 * num3;
					double num5 = Y[0] + Y[1] * num3 + Y[2] * num3 * num3;
					double num6 = D[0] + D[1] * num3 + D[2] * num3 * num3;
					double num7 = Math.Sin(num6);
					double num8 = Math.Cos(num6);
					double num9 = Mu[0] + Mu[1] * num3;
					LL2 = L2[0] + L2[1] * num3 + L2[2] * num3 * num3;
					num10 = L1[0] + L1[1] * num3 + L1[2] * num3 * num3;
					double num11 = num9 + Longitude;
					double num12 = 1.000242;
					double num13 = pCos * Math.Sin(num11) * num12;
					double num14 = (pSin * num8 - pCos * num7 * Math.Cos(num11)) * num12;
					double num15 = (pSin * num7 + pCos * num8 * Math.Cos(num11)) * num12;
					double num16 = num4 - num13;
					double num17 = num5 - num14;
					double num18 = X[1] + 2.0 * X[2] * num3 - Mu[1] * pCos * Math.Cos(num11);
					double num19 = Y[1] + 2.0 * Y[2] * num3 - Mu[1] * num13 * num7 - num15 * D[1];
					double d = num18 * num18 + num19 * num19;
					n = Math.Sqrt(d);
					Delta = (num16 * num19 - num17 * num18) / n / num10;
					num20 = (0.0 - Mu[1]) * num13 * num8 + num14 * D[1];
					num21 = Math.Sqrt(num16 * num16 + num17 * num17);
					double num22 = (0.0 - (num15 + 0.01)) / num20;
					if (Math.Abs(num22) > 1.0)
					{
						num22 = Math.Sign(num22);
					}
					num += num22;
					if (Math.Abs(num22) < 0.001)
					{
						break;
					}
					num2++;
				}
				while (num2 <= 20 && num2 > -1);
				if (num21 < num10 && num2 < 21)
				{
					double num23 = (num10 - num21) / (num10 + LL2);
					ContactTimes[6] = hour + num - 1.0;
					if (UseUTC)
					{
						ContactTimes[6] -= deltaThours;
					}
					string text = " ";
					if (ContactTimes[6] < 0.0)
					{
						ContactTimes[6] += 24.0;
						text = "- ";
					}
					if (ContactTimes[6] > 24.0)
					{
						ContactTimes[6] -= 24.0;
						text = "+ ";
					}
					if (num20 > 0.0)
					{
						Rise_Magnitude = "At sunrise       " + Utilities.DEGtoDMS(ContactTimes[6], 2, 0, MinutesOnly: false) + text + "     Magnitude " + string.Format("{0,5:F3}", num23);
					}
					else
					{
						Set_Magnitude = "At sunset        " + Utilities.DEGtoDMS(ContactTimes[6], 2, 0, MinutesOnly: false) + text + "     Magnitude " + string.Format("{0,5:F3}", num23);
					}
				}
			}
			for (int j = -1; j <= 1; j++)
			{
				double num = 0.0;
				int num2 = 0;
				double num3;
				double num5;
				double num7;
				double num8;
				double num11;
				double num27;
				double num13;
				double num16;
				double num17;
				double num15;
				double num18;
				double num19;
				double num28;
				double num29;
				double d;
				double num30;
				do
				{
					num3 = num;
					double num4 = X[0] + X[1] * num3 + X[2] * num3 * num3;
					num5 = Y[0] + Y[1] * num3 + Y[2] * num3 * num3;
					double num24 = D[0] + D[1] * num3 + D[2] * num3 * num3;
					num7 = Math.Sin(num24);
					num8 = Math.Cos(num24);
					double num25 = Mu[0] + Mu[1] * num3;
					LL2 = L2[0] + L2[1] * num3 + L2[2] * num3 * num3;
					double num10 = L1[0] + L1[1] * num3 + L1[2] * num3 * num3;
					num11 = num25 + Longitude;
					num15 = pSin * num7 + pCos * num8 * Math.Cos(num11);
					double num26 = Math.Asin(num15);
					num27 = Utilities.Refraction_deg(num26 * (180.0 / Math.PI), 1016.0, 15.0) / (180.0 / Math.PI);
					double num12 = ((!(num15 < 0.7)) ? 1.0 : (1.000278 * (Math.Cos(num27) - Math.Sin(num27) * Math.Tan(num26))));
					num13 = pCos * Math.Sin(num11);
					num16 = num4 - num13 * num12;
					num17 = num5 - (pSin * num8 - pCos * num7 * Math.Cos(num11)) * num12;
					num15 = (pSin * num7 + pCos * num8 * Math.Cos(num11)) * num12;
					num18 = X[1] + 2.0 * X[2] * num3 - Mu[1] * pCos * Math.Cos(num11);
					num19 = Y[1] + 2.0 * Y[2] * num3 - Mu[1] * num13 * num7 - num15 * D[1];
					num28 = num10 - num15 * tF1;
					num29 = LL2 - num15 * tF2;
					d = num18 * num18 + num19 * num19;
					n = Math.Sqrt(d);
					num30 = num16 * num18 + num17 * num19;
					Delta = (num16 * num19 - num17 * num18) / n / num28;
					if (!(Math.Abs(Delta) > 1.0))
					{
						num = ((j != 0) ? (num3 + (double)j * Math.Sqrt(1.0 - Delta * Delta) * num28 / n - num30 / d) : (num3 - num30 / d));
						continue;
					}
					num2++;
					num = num3 - num30 / d;
					if (num2 > 7)
					{
						for (int k = 0; k < 5; k++)
						{
							ValidLocalTime[k] = false;
						}
						break;
					}
				}
				while (Math.Abs(num - num3) > 0.0001);
				double num31;
				double num32;
				double num33;
				double num34;
				double num35;
				double num36;
				double num37;
				double num38;
				double num39;
				double num43;
				if (j == 0)
				{
					if (Math.Abs(Delta) > 1.0)
					{
						for (int l = 0; l < 5; l++)
						{
							ValidLocalTime[l] = false;
						}
						break;
					}
					if (num15 < 0.0 - Math.Sin(num27))
					{
						for (int m = 1; m < 4; m++)
						{
							ValidLocalTime[m] = false;
						}
					}
					else
					{
						Delta = (num16 * num19 - num17 * num18) / n;
						num31 = (0.0 - pCos) * Math.Cos(num11);
						num32 = (0.0 - pSin) * Math.Sin(num11);
						num33 = num13;
						num34 = (0.0 - num13) * num7;
						num35 = pCos * num8 + pSin * num7 * Math.Cos(num11);
						num36 = num5 - num17;
						num37 = -1.047 * (num18 * num31 + num19 * num34) / d;
						num38 = 1.047 * (num18 * num32 + num19 * num35) / d;
						num39 = 0.000564 * (num18 * num33 + num19 * num36) / d;
						double num40 = -290.9 * (num18 * num34 - num19 * num31) / n;
						double num41 = 290.9 * (num18 * num35 - num19 * num32) / n;
						double num42 = 0.1567 * (num18 * num36 - num19 * num33) / n;
						Magnitude = (num28 - Math.Abs(Delta)) / (num28 + num29);
						if (Math.Abs(Delta) < Math.Abs(num29))
						{
							Magnitude_DiameterRatio = (num28 - num29) / (num28 + num29);
						}
						else
						{
							Magnitude_DiameterRatio = 0.0;
						}
						ContactTimes[2] = hour + num - 1.0;
						if (UseUTC)
						{
							ContactTimes[2] -= deltaThours;
						}
						diffA[2] = num37 / 60.0;
						diffB[2] = num38 / 60.0;
						diffC[2] = num39;
						Altit[2] = Math.Asin(num15);
						ContactTimes[5] = Delta;
						diffA[5] = num40;
						diffB[5] = num41;
						diffC[5] = num42;
						Altit[5] = num29;
						PA[5] = n;
					}
					if (Math.Abs(Delta) > Math.Abs(num29))
					{
						ValidLocalTime[1] = (ValidLocalTime[3] = false);
						continue;
					}
					Delta /= num29;
					SemiDurn = Math.Sqrt(1.0 - Delta * Delta) * num29 / n;
					ContactTimes[1] = hour + num - 1.0 - (double)Math.Sign(num29) * SemiDurn;
					if (UseUTC)
					{
						ContactTimes[1] -= deltaThours;
					}
					ContactTimes[3] = hour + num - 1.0 + (double)Math.Sign(num29) * SemiDurn;
					if (UseUTC)
					{
						ContactTimes[3] -= deltaThours;
					}
					num43 = Math.Atan(Delta / Math.Sqrt(1.0 - Delta * Delta));
					double num44 = Math.Atan2(num18, num19);
					if (num29 < 0.0)
					{
						PA[1] = (num44 + num43) * (180.0 / Math.PI);
						PA[3] = (num44 + Math.PI - num43) * (180.0 / Math.PI);
					}
					else
					{
						PA[3] = (num44 + num43) * (180.0 / Math.PI);
						PA[1] = (num44 + Math.PI - num43) * (180.0 / Math.PI);
					}
					PA[1] = Utilities.NormaliseDegrees(PA[1]);
					PA[3] = Utilities.NormaliseDegrees(PA[3]);
					continue;
				}
				if (num15 < 0.0 - Math.Sin(num27))
				{
					if (j == -1)
					{
						ValidLocalTime[0] = false;
					}
					if (j == 1)
					{
						ValidLocalTime[4] = false;
					}
					continue;
				}
				num31 = (0.0 - pCos) * Math.Cos(num11);
				num32 = (0.0 - pSin) * Math.Sin(num11);
				num33 = num13;
				num34 = (0.0 - num13) * num7;
				num35 = pCos * num8 + pSin * num7 * Math.Cos(num11);
				num36 = num5 - num17;
				num37 = -1.047 * (num16 * num31 + num17 * num34) / num30;
				num38 = 1.047 * (num16 * num32 + num17 * num35) / num30;
				num39 = 0.000564 * (num16 * num33 + num17 * num36) / num30;
				num43 = Math.Atan2(num16, num17) * (180.0 / Math.PI);
				if (num43 < 0.0)
				{
					num43 += 360.0;
				}
				if (j == -1)
				{
					ContactTimes[0] = hour + num - 1.0;
					if (UseUTC)
					{
						ContactTimes[0] -= deltaThours;
					}
					diffA[0] = num37 / 60.0;
					diffB[0] = num38 / 60.0;
					diffC[0] = num39;
					PA[0] = num43;
					Altit[0] = Math.Asin(num15);
				}
				else
				{
					ContactTimes[4] = hour + num - 1.0;
					if (UseUTC)
					{
						ContactTimes[4] -= deltaThours;
					}
					diffA[4] = num37 / 60.0;
					diffB[4] = num38 / 60.0;
					diffC[4] = num39;
					PA[4] = num43;
					Altit[4] = Math.Asin(num15);
				}
			}
		}

		internal static void GenerateLunarProfile(double Longitude_deg, double Latitude_deg, bool SaveFile)
		{
			double SlopeBefore_Deg = 0.0;
			double SlopeAfter_Deg = 0.0;
			double num = -10.0;
			double num2 = 10.0;
			double num3 = -10.0;
			double num4 = 10.0;
			int num5 = 0;
			int num6 = 0;
			StreamWriter streamWriter = null;
			double num7 = Latitude_deg / (180.0 / Math.PI);
			CurrentSiteLong = Longitude_deg;
			CurrentSiteLat = Latitude_deg;
			if (ContactTimes[2] < -2.0)
			{
				double num8 = 1.0 / Math.Sqrt(Math.Cos(num7) * Math.Cos(num7) + 0.993305615000412 * Math.Sin(num7) * Math.Sin(num7));
				double pSin = 0.993305615000412 * num8 * Math.Sin(num7);
				double pCos = num8 * Math.Cos(num7);
				CalculateLocalCircumstances(Longitude_deg / (180.0 / Math.PI), pCos, pSin, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _);
			}
			double num9 = ContactTimes[2] / 24.0;
			Utilities.TopocentricMoon(Utilities.JD_from_Date(CurrentYear, CurrentMonth, CurrentDay) + num9, Longitude_deg / (180.0 / Math.PI), num7, 0.0, Altitude_is_MSL: true, 0, out var _, out var _, out var _, out var MoonScale, out var l, out var b, out var C, out var _);
			string text = "Lunar limb for eclipse of " + CurrentYear + " " + Utilities.ShortMonths[CurrentMonth] + string.Format(" {0,1:f0}, ", CurrentDay) + Utilities.DEGtoDMS(ContactTimes[2], 2, 1, MinutesOnly: true).Insert(2, "h") + string.Format("m as seen at Longitude {0,1:f0}, Latitude {1,1:f0}; Libn = {2,1:f1}°", Longitude_deg, Latitude_deg, l);
			if (SaveFile)
			{
				streamWriter = new StreamWriter(Utilities.AppPath + "\\Predictions\\MoonLimb_" + CurrentYear + Utilities.ShortMonths[CurrentMonth] + CurrentDay + string.Format("Long{0,1:f0}Lat{1,1:f0}", Longitude_deg, Latitude_deg) + ".txt");
				streamWriter.WriteLine(text);
				streamWriter.Write(string.Format("Topocentric lunar librations: L {0,5:f2}, B {1,5:f2}, C {2,6:f2}", l, b, C));
			}
			PBar pBar = new PBar();
			pBar.pBarFTP.set_Minimum(0);
			pBar.pBarFTP.set_Value(0);
			pBar.pBarFTP.set_Maximum(36000);
			((Control)pBar).set_Text("Generating " + text);
			((Form)pBar).set_StartPosition((FormStartPosition)1);
			((Form)pBar).set_TopMost(true);
			((Control)pBar).Show();
			((Control)pBar).set_Width(620);
			for (int i = 0; i < 36000; i++)
			{
				if (i % 10 == 0)
				{
					if (i % 100 == 0 && SaveFile)
					{
						streamWriter.WriteLine();
						streamWriter.Write(string.Format("{0,6:f2}", (double)i / 100.0));
					}
					if (i > 0)
					{
						MinLimbHeightsX10[num6] = num4;
						MaxLimbHeightsX10[num6] = num3;
						num6++;
						num4 = 10.0;
						num3 = -10.0;
						if (i % 100 == 0)
						{
							MinLimbHeights[num5] = num2;
							MaxLimbHeights[num5] = num;
							num5++;
							num2 = 10.0;
							num = -10.0;
						}
					}
					pBar.pBarFTP.set_Value(i);
					Application.DoEvents();
				}
				double P_Limb_deg;
				double D_Limb_deg;
				double num10 = LOLAHiRes.LimbHeight_Slope((double)i / 100.0, l, b, MoonScale, IncludeSlope: false, WideSlope: false, out SlopeBefore_Deg, out SlopeAfter_Deg, out P_Limb_deg, out D_Limb_deg);
				if (num10 > num3)
				{
					num3 = num10;
				}
				if (num10 < num4)
				{
					num4 = num10;
				}
				if (num10 > num)
				{
					num = num10;
				}
				if (num10 < num2)
				{
					num2 = num10;
				}
				if (SaveFile)
				{
					streamWriter.Write(string.Format("{0,6:f2}", num10));
				}
			}
			MinLimbHeightsX10[num6] = num4;
			MaxLimbHeightsX10[num6] = num3;
			MinLimbHeights[num5] = num2;
			MaxLimbHeights[num5] = num;
			((Form)pBar).Close();
			if (SaveFile)
			{
				streamWriter.Close();
			}
		}

		internal static bool MinimumLimbDistance(double SunMoonX, double SunMoonY, double SolarRadius, double LunarRadius, double PA_LunarPole, out double LimbDistance, out double ContactPA)
		{
			double num = 1.0;
			double num2 = 1.0;
			double num3 = 0.0;
			double num4 = 0.0;
			double num5 = -1000.0;
			double num6 = 1000.0;
			ContactPA = -180.0;
			LimbDistance = 0.0;
			GetLimitingRadii(LunarRadius);
			if ((SolarRadius >= MinRad_R) & (SolarRadius < MaxRad_R))
			{
				return false;
			}
			bool flag = SolarRadius < MinRad_R;
			bool flag2 = SolarRadius > MaxRad_R;
			if (flag || flag2)
			{
				for (int i = 0; i < 3600; i++)
				{
					if (flag)
					{
						num3 = LunarRadius + MinLimbHeightsX10[i];
					}
					else if (flag2)
					{
						num3 = LunarRadius + MaxLimbHeightsX10[i];
					}
					double num7 = Utilities.NormaliseDegrees(((double)i + 0.5) / 10.0 + PA_LunarPole);
					num = num3 * Math.Sin(num7 / (180.0 / Math.PI)) - SunMoonX;
					num2 = num3 * Math.Cos(num7 / (180.0 / Math.PI)) - SunMoonY;
					num4 = SolarRadius - Math.Sqrt(num * num + num2 * num2);
					if (flag)
					{
						if (num4 > num5)
						{
							num5 = num4;
							ContactPA = num7;
						}
					}
					else if (num4 < num6)
					{
						num6 = num4;
						ContactPA = num7;
					}
				}
			}
			if (flag)
			{
				LimbDistance = num5;
			}
			else
			{
				LimbDistance = num6;
			}
			return true;
		}

		internal static void GetLimitingRadii(double MoonRadius_arcSec)
		{
			double num = -10.0;
			double num2 = 0.0;
			double num3 = 10.0;
			double num4 = 0.0;
			double num5 = -10.0;
			double num6 = 0.0;
			double num7 = 10.0;
			double num8 = 0.0;
			double num9 = -10.0;
			double num10 = 0.0;
			double num11 = 10.0;
			double num12 = 0.0;
			for (int i = 0; i < 360; i++)
			{
				if (MaxLimbHeights[i] > num)
				{
					num = MaxLimbHeights[i];
					num2 = i;
				}
				if (MinLimbHeights[i] < num3)
				{
					num3 = MinLimbHeights[i];
					num4 = i;
				}
			}
			double x = (MoonRadius_arcSec + num) * Math.Sin(num2 / (180.0 / Math.PI));
			double y = (MoonRadius_arcSec + num) * Math.Cos(num2 / (180.0 / Math.PI));
			double x2 = (MoonRadius_arcSec + num3) * Math.Sin(num4 / (180.0 / Math.PI));
			double y2 = (MoonRadius_arcSec + num3) * Math.Cos(num4 / (180.0 / Math.PI));
			for (int j = -70; j < 71; j++)
			{
				double num13 = Utilities.NormaliseDegrees(num2 + 180.0 + (double)j);
				if (MaxLimbHeights[(int)num13] > num5)
				{
					num5 = MaxLimbHeights[(int)num13];
					num6 = num13;
				}
				num13 = Utilities.NormaliseDegrees(num4 + 180.0 + (double)j);
				if (MinLimbHeights[(int)num13] < num7)
				{
					num7 = MinLimbHeights[(int)num13];
					num8 = num13;
				}
			}
			double x3 = (MoonRadius_arcSec + num5) * Math.Sin(num6 / (180.0 / Math.PI));
			double y3 = (MoonRadius_arcSec + num5) * Math.Cos(num6 / (180.0 / Math.PI));
			double x4 = (MoonRadius_arcSec + num7) * Math.Sin(num8 / (180.0 / Math.PI));
			double y4 = (MoonRadius_arcSec + num7) * Math.Cos(num8 / (180.0 / Math.PI));
			double num14 = (num6 + num2) / 2.0;
			if (Math.Abs(num6 - num2) < 180.0)
			{
				num14 = Utilities.NormaliseDegrees(num14 + 180.0);
			}
			double num15 = (num8 + num4) / 2.0;
			if (Math.Abs(num8 - num4) < 180.0)
			{
				num15 = Utilities.NormaliseDegrees(num15 + 180.0);
			}
			for (int k = -70; k < 71; k++)
			{
				double num16 = Utilities.NormaliseDegrees(num14 + (double)k);
				if (MaxLimbHeights[(int)num16] > num9)
				{
					num9 = MaxLimbHeights[(int)num16];
					num10 = num16;
				}
				num16 = Utilities.NormaliseDegrees(num15 + (double)k);
				if (MinLimbHeights[(int)num16] < num11)
				{
					num11 = MinLimbHeights[(int)num16];
					num12 = num16;
				}
			}
			double x5 = (MoonRadius_arcSec + num9) * Math.Sin(num10 / (180.0 / Math.PI));
			double y5 = (MoonRadius_arcSec + num9) * Math.Cos(num10 / (180.0 / Math.PI));
			double x6 = (MoonRadius_arcSec + num11) * Math.Sin(num12 / (180.0 / Math.PI));
			double y6 = (MoonRadius_arcSec + num11) * Math.Cos(num12 / (180.0 / Math.PI));
			Utilities.GetCircle(x, y, x3, y3, x5, y5, out MaxRad_x, out MaxRad_y, out MaxRad_R);
			Utilities.GetCircle(x2, y2, x4, y4, x6, y6, out MinRad_x, out MinRad_y, out MinRad_R);
			using StreamWriter streamWriter = new StreamWriter(Utilities.AppPath + "/Predictions/testLimbMax.txt");
			using StreamWriter streamWriter2 = new StreamWriter(Utilities.AppPath + "/Predictions/testLimbMin.txt");
			for (int l = 0; l < 360; l++)
			{
				double num17 = MaxRad_R * Math.Sin((double)l / (180.0 / Math.PI)) + MaxRad_x;
				double num18 = MaxRad_R * Math.Cos((double)l / (180.0 / Math.PI)) + MaxRad_y;
				streamWriter.WriteLine("{0,4:f0} {1,5:f2}", l, MoonRadius_arcSec + MaxLimbHeights[l] - Math.Sqrt(num17 * num17 + num18 * num18));
				num17 = MinRad_R * Math.Sin((double)l / (180.0 / Math.PI)) + MinRad_x;
				num18 = MinRad_R * Math.Cos((double)l / (180.0 / Math.PI)) + MinRad_y;
				streamWriter2.WriteLine("{0,4:f0} {1,5:f2}", l, MoonRadius_arcSec + MinLimbHeights[l] - Math.Sqrt(num17 * num17 + num18 * num18));
			}
		}

		internal static void GenerateLimbCorrectionTimes(double Longitude_deg, double Latitude_deg, double Alt)
		{
			double LimbDistance = 0.0;
			double LimbDistance2 = 0.0;
			double num = 0.0;
			double ContactPA = 0.0;
			double ContactPA2 = 0.0;
			double num2 = -1.0;
			ValidLocalTime[7] = (ValidLocalTime[8] = false);
			if (!ValidLocalTime[2] || !(Math.Abs(ContactTimes[5]) < Math.Abs(Altit[5]) + 0.02))
			{
				return;
			}
			double MoonRadius_arcSec;
			double SunRadius_arcSec;
			double C;
			double SunMoonX;
			double SunMoonY;
			double SunMoonX2;
			double SunMoonY2;
			for (int i = 1; i < 4; i += 2)
			{
				if ((Math.Abs(Longitude_deg - CurrentSiteLong) > 2.0) | (Math.Abs(Latitude_deg - CurrentSiteLat) > 2.0))
				{
					GenerateLunarProfile(Longitude_deg, Latitude_deg, SaveFile: false);
				}
				double num3 = Utilities.JD_from_Date(CurrentYear, CurrentMonth, CurrentDay) + ContactTimes[i] / 24.0;
				num2 = -1.0;
				if (i == 3)
				{
					num2 = 1.0;
				}
				num = 0.0;
				for (int j = 0; j < 3; j++)
				{
					GetRelativeCoords(num3 + num, Longitude_deg, Latitude_deg, Alt, out MoonRadius_arcSec, out SunRadius_arcSec, out C, out SunMoonX, out SunMoonY);
					GetRelativeCoords(num3 + num + num2 * 5.787037037037037E-05, Longitude_deg, Latitude_deg, Alt, out MoonRadius_arcSec, out SunRadius_arcSec, out C, out SunMoonX2, out SunMoonY2);
					MinimumLimbDistance(SunMoonX, SunMoonY, SunRadius_arcSec, MoonRadius_arcSec, C, out LimbDistance, out ContactPA);
					if ((SunRadius_arcSec < MaxRad_R) & (SunRadius_arcSec > MinRad_R))
					{
						num = 0.0;
						ValidLocalTime[i] = false;
					}
					else
					{
						MinimumLimbDistance(SunMoonX2, SunMoonY2, SunRadius_arcSec, MoonRadius_arcSec, C, out LimbDistance2, out ContactPA2);
						num = ((LimbDistance != LimbDistance2) ? (num + LimbDistance / (LimbDistance - LimbDistance2) * (num2 * 5.787037037037037E-05)) : 0.0);
					}
				}
				ContactTimes[i] += num * 24.0;
				PA[i] = ContactPA;
			}
			if (ContactTimes[1] > ContactTimes[3])
			{
				ValidLocalTime[1] = (ValidLocalTime[3] = false);
			}
			if (ValidLocalTime[2])
			{
				double num3 = Utilities.JD_from_Date(CurrentYear, CurrentMonth, CurrentDay) + ContactTimes[2] / 24.0;
				GetRelativeCoords(num3, Longitude_deg, Latitude_deg, Alt, out MoonRadius_arcSec, out SunRadius_arcSec, out C, out SunMoonX, out SunMoonY);
				GetRelativeCoords(num3 + 0.0006944444444444445, Longitude_deg, Latitude_deg, Alt, out MoonRadius_arcSec, out SunRadius_arcSec, out C, out SunMoonX2, out SunMoonY2);
				double num4 = 0.0 - SunMoonX + (MinRad_x * Math.Cos(C / (180.0 / Math.PI)) + MinRad_y * Math.Sin(C / (180.0 / Math.PI)));
				double num5 = 0.0 - SunMoonY + ((0.0 - MinRad_x) * Math.Sin(C / (180.0 / Math.PI)) + MinRad_y * Math.Cos(C / (180.0 / Math.PI)));
				double num6 = (0.0 - (SunMoonX2 - SunMoonX)) * 60.0;
				double num7 = (0.0 - (SunMoonY2 - SunMoonY)) * 60.0;
				double num8 = num6 * num6 + num7 * num7;
				double num9 = Math.Sqrt(num8);
				double num10 = num4 * num6 + num5 * num7;
				double num11 = Math.Abs(SunRadius_arcSec - MinRad_R);
				double num12 = (0.0 - num10) / num8;
				double num13 = Math.Abs((num4 * num7 - num6 * num5) / num9 / num11);
				double num14;
				if (num13 < 1.0)
				{
					num14 = Math.Sqrt(1.0 - num13 * num13) * num11 / num9;
					ValidLocalTime[7] = (ValidLocalTime[8] = true);
				}
				else
				{
					num14 = 0.0;
				}
				double num15 = ContactTimes[2] + num12 - num14;
				double num16 = ContactTimes[2] + num12 + num14;
				double num17 = 0.0 - SunMoonX + (MaxRad_x * Math.Cos(C / (180.0 / Math.PI)) + MaxRad_y * Math.Sin(C / (180.0 / Math.PI)));
				num5 = 0.0 - SunMoonY + ((0.0 - MaxRad_x) * Math.Sin(C / (180.0 / Math.PI)) + MaxRad_y * Math.Cos(C / (180.0 / Math.PI)));
				double num18 = num17 * num6 + num5 * num7;
				num11 = Math.Abs(SunRadius_arcSec - MaxRad_R);
				num12 = (0.0 - num18) / num8;
				num13 = Math.Abs((num17 * num7 - num6 * num5) / num9 / num11);
				if (num13 < 1.0)
				{
					num14 = Math.Sqrt(1.0 - num13 * num13) * num11 / num9;
					ValidLocalTime[7] = (ValidLocalTime[8] = true);
				}
				else
				{
					num14 = 0.0;
				}
				double num19 = ContactTimes[2] + num12 - num14;
				double num20 = ContactTimes[2] + num12 + num14;
				if (num15 < num19)
				{
					ContactTimes[7] = num15;
				}
				else
				{
					ContactTimes[7] = num19;
				}
				if (num20 > num16)
				{
					ContactTimes[8] = num20;
				}
				else
				{
					ContactTimes[8] = num16;
				}
			}
		}

		private static void GetRelativeCoords(double jd, double Longitude_deg, double Latitude_deg, double Altitude, out double MoonRadius_arcSec, out double SunRadius_arcSec, out double C, out double SunMoonX, out double SunMoonY)
		{
			Utilities.TopocentricMoon(jd, Longitude_deg / (180.0 / Math.PI), Latitude_deg / (180.0 / Math.PI), Altitude, Altitude_is_MSL: true, 0, out var RA, out var Dec, out MoonRadius_arcSec, out var _, out var _, out var _, out C, out var _);
			Utilities.TopocentricSun(jd, Longitude_deg / (180.0 / Math.PI), Latitude_deg / (180.0 / Math.PI), Altitude, out var raPlanet, out var decPlanet, out SunRadius_arcSec, out var _);
			SunMoonX = (raPlanet - RA) * (180.0 / Math.PI) * 3600.0 * Math.Cos((Dec + decPlanet) / 2.0);
			SunMoonY = (decPlanet - Dec) * (180.0 / Math.PI) * 3600.0;
			SunRadius_arcSec *= 648000.0 / Math.PI;
			MoonRadius_arcSec *= 648000.0 / Math.PI;
		}

		public static void GoogleEarthCurves(bool View)
		{
			string CreatedFile;
			if (View)
			{
				if (!GoogleEarth.Create_New_GoogleEarthKMZ_File(AppPath + "\\Predictions\\SolarEclipse.KML", SolarEclipseLabel, AutoOpenFile: true, out CreatedFile))
				{
					return;
				}
			}
			else if (!GoogleEarth.Create_New_GoogleEarthKMZ_File(SolarEclipseLabel, SolarEclipseLabel, AutoOpenFile: false, out CreatedFile))
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
					double num = StartEndLongitude[j, i];
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
				if (MaxCount[i] <= 0)
				{
					continue;
				}
				GoogleEarth.Write_Tags_For_New_GoogleEarthKMZ_Path(2);
				for (int k = 0; k < MaxCount[i]; k++)
				{
					double num = MaxLongitude[k, i];
					if (k > 0 && Math.Abs(MaxLongitude[k, i] - MaxLongitude[k - 1, i]) % 360.0 > 10.0)
					{
						GoogleEarth.Write_Tags_At_End_of_Path_GoogleEarthKMZ();
						GoogleEarth.Write_Tags_For_New_GoogleEarthKMZ_Path(2);
					}
					if (num > 180.0)
					{
						num -= 360.0;
					}
					GoogleEarth.Write_Path_Coordinate_GoogleEarthKMZ(num, MaxLatitude[k, i], 0.0);
				}
				GoogleEarth.Write_Tags_At_End_of_Path_GoogleEarthKMZ();
			}
			for (int i = 0; i <= 4; i++)
			{
				if (LimitCount[i] <= 0)
				{
					continue;
				}
				GoogleEarth.Write_Tags_For_New_GoogleEarthKMZ_Path(2);
				for (int l = 0; l < LimitCount[i]; l++)
				{
					double num = LimitLongitude[l, i];
					if (num > 180.0)
					{
						num -= 360.0;
					}
					GoogleEarth.Write_Path_Coordinate_GoogleEarthKMZ(num, LimitLatitude[l, i], 0.0);
				}
				GoogleEarth.Write_Tags_At_End_of_Path_GoogleEarthKMZ();
			}
			CreatedFile = GoogleEarth.Close_GoogleEarthKMZ_File(CreatedFile, ConvertToKMZ: true);
			if (View)
			{
				GoogleEarth.DisplayGoogleMap(CreatedFile);
			}
		}

		public static void GoogleMapCurves()
		{
			if (!GoogleEarth.Create_New_GoogleMap_File(SolarEclipseLabel, SolarEclipseLabel, PositiveTimeIncrement: true, IncludeDirectionArrow: false, AutoOpenFile: false))
			{
				return;
			}
			int i;
			for (i = 0; i <= 1; i++)
			{
				if (StartEndCount[i] <= 0)
				{
					continue;
				}
				GoogleEarth.Write_Tags_For_New_GoogleMap_Polyline_Path();
				for (int j = 0; j < StartEndCount[i]; j++)
				{
					double num = StartEndLongitude[j, i];
					if (j == 0 && num == 180.0)
					{
						num = 179.9;
						if (StartEndLongitude[1, i] > 180.0)
						{
							num = 0.0 - num;
						}
					}
					if (num > 180.0)
					{
						num -= 360.0;
					}
					GoogleEarth.Write_PolyLine_Path_Coordinate_GoogleMap(num, StartEndLatitude[j, i], 1);
				}
				GoogleEarth.Write_Tags_At_End_of_Polyline_Path_GoogleMap(1);
			}
			for (i = 0; i <= 1; i++)
			{
				if (MaxCount[i] <= 0)
				{
					continue;
				}
				GoogleEarth.Write_Tags_For_New_GoogleMap_Polyline_Path();
				for (int k = 0; k < MaxCount[i]; k++)
				{
					double num = MaxLongitude[k, i];
					if (k == 0 && num == 180.0)
					{
						num = 179.9;
						if (MaxLongitude[1, i] > 180.0)
						{
							num = 0.0 - num;
						}
					}
					if (k > 0 && Math.Abs(MaxLongitude[k, i] - MaxLongitude[k - 1, i]) % 360.0 > 10.0)
					{
						GoogleEarth.Write_Tags_At_End_of_Polyline_Path_GoogleMap(1);
						GoogleEarth.Write_Tags_For_New_GoogleMap_Polyline_Path();
					}
					if (num > 180.0)
					{
						num -= 360.0;
					}
					GoogleEarth.Write_PolyLine_Path_Coordinate_GoogleMap(num, MaxLatitude[k, i], 1);
				}
				GoogleEarth.Write_Tags_At_End_of_Polyline_Path_GoogleMap(1);
			}
			for (i = 1; i <= 4; i++)
			{
				if (LimitCount[i] <= 0)
				{
					continue;
				}
				GoogleEarth.Write_Tags_For_New_GoogleMap_Polyline_Path();
				for (int l = 0; l < LimitCount[i]; l++)
				{
					double num = LimitLongitude[l, i];
					if (l == 0 && num == 180.0)
					{
						num = 179.9;
						if (LimitLongitude[1, i] > 180.0)
						{
							num = 0.0 - num;
						}
					}
					if (num > 180.0)
					{
						num -= 360.0;
					}
					GoogleEarth.Write_PolyLine_Path_Coordinate_GoogleMap(num, LimitLatitude[l, i], 1);
				}
				GoogleEarth.Write_Tags_At_End_of_Polyline_Path_GoogleMap(1);
			}
			i = 0;
			if (LimitCount[i] > 0)
			{
				GoogleEarth.Write_Tags_For_New_GoogleMap_PolyPoint_Path();
				for (int m = 0; m < LimitCount[i]; m++)
				{
					double num = LimitLongitude[m, i];
					if (m == 0 && num == 180.0)
					{
						num = 179.9;
						if (LimitLongitude[1, i] > 180.0)
						{
							num = 0.0 - num;
						}
					}
					if (num > 180.0)
					{
						num -= 360.0;
					}
					GoogleEarth.Write_PolyPoint_Path_Coordinate_GoogleMap(num, LimitLatitude[m, i]);
				}
				GoogleEarth.Write_Tags_At_End_of_Polypoint_Path_GoogleMap(-1);
			}
			double num2 = 0.0;
			double num3 = 0.0;
			int num4 = 0;
			for (i = 0; i <= 4; i++)
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

		public static void mfi_MapCurves()
		{
			if (!GoogleEarth.Create_New_MIF_File(SolarEclipseLabel))
			{
				return;
			}
			int i;
			for (i = 0; i <= 1; i++)
			{
				if (StartEndCount[i] <= 0)
				{
					continue;
				}
				for (int j = 0; j < StartEndCount[i]; j++)
				{
					double num = StartEndLongitude[j, i];
					if (num > 180.0)
					{
						num -= 360.0;
					}
					GoogleEarth.Add_point_to_MIF_Line(num, StartEndLatitude[j, i]);
				}
				GoogleEarth.Write_MIF_Line_Block(1);
			}
			for (i = 0; i <= 1; i++)
			{
				if (MaxCount[i] <= 0)
				{
					continue;
				}
				for (int k = 0; k < MaxCount[i]; k++)
				{
					double num = MaxLongitude[k, i];
					if (num > 180.0)
					{
						num -= 360.0;
					}
					GoogleEarth.Add_point_to_MIF_Line(num, MaxLatitude[k, i]);
				}
				GoogleEarth.Write_MIF_Line_Block(1);
			}
			for (i = 1; i <= 4; i++)
			{
				if (LimitCount[i] <= 0)
				{
					continue;
				}
				for (int l = 0; l < LimitCount[i]; l++)
				{
					double num = LimitLongitude[l, i];
					if (num > 180.0)
					{
						num -= 360.0;
					}
					GoogleEarth.Add_point_to_MIF_Line(num, LimitLatitude[l, i]);
				}
				GoogleEarth.Write_MIF_Line_Block(1);
			}
			i = 0;
			if (LimitCount[i] > 0)
			{
				for (int m = 0; m < LimitCount[i]; m++)
				{
					double num = LimitLongitude[m, i];
					if (num > 180.0)
					{
						num -= 360.0;
					}
					GoogleEarth.Add_point_to_MIF_Line(num, LimitLatitude[m, i]);
				}
				GoogleEarth.Write_MIF_Line_Block(-1);
			}
			GoogleEarth.Close_MIF_File();
		}

		public static void Compute_StartEnd_at_RiseSet_Curves()
		{
			double[,] array = new double[700, 2];
			double[,] array2 = new double[700, 2];
			int[] array3 = new int[2];
			int[] array4 = new int[2];
			double num = 0.0;
			double num2 = 0.0;
			int num3 = 0;
			_ = new float[4];
			_ = new float[4];
			double num4 = (-1.6 - X[0]) / X[1] - 0.1;
			double num5 = (1.6 - X[0]) / X[1] + 0.1;
			num3 = 0;
			int num6 = 0;
			array3[0] = (array3[1] = 0);
			array4[0] = (array4[1] = -1);
			for (int i = 0; i <= 1; i++)
			{
				for (double num7 = num4; num7 <= num5; num7 += 0.01)
				{
					double num8 = Mu[0] + Mu[1] * num7;
					double num9 = X[0] + X[1] * num7 + X[2] * num7 * num7;
					double num10 = Y[0] + Y[1] * num7 + Y[2] * num7 * num7;
					double num11 = L1[0] + L1[1] * num7 + L1[1] * num7 * num7;
					double num12 = D[0] + D[1] * num7 + D[2] * num7 * num7;
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
			if ((array4[0] > 0) & (array4[0] <= array3[0]))
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
			if (!flag)
			{
				return;
			}
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
				if (num27 >= 0)
				{
					StartEndLongitude[StartEndCount[1], 1] = array[num27, 1];
					StartEndLatitude[StartEndCount[1], 1] = array2[num27, 1];
					StartEndCount[1]++;
				}
			}
			StartEndLongitude[StartEndCount[1], 1] = array[num22, 0];
			StartEndLatitude[StartEndCount[1], 1] = array2[num22, 0];
			StartEndCount[1]++;
		}

		public static void Compute_Max_at_RiseSet_Curves()
		{
			double[,] array = new double[900, 2];
			double[,] array2 = new double[900, 2];
			int[] array3 = new int[2];
			int[] array4 = new int[2];
			double num = 0.0;
			double num2 = 0.0;
			double num3 = 0.0;
			bool[] array5 = new bool[2];
			bool[] array6 = new bool[2];
			double num4 = (-1.6 - X[0]) / X[1] - 0.1;
			double num5 = (1.6 - X[0]) / X[1] + 0.1;
			for (int i = 0; i <= 1; i++)
			{
				array4[i] = -1;
				double num6 = num4;
				do
				{
					double num7 = Mu[0] + Mu[1] * num6;
					double num8 = X[0] + X[1] * num6 + X[2] * num6 * num6;
					double num9 = Y[0] + Y[1] * num6 + Y[2] * num6 * num6;
					double num10 = L1[0] + L1[1] * num6 + L1[1] * num6 * num6;
					double num11 = D[0] + D[1] * num6 + D[2] * num6 * num6;
					double y = 0.0 - Y[1] + Mu[1] * num8 * Math.Sin(num11);
					double x = X[1] + Mu[1] * (num9 * Math.Sin(num11) + num10 * tF1 * Math.Cos(num11));
					array5[i] = false;
					double num12 = 0.998;
					for (int j = 0; j <= 2; j++)
					{
						double num13 = Math.Atan2(y, x);
						if (i == 1)
						{
							num13 += Math.PI;
						}
						if (num13 > Math.PI * 2.0)
						{
							num13 -= Math.PI * 2.0;
						}
						if (num13 < 0.0)
						{
							num13 += Math.PI * 2.0;
						}
						num3 = (num8 * Math.Cos(num13) - num9 * Math.Sin(num13)) / num12;
						double num15;
						if (Math.Abs(num3) <= 1.0)
						{
							double num14 = Math.Asin(num3);
							num = num13 + num14;
							num15 = Math.Sqrt(Math.Pow(num8 - Math.Sin(num), 2.0) + Math.Pow(num9 - Math.Cos(num), 2.0));
						}
						else
						{
							num15 = 1.0;
						}
						if (num15 < num10)
						{
							double num16 = Math.Sin(num);
							double num17 = (0.0 - Math.Cos(num)) * Math.Sin(num11) / num12;
							double num18 = Math.Cos(num) * Math.Cos(num11) / num12;
							double num19 = (Math.Atan(num16 / num17) - num7) * (180.0 / Math.PI);
							if (num17 < 0.0)
							{
								num19 += 180.0;
							}
							while (num19 > 360.0)
							{
								num19 -= 360.0;
							}
							for (; num19 < 0.0; num19 += 360.0)
							{
							}
							num2 = 180.0 / Math.PI * Math.Atan(num18 / Math.Sqrt(num16 * num16 + num17 * num17) / Utilities.sqrt_1LessEarthEllipticitySqrd);
							if (j == 2)
							{
								array[array3[i], i] = num19;
								array2[array3[i], i] = num2;
								array3[i]++;
								array5[i] = true;
							}
						}
						num12 = 0.998327 + 0.001676 * Math.Cos(2.0 * num2 / (180.0 / Math.PI));
						if (array3[i] >= 900)
						{
							array5[i] = true;
							break;
						}
					}
					if (array6[i] & !array5[i] & (array4[i] < 0))
					{
						array4[i] = array3[i];
					}
					array6[i] = array5[i];
					num6 = (((Math.Abs(num3) > 1.1) | (Math.Abs(num3) < 0.9)) ? (num6 + 0.01) : ((!((Math.Abs(num3) > 1.03) | (Math.Abs(num3) < 0.97))) ? (num6 + 0.0005) : (num6 + 0.005)));
				}
				while (num6 < num5);
			}
			int num20 = 0;
			int num21 = 0;
			MaxCount[0] = 0;
			MaxCount[1] = 0;
			bool flag;
			int num22;
			int num23;
			if ((array4[0] > 0) & (array4[0] <= array3[0]))
			{
				num22 = array4[0] - 1;
				num23 = array4[1] - 1;
				flag = true;
			}
			else
			{
				num22 = array3[0] - 1;
				num23 = array3[1] - 1;
				flag = false;
			}
			if (num22 != num20)
			{
				for (int num24 = num22; num24 >= num20; num24--)
				{
					MaxLongitude[MaxCount[0], 0] = array[num24, 0];
					MaxLatitude[MaxCount[0], 0] = array2[num24, 0];
					MaxCount[0]++;
				}
			}
			if (num21 != num23)
			{
				for (int k = num21; k <= num23; k++)
				{
					MaxLongitude[MaxCount[0], 0] = array[k, 1];
					MaxLatitude[MaxCount[0], 0] = array2[k, 1];
					MaxCount[0]++;
				}
			}
			if (flag)
			{
				num20 = array4[0];
				num22 = array3[0] - 1;
				num23 = array3[1] - 1;
				num21 = array4[1];
				if (num21 > 0 && Math.Abs(array[num21, 1] - array[num21 - 1, 1]) < 2.0)
				{
					num21--;
				}
				if (num21 < 0)
				{
					num21 = 0;
				}
				array3[1] = 0;
				for (int l = num20; l <= num22; l++)
				{
					MaxLongitude[MaxCount[1], 1] = array[l, 0];
					MaxLatitude[MaxCount[1], 1] = array2[l, 0];
					MaxCount[1]++;
				}
				if (num23 >= 0)
				{
					MaxLongitude[MaxCount[1], 1] = array[num23, 1];
					MaxLatitude[MaxCount[1], 1] = array2[num23, 1];
					MaxCount[1]++;
				}
				if (num22 >= 0)
				{
					MaxLongitude[MaxCount[1], 1] = array[num22, 0];
					MaxLatitude[MaxCount[1], 1] = array2[num22, 0];
					MaxCount[1]++;
				}
				for (int num25 = num23; num25 >= num21; num25--)
				{
					MaxLongitude[MaxCount[1], 1] = array[num25, 1];
					MaxLatitude[MaxCount[1], 1] = array2[num25, 1];
					MaxCount[1]++;
				}
			}
		}

		public static void Compute_Limit_Lines()
		{
			double[,] array = new double[721, 2];
			double[,] array2 = new double[721, 2];
			double[,] array3 = new double[721, 2];
			int num = (int)(180.0 - (Mu[0] - X[0] / X[1] * Mu[1]) * (180.0 / Math.PI)) % 360;
			int[] array4 = new int[2];
			int[] array5 = new int[2];
			int[] array6 = new int[2];
			for (int i = 0; i <= 4; i++)
			{
				int num2 = 0;
				PathLimits(i + 1, FromPole: false, (num + 180) % 360, out var Valid, out var Latitude_deg, out var z, out var Tcentral, out var Duration, out var Magnitude);
				if (!Valid)
				{
					num2 = 180;
				}
				array4[0] = (array4[1] = 0);
				array6[0] = (array6[1] = 0);
				array5[0] = (array5[1] = 0);
				for (int j = 0; j < 720; j++)
				{
					for (int k = 0; k <= 1; k++)
					{
						PathLimits(i + 1, k == 1, ((double)j / 2.0 + (double)num + (double)num2) % 360.0, out Valid, out Latitude_deg, out z, out Tcentral, out Duration, out Magnitude);
						array[array4[k], k] = ((double)j / 2.0 + (double)num + (double)num2) % 360.0;
						if (Valid)
						{
							array2[array4[k], k] = Latitude_deg;
							array3[array4[k], k] = Tcentral;
							if (array6[k] < 1)
							{
								array6[k] = j;
							}
							array5[k] = j;
						}
						else
						{
							array2[array4[k], k] = (array3[array4[k], k] = -99.0);
						}
						array4[k]++;
					}
				}
				int num3 = 1;
				double num4 = array3[array6[0], 0];
				double num5 = array3[array5[0], 0];
				if (num5 < num4)
				{
					num3 = -1;
				}
				LimitCount[i] = 0;
				for (int num6 = 719; num6 >= 0; num6--)
				{
					if (array2[num6, 1] > -90.0 && ((Math.Abs(array2[num6, 0] - array2[num6, 1]) > 0.1) & (((num3 == 1) & (array3[num6, 1] < num4)) | ((num3 == -1) & (array3[num6, 1] > num5)))))
					{
						LimitLongitude[LimitCount[i], i] = array[num6, 1];
						LimitLatitude[LimitCount[i], i] = array2[num6, 1];
						LimitCount[i]++;
					}
				}
				for (int l = 0; l <= 719; l++)
				{
					if (array2[l, 0] > -90.0)
					{
						LimitLongitude[LimitCount[i], i] = array[l, 0];
						LimitLatitude[LimitCount[i], i] = array2[l, 0];
						LimitCount[i]++;
					}
				}
				for (int num7 = 719; num7 >= 0; num7--)
				{
					if (array2[num7, 1] > -90.0 && ((Math.Abs(array2[num7, 0] - array2[num7, 1]) > 0.1) & (((num3 == 1) & (array3[num7, 1] > num5)) | ((num3 == -1) & (array3[num7, 1] < num4)))))
					{
						LimitLongitude[LimitCount[i], i] = array[num7, 1];
						LimitLatitude[LimitCount[i], i] = array2[num7, 1];
						LimitCount[i]++;
					}
				}
			}
		}

		public static void ShowWorld()
		{
			try
			{
				((Control)WorldMap).Show();
			}
			catch
			{
				WorldMap = new SolarEclipseWorldView();
				((Control)WorldMap).Show();
			}
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
				printDocument.PrintPage += PrintWorld;
				printDocument.Print();
			}
		}

		public static void PrintPreviewWorldGraphic()
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
				printDocument.PrintPage += PrintWorld;
				((Form)val).ShowDialog();
			}
		}

		private static void PrintWorld(object sender, PrintPageEventArgs e)
		{
			Graphics graphics = e.Graphics;
			int chartHeight = (int)(0.92 * (double)e.PageBounds.Height);
			int chartWidth = (int)(0.92 * (double)e.PageBounds.Width);
			DrawEclipseWorld(graphics, chartWidth, chartHeight, Printer: true, BWFlag: true);
		}

		public static void PlotWorld(bool BWFlag)
		{
			int width = ((Control)WorldMap.picEclipse).get_Width();
			int height = ((Control)WorldMap.picEclipse).get_Height();
			Bitmap image = new Bitmap(width, height);
			Graphics graphics = Graphics.FromImage(image);
			DrawEclipseWorld(graphics, width, height, Printer: false, BWFlag);
			WorldMap.picEclipse.set_Image((Image)image);
			graphics.Dispose();
		}

		private static void DrawEclipseWorld(Graphics formGraphics, int ChartWidth, int ChartHeight, bool Printer, bool BWFlag)
		{
			if (Settings.Default.GraphicsSmoothed)
			{
				formGraphics.SmoothingMode = SmoothingMode.AntiAlias;
			}
			float x = 0f;
			float y = 0f;
			float num = (float)(180.0 - (Mu[0] - X[0] / X[1] * Mu[1]) * (180.0 / Math.PI)) % 360f;
			TestDeltaX = ChartWidth / 10;
			if (BWFlag)
			{
				formGraphics.Clear(Color.White);
			}
			else
			{
				formGraphics.Clear(Color.Black);
			}
			Maps.PlotMercatorWorld(formGraphics, ChartWidth, ChartHeight, num, BWFlag, MultiPlot: false, 0);
			Draw_Eclipse_Curves(formGraphics, MercatorFlag: true, Printer, BWFlag);
			string text = "World  Map  -  " + SolarEclipseLabel;
			Font font = (Printer ? new Font("Times New Roman", 16f, FontStyle.Bold) : new Font("Times New Roman", 16f, FontStyle.Regular));
			Brush brush = ((!BWFlag) ? Brushes.Yellow : Brushes.Black);
			Maps.MercatorXY(0f, 90f, ref x, ref y);
			float width = formGraphics.MeasureString(text, font).Width;
			float height = formGraphics.MeasureString(text, font).Height;
			formGraphics.DrawString(text, font, brush, ((float)ChartWidth - width) / 2f, (y - height) / 2f);
			font = new Font("Times New Roman", 8f, FontStyle.Regular);
			if (!BWFlag)
			{
				brush = Brushes.Orange;
			}
			float width2 = formGraphics.MeasureString(ElongationsWorldMap, font).Width;
			formGraphics.DrawString(ElongationsWorldMap, font, brush, ((float)ChartWidth - width2) / 2f, (y - height) / 2f + 26f);
			Maps.MercatorXY(num, -90f, ref x, ref y);
			if (!BWFlag)
			{
				brush = Brushes.LightGray;
			}
			font = new Font("Times New Roman", 7f, FontStyle.Regular);
			formGraphics.DrawString("Occult " + Assembly.GetExecutingAssembly().GetName(copiedName: false).Version, font, brush, x + 6f, y + 30f);
			if (deltaTUncertainty > 1)
			{
				double num2 = (double)deltaTUncertainty / 240.0;
				string text2 = "Uncertainty in Earth's orientation = ±";
				text2 = ((num2 < 10.0) ? (text2 + string.Format("{0,1:f1} degrees", num2)) : ((!(num2 < 180.0)) ? (text2 + string.Format("{0,1:f1} revolutions", num2 / 360.0)) : (text2 + string.Format("{0,1:f0} degrees", num2))));
				width2 = formGraphics.MeasureString(text2, font).Width;
				formGraphics.DrawString(text2, font, brush, ((float)ChartWidth - width2) / 2f, y + 30f);
			}
			string text3 = "Ephemeris: ";
			text3 = ((EphemerisSource <= 100) ? (text3 + "Chapront 87B + VSOP87A") : (text3 + "DE" + EphemerisSource));
			width2 = formGraphics.MeasureString(text3, font).Width;
			formGraphics.DrawString(text3, font, brush, (float)ChartWidth - width2 - 70f, y + 30f);
		}

		private static void Draw_Eclipse_Curves(Graphics formGraphics, bool MercatorFlag, bool Printer, bool BWFlag)
		{
			Pen black = Pens.Black;
			Pen black2 = Pens.Black;
			if (Printer || BWFlag)
			{
				black = (black2 = Pens.Black);
			}
			else
			{
				black = Pens.Aqua;
				black2 = Pens.BlueViolet;
			}
			double num = 300.0;
			float num2 = 0f;
			float num3 = 0f;
			float x = 0f;
			float y = 0f;
			for (int i = 0; i <= 1; i++)
			{
				for (int j = 0; j < StartEndCount[i]; j++)
				{
					if (MercatorFlag)
					{
						Maps.MercatorXY((float)StartEndLongitude[j, i] % 360f, (float)StartEndLatitude[j, i], ref x, ref y);
						if (j > 0 && (double)Math.Abs(x - num2) < num)
						{
							formGraphics.DrawLine(black, num2, num3, x, y);
						}
					}
					else
					{
						Maps.MapProjection(StartEndLongitude[j, i], StartEndLatitude[j, i], out x, out y);
						if (j > 0 && (((double)Math.Abs(x - num2) < num) & ((double)Math.Abs(y - num3) < num)))
						{
							formGraphics.DrawLine(black, num2, num3, x, y);
						}
					}
					num2 = x;
					num3 = y;
				}
			}
			for (int i = 0; i <= 1; i++)
			{
				for (int k = 0; k < MaxCount[i]; k++)
				{
					if (MercatorFlag)
					{
						Maps.MercatorXY((float)MaxLongitude[k, i] % 360f, (float)MaxLatitude[k, i], ref x, ref y);
						if (k > 0 && (double)Math.Abs(x - num2) < num)
						{
							formGraphics.DrawLine(black, num2, num3, x, y);
						}
					}
					else
					{
						Maps.MapProjection(MaxLongitude[k, i], MaxLatitude[k, i], out x, out y);
						if (k > 0 && (((double)Math.Abs(x - num2) < num) & ((double)Math.Abs(y - num3) < num)))
						{
							formGraphics.DrawLine(black, num2, num3, x, y);
						}
					}
					num2 = x;
					num3 = y;
				}
			}
			for (int i = 1; i <= 4; i++)
			{
				for (int l = 0; l < LimitCount[i]; l++)
				{
					if (MercatorFlag)
					{
						Maps.MercatorXY((float)LimitLongitude[l, i] % 360f, (float)LimitLatitude[l, i], ref x, ref y);
						if (l > 0)
						{
							if ((double)Math.Abs(x - num2) < num && i > 2)
							{
								formGraphics.DrawLine(black, num2, num3, x, y);
							}
							if ((double)Math.Abs(x - num2) < num && i < 3)
							{
								formGraphics.DrawLine(black2, num2, num3, x, y);
							}
						}
					}
					else
					{
						Maps.MapProjection(LimitLongitude[l, i], LimitLatitude[l, i], out x, out y);
						if (l > 0)
						{
							if ((((double)Math.Abs(x - num2) < num) & ((double)Math.Abs(y - num3) < num)) && i > 2)
							{
								formGraphics.DrawLine(black, num2, num3, x, y);
							}
							if ((((double)Math.Abs(x - num2) < num) & ((double)Math.Abs(y - num3) < num)) && i < 3)
							{
								formGraphics.DrawLine(black2, num2, num3, x, y);
							}
						}
					}
					num2 = x;
					num3 = y;
				}
			}
		}

		public static void ShowLocalMap(bool UseCursorPosition)
		{
			try
			{
				((Control)LocalMap).Show();
			}
			catch
			{
				LocalMap = new SolarEclipseLocalMap();
				if (!UseCursorPosition)
				{
					for (MidLongitude_deg = (0.0 - (Mu[0] - X[0] / X[1] * Mu[1])) * (180.0 / Math.PI); MidLongitude_deg < -180.0; MidLongitude_deg += 360.0)
					{
					}
					while (MidLongitude_deg > 180.0)
					{
						MidLongitude_deg -= 360.0;
					}
					double num = Y[0] - X[0] / X[1] * Y[1];
					if (Math.Abs(num) >= 1.0)
					{
						MidLatitude_deg = (double)(90 * Math.Sign(num)) - D[0];
					}
					else
					{
						MidLatitude_deg = 180.0 / Math.PI * Math.Asin(num) - D[0];
					}
					if (Math.Abs(MidLatitude_deg) > 70.0)
					{
						MidLatitude_deg = 70 * Math.Sign(MidLatitude_deg);
					}
					Range_deg = 40.0;
				}
				((Control)LocalMap).Show();
			}
			double num2;
			for (num2 = MidLongitude_deg + Range_deg / 2.0; num2 > 180.0; num2 -= 360.0)
			{
			}
			for (; num2 < -180.0; num2 += 360.0)
			{
			}
			LocalMap.updnEast.set_Value((decimal)num2);
			for (num2 = MidLongitude_deg - Range_deg / 2.0; num2 > 180.0; num2 -= 360.0)
			{
			}
			for (; num2 < -180.0; num2 += 360.0)
			{
			}
			LocalMap.updnWest.set_Value((decimal)num2);
			num2 = MidLatitude_deg + Range_deg / 2.0;
			if (num2 > 90.0)
			{
				num2 = 90.0;
			}
			LocalMap.updnNorth.set_Value((decimal)num2);
			num2 = MidLatitude_deg - Range_deg / 2.0;
			if (num2 < -90.0)
			{
				num2 = -90.0;
			}
			LocalMap.updnSouth.set_Value((decimal)num2);
			LocalMap.PlotMap();
		}

		public static void PrintLocalGraphic()
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
				printDocument.PrintPage += PrintLocalMap;
				printDocument.Print();
			}
		}

		public static void PrintPreviewLocalGraphic()
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
				printDocument.PrintPage += PrintLocalMap;
				((Form)val).ShowDialog();
			}
		}

		private static void PrintLocalMap(object sender, PrintPageEventArgs e)
		{
			Graphics graphics = e.Graphics;
			int chartHeight = (int)(0.92 * (double)e.PageBounds.Height);
			int chartWidth = (int)(0.92 * (double)e.PageBounds.Width);
			DrawEclipseLocalMap(graphics, chartWidth, chartHeight, Printer: true, BWFlag: true);
		}

		public static void PlotLocalMap(bool BWFlag)
		{
			int width = ((Control)LocalMap.picLocalMap).get_Width();
			int height = ((Control)LocalMap.picLocalMap).get_Height();
			Bitmap image = new Bitmap(width, height);
			Graphics graphics = Graphics.FromImage(image);
			DrawEclipseLocalMap(graphics, width, height, Printer: false, BWFlag);
			LocalMap.picLocalMap.set_Image((Image)image);
			graphics.Dispose();
		}

		private static void DrawEclipseLocalMap(Graphics formGraphics, int ChartWidth, int ChartHeight, bool Printer, bool BWFlag)
		{
			try
			{
				SolarEclipseLocalMap.MouseMoveDisabled = true;
			}
			catch
			{
			}
			try
			{
				SolarEclipseWorldView.MouseMoveDisabled = true;
			}
			catch
			{
			}
			if (Settings.Default.GraphicsSmoothed)
			{
				formGraphics.SmoothingMode = SmoothingMode.AntiAlias;
			}
			Font font = new Font("Courier New", 8f);
			new Font("Courier New", 12f, FontStyle.Bold);
			Pen pen;
			Pen pen2;
			Brush brush;
			if (Settings.Default.BWFlag || Printer)
			{
				pen = new Pen(Brushes.Black, 1f);
				pen2 = new Pen(Brushes.Black, 1f);
				brush = Brushes.Black;
			}
			else
			{
				pen = new Pen(Brushes.Orange, 1f);
				pen2 = new Pen(Brushes.GreenYellow, 1f);
				brush = Brushes.LightGray;
			}
			double num = (double)LocalMap.updnWest.get_Value();
			double num2 = (double)LocalMap.updnEast.get_Value();
			double northLatitude_deg = (double)LocalMap.updnNorth.get_Value();
			double southLatitude_deg = (double)LocalMap.updnSouth.get_Value();
			if (num > num2)
			{
				num2 += 360.0;
			}
			TestDeltaX = ChartWidth / 4;
			Maps.MapPlot(formGraphics, (float)ChartWidth / 2f, (float)ChartHeight / 2f, num, num2, northLatitude_deg, southLatitude_deg, PlotCities: false, BWFlag || Printer);
			Draw_Eclipse_Curves(formGraphics, MercatorFlag: false, Printer, BWFlag);
			string path = AppPath + "\\Sites\\" + Settings.Default.SiteFileForEclipseMap;
			int selectedIndex = ((ListControl)LocalMap.cmbNames).get_SelectedIndex();
			if (File.Exists(path))
			{
				Sites sites = new Sites();
				float num3 = 1f;
				int num4 = 0;
				StreamReader streamReader = new StreamReader(path);
				do
				{
					sites.Read_SiteFile(streamReader.ReadLine());
					if (sites.PlotOnMap > num4)
					{
						Maps.MapProjection(sites.Longitude, sites.Latitude, out var x, out var y);
						if (sites.PlotOnMap <= 3)
						{
							num3 = sites.PlotOnMap switch
							{
								1 => 1f, 
								2 => 2f, 
								3 => 4f, 
								_ => 1f, 
							};
							formGraphics.DrawEllipse(pen, x - num3, y - num3, 2f * num3, 2f * num3);
						}
						else if (sites.PlotOnMap > 3)
						{
							num3 = 1.5f;
							formGraphics.DrawRectangle(pen2, x - num3, y - num3, 2f * num3, 2f * num3);
						}
						if ((sites.PlotOnMap >= selectedIndex || selectedIndex == 1) && selectedIndex > 0)
						{
							formGraphics.DrawString(sites.ShortName, font, brush, x + num3, y - 5f);
						}
					}
				}
				while (!streamReader.EndOfStream);
				streamReader.Close();
			}
			font = new Font("Times New Roman", 7f, FontStyle.Regular);
			formGraphics.DrawString("Occult " + Assembly.GetExecutingAssembly().GetName(copiedName: false).Version, font, brush, 6f, ChartHeight - 12);
			try
			{
				SolarEclipseLocalMap.MouseMoveDisabled = false;
			}
			catch
			{
			}
			try
			{
				SolarEclipseWorldView.MouseMoveDisabled = false;
			}
			catch
			{
			}
		}

		public static void ShowSingleLocation()
		{
			try
			{
				((Control)SingleLocation).Show();
			}
			catch
			{
				SingleLocation = new SolarEclipseLocalPrediction();
				((Control)SingleLocation).Show();
			}
			((Control)SingleLocation).Focus();
			string text = Utilities.DEGtoDMS(LocalLongitude, 4, 1, MinutesOnly: true);
			((Control)SingleLocation.txtLongitudeDeg).set_Text(text.Substring(0, 4).Trim());
			((Control)SingleLocation.txtLongitudeMin).set_Text(text.Substring(4).Trim());
			text = Utilities.DEGtoDMS(LocalLatitude, 3, 1, MinutesOnly: true);
			((Control)SingleLocation.txtLatitudeDeg).set_Text(text.Substring(0, 3).Trim());
			((Control)SingleLocation.txtLatitudeMin).set_Text(text.Substring(3).Trim());
			((Control)SingleLocation.txtAltitude).set_Text(LocalAltitude.ToString());
			((Control)SingleLocation.txtSite).set_Text(LocalName);
			SingleLocation.ComputePrediction();
		}

		public static string LocalCircumstances(string SiteName, double Longitude_deg, double Latitude_deg, double Altitude_m, bool IncludeLimbCorrections)
		{
			int num = 0;
			string Rise_Magnitude = "";
			string Set_Magnitude = "";
			double num2 = Latitude_deg / (180.0 / Math.PI);
			double num3 = 1.0 / Math.Sqrt(Math.Cos(num2) * Math.Cos(num2) + 0.993305615000412 * Math.Sin(num2) * Math.Sin(num2));
			double pSin = (0.993305615000412 * num3 + Altitude_m / 6378137.0) * Math.Sin(num2);
			double pCos = (num3 + Altitude_m / 6378137.0) * Math.Cos(num2);
			CalculateLocalCircumstances(Longitude_deg / (180.0 / Math.PI), pCos, pSin, out var Magnitude, out var Magnitude_DiameterRatio, out var _, out var _, out var _, out var _, out Rise_Magnitude, out Set_Magnitude);
			if (IncludeLimbCorrections)
			{
				GenerateLimbCorrectionTimes(Longitude_deg, Latitude_deg, Altitude_m);
			}
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.AppendLine("S O L A R     E C L I P S E     P R E D I C T I O N   for " + SolarEclipseDateLabel);
			stringBuilder.AppendLine("".PadRight(20) + SiteName.Trim());
			stringBuilder.AppendLine(" East Longitude " + Utilities.DEGtoDMS(Longitude_deg, 4, 2, MinutesOnly: true));
			stringBuilder.AppendLine("       Latitude  " + Utilities.DEGtoDMS(Latitude_deg, 3, 2, MinutesOnly: true));
			stringBuilder.AppendFormat("       Altitude  {0,1:F0} m.\r\n", Altitude_m);
			string text = ((EphemerisSource <= 100) ? "Chapront 87B + VSOP87A" : ("DE" + EphemerisSource));
			if ((UT1_UTC != 0.0) | ((CurrentYear > 1970) & (CurrentYear < DateTime.Now.Year + 10)))
			{
				stringBuilder.AppendFormat("\r\n         TT-UTC  {0,1:F1} secs", deltaThours * 3600.0);
				stringBuilder.Append("    Prediction basis: " + text);
				stringBuilder.AppendFormat("\r\n        UT1-UTC  {0,1:F1} secs    ▲T [= TT-UT1]  {1,1:f1} secs", UT1_UTC, deltaThours * 3600.0 - UT1_UTC);
			}
			else
			{
				if (Math.Abs(deltaThours) > 24.0)
				{
					int num4 = (int)Math.Floor(deltaThours / 24.0);
					stringBuilder.AppendFormat("\r\n        ▲T = {0,1:F0}d {1,1:F1}h", num4, deltaThours - (double)(24 * num4));
				}
				else if (Math.Abs(deltaThours) > 0.5)
				{
					stringBuilder.AppendFormat("\r\n        ▲T = {0,5:F3} hrs", deltaThours);
				}
				else if (Math.Abs(deltaThours) > 0.04)
				{
					int num5 = (int)(deltaThours * 60.0);
					stringBuilder.AppendFormat("\r\n        ▲T = {0,1:F0}m {1,1:F0}s", num5, 60.0 * (60.0 * deltaThours - (double)num5));
				}
				else if (Math.Abs(deltaThours) > 0.0278)
				{
					stringBuilder.AppendFormat("\r\n        ▲T = {0,5:F0} secs", deltaThours * 3600.0);
				}
				else
				{
					stringBuilder.AppendFormat("\r\n        ▲T = {0,5:F1} secs", deltaThours * 3600.0);
				}
				stringBuilder.Append(deltaTUncertainty_string);
				stringBuilder.Append("    Prediction basis: " + text);
			}
			string text2 = "TT ";
			if (UseUTC)
			{
				text2 = ((!((CurrentYear > 1970) & (CurrentYear < DateTime.Now.Year + 10))) ? "UT " : "UTC");
			}
			stringBuilder.AppendLine("\r\n\r\nEvent               " + text2 + "       P.A.   alt.      Differential Corrections");
			stringBuilder.AppendLine("                  h  m  s       o      o          A       B       C");
			if (Rise_Magnitude.Length > 0)
			{
				stringBuilder.AppendLine(Rise_Magnitude);
			}
			for (int i = 0; i < 9; i++)
			{
				if (i == 5)
				{
					i = 7;
				}
				DayFlag[i] = "  ";
				if (ContactTimes[i] < 0.0)
				{
					ContactTimes[i] += 24.0;
					DayFlag[i] = "- ";
				}
				if (ContactTimes[i] > 24.0)
				{
					ContactTimes[i] -= 24.0;
					DayFlag[i] = "+ ";
				}
			}
			for (int i = 0; i < 5; i++)
			{
				if (((i == 1) & ValidLocalTime[7]) && IncludeLimbCorrections)
				{
					stringBuilder.AppendLine("Baily beads @2nd " + Utilities.DEGtoDMS(ContactTimes[7], 2, 0, MinutesOnly: false) + DayFlag[7]);
				}
				switch (i)
				{
				case 0:
					stringBuilder.Append("First Contact  ");
					break;
				case 1:
					stringBuilder.Append("Second Contact ");
					break;
				case 2:
					stringBuilder.Append("Maximum Eclipse");
					break;
				case 3:
					stringBuilder.Append("Third Contact  ");
					break;
				case 4:
					stringBuilder.Append("Last Contact   ");
					break;
				}
				if (ValidLocalTime[i])
				{
					num = Convert.ToInt32(IncludeLimbCorrections && (i == 1 || i == 3));
					stringBuilder.Append("  " + Utilities.DEGtoDMS(ContactTimes[i], 2, num, MinutesOnly: false) + DayFlag[i]);
					if (i != 2)
					{
						if (num == 0)
						{
							stringBuilder.AppendFormat("{0,6:F0}", PA[i]);
						}
						else
						{
							stringBuilder.AppendFormat("{0,6:F1}", PA[i]);
						}
					}
					else
					{
						stringBuilder.AppendFormat("".PadRight(6));
					}
					if (i == 1 || i == 3)
					{
						stringBuilder.AppendFormat("".PadRight(7));
					}
					else
					{
						stringBuilder.AppendFormat("{0,7:F0}", Altit[i] * (180.0 / Math.PI));
						stringBuilder.AppendFormat("{0,13:F2}", 60.0 * diffA[i]);
						stringBuilder.AppendFormat("{0,8:F2}", 60.0 * diffB[i]);
						stringBuilder.AppendFormat("{0,8:F2}", 60.0 * diffC[i]);
					}
				}
				else
				{
					stringBuilder.Append("  .. .. ..");
				}
				stringBuilder.AppendLine("");
				if (((i == 3) & ValidLocalTime[8]) && IncludeLimbCorrections)
				{
					stringBuilder.AppendLine("Baily beads @3rd " + Utilities.DEGtoDMS(ContactTimes[8], 2, 0, MinutesOnly: false) + DayFlag[8]);
				}
			}
			if (Set_Magnitude.Length > 0)
			{
				stringBuilder.AppendLine(Set_Magnitude);
			}
			if (!UseUTC)
			{
				string text3 = "approximate";
				if (deltaTUncertainty > 43200)
				{
					text3 = "meaningless";
				}
				stringBuilder.Append("\r\n[To convert times to " + text3 + " UT, subtract ");
				int num6 = (int)Math.Floor(deltaThours / 24.0);
				int num7 = (int)Math.Floor(deltaThours - (double)(24 * num6));
				double num8 = 60.0 * (deltaThours - (double)(24 * num6) - (double)num7);
				if (num6 > 0)
				{
					stringBuilder.Append(num6 + "d ");
				}
				if (num7 > 4)
				{
					stringBuilder.Append(num7 + "h " + string.Format("{0,1:f0}m]", num8));
				}
				else
				{
					stringBuilder.Append(num7 + "h " + string.Format("{0,1:f1}m]", num8));
				}
			}
			if ((Utilities.LOLAFileExists & (ValidLocalTime[1] | ValidLocalTime[3])) && IncludeLimbCorrections)
			{
				stringBuilder.AppendLine(" [2nd and 3rd contacts are based on the lunar limb derived from LRO LOLA]");
			}
			stringBuilder.AppendFormat("\r\nMagnitude at Maximum Eclipse {0,5:F3}\r\n", Magnitude);
			if (Magnitude_DiameterRatio != 0.0)
			{
				stringBuilder.AppendFormat("Magnitude (diameter ratio)   {0,5:F3}\r\n", Magnitude_DiameterRatio);
			}
			if (ValidLocalTime[2])
			{
				stringBuilder.Append("\r\n\r\nAt Maximum Eclipse\r\nDistance from Centre of eclipse  ");
				stringBuilder.AppendFormat("{0,10:F6}", ContactTimes[5]);
				stringBuilder.AppendFormat("{0,10:F1}", diffA[5]);
				stringBuilder.AppendFormat("{0,8:F1}", diffB[5]);
				stringBuilder.AppendFormat("{0,8:F2}\r\n", diffC[5]);
				if (Math.Abs(ContactTimes[5]) < 0.04 + Math.Abs(Altit[5]))
				{
					stringBuilder.AppendFormat("             Shadow Radius   L2   {0,9:F6}", Altit[5]);
					stringBuilder.AppendFormat("\r\n           Shadow movement    n   {0,9:F6}\r\n", PA[5]);
				}
			}
			stringBuilder.AppendLine("\r\nPlanetary elongations");
			stringBuilder.AppendLine(ElongationsWorldMap.Substring(18));
			stringBuilder.AppendLine("\r\n\r\nDifferential Corrections:\r\nUnits: for Contact Times - seconds; Distance - 0.000001\r\nA and B - per arc minute, C - per km in altitude");
			return stringBuilder.ToString();
		}

		public static void Show_Multilocations()
		{
			try
			{
				((Control)MultiLocation).Show();
			}
			catch
			{
				MultiLocation = new SolarEclipseMultiLocations();
				((Control)MultiLocation).Show();
			}
			MultiLocation.ComputePrediction();
		}

		public static void MultiLocationComputation(string ActiveFile, bool FullSitePrecisionInOutput)
		{
			string text = "";
			Sites sites = new Sites();
			EclipseContactTimes eclipseContactTimes = new EclipseContactTimes();
			StreamReader streamReader = new StreamReader(AppPath + "\\Sites\\" + ActiveFile);
			bool flag = false;
			MultiEvent = new List<EclipseContactTimes>();
			MultiEvent.Clear();
			do
			{
				sites.Read_SiteFile(streamReader.ReadLine());
				CalculateLocalCircumstances(sites.Longitude / (180.0 / Math.PI), sites.pCos, sites.pSin, out var Magnitude, out var Magnitude_DiameterRatio, out var _, out var _, out var _, out var SemiDurn, out var _, out var _);
				flag = false;
				for (int i = 0; i < 6; i++)
				{
					flag |= ValidLocalTime[i];
				}
				if (!flag)
				{
					continue;
				}
				eclipseContactTimes = new EclipseContactTimes();
				eclipseContactTimes.SiteName = sites.Name;
				eclipseContactTimes.Longitude = sites.Longitude;
				eclipseContactTimes.Latitude = sites.Latitude;
				eclipseContactTimes.Altitude = sites.Altitude;
				StringBuilder stringBuilder = new StringBuilder();
				for (int j = 0; j <= 4; j++)
				{
					if (ValidLocalTime[j])
					{
						text = " ";
						if (ContactTimes[j] < 0.0)
						{
							ContactTimes[j] += 24.0;
							text = "-";
						}
						if (ContactTimes[j] > 24.0)
						{
							ContactTimes[j] -= 24.0;
							text = "+";
						}
						stringBuilder.Append("   " + Utilities.DEGtoDMS(ContactTimes[j], 2, 0, MinutesOnly: false) + text);
						if (j != 2)
						{
							stringBuilder.AppendFormat("{0,5:F0}", PA[j]);
						}
						if (j != 1 && j != 3)
						{
							stringBuilder.AppendFormat("{0,3:F0}", Altit[j] * (180.0 / Math.PI));
						}
					}
					else if (j == 0 || j == 4)
					{
						stringBuilder.Append("   .. .. ..   ... ..");
					}
					else if (j == 1 || j == 3)
					{
						stringBuilder.Append("   .. .. ..   ...");
					}
					else
					{
						stringBuilder.Append("   .. .. ..  ..");
					}
				}
				if (ValidLocalTime[2])
				{
					stringBuilder.AppendFormat("{0,8:F3}", Magnitude);
					if (Magnitude_DiameterRatio != 0.0)
					{
						stringBuilder.AppendFormat("{0,7:F3}", Magnitude_DiameterRatio);
					}
					else
					{
						stringBuilder.Append("  .....");
					}
				}
				else
				{
					stringBuilder.Append("   .....  .....");
				}
				if (ValidLocalTime[1])
				{
					stringBuilder.AppendFormat(" {0,6:F1}", Math.Abs(7200.0 * SemiDurn));
				}
				else
				{
					stringBuilder.Append("  .....");
				}
				eclipseContactTimes.Times = stringBuilder.ToString();
				MultiEvent.Add(eclipseContactTimes);
			}
			while (!streamReader.EndOfStream);
			streamReader.Close();
		}

		public static void Show_PathCoordinates()
		{
			try
			{
				((Control)PathCoordinates).Show();
			}
			catch
			{
				PathCoordinates = new SolarEclipsePathCoordinates();
				((Control)PathCoordinates).Show();
			}
			((Control)PathCoordinates).Focus();
		}

		public static void ComputePathCoords(double StartLongitude_deg, double EndLongitude_deg, double LongitudeStep_deg)
		{
			Coordinates = new ArrayList();
			if (LongitudeStep_deg < 0.1)
			{
				LongitudeStep_deg = 1.0;
			}
			if (EndLongitude_deg < StartLongitude_deg)
			{
				EndLongitude_deg += 360.0;
			}
			for (double num = StartLongitude_deg; num <= EndLongitude_deg; num += LongitudeStep_deg)
			{
				StringBuilder stringBuilder = new StringBuilder();
				stringBuilder.Append(Utilities.DEGtoDMS(num, 4, 1, MinutesOnly: true));
				for (int i = 1; i <= 5; i++)
				{
					PathLimits(i, FromPole: false, num, out var Valid, out var Latitude_deg, out var z, out var Tcentral, out var Duration, out var Magnitude);
					if (Valid)
					{
						if (i == 1)
						{
							if (Tcentral < 0.0)
							{
								Tcentral += 24.0;
								stringBuilder.Append("-");
							}
							else if (Tcentral > 24.0)
							{
								Tcentral -= 24.0;
								stringBuilder.Append("+");
							}
							else
							{
								stringBuilder.Append(" ");
							}
							stringBuilder.Append("   " + Utilities.DEGtoDMS(Tcentral, 2, 0, MinutesOnly: false));
							if (Duration < 0.0)
							{
								stringBuilder.AppendFormat("{0,7:F1}T", 0.0 - Duration);
							}
							else
							{
								stringBuilder.AppendFormat("{0,7:F1}A", Duration);
							}
							stringBuilder.AppendFormat("{0,7:F3}", Magnitude);
							stringBuilder.AppendFormat("{0,4:F0}    ", Math.Asin(z) * (180.0 / Math.PI));
						}
						if (i < 4)
						{
							stringBuilder.Append(" " + Utilities.DEGtoDMS(Latitude_deg, 3, 2, MinutesOnly: true));
						}
						else
						{
							stringBuilder.Append("   " + Utilities.DEGtoDMS(Latitude_deg, 3, 0, MinutesOnly: true));
						}
					}
					else if (i == 1)
					{
						stringBuilder.Append("    .. .. ..   .....  ....   ..     ... ...  ");
					}
					else if (i < 4)
					{
						stringBuilder.Append(" ...  ... ");
					}
					else
					{
						stringBuilder.Append("   ... ..");
					}
				}
				Coordinates.Add(stringBuilder.ToString());
			}
		}

		internal static void ShowClosePlanets(bool LunarEclipse)
		{
			try
			{
				((Control)ClosePlanetaryConjunctions).Show();
			}
			catch
			{
				ClosePlanetaryConjunctions = new ClosePlanetaryConjunctions(LunarEclipse);
				((Control)ClosePlanetaryConjunctions).Show();
			}
		}

		public static void ShowLunarEclipse()
		{
			try
			{
				((Control)LunarEclipse).Show();
			}
			catch
			{
				LunarEclipse = new Lunar_Eclipses();
				((Control)LunarEclipse).Show();
			}
			((Control)LunarEclipse).Focus();
		}
	}
}
