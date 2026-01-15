using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Windows.Forms;

namespace Occult.Eclipses
{
	internal class TransitMaths
	{
		private const double Radian = 180.0 / Math.PI;

		private const double TwoPi = Math.PI * 2.0;

		public static TransitsMultiLocation TransitMultiLocation;

		internal static List<EclipseContactTimes> MultiEvent;

		internal static double[] RASun = new double[3];

		internal static double[] DecSun = new double[3];

		internal static double[] RSun = new double[3];

		internal static double[] RAPlanet = new double[3];

		internal static double[] DecPlanet = new double[3];

		internal static double[] RPlanet = new double[3];

		internal static double[] Xb = new double[3];

		internal static double[] Yb = new double[3];

		internal static double[] Zb = new double[3];

		internal static double[] Db = new double[3];

		internal static double[] L1b = new double[3];

		internal static double[] L2b = new double[3];

		internal static double[] Mub = new double[3];

		internal static double[] PA = new double[5];

		internal static double[] VA = new double[5];

		internal static double[] X = new double[3];

		internal static double[] Y = new double[3];

		internal static double[] D = new double[3];

		internal static double[] L1 = new double[3];

		internal static double[] L2 = new double[3];

		internal static double[] Mu = new double[3];

		internal static double[] sunX = new double[3];

		internal static double[] sunY = new double[3];

		internal static double[] ZenithL = new double[3];

		internal static double[] ZenithD = new double[3];

		internal static double[] EventTime = new double[5];

		internal static double[] LimitLong = new double[360];

		internal static double[] LimitLat = new double[360];

		internal static int[] PlanetID = new int[20];

		internal static string[] ContactTimes = new string[3] { "", "", "" };

		internal static double LL1;

		internal static double LL2;

		internal static double tF1;

		internal static double tF2;

		private static double CurrentJD_TT;

		private static double midT;

		private static double deltaT = 0.0;

		private static double deltaThours = 0.0;

		private static double Latitude;

		internal static int CurrentYear = 0;

		internal static int CurrentMonth = 0;

		internal static int MissCount = 0;

		internal static int LimitID = 0;

		internal static int LimitCount;

		internal static int deltaTUncertainty;

		private static double CurrentDay = 0.0;

		private static double deltaL1;

		private static double deltaL2;

		private static double deltaB1;

		private static double deltaB2;

		private static double SunRadius;

		private static double PlanetRadius;

		internal static string TransitLabel = "";

		internal static string deltaTUncertainty_string = "";

		internal static bool OuterLimit = false;

		internal static bool Use_UT = false;

		private static int Planet;

		private static bool[] ValidLocalTime = new bool[6];

		internal static void DeltaLB(double JD_TT, int Planet, out double deltaL, out double deltaB, out double SunRadius)
		{
			double Longitude = 0.0;
			double Longitude2 = 0.0;
			double Latitude = 0.0;
			double Latitude2 = 0.0;
			double num = 0.0;
			double HeliocentricDistance;
			double HeliocentricDistance2;
			if (Math.Abs(JD_TT - 2451545.0) < 360000.0)
			{
				Utilities.QuickLongLatMercuryVenusEarth(JD_TT, 3, out Longitude, out Latitude, out HeliocentricDistance);
				Utilities.QuickLongLatMercuryVenusEarth(JD_TT, Planet, out Longitude2, out Latitude2, out HeliocentricDistance2);
			}
			else
			{
				num = ((Math.Abs(JD_TT - 2451545.0) > 1000000.0) ? ((Planet != 1) ? 0.0 : 1E-06) : ((Planet != 1) ? 1E-06 : 1E-05));
				Utilities.HeliocentricLatLong(JD_TT, 3, num, EquinoxOfDate: true, out Longitude, out Latitude, out HeliocentricDistance);
				Utilities.HeliocentricLatLong(JD_TT, Planet, num, EquinoxOfDate: true, out Longitude2, out Latitude2, out HeliocentricDistance2);
			}
			deltaL = (Longitude2 - Longitude) * (180.0 / Math.PI);
			if (deltaL < -180.0)
			{
				deltaL += 360.0;
			}
			if (deltaL > 180.0)
			{
				deltaL -= 360.0;
			}
			deltaB = Math.Atan(HeliocentricDistance2 * Math.Sin(Latitude2) / Math.Sqrt(HeliocentricDistance * HeliocentricDistance + HeliocentricDistance2 * HeliocentricDistance2 - 2.0 * HeliocentricDistance * HeliocentricDistance2 * Math.Cos(Longitude2 - Longitude))) * (180.0 / Math.PI);
			SunRadius = 15.993833333333333 / HeliocentricDistance;
		}

		internal static void TransitElements(double JD_TT, int PlanetNo)
		{
			new StringBuilder();
			CurrentJD_TT = JD_TT;
			Planet = PlanetNo;
			PlanetRadius = 0.3824;
			if (Planet == 2)
			{
				PlanetRadius = 0.9598409999999999;
			}
			LimitCount = 0;
			Utilities.Date_from_JD(JD_TT, out CurrentYear, out CurrentMonth, out CurrentDay);
			deltaT = Utilities.delta_T(CurrentYear, CurrentMonth, CurrentDay);
			deltaThours = deltaT / 3600.0;
			Utilities.deltaT_Uncertainty(CurrentYear, out deltaTUncertainty, out deltaTUncertainty_string);
			if (Math.Abs(deltaTUncertainty) < 3600)
			{
				Use_UT = true;
			}
			else
			{
				Use_UT = false;
			}
			DeltaLB(JD_TT, Planet, out deltaL1, out deltaB1, out SunRadius);
			DeltaLB(JD_TT + 1.0, Planet, out deltaL2, out deltaB2, out SunRadius);
			midT = Math.Floor((0.0 - deltaL1) / (deltaL2 - deltaL1) * 24.0);
			for (int i = 0; i <= 2; i++)
			{
				Utilities.PlanetGeocentric(JD_TT + (midT + (double)i - 1.0) / 24.0, Planet, 0.0, 0, out RAPlanet[i], out DecPlanet[i], out RPlanet[i]);
				Utilities.PlanetGeocentric(JD_TT + (midT + (double)i - 1.0) / 24.0, 3, 0.0, 0, out RASun[i], out DecSun[i], out RSun[i]);
				double num = 4.26345E-05 / RPlanet[i];
				double num2 = RPlanet[i] / RSun[i];
				double num3 = Math.Cos(DecSun[i]) * Math.Cos(RASun[i]) - num2 * Math.Cos(DecPlanet[i]) * Math.Cos(RAPlanet[i]);
				double num4 = Math.Cos(DecSun[i]) * Math.Sin(RASun[i]) - num2 * Math.Cos(DecPlanet[i]) * Math.Sin(RAPlanet[i]);
				double num5 = Math.Sin(DecSun[i]) - num2 * Math.Sin(DecPlanet[i]);
				double num6 = Math.Atan2(num4, num3);
				Db[i] = Math.Atan(num5 / Math.Sqrt(num3 * num3 + num4 * num4));
				double num7 = Math.Sqrt(num3 * num3 + num4 * num4 + num5 * num5);
				Xb[i] = Math.Cos(DecPlanet[i]) * Math.Sin(RAPlanet[i] - num6) / num;
				Yb[i] = (Math.Sin(DecPlanet[i]) * Math.Cos(Db[i]) - Math.Cos(DecPlanet[i]) * Math.Sin(Db[i]) * Math.Cos(RAPlanet[i] - num6)) / num;
				Zb[i] = (Math.Sin(DecPlanet[i]) * Math.Sin(Db[i]) + Math.Cos(DecPlanet[i]) * Math.Cos(Db[i]) * Math.Cos(RAPlanet[i] - num6)) / num;
				Mub[i] = (Utilities.SiderealTime_deg(JD_TT, Apparent: true) + 15.041067 * ((double)i + midT - 1.0)) / (180.0 / Math.PI) - num6;
				if (Use_UT)
				{
					Mub[i] -= 15.041067 * deltaThours / (180.0 / Math.PI);
				}
				while (Mub[i] < 0.0)
				{
					Mub[i] += Math.PI * 2.0;
				}
				while (Mub[i] > Math.PI * 2.0)
				{
					Mub[i] -= Math.PI * 2.0;
				}
				if (i > 0 && Mub[i] < Mub[0])
				{
					Mub[i] += Math.PI * 2.0;
				}
				double num8 = (0.004652418 + PlanetRadius * 4.26345E-05) / num7 / RSun[i];
				double num9 = (0.004652418 - PlanetRadius * 4.26345E-05) / num7 / RSun[i];
				tF1 = num8 / Math.Sqrt(1.0 - num8 * num8);
				tF2 = num9 / Math.Sqrt(1.0 - num9 * num9);
				L1b[i] = (Zb[i] + PlanetRadius / num8) * tF1;
				L2b[i] = (Zb[i] - PlanetRadius / num9) * tF2;
			}
			X[0] = Xb[0];
			X[2] = (Xb[2] - 2.0 * Xb[1] + Xb[0]) / 2.0;
			X[1] = Xb[1] - Xb[0] - X[2];
			Y[0] = Yb[0];
			Y[2] = (Yb[2] - 2.0 * Yb[1] + Yb[0]) / 2.0;
			Y[1] = Yb[1] - Yb[0] - Y[2];
			L1[0] = L1b[0];
			L1[2] = (L1b[2] - 2.0 * L1b[1] + L1b[0]) / 2.0;
			L1[1] = L1b[1] - L1b[0] - L1[2];
			L2[0] = L2b[0];
			L2[2] = (L2b[2] - 2.0 * L2b[1] + L2b[0]) / 2.0;
			L2[1] = L2b[1] - L2b[0] - L2[2];
			Mu[0] = Mub[0];
			Mu[2] = (Mub[2] - 2.0 * Mub[1] + Mub[0]) / 2.0;
			Mu[1] = Mub[1] - Mub[0] - Mu[2];
			D[0] = Db[0];
			D[2] = (Db[2] - 2.0 * Db[1] + Db[0]) / 2.0;
			D[1] = Db[1] - Db[0] - D[2];
		}

		internal static string GeocentricContactTime(out string TransitType, out string LimitLine)
		{
			double num = 0.0;
			double num2 = 0.0;
			double num3 = 0.0;
			int num4 = 0;
			StringBuilder stringBuilder = new StringBuilder();
			for (int i = -1; i <= 1; i++)
			{
				double num5 = 0.0;
				MissCount = 0;
				int num6 = 0;
				double num7;
				double num10;
				double num11;
				double num12;
				do
				{
					num7 = num5;
					double num8 = X[0] + X[1] * num7 + X[2] * num7 * num7;
					double num9 = Y[0] + Y[1] * num7 + Y[2] * num7 * num7;
					num10 = D[0] + D[1] * num7 + D[2] * num7 * num7;
					Math.Sin(num10);
					Math.Cos(num10);
					num = Mu[0] + Mu[1] * num7;
					LL1 = L1[0] + L1[1] * num7 + L1[2] * num7 * num7;
					LL2 = L2[0] + L2[1] * num7 + L2[2] * num7 * num7;
					num11 = num8;
					num12 = num9;
					double num13 = X[1] + 2.0 * X[2] * num7 - Mu[1] * Math.Cos(num);
					double num14 = Y[1] + 2.0 * Y[2] * num7;
					double num15 = num13 * num13 + num14 * num14;
					double num16 = Math.Sqrt(num15);
					num2 = (num11 * num14 - num12 * num13) / num16 / LL1;
					if (i == 0)
					{
						num3 = num2;
						num5 = num7 - (num11 * num13 + num12 * num14) / num15;
					}
					else if (Math.Abs(num2) > 1.0)
					{
						MissCount++;
						num5 = num7 - (num11 * num13 + num12 * num14) / num15;
						if (MissCount > 7)
						{
							break;
						}
					}
					else
					{
						num5 = num7 + (double)i * Math.Sqrt(1.0 - num2 * num2) * LL1 / num16 - (num11 * num13 + num12 * num14) / num15;
					}
					num6++;
				}
				while (Math.Abs(num5 - num7) > 1E-05 && num6 < 10);
				EventTime[i + 1] = midT + num5 - 1.0;
				if (Use_UT)
				{
					EventTime[i + 1] -= deltaThours;
				}
				PA[i + 1] = Math.Atan2(num11, num12) * (180.0 / Math.PI);
				if (PA[i + 1] < 0.0)
				{
					PA[i + 1] += 360.0;
				}
				ZenithL[i + 1] = RASun[1] * (180.0 / Math.PI) - Utilities.SiderealTime_deg(CurrentJD_TT, Apparent: true) - 15.041067 * EventTime[i + 1];
				if (!Use_UT)
				{
					ZenithL[i + 1] -= -15.041067 * deltaThours;
				}
				while (ZenithL[i + 1] < -180.0)
				{
					ZenithL[i + 1] += 360.0;
				}
				while (ZenithL[i + 1] > 180.0)
				{
					ZenithL[i + 1] -= 360.0;
				}
				ZenithD[i + 1] = num10 * (180.0 / Math.PI);
				sunX[i + 1] = num11 / LL1;
				sunY[i + 1] = num12 / LL1;
			}
			if (EventTime[0] == EventTime[2])
			{
				MissCount = 8;
			}
			string text = "TT ";
			if (Use_UT)
			{
				text = ((!((CurrentYear > 1970) & (CurrentYear < DateTime.Now.Year + 10))) ? "UT " : "UTC");
			}
			stringBuilder.AppendFormat("Transit of {0} on {1,1:F0} {2,1:A0} {3,1:F0} (TT)", Utilities.Planets[Planet], CurrentYear, Utilities.ShortMonths[CurrentMonth], CurrentDay);
			TransitLabel = stringBuilder.ToString();
			stringBuilder.Append("\r\n   {'+' => next day; '-' => previous day }");
			stringBuilder.Append("\r\n" + "Overhead at".PadLeft(54) + "\r\n");
			stringBuilder.Append("     Geocentric Event      " + text + "         P.A.  Long  Lat\r\n");
			stringBuilder.Append("".PadLeft(25) + "h  m  s       o      o    o \r\n");
			for (int j = 0; j <= 2; j++)
			{
				string text2 = "  ";
				if (EventTime[j] < 0.0)
				{
					EventTime[j] += 24.0;
					text2 = "- ";
				}
				if (EventTime[j] > 24.0)
				{
					EventTime[j] -= 24.0;
					text2 = "+ ";
				}
				ContactTimes[j] = Utilities.DEGtoDMS(EventTime[j], 2, 0, MinutesOnly: false) + text2;
			}
			TransitType = "";
			if (Math.Abs(LL2 - Math.Abs(num2 * LL1)) < 1.0)
			{
				TransitType = "Total/Partial transit";
				num4 = 2;
				if (num3 < 0.0)
				{
					num4 = 3;
				}
			}
			if (Math.Abs(LL1 - Math.Abs(num2 * LL1)) < 1.0)
			{
				TransitType = "Partial transit/Miss";
				num4 = 4;
				if (num3 < 0.0)
				{
					num4 = 5;
				}
			}
			if ((TransitType == "") & (Math.Sign(LL2 - Math.Abs(num2 * LL1)) != Math.Sign(LL2 - Math.Abs(num2 * LL2))))
			{
				TransitType = "Partial transit";
				num4 = -1;
			}
			if ((MissCount > 7) & (TransitType.Length == 0))
			{
				stringBuilder.Append("\r\nNo transit on this date");
			}
			if (TransitType.Length > 0)
			{
				stringBuilder.Append("     *** " + TransitType + " ***");
			}
			if (ContactTimes[0] == ContactTimes[2])
			{
				MissCount = 10;
			}
			for (int k = 0; k <= 2; k++)
			{
				if ((k == 1) | (MissCount < 7))
				{
					switch (k)
					{
					case 0:
						stringBuilder.Append("\r\n[1]  Exterior Ingress   ");
						break;
					case 1:
						stringBuilder.Append("\r\n[2]  Minimum Separation ");
						break;
					case 2:
						stringBuilder.Append("\r\n[3]  Exterior Egress    ");
						break;
					}
					stringBuilder.Append(ContactTimes[k]);
					if (k != 1)
					{
						stringBuilder.AppendFormat("{0,7:F1}", PA[k]);
					}
					else
					{
						stringBuilder.Append("       ");
					}
					stringBuilder.AppendFormat("{0,7:F0}{1,5:F0}", ZenithL[k], ZenithD[k]);
				}
			}
			stringBuilder.AppendFormat("\r\n\r\nMinimum sepn {0:F1}\"", Math.Abs(959.63 * num3 / RSun[0] * (L1[0] + L2[0]) / L2[0] / 2.0));
			stringBuilder.AppendFormat(";  Radii - Sun {0:F1}\", ", 959.63 / RSun[0]);
			if (Planet == 1)
			{
				stringBuilder.AppendFormat("Mercury {0:F1}\"\r\n", PlanetRadius * 8.794143836182533 / RPlanet[0]);
			}
			else
			{
				stringBuilder.AppendFormat("Venus {0:F1}\"\r\n", PlanetRadius * 8.794143836182533 / RPlanet[0]);
			}
			if (Math.Abs(deltaT) > 86400.0)
			{
				int num17 = (int)(deltaT / 86400.0);
				stringBuilder.AppendFormat("delta T = {0,1:F0}d {1,1:F1}h", num17, (deltaT - (double)(86400 * num17)) / 3600.0);
			}
			else if (Math.Abs(deltaT) > 1800.0)
			{
				stringBuilder.AppendFormat("delta T = {0,5:F2} hrs", deltaT / 3600.0);
			}
			else if (Math.Abs(deltaT) > 144.0)
			{
				int num18 = (int)(deltaThours * 60.0);
				stringBuilder.AppendFormat("delta T = {0,1:F0}m {1,1:F0}s", num18, 60.0 * (60.0 * deltaThours - (double)num18));
			}
			else if (Math.Abs(deltaT) > 100.0)
			{
				stringBuilder.AppendFormat("delta T = {0,5:F0} secs", deltaT);
			}
			else
			{
				stringBuilder.AppendFormat("delta T = {0,5:F1} secs", deltaT);
			}
			stringBuilder.Append(deltaTUncertainty_string);
			stringBuilder.Append(",  Ephemeris = " + Utilities.EphemerisBasis(CurrentJD_TT));
			LimitLine = "";
			if (TransitType != "")
			{
				StringBuilder stringBuilder2 = new StringBuilder();
				if (num4 == -1)
				{
					stringBuilder2.Append(TransitType + "\r\n\r\nTransit is partial\r\nat all locations\r\non the Earth");
				}
				else
				{
					stringBuilder2.Append(TransitType + ":\r\n\r\n");
					if (num3 < 0.0)
					{
						stringBuilder2.Append("Northern");
					}
					else
					{
						stringBuilder2.Append("Southern");
					}
					stringBuilder2.AppendLine(" Limit of");
					if (num4 < 4)
					{
						stringBuilder2.AppendLine("total transit");
						OuterLimit = false;
					}
					else
					{
						stringBuilder2.AppendLine("partial transit");
						OuterLimit = true;
					}
					stringBuilder2.AppendLine("\r\n Long.   Lat.");
					double num19 = 5.0 * Math.Floor(ZenithL[1] / 5.0) - 180.0;
					double num20 = num19 + 360.0;
					bool Valid = false;
					LimitCount = 0;
					for (double num21 = num19; num21 <= num20; num21 += 5.0)
					{
						PathLimits(num4, num21, out Valid, out Latitude);
						if (Valid)
						{
							double num22;
							for (num22 = num21; num22 < -180.0; num22 += 360.0)
							{
							}
							while (num22 > 180.0)
							{
								num22 -= 360.0;
							}
							LimitLong[LimitCount] = num22;
							LimitLat[LimitCount] = Latitude;
							LimitCount++;
							stringBuilder2.AppendFormat("{0,6:F1}  {1}\r\n", num22, Utilities.DEGtoDMS(Latitude, 3, 0, MinutesOnly: true));
							if (LimitCount % 5 == 0)
							{
								stringBuilder2.AppendLine("");
							}
						}
					}
					if (deltaTUncertainty > 12)
					{
						stringBuilder2.AppendFormat("\r\n Longitude uncert.\r\n  = ±{0,1:f1} deg", (double)deltaTUncertainty / 240.0);
					}
				}
				LimitLine = stringBuilder2.ToString();
			}
			return stringBuilder.ToString();
		}

		private static void PathLimits(int LimitID, double Longitude_deg, out bool Valid, out double Latitude_deg)
		{
			double num = 0.0;
			double num2 = 0.0;
			double num3 = 0.005;
			Latitude_deg = 0.0;
			int num4 = 0;
			double num5 = 0.0;
			Valid = true;
			double num23;
			double num30;
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
				double num10;
				double num20;
				double num21;
				double num22;
				double num24;
				double num26;
				do
				{
					num10 = num5;
					double num11 = X[0] + X[1] * num10 + X[2] * num10 * num10;
					double num12 = Y[0] + Y[1] * num10 + Y[2] * num10 * num10;
					double num13 = D[0] + D[1] * num10 + D[2] * num10 * num10;
					double num14 = Math.Sin(num13);
					double num15 = Math.Cos(num13);
					double num16 = Mu[0] + Mu[1] * num10;
					if (!Use_UT)
					{
						num16 += 15.041067 * deltaThours / (180.0 / Math.PI);
					}
					double num17 = num16 + Longitude_deg / (180.0 / Math.PI);
					double num18 = num8 * Math.Sin(num17);
					double num19 = num8 * Math.Cos(num17);
					num20 = num11 - num18;
					num21 = X[1] + 2.0 * X[2] * num10 - Mu[1] * num19;
					num22 = num12 - num9 * num15 + num19 * num14;
					num23 = num9 * num14 + num19 * num15;
					num24 = Y[1] + 2.0 * Y[2] * num10 - Mu[1] * num18 * num14 - num23 * D[1];
					double num25 = num21 * num21 + num24 * num24;
					num26 = Math.Sqrt(num25);
					double num27 = (0.0 - (num20 * num21 + num22 * num24)) / num25;
					if (Math.Abs(num27) > 0.1)
					{
						num27 = 0.1 * (double)Math.Sign(num27);
					}
					num5 = num10 + num27;
					num6++;
				}
				while (num6 <= 20 && Math.Abs(num5 - num10) > 0.0001);
				double value = ((LimitID == 1) ? 0.0 : ((!(LimitID == 2 || LimitID == 3)) ? (L1[0] + L1[1] * num10 + L1[2] * num10 * num10 - num23 * tF1) : (L2[0] + L2[1] * num10 + L2[2] * num10 * num10 - num23 * tF2)));
				double num28 = Math.Abs(value);
				if (LimitID == 3 || LimitID == 5)
				{
					num28 = 0.0 - num28;
				}
				double num29 = num28 - (num20 * num24 - num22 * num21) / num26;
				if (num4 == 0)
				{
					num4 = 1;
					num = num29;
					num2 = Latitude_deg;
					Latitude_deg += 1.0;
					num30 = 1.0;
					continue;
				}
				num4++;
				if (num29 == num || num4 > 12)
				{
					Latitude_deg = 100.0;
					break;
				}
				num30 = (Latitude_deg - num2) / (num - num29) * num29;
				if (Math.Abs(num30) > 20.0)
				{
					num30 = 20 * Math.Sign(num30);
				}
				num2 = Latitude_deg;
				num = num29;
				Latitude_deg += num30;
			}
			while (Math.Abs(num30) > num3);
			Valid = true;
			if (num23 < 0.0)
			{
				Valid = false;
			}
			if (Math.Abs(Latitude_deg) > 90.0)
			{
				Valid = false;
			}
		}

		public static void Show_TransitMultilocations()
		{
			try
			{
				((Control)TransitMultiLocation).Show();
			}
			catch
			{
				TransitMultiLocation = new TransitsMultiLocation();
				((Control)TransitMultiLocation).Show();
			}
			TransitMultiLocation.Display();
		}

		public static void MultiLocationTransitComputation(string ActiveFile, bool FullSitePrecisionInOutput)
		{
			Sites sites = new Sites();
			EclipseContactTimes eclipseContactTimes = new EclipseContactTimes();
			StreamReader streamReader = new StreamReader(Utilities.AppPath + "\\Sites\\" + ActiveFile);
			bool EventsExist = false;
			MultiEvent = new List<EclipseContactTimes>();
			MultiEvent.Clear();
			do
			{
				sites.Read_SiteFile(streamReader.ReadLine());
				CalculateLocalCircumstances(sites.Longitude / (180.0 / Math.PI), sites.pCos, sites.pSin, out var PredictionLine, out EventsExist);
				if (EventsExist)
				{
					eclipseContactTimes = new EclipseContactTimes();
					eclipseContactTimes.SiteName = sites.Name;
					eclipseContactTimes.Longitude = sites.Longitude;
					eclipseContactTimes.Latitude = sites.Latitude;
					eclipseContactTimes.Altitude = sites.Altitude;
					eclipseContactTimes.Times = PredictionLine;
					MultiEvent.Add(eclipseContactTimes);
				}
			}
			while (!streamReader.EndOfStream);
			streamReader.Close();
		}

		public static void CalculateLocalCircumstances(double Longitude, double pCos, double pSin, out string PredictionLine, out bool EventsExist)
		{
			double num = 0.0;
			double num2 = 0.0;
			double num3 = 0.0;
			double[] array = new double[5];
			bool[] array2 = new bool[6];
			for (int i = -2; i <= 2; i++)
			{
				double num4 = 0.0;
				MissCount = 0;
				double num5;
				double num12;
				double num14;
				double num15;
				double num16;
				double num17;
				double num18;
				double num19;
				double num21;
				double num22;
				do
				{
					num5 = num4;
					double num6 = X[0] + X[1] * num5 + X[2] * num5 * num5;
					double num7 = Y[0] + Y[1] * num5 + Y[2] * num5 * num5;
					double num8 = D[0] + D[1] * num5 + D[2] * num5 * num5;
					LL1 = L1[0] + L1[1] * num5 + L1[2] * num5 * num5;
					LL2 = L2[0] + L2[1] * num5 + L2[2] * num5 * num5;
					double num9 = Math.Sin(num8);
					double num10 = Math.Cos(num8);
					double num11 = Mu[0] + Mu[1] * num5 + Longitude;
					num12 = pCos * Math.Sin(num11);
					double num13 = pCos * Math.Cos(num11);
					num14 = num6 - num12;
					num15 = X[1] + 2.0 * X[2] * num5 - Mu[1] * num13;
					num16 = pSin * num10 - num13 * num9;
					num17 = num7 - num16;
					num18 = pSin * num9 + num13 * num10;
					num19 = Y[1] + 2.0 * Y[2] * num5 - Mu[1] * num12 * num9 - num18 * D[1];
					double num20 = num15 * num15 + num19 * num19;
					num21 = Math.Sqrt(num20);
					if (Math.Abs(i) == 2)
					{
						num22 = LL1 - num18 * tF1;
					}
					else
					{
						num22 = LL2 - num18 * tF2;
						num2 = LL1 - num18 * tF1;
					}
					double num23 = num14 * num15 + num17 * num19;
					double num24 = (num14 * num19 - num17 * num15) / num21 / num22;
					if (Math.Abs(num24) > 1.0)
					{
						MissCount++;
						num4 = num5 - num23 / num20;
						if (MissCount > 0 && i != 0)
						{
							array2[i + 2] = true;
						}
					}
					else
					{
						array2[i + 2] = false;
						num4 = num5 + (double)Math.Sign(i) * Math.Sqrt(1.0 - num24 * num24) * num22 / num21 - num23 / num20;
					}
				}
				while (Math.Abs(num4 - num5) > 0.0001);
				EventTime[i + 2] = midT + num4 - 1.0 - deltaThours;
				double num25 = Math.Atan2(num14, num17) * (180.0 / Math.PI);
				if (num25 < 0.0)
				{
					num25 += 360.0;
				}
				PA[i + 2] = num25;
				num3 = Math.Atan2(num12, num16) * (180.0 / Math.PI);
				VA[i + 2] = num25 - num3;
				while (VA[i + 2] >= 360.0)
				{
					VA[i + 2] -= 360.0;
				}
				while (VA[i + 2] < 0.0)
				{
					VA[i + 2] += 360.0;
				}
				array[i + 2] = Math.Asin(num18) * (180.0 / Math.PI);
				if (i == 0)
				{
					if (num18 < 0.0)
					{
						array2[2] = true;
						continue;
					}
					double num24 = (num14 * num19 - num17 * num15) / num21;
					num = Math.Abs(1919.26 * num24 / (num22 + num2) / RSun[0]);
				}
				else if (num18 < 0.0)
				{
					array2[i + 2] = true;
				}
			}
			PredictionLine = "";
			EventsExist = false;
			for (int j = 0; j <= 4; j++)
			{
				if (!array2[j])
				{
					EventsExist = true;
					string text = "  ";
					if (EventTime[j] < 0.0)
					{
						EventTime[j] += 24.0;
						text = "- ";
					}
					if (EventTime[j] > 24.0)
					{
						EventTime[j] -= 24.0;
						text = "+ ";
					}
					PredictionLine = PredictionLine + "   " + Utilities.DEGtoDMS(EventTime[j], 2, 0, MinutesOnly: false) + text + string.Format("{0,4:F0}", PA[j]) + string.Format("{0,4:F0}", VA[j]);
					if (j != 1 && j != 3)
					{
						PredictionLine += string.Format("{0,3:F0}", array[j]);
					}
				}
				else if (j == 0 || j == 4)
				{
					PredictionLine += "   .. .. ..   ... ... ..";
				}
				else if (j == 1 || j == 3)
				{
					PredictionLine += "   .. .. ..   ... ...";
				}
				else
				{
					PredictionLine += "   .. .. ..   ... ... ..";
				}
			}
			if (!array2[2])
			{
				PredictionLine += string.Format("{0,9:F1}", num);
			}
			else
			{
				PredictionLine += "    .....";
			}
		}
	}
}
