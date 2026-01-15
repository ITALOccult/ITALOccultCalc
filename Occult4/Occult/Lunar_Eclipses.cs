using System;
using System.ComponentModel;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Drawing.Printing;
using System.IO;
using System.Reflection;
using System.Text;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult
{
	public class Lunar_Eclipses : Form
	{
		private const double DanjonKm = 86.9;

		private readonly string AppPath;

		private const double Radian = 180.0 / Math.PI;

		private double[] X = new double[3];

		private double[] Y = new double[3];

		private double[] D = new double[3];

		private double[] PiMoon = new double[3];

		private double[] R_Sun = new double[3];

		private double hour;

		private double JD_TT_atMidEclipse;

		private string deltaToutput = "";

		private string CurrentDate = "";

		private static double[] MoonEclipseX = new double[9];

		private static double[] MoonEclipseY = new double[9];

		private static double[] ZenithL = new double[9];

		private static double[] ZenithD = new double[9];

		private static double[] EventTime = new double[9];

		private static double[] PA = new double[9];

		private static double[] T_Offset_Chauvenet = new double[9];

		private static double[] T_Offset_Danjon = new double[9];

		private static double[] EclipseJD_TT = new double[50];

		private static bool[] NoEclipse = new bool[9];

		private static double MoonRadius;

		private static double F1_Oblate = 0.0;

		private static double F2_Oblate = 0.0;

		private static double F1_Danjon = 0.0;

		private static double F2_Danjon = 0.0;

		private static double F1_Chauvenet = 0.0;

		private static double F2_Chauvenet = 0.0;

		private static double CurrentEclipseJD_TT;

		private static double Gamma;

		private static double[,] ShadowRadius = new double[4, 9];

		private static string Prediction;

		private static string deltaTUncertainty_string = "";

		private static string EphemerisBasis = "";

		private static bool BWFlag;

		private static bool Initialised = false;

		private static int deltaTUncertainty;

		private static string PlanetaryElongations = "";

		private static string[] ElongationsLocal = new string[9];

		internal bool CancelPlanetSearch;

		private IContainer components;

		private ListBox lstEclipses;

		private NumericUpDown UpDownYear;

		private TextBox txtMoonEclipse;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem helpToolStripMenuItem1;

		private ToolStripMenuItem withPredictionToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem1;

		private ToolStripMenuItem printToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private PrintDocument printDocument1;

		private PictureBox picEarth;

		private ToolStripMenuItem drawInBWToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator1;

		private PictureBox picShadow;

		private Label label2;

		private ToolStripMenuItem saveGraphicsToolStripMenuItem;

		private ToolStripMenuItem printPreviewToolStripMenuItem;

		private ToolStripMenuItem listBesselianElementsToolStripMenuItem;

		private ToolStripMenuItem save10yrsPredictionsToolStripMenuItem;

		private ToolStripMenuItem includeChauvenetDanjonOffsetsToolStripMenuItem;

		private Label label1;

		private ToolStripMenuItem findClosePlanetaryConjunctionsToolStripMenuItem;

		public Lunar_Eclipses()
		{
			InitializeComponent();
			AppPath = Utilities.AppPath;
			BWFlag = false;
			includeChauvenetDanjonOffsetsToolStripMenuItem.set_Checked(Settings.Default.LunarEclipseOffsets);
			UpDownYear.set_Value((decimal)DateTime.Now.Year);
			Initialised = true;
			((Control)lstEclipses).Focus();
		}

		public void LunarEclipse(double JD_TT, bool IncludeMaps, out string Prediction)
		{
			double num = 0.01362466814369149;
			int Year = 0;
			int Month = 0;
			int Source = 0;
			double day = 0.0;
			double RAMoon = 0.0;
			double DecMoon = 0.0;
			double ParMoon = 0.0;
			bool flag = false;
			double num2 = 0.0;
			double num3 = 0.0;
			double num4 = 0.0;
			double num5 = 0.0;
			double num6 = 0.0;
			double num7 = 0.0;
			double num8 = 0.0;
			double num9 = 0.0;
			double num10 = 0.0;
			double num11 = 1.0;
			double num12 = 3600.0;
			double num13 = 1.0;
			double num14 = 0.0;
			double num15 = 0.0;
			double num16 = 0.0;
			double[] RA = new double[6];
			double[] Dec = new double[6];
			double[] Parallax = new double[6];
			double[] array = new double[3];
			double[] array2 = new double[3];
			double[] array3 = new double[3];
			double[] array4 = new double[3];
			double[] array5 = new double[3];
			double[] array6 = new double[3];
			double[] array7 = new double[3];
			double[] array8 = new double[3];
			_ = new double[3];
			float LongCorrection_arcsec1e = 0f;
			float LatCorrection_arcsec1e = 0f;
			string text = "";
			string text2 = "";
			StringBuilder stringBuilder = new StringBuilder();
			Utilities.MoonSeries_Retrieve(JD_TT, out RA, out Dec, out Parallax, out LongCorrection_arcsec1e, out LatCorrection_arcsec1e, out Source);
			Utilities.Date_from_JD(JD_TT, out Year, out Month, out day);
			double num17 = Utilities.delta_T(Year, Month, day);
			double num18 = num17 / 3600.0;
			EphemerisBasis = Utilities.EphemerisBasis(JD_TT);
			Utilities.deltaT_Uncertainty(Year, out deltaTUncertainty, out deltaTUncertainty_string);
			flag = Math.Abs(deltaTUncertainty) < 3600;
			double RAMoon2 = 0.0;
			double DecMoon2 = 0.0;
			double ParMoon2 = 0.0;
			double RA2 = 0.0;
			double Dec2 = 0.0;
			double GeocentricDistance = 0.0;
			Utilities.MoonPositionFromSeries(0.0, RA, Dec, Parallax, out RAMoon2, out DecMoon2, out ParMoon2);
			Utilities.QuickPlanet(JD_TT, 3, EquinoxOfDate: true, out RA2, out Dec2, out GeocentricDistance);
			double RAMoon3 = 0.0;
			double DecMoon3 = 0.0;
			double ParMoon3 = 0.0;
			double RA3 = 0.0;
			double Dec3 = 0.0;
			double GeocentricDistance2 = 0.0;
			Utilities.MoonPositionFromSeries(1.0, RA, Dec, Parallax, out RAMoon3, out DecMoon3, out ParMoon3);
			Utilities.QuickPlanet(JD_TT + 1.0, 3, EquinoxOfDate: true, out RA3, out Dec3, out GeocentricDistance2);
			double num19 = RAMoon2 - RA2 * (180.0 / Math.PI);
			if (num19 > 360.0)
			{
				num19 -= 360.0;
			}
			if (num19 < 0.0)
			{
				num19 += 360.0;
			}
			double num20 = RAMoon3 - RA3 * (180.0 / Math.PI);
			if (num20 > 360.0)
			{
				num20 -= 360.0;
			}
			if (num20 < 0.0)
			{
				num20 += 360.0;
			}
			hour = (int)((180.0 - num19) / (num20 - num19) * 24.0);
			for (int i = 0; i <= 8; i++)
			{
				NoEclipse[i] = false;
			}
			for (int j = 0; j <= 2; j++)
			{
				Utilities.MoonPositionFromSeries((hour + (double)j - 1.0) / 24.0, RA, Dec, Parallax, out array4[j], out array5[j], out array6[j]);
				Utilities.PlanetGeocentric(JD_TT + (hour + (double)j - 1.0) / 24.0, 3, 0.0, 0, out array[j], out array2[j], out array3[j]);
				array7[j] = Math.Cos(array5[j] / (180.0 / Math.PI)) * Math.Sin(array4[j] / (180.0 / Math.PI) - array[j] + Math.PI) * (180.0 / Math.PI) * 3600.0;
				array8[j] = (Math.Sin(array5[j] / (180.0 / Math.PI)) * Math.Cos(0.0 - array2[j]) - Math.Cos(array5[j] / (180.0 / Math.PI)) * Math.Sin(0.0 - array2[j]) * Math.Cos(array4[j] / (180.0 / Math.PI) - array[j] + Math.PI)) * (180.0 / Math.PI) * 3600.0;
				MoonRadius = Math.Asin(0.2725076 * Math.Sin(array6[j] / (180.0 / Math.PI))) * (180.0 / Math.PI) * 3600.0;
			}
			X[0] = array7[0];
			X[2] = (array7[2] - 2.0 * array7[1] + array7[0]) / 2.0;
			X[1] = array7[1] - array7[0] - X[2];
			Y[0] = array8[0];
			Y[2] = (array8[2] - 2.0 * array8[1] + array8[0]) / 2.0;
			Y[1] = array8[1] - array8[0] - Y[2];
			PiMoon[0] = array6[0];
			PiMoon[2] = (array6[2] - 2.0 * array6[1] + array6[0]) / 2.0;
			PiMoon[1] = array6[1] - array6[0] - PiMoon[2];
			R_Sun[0] = array3[0];
			R_Sun[2] = (array3[2] - 2.0 * array3[1] + array3[0]) / 2.0;
			R_Sun[1] = array3[1] - array3[0] - R_Sun[2];
			for (int k = 0; k < 3; k++)
			{
				for (int l = 0; l < 9; l++)
				{
					ShadowRadius[k, l] = 0.0;
				}
			}
			for (int m = 1; m <= 4; m++)
			{
				for (int n = -1; n <= 1; n += 2)
				{
					int num21 = m;
					if (n == 1)
					{
						num21 = 8 - m;
					}
					double num22 = 0.0;
					int num23 = 0;
					double num24;
					double num27;
					double num28;
					do
					{
						num24 = num22;
						num2 = X[0] + X[1] * num24 + X[2] * num24 * num24;
						num3 = Y[0] + Y[1] * num24 + Y[2] * num24 * num24;
						num4 = X[1] + 2.0 * X[2] * num24;
						num5 = Y[1] + 2.0 * Y[2] * num24;
						num12 = PiMoon[0] + PiMoon[1] * num24 + PiMoon[2] * num24 * num24;
						MoonRadius = Math.Asin(0.2725076 * Math.Sin(num12 / (180.0 / Math.PI))) * (180.0 / Math.PI) * 3600.0;
						num12 *= 3600.0;
						num13 = R_Sun[0] + R_Sun[1] * num24 + R_Sun[2] * num24 * num24;
						num10 = Math.Atan(Math.Abs(num3 / num2));
						num11 = 0.99834;
						for (int num25 = 0; num25 <= 1; num25++)
						{
							num9 = (959.63 - num11 * 8.794143836182533) / num13 / 3600.0 / (180.0 / Math.PI);
							double num26 = Math.Cos(array2[1] + num9 * Math.Sin(num10)) * Math.Sin(num10);
							num11 = 1.0 - 0.003353 * num26 * num26;
						}
						F1_Oblate = (num11 + num) * num12 + 959.63 / num13 + 8.794143836182533 / num13;
						F1_Chauvenet = 1.02 * (0.99834 * num12 + 959.63 / num13 + 8.794143836182533 / num13);
						F1_Danjon = 1.01 * num12 + 959.63 / num13 + 8.794143836182533 / num13;
						F2_Oblate = (num11 + num) * num12 - 959.63 / num13 + 8.794143836182533 / num13;
						F2_Chauvenet = 1.02 * (0.99834 * num12 - 959.63 / num13 + 8.794143836182533 / num13);
						F2_Danjon = 1.01 * num12 - 959.63 / num13 + 8.794143836182533 / num13;
						if (m == 1)
						{
							ShadowRadius[0, num21] = F1_Oblate;
							ShadowRadius[1, num21] = F1_Chauvenet;
							ShadowRadius[2, num21] = F1_Danjon;
						}
						else
						{
							ShadowRadius[0, num21] = F2_Oblate;
							ShadowRadius[1, num21] = F2_Chauvenet;
							ShadowRadius[2, num21] = F2_Danjon;
						}
						ShadowRadius[3, num21] = MoonRadius;
						switch (m)
						{
						case 1:
							num14 = F1_Oblate + MoonRadius;
							num16 = F1_Chauvenet + MoonRadius;
							num15 = F1_Danjon + MoonRadius;
							break;
						case 2:
							num14 = F2_Oblate + MoonRadius;
							num16 = F2_Chauvenet + MoonRadius;
							num15 = F2_Danjon + MoonRadius;
							break;
						default:
							num14 = F2_Oblate - MoonRadius;
							num16 = F2_Chauvenet - MoonRadius;
							num15 = F2_Danjon - MoonRadius;
							break;
						}
						num27 = num4 * num4 + num5 * num5;
						num28 = Math.Abs(num2 * num5 - num3 * num4) / Math.Sqrt(num27);
						if (m == 4)
						{
							num22 = num24 - (num2 * num4 + num3 * num5) / num27;
							num6 = (F2_Oblate + MoonRadius - Math.Sqrt(num2 * num2 + num3 * num3)) / (2.0 * MoonRadius);
							num7 = (F2_Chauvenet + MoonRadius - Math.Sqrt(num2 * num2 + num3 * num3)) / (2.0 * MoonRadius);
							num8 = (F2_Danjon + MoonRadius - Math.Sqrt(num2 * num2 + num3 * num3)) / (2.0 * MoonRadius);
							text = "Umbral";
							if (num6 < 0.0)
							{
								text = "Penumbral";
								num6 = (F1_Oblate + MoonRadius - Math.Sqrt(num2 * num2 + num3 * num3)) / (2.0 * MoonRadius);
								num7 = (F1_Chauvenet + MoonRadius - Math.Sqrt(num2 * num2 + num3 * num3)) / (2.0 * MoonRadius);
								num8 = (F1_Danjon + MoonRadius - Math.Sqrt(num2 * num2 + num3 * num3)) / (2.0 * MoonRadius);
								ShadowRadius[0, 4] = F1_Oblate;
								ShadowRadius[1, 4] = F1_Chauvenet;
								ShadowRadius[2, 4] = F1_Danjon;
							}
							Gamma = Math.Sqrt(num2 * num2 + num3 * num3) / 3600.0 / PiMoon[0] * (double)Math.Sign(num3);
						}
						else if (Math.Abs(num14) > Math.Abs(num28))
						{
							num22 = num24 + (double)n * Math.Sqrt((num14 * num14 - num28 * num28) / num27) - (num2 * num4 + num3 * num5) / num27;
						}
						else
						{
							num22 = (num24 = 0.0);
							NoEclipse[m] = true;
						}
						num23++;
					}
					while (Math.Abs(num24 - num22) > 0.0001 && num23 < 6);
					T_Offset_Chauvenet[num21] = 3600.0 * ((double)n * Math.Sqrt((num16 * num16 - num28 * num28) / num27) - (num2 * num4 + num3 * num5) / num27);
					T_Offset_Danjon[num21] = 3600.0 * ((double)n * Math.Sqrt((num15 * num15 - num28 * num28) / num27) - (num2 * num4 + num3 * num5) / num27);
					EventTime[num21] = hour - 1.0 + num22;
					if (m == 4)
					{
						JD_TT_atMidEclipse = JD_TT + (hour - 1.0 + num22) / 24.0;
					}
					if (flag)
					{
						EventTime[num21] -= num18;
					}
					PA[num21] = Math.Atan2(num2, num3) * (180.0 / Math.PI);
					if (m < 3)
					{
						PA[num21] += 180.0;
					}
					while (PA[num21] < 0.0)
					{
						PA[num21] += 360.0;
					}
					while (PA[num21] > 360.0)
					{
						PA[num21] -= 360.0;
					}
					MoonEclipseX[num21] = num2;
					MoonEclipseY[num21] = num3;
					Utilities.MoonPositionFromSeries(EventTime[num21] / 24.0, RA, Dec, Parallax, out RAMoon, out DecMoon, out ParMoon);
					ZenithL[num21] = RAMoon - Utilities.SiderealTime_deg(JD_TT, Apparent: false) - 15.041067 * EventTime[num21];
					if (!flag)
					{
						ZenithL[num21] -= -15.041067 * num18;
					}
					while (ZenithL[num21] < -180.0)
					{
						ZenithL[num21] += 360.0;
					}
					while (ZenithL[num21] > 180.0)
					{
						ZenithL[num21] -= 360.0;
					}
					ZenithD[num21] = DecMoon;
				}
			}
			double partial_Semiduration = (EventTime[6] - EventTime[2]) / 48.0;
			if (NoEclipse[6] | NoEclipse[2])
			{
				partial_Semiduration = 0.0;
			}
			Utilities.Planet_LunarEclipseElongations(JD_TT_atMidEclipse, partial_Semiduration, (ShadowRadius[0, 4] + num12) / 3600.0, out ElongationsLocal);
			string text3 = "TT ";
			if (flag)
			{
				text3 = ((!((Year > 1970) & (Year < DateTime.Now.Year + 10))) ? "UT " : "UTC");
			}
			CurrentDate = string.Format("{0,6:F0} {1,1:A0} {2,2:F0} (TT)", Year, Utilities.ShortMonths[Month], day);
			stringBuilder.AppendFormat("         L U N A R   E C L I P S E   on   " + CurrentDate);
			stringBuilder.Append("\r\n\r\n" + "Overhead at".PadLeft(56));
			if (includeChauvenetDanjonOffsetsToolStripMenuItem.get_Checked())
			{
				stringBuilder.Append("  Offsets");
			}
			stringBuilder.Append("\r\n     Event                      " + text3 + "     P.A.  Long  Lat");
			if (includeChauvenetDanjonOffsetsToolStripMenuItem.get_Checked())
			{
				stringBuilder.Append("   Cha Dan");
			}
			stringBuilder.Append("\r\n" + "".PadLeft(30) + "h  m  s     o      o    o");
			if (includeChauvenetDanjonOffsetsToolStripMenuItem.get_Checked())
			{
				stringBuilder.Append("     s   s");
			}
			stringBuilder.Append("\r\n");
			if (NoEclipse[1])
			{
				stringBuilder.Append("\r\nNo Lunar eclipse on this date");
			}
			else
			{
				for (int num29 = 1; num29 < 8; num29++)
				{
					if ((((num29 < 4) & !NoEclipse[num29]) | ((num29 > 4) & !NoEclipse[8 - num29])) || num29 == 4)
					{
						switch (num29)
						{
						case 1:
							stringBuilder.Append("\r\n[1]  Moon Enters Penumbra");
							break;
						case 2:
							stringBuilder.Append("\r\n[2]  Moon Enters Umbra   ");
							break;
						case 3:
							stringBuilder.Append("\r\n[3]  Total Eclipse Starts");
							break;
						case 4:
							stringBuilder.Append("\r\n[4]  Maximum Eclipse     ");
							break;
						case 5:
							stringBuilder.Append("\r\n[5]  Total Eclipse Ends  ");
							break;
						case 6:
							stringBuilder.Append("\r\n[6]  Moon Leaves Umbra   ");
							break;
						case 7:
							stringBuilder.Append("\r\n[7]  Moon Leaves Penumbra");
							break;
						}
						string text4 = "  ";
						if (EventTime[num29] < 0.0)
						{
							EventTime[num29] += 24.0;
							text4 = "- ";
						}
						if (EventTime[num29] > 24.0)
						{
							EventTime[num29] -= 24.0;
							text4 = "+ ";
						}
						stringBuilder.Append(text2 + "    " + Utilities.DEGtoDMS(EventTime[num29], 2, 0, MinutesOnly: false) + text4);
						if (num29 != 4)
						{
							stringBuilder.AppendFormat("{0,4:F0}", PA[num29]);
						}
						else
						{
							stringBuilder.Append("    ");
						}
						stringBuilder.AppendFormat("{0,7:F0}{1,5:F0}", ZenithL[num29], ZenithD[num29]);
						if ((num29 != 4) & Settings.Default.LunarEclipseOffsets)
						{
							stringBuilder.AppendFormat("{0,6:f0}{1,4:f0}", T_Offset_Chauvenet[num29], T_Offset_Danjon[num29]);
						}
						else
						{
							stringBuilder.Append("".PadLeft(10));
						}
					}
				}
				stringBuilder.AppendLine("\r\n");
				stringBuilder.AppendFormat("     Magnitude of {0} Eclipse = {1,5:F3}", text, num6);
				string text5 = "\r\n".PadRight(25);
				if (text.Contains("Pen"))
				{
					text5 = "\r\n".PadRight(28);
				}
				if (includeChauvenetDanjonOffsetsToolStripMenuItem.get_Checked())
				{
					stringBuilder.AppendFormat(text5 + "(Chauvenet: {0,5:F3}, Danjon: {1,5:F3})\r\n", num7, num8);
				}
				else
				{
					stringBuilder.Append("\r\n");
				}
				PlanetaryElongations = "";
				for (int num30 = 1; num30 < 9; num30++)
				{
					if (num30 != 3 && ElongationsLocal[num30].Length > 1)
					{
						PlanetaryElongations = PlanetaryElongations + Utilities.Planets[num30] + " " + ElongationsLocal[num30] + "  ";
					}
				}
				if (PlanetaryElongations.Length > 1)
				{
					stringBuilder.AppendLine("     Elongations:  " + PlanetaryElongations + "\r\n");
				}
				else
				{
					stringBuilder.AppendLine();
				}
				if (Math.Abs(num17) > 86400.0)
				{
					int num31 = (int)(num17 / 86400.0);
					deltaToutput = string.Format("delta T = {0,1:F0}d {1,1:F1}h", num31, (num17 - (double)(86400 * num31)) / 3600.0);
				}
				else if (Math.Abs(num17) > 1800.0)
				{
					deltaToutput = string.Format("delta T = {0,1:F2} hrs", num17 / 3600.0);
				}
				else if (Math.Abs(num17) > 144.0)
				{
					int num32 = (int)(num18 * 60.0);
					deltaToutput = string.Format("delta T = {0,1:F0}m {1,1:F0}s", num32, 60.0 * (60.0 * num18 - (double)num32));
				}
				else if (Math.Abs(num17) > 100.0)
				{
					deltaToutput = string.Format("delta T = {0,1:F0} secs", num17);
				}
				else
				{
					deltaToutput = string.Format("delta T = {0,1:F1} secs", num17);
				}
				stringBuilder.Append("     " + deltaToutput);
				stringBuilder.Append(deltaTUncertainty_string);
				stringBuilder.AppendLine(",   Ephemeris = " + EphemerisBasis);
			}
			Prediction = stringBuilder.ToString();
			Application.DoEvents();
			if (IncludeMaps)
			{
				PlotEarthInEclipse();
				PlotShadow();
			}
		}

		public void FindLunarEclipse(int Year_TT)
		{
			double RA = 0.0;
			double Dec = 0.0;
			double Distance = 0.0;
			double Longitude = 0.0;
			double Latitude = 0.0;
			double PlanetHeliocentricDistance = 0.0;
			int num = 0;
			lstEclipses.get_Items().Clear();
			double num2 = Utilities.JD_from_Date(Year_TT, 1, 1.0);
			double num3 = Utilities.JD_from_Date(Year_TT + 10, 1, 1.0);
			_ = Utilities.delta_T(Year_TT, 7, 1.0) / 3600.0;
			double num4 = num2;
			do
			{
				double num5 = 0.0;
				double MoonLatitude;
				double num6;
				do
				{
					Utilities.QuickMoon(num4 + num5, out var _, out var _, out var _, out var MoonLongitude, out MoonLatitude);
					Utilities.QuickPlanet(num4 + num5, 3, EquinoxOfDate: true, out RA, out Dec, out Distance, out Longitude, out Latitude, out PlanetHeliocentricDistance);
					num6 = (Longitude - MoonLongitude) * (180.0 / Math.PI) - 180.0;
					if (num6 < -90.0)
					{
						num6 += 360.0;
					}
					if (num6 > 270.0)
					{
						num6 -= 360.0;
					}
					num5 += num6 / 12.191;
				}
				while (Math.Abs(num6) > 0.003);
				MoonLatitude *= 180.0 / Math.PI;
				if (Math.Abs(MoonLatitude) < 1.7)
				{
					string text = ((Math.Abs(MoonLatitude) > 1.45) ? "Possible Penumbral Eclipse " : ((Math.Abs(MoonLatitude) > 1.15) ? "Penumbral Eclipse " : ((!(Math.Abs(MoonLatitude) > 0.85)) ? "Umbral Eclipse " : "Possible Umbral Eclipse ")));
					EclipseJD_TT[num] = Math.Floor(num4 + num5 - 0.5) + 0.5;
					StringBuilder stringBuilder = new StringBuilder();
					stringBuilder.Append(Utilities.Date_from_JD(EclipseJD_TT[num], 0, Use_BC: false));
					stringBuilder.AppendFormat(" {0,4:F1}hr   ", (num4 + num5 - EclipseJD_TT[num]) * 24.0);
					lstEclipses.get_Items().Add((object)(stringBuilder.ToString() + text));
					num++;
				}
				num4 = num4 + num5 + 29.5;
			}
			while (num4 < num3);
			((ListControl)lstEclipses).set_SelectedIndex(0);
			((Control)lstEclipses).Focus();
		}

		private void UpDownYear_ValueChanged(object sender, EventArgs e)
		{
			FindLunarEclipse((int)UpDownYear.get_Value());
			((Control)lstEclipses).Focus();
		}

		private void copyToolStripMenuItem1_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(Prediction);
		}

		private void printPreviewToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.PrintPreviewText(Prediction);
		}

		private void printToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.PrintText(Prediction);
		}

		private void PlotEarthInEclipse()
		{
			string text = "";
			Bitmap image = new Bitmap(((Control)picEarth).get_Width(), ((Control)picEarth).get_Height());
			Graphics graphics = Graphics.FromImage(image);
			if (Settings.Default.GraphicsSmoothed)
			{
				graphics.SmoothingMode = SmoothingMode.AntiAlias;
			}
			Font font = new Font("Courier New", 8f);
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			Brush brush2;
			Brush brush;
			if (BWFlag)
			{
				graphics.Clear(Color.White);
				brush2 = (brush = Brushes.Black);
			}
			else
			{
				graphics.Clear(Color.Black);
				brush2 = Brushes.Yellow;
				brush = Brushes.LightGray;
			}
			float num = (float)((double)((Control)picEarth).get_Width() / 7.6);
			float num2 = num / 2f;
			for (int i = 1; i <= 7; i++)
			{
				if ((((i < 4) & !NoEclipse[i]) | ((i > 4) & !NoEclipse[8 - i])) || i == 4)
				{
					Maps.EarthGlobe(graphics, (float)((double)num / 2.2), (float)(((double)i - 0.2) * (double)num), num2, ZenithL[i], ZenithD[i], 0.0, 0.0, 0.0, 0.0, ShowSunLit: false, SiteCentered: false, PlotCities: false, FullResolution: false, BWFlag, Mirrored: false);
					string s = "[" + Convert.ToString(i) + "]";
					graphics.DrawString(s, font, brush2, (float)(((double)i - 0.2) * (double)num - 12.0), 2f * num2);
				}
			}
			font = new Font("Times New Roman", 9f, FontStyle.Regular);
			graphics.DrawString(CurrentDate, font, brush, 6f, ((Control)picEarth).get_Height() - 17);
			font = new Font("Times New Roman", 8f, FontStyle.Regular);
			text = "Occult " + Assembly.GetExecutingAssembly().GetName(copiedName: false).Version;
			graphics.DrawString(text, font, brush, (float)(((Control)picEarth).get_Width() - 2) - graphics.MeasureString(CurrentDate, font).Width, ((Control)picEarth).get_Height() - 15);
			if (deltaTUncertainty > 1)
			{
				double num3 = (double)deltaTUncertainty / 240.0;
				string text2 = "Uncertainty in Earth's orientation = ±";
				text2 = ((num3 < 10.0) ? (text2 + string.Format("{0,1:f1} degrees", num3)) : ((!(num3 < 180.0)) ? (text2 + string.Format("{0,1:f1} revolutions", num3 / 360.0)) : (text2 + string.Format("{0,1:f0} degrees", num3))));
				float width = graphics.MeasureString(text2, font).Width;
				graphics.DrawString(text2, font, brush, ((float)((Control)picEarth).get_Width() - width) / 2f, ((Control)picEarth).get_Height() - 15);
			}
			picEarth.set_Image((Image)image);
			graphics.Dispose();
			((Control)this).set_Cursor(Cursors.get_Default());
		}

		private void PlotShadow()
		{
			Bitmap image = new Bitmap(((Control)picShadow).get_Width(), ((Control)picShadow).get_Height());
			Graphics graphics = Graphics.FromImage(image);
			if (Settings.Default.GraphicsSmoothed)
			{
				graphics.SmoothingMode = SmoothingMode.AntiAlias;
			}
			Font font = new Font("Courier New", 8f);
			Pen pen = new Pen(Color.Black);
			float num = ((Control)picShadow).get_Width() / 2;
			float num2 = ((Control)picShadow).get_Height() / 2;
			float num3 = (float)((double)((Control)picShadow).get_Width() / MoonRadius / 16.0);
			float num4 = (float)(MoonEclipseX[4] / 2.0);
			float num5 = (float)(MoonEclipseY[4] / 2.0);
			float num6 = num + num4 * num3;
			float num7 = num2 + num5 * num3;
			pen.Color = Color.Black;
			Brush black = Brushes.Black;
			if (BWFlag)
			{
				graphics.Clear(Color.White);
				float num8 = (float)(F1_Oblate * (double)num3);
				graphics.DrawEllipse(pen, num - num8, num2 - num8, 2f * num8, 2f * num8);
				num8 = (float)(F2_Oblate * (double)num3);
				graphics.DrawEllipse(pen, num - num8, num2 - num8, 2f * num8, 2f * num8);
			}
			else
			{
				graphics.Clear(Color.LightGray);
				for (int i = 0; (float)i < 30f; i++)
				{
					float num9 = (float)((F2_Oblate + (F1_Oblate - F2_Oblate) * (double)(1f - (float)i / 30f)) * (double)num3);
					SolidBrush brush = new SolidBrush(Color.FromArgb((int)(200f - 3.6666667f * (float)i), (int)(200f - 5f * (float)i), (int)(200f - 5.3333335f * (float)i)));
					graphics.FillEllipse(brush, num - num9, num2 - num9, 2f * num9, 2f * num9);
				}
				for (int i = 0; (float)i < 30f; i++)
				{
					float num9 = (float)(F2_Oblate * (double)num3 * (double)(1f - (float)i / 30f));
					SolidBrush brush2 = new SolidBrush(Color.FromArgb(90 - 2 * i, (int)(50.0 - 0.7 * (double)i), 40));
					graphics.FillEllipse(brush2, num - num9, num2 - num9, 2f * num9, 2f * num9);
				}
			}
			for (int i = 1; i <= 7; i++)
			{
				if ((((i < 4) & !NoEclipse[i]) | ((i > 4) & !NoEclipse[8 - i])) || i == 4)
				{
					num6 = (float)((double)num - MoonEclipseX[i] * (double)num3);
					num7 = (float)((double)num2 - MoonEclipseY[i] * (double)num3);
					float num8 = (float)(MoonRadius * (double)num3);
					if (BWFlag)
					{
						pen.Color = Color.Black;
						black = Brushes.Black;
					}
					else
					{
						pen.Color = Color.DarkGoldenrod;
						black = Brushes.Black;
					}
					graphics.DrawEllipse(pen, num6 - num8, num7 - num8, 2f * num8, 2f * num8);
					if (i == 1 || i == 7)
					{
						string s = "{" + Convert.ToString(i) + "}";
						graphics.DrawString(s, font, black, num6 - 11f, num7 - 8f);
					}
				}
			}
			font = new Font("Times New Roman", 12f, FontStyle.Bold);
			graphics.DrawString(CurrentDate, font, black, (float)((Control)picShadow).get_Width() / 2f - graphics.MeasureString(CurrentDate, font).Width / 2f, ((Control)picShadow).get_Height() - 20);
			font = new Font("Times New Roman", 10f, FontStyle.Bold);
			graphics.DrawString("N", font, black, (float)((Control)picShadow).get_Width() / 2f - 5f, 3f);
			graphics.DrawString("E", font, black, 3f, (float)((Control)picShadow).get_Height() / 2f - 7f);
			font = new Font("Times New Roman", 7f, FontStyle.Regular);
			graphics.DrawString("Occult " + Assembly.GetExecutingAssembly().GetName(copiedName: false).Version, font, black, 6f, ((Control)picShadow).get_Height() - 15);
			picShadow.set_Image((Image)image);
			graphics.Dispose();
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
		}

		private void drawInBWToolStripMenuItem_Click(object sender, EventArgs e)
		{
			BWFlag = !BWFlag;
			if (BWFlag)
			{
				((ToolStripItem)drawInBWToolStripMenuItem).set_Text("Draw in colour");
			}
			else
			{
				((ToolStripItem)drawInBWToolStripMenuItem).set_Text("Draw in B&&W");
			}
			PlotEarthInEclipse();
			PlotShadow();
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings.Default.Save_LunarEclipses = Output.SaveAppendPredictionText(Prediction, Utilities.Date_from_JD(CurrentEclipseJD_TT, 0) + " Lunar eclipse", Settings.Default.Save_LunarEclipses);
		}

		private void saveGraphicsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings.Default.Save_LunarEclipses = Output.SaveGraphic(picShadow.get_Image(), Utilities.Date_from_JD(CurrentEclipseJD_TT, 0) + "Lunar eclipse shadow", Settings.Default.Save_LunarEclipses);
			Settings.Default.Save_LunarEclipses = Output.SaveGraphic(picEarth.get_Image(), Utilities.Date_from_JD(CurrentEclipseJD_TT, 0) + "Lunar eclipse Earthmaps", Settings.Default.Save_LunarEclipses);
		}

		private void lstEclipses_SelectedIndexChanged(object sender, EventArgs e)
		{
			GeneratePrediction();
		}

		private void lstEclipses_DoubleClick(object sender, EventArgs e)
		{
			GeneratePrediction();
		}

		private void GeneratePrediction()
		{
			int selectedIndex = ((ListControl)lstEclipses).get_SelectedIndex();
			CurrentEclipseJD_TT = EclipseJD_TT[selectedIndex];
			LunarEclipse(CurrentEclipseJD_TT, IncludeMaps: true, out Prediction);
			((Control)txtMoonEclipse).set_Text(Prediction);
		}

		internal void GeneratePrediction(double CurrentEclipseJD_TT)
		{
			LunarEclipse(CurrentEclipseJD_TT, IncludeMaps: true, out Prediction);
			((Control)txtMoonEclipse).set_Text(Prediction);
		}

		private void UpDownYear_Enter(object sender, EventArgs e)
		{
			((UpDownBase)UpDownYear).Select(0, 10);
		}

		private void helpToolStripMenuItem1_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Lunar Eclipses");
		}

		private void Lunar_Eclipses_Load(object sender, EventArgs e)
		{
			if (((Control)this).get_Top() < -30000)
			{
				((Control)this).set_Top(0);
			}
			if ((double)((Control)this).get_Left() + (double)((Control)this).get_Width() / 1.2 < (double)Screen.GetWorkingArea((Control)(object)this).Left)
			{
				((Control)this).set_Left(Screen.GetWorkingArea((Control)(object)this).Left);
			}
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
		}

		private void listBesselianElementsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0239: Unknown result type (might be due to invalid IL or missing references)
			MoonSeriesViewer moonSeriesViewer = new MoonSeriesViewer();
			string[] array = new string[20]
			{
				"Besselian elements for the lunar eclipse of", null, null, null, null, null, null, null, null, null,
				null, null, null, null, null, null, null, null, null, null
			};
			Utilities.Date_from_JD(CurrentEclipseJD_TT, out var Year, out var Month, out var day);
			array[1] = Year + " " + Utilities.ShortMonths[Month] + string.Format(" {0,1:f0}", day);
			array[2] = "  ";
			array[3] = string.Format("Zero time for hourly series is {0,1:F0}h TT", hour - 1.0);
			array[4] = deltaToutput + deltaTUncertainty_string + ",   Ephemeris = " + EphemerisBasis;
			array[5] = "  ";
			array[6] = string.Format("  X ={0,9:f2}\",   Y ={1,9:f2}\"", X[0], Y[0]);
			array[7] = string.Format(" dX ={0,9:f2}\",  dY ={1,9:f2}\"", X[1], Y[1]);
			array[8] = string.Format("d2X ={0,9:f2}\", d2Y ={1,9:f2}\"", X[2], Y[2]);
			array[9] = string.Format("Gamma = {0,1:f4}", Gamma);
			array[10] = "  ";
			array[11] = "             Shadow radii            Moon";
			array[12] = "      Oblate   Chauvenet  Danjon    Radius";
			for (int i = 1; i <= 7; i++)
			{
				if ((((i < 4) & !NoEclipse[i]) | ((i > 4) & !NoEclipse[8 - i])) || i == 4)
				{
					array[12 + i] = string.Format("({0,1:f0}) {1,8:f1}\" {2,8:f1}\" {3,8:f1}\" {4,8:f1}\"", i, ShadowRadius[0, i], ShadowRadius[1, i], ShadowRadius[2, i], ShadowRadius[3, i]);
				}
				else
				{
					array[12 + i] = string.Format("({0,1:f0})", i);
				}
			}
			moonSeriesViewer.ShowElements(array);
			((Control)moonSeriesViewer).set_Text("Lunar eclipse - Besselian elements viewer");
			moonSeriesViewer.HelpKeyword = "Lunar eclipse elements";
			((Form)moonSeriesViewer).ShowDialog();
		}

		private void save10yrsPredictionsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			using StreamWriter streamWriter = new StreamWriter(Utilities.AppPath + "\\Predictions\\LunarEclipses 10Yrs from " + UpDownYear.get_Value() + ".txt");
			for (int i = 0; i < lstEclipses.get_Items().get_Count(); i++)
			{
				CurrentEclipseJD_TT = EclipseJD_TT[i];
				LunarEclipse(CurrentEclipseJD_TT, IncludeMaps: true, out Prediction);
				streamWriter.Write(Prediction + "\r\n");
			}
		}

		private void includeChauvenetDanjonOffsetsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			includeChauvenetDanjonOffsetsToolStripMenuItem.set_Checked(!includeChauvenetDanjonOffsetsToolStripMenuItem.get_Checked());
			Settings.Default.LunarEclipseOffsets = includeChauvenetDanjonOffsetsToolStripMenuItem.get_Checked();
			if (Initialised)
			{
				GeneratePrediction();
			}
		}

		internal void FindPlanetConjunctions(int StartYear, int EndYear)
		{
			if (EndYear <= StartYear)
			{
				return;
			}
			SolarEclipses.ClosePlanetaryConjunctions.lstEclipses.get_Items().Clear();
			SolarEclipses.ClosePlanetaryConjunctions.EclipseJD_TT_Planets.Clear();
			SolarEclipses.ClosePlanetaryConjunctions.pBar.set_Minimum(0);
			SolarEclipses.ClosePlanetaryConjunctions.pBar.set_Maximum(EndYear - StartYear);
			((Control)SolarEclipses.ClosePlanetaryConjunctions.pBar).set_Visible(true);
			CancelPlanetSearch = false;
			for (int i = StartYear; i < EndYear; i += 10)
			{
				SolarEclipses.ClosePlanetaryConjunctions.pBar.set_Value(i - StartYear);
				FindLunarEclipse(i);
				for (int j = 0; j < lstEclipses.get_Items().get_Count(); j++)
				{
					LunarEclipse(EclipseJD_TT[j], IncludeMaps: false, out var _);
					if (NoEclipse[1])
					{
						continue;
					}
					string planetaryElongations = PlanetaryElongations;
					if (planetaryElongations.Length > 1)
					{
						StringBuilder stringBuilder = new StringBuilder();
						stringBuilder.Append(CurrentDate.Replace(" (TT)", "").TrimEnd(Array.Empty<char>()));
						if (planetaryElongations.Contains("‡"))
						{
							stringBuilder.Append(" ☺ ");
						}
						else if (planetaryElongations.Contains("†"))
						{
							stringBuilder.Append(" ☼ ");
						}
						else
						{
							stringBuilder.Append("   ");
						}
						stringBuilder.Append(planetaryElongations);
						SolarEclipses.ClosePlanetaryConjunctions.lstEclipses.get_Items().Add((object)stringBuilder.ToString());
						SolarEclipses.ClosePlanetaryConjunctions.EclipseJD_TT_Planets.Add(EclipseJD_TT[j]);
					}
				}
				Application.DoEvents();
				Utilities.PurgeLunarEphemerisCache();
				if (CancelPlanetSearch)
				{
					break;
				}
			}
			((Control)SolarEclipses.ClosePlanetaryConjunctions.pBar).set_Visible(false);
		}

		private void findClosePlanetaryConjunctionsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SolarEclipses.ShowClosePlanets(LunarEclipse: true);
		}

		protected override void Dispose(bool disposing)
		{
			if (disposing && components != null)
			{
				components.Dispose();
			}
			((Form)this).Dispose(disposing);
		}

		private void InitializeComponent()
		{
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			//IL_001b: Expected O, but got Unknown
			//IL_001c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0026: Expected O, but got Unknown
			//IL_0027: Unknown result type (might be due to invalid IL or missing references)
			//IL_0031: Expected O, but got Unknown
			//IL_0032: Unknown result type (might be due to invalid IL or missing references)
			//IL_003c: Expected O, but got Unknown
			//IL_003d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0047: Expected O, but got Unknown
			//IL_0048: Unknown result type (might be due to invalid IL or missing references)
			//IL_0052: Expected O, but got Unknown
			//IL_0053: Unknown result type (might be due to invalid IL or missing references)
			//IL_005d: Expected O, but got Unknown
			//IL_005e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0068: Expected O, but got Unknown
			//IL_0069: Unknown result type (might be due to invalid IL or missing references)
			//IL_0073: Expected O, but got Unknown
			//IL_0074: Unknown result type (might be due to invalid IL or missing references)
			//IL_007e: Expected O, but got Unknown
			//IL_007f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0089: Expected O, but got Unknown
			//IL_008a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0094: Expected O, but got Unknown
			//IL_0095: Unknown result type (might be due to invalid IL or missing references)
			//IL_009f: Expected O, but got Unknown
			//IL_00a0: Unknown result type (might be due to invalid IL or missing references)
			//IL_00aa: Expected O, but got Unknown
			//IL_00ab: Unknown result type (might be due to invalid IL or missing references)
			//IL_00b5: Expected O, but got Unknown
			//IL_00b6: Unknown result type (might be due to invalid IL or missing references)
			//IL_00c0: Expected O, but got Unknown
			//IL_00c1: Unknown result type (might be due to invalid IL or missing references)
			//IL_00cb: Expected O, but got Unknown
			//IL_00d7: Unknown result type (might be due to invalid IL or missing references)
			//IL_00e1: Expected O, but got Unknown
			//IL_00e2: Unknown result type (might be due to invalid IL or missing references)
			//IL_00ec: Expected O, but got Unknown
			//IL_00ed: Unknown result type (might be due to invalid IL or missing references)
			//IL_00f7: Expected O, but got Unknown
			//IL_00f8: Unknown result type (might be due to invalid IL or missing references)
			//IL_0102: Expected O, but got Unknown
			//IL_0103: Unknown result type (might be due to invalid IL or missing references)
			//IL_010d: Expected O, but got Unknown
			//IL_0c76: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c80: Expected O, but got Unknown
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(Lunar_Eclipses));
			lstEclipses = new ListBox();
			UpDownYear = new NumericUpDown();
			txtMoonEclipse = new TextBox();
			menuStrip1 = new MenuStrip();
			withPredictionToolStripMenuItem = new ToolStripMenuItem();
			includeChauvenetDanjonOffsetsToolStripMenuItem = new ToolStripMenuItem();
			listBesselianElementsToolStripMenuItem = new ToolStripMenuItem();
			drawInBWToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator1 = new ToolStripSeparator();
			copyToolStripMenuItem1 = new ToolStripMenuItem();
			printPreviewToolStripMenuItem = new ToolStripMenuItem();
			printToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			saveGraphicsToolStripMenuItem = new ToolStripMenuItem();
			save10yrsPredictionsToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem1 = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			printDocument1 = new PrintDocument();
			label2 = new Label();
			picShadow = new PictureBox();
			picEarth = new PictureBox();
			label1 = new Label();
			findClosePlanetaryConjunctionsToolStripMenuItem = new ToolStripMenuItem();
			((ISupportInitialize)UpDownYear).BeginInit();
			((Control)menuStrip1).SuspendLayout();
			((ISupportInitialize)picShadow).BeginInit();
			((ISupportInitialize)picEarth).BeginInit();
			((Control)this).SuspendLayout();
			((Control)lstEclipses).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstEclipses).set_FormattingEnabled(true);
			lstEclipses.set_ItemHeight(14);
			((Control)lstEclipses).set_Location(new Point(313, 35));
			((Control)lstEclipses).set_Name("lstEclipses");
			((Control)lstEclipses).set_Size(new Size(476, 102));
			((Control)lstEclipses).set_TabIndex(0);
			lstEclipses.add_SelectedIndexChanged((EventHandler)lstEclipses_SelectedIndexChanged);
			((Control)lstEclipses).add_DoubleClick((EventHandler)lstEclipses_DoubleClick);
			((Control)UpDownYear).set_BackColor(SystemColors.Control);
			((UpDownBase)UpDownYear).set_BorderStyle((BorderStyle)1);
			((Control)UpDownYear).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			UpDownYear.set_Increment(new decimal(new int[4] { 10, 0, 0, 0 }));
			((Control)UpDownYear).set_Location(new Point(220, 73));
			UpDownYear.set_Maximum(new decimal(new int[4] { 16990, 0, 0, 0 }));
			UpDownYear.set_Minimum(new decimal(new int[4] { 13000, 0, 0, -2147483648 }));
			((Control)UpDownYear).set_Name("UpDownYear");
			((Control)UpDownYear).set_Size(new Size(76, 26));
			((Control)UpDownYear).set_TabIndex(1);
			((UpDownBase)UpDownYear).set_TextAlign((HorizontalAlignment)1);
			UpDownYear.add_ValueChanged((EventHandler)UpDownYear_ValueChanged);
			((Control)UpDownYear).add_Enter((EventHandler)UpDownYear_Enter);
			((Control)txtMoonEclipse).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtMoonEclipse).set_Location(new Point(313, 151));
			((TextBoxBase)txtMoonEclipse).set_Multiline(true);
			((Control)txtMoonEclipse).set_Name("txtMoonEclipse");
			((Control)txtMoonEclipse).set_Size(new Size(476, 275));
			((Control)txtMoonEclipse).set_TabIndex(2);
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[4]
			{
				(ToolStripItem)withPredictionToolStripMenuItem,
				(ToolStripItem)findClosePlanetaryConjunctionsToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem1,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(806, 24));
			((Control)menuStrip1).set_TabIndex(3);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withPredictionToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[10]
			{
				(ToolStripItem)includeChauvenetDanjonOffsetsToolStripMenuItem,
				(ToolStripItem)listBesselianElementsToolStripMenuItem,
				(ToolStripItem)drawInBWToolStripMenuItem,
				(ToolStripItem)toolStripSeparator1,
				(ToolStripItem)copyToolStripMenuItem1,
				(ToolStripItem)printPreviewToolStripMenuItem,
				(ToolStripItem)printToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem,
				(ToolStripItem)saveGraphicsToolStripMenuItem,
				(ToolStripItem)save10yrsPredictionsToolStripMenuItem
			});
			((ToolStripItem)withPredictionToolStripMenuItem).set_ImageTransparentColor(Color.Magenta);
			((ToolStripItem)withPredictionToolStripMenuItem).set_Name("withPredictionToolStripMenuItem");
			((ToolStripItem)withPredictionToolStripMenuItem).set_Size(new Size(108, 20));
			((ToolStripItem)withPredictionToolStripMenuItem).set_Text("with Prediction...");
			((ToolStripItem)includeChauvenetDanjonOffsetsToolStripMenuItem).set_Name("includeChauvenetDanjonOffsetsToolStripMenuItem");
			((ToolStripItem)includeChauvenetDanjonOffsetsToolStripMenuItem).set_Size(new Size(255, 22));
			((ToolStripItem)includeChauvenetDanjonOffsetsToolStripMenuItem).set_Text("Show Chauvenet && Danjon offsets");
			((ToolStripItem)includeChauvenetDanjonOffsetsToolStripMenuItem).add_Click((EventHandler)includeChauvenetDanjonOffsetsToolStripMenuItem_Click);
			((ToolStripItem)listBesselianElementsToolStripMenuItem).set_Image((Image)Resources.ShowGridlines2HS);
			((ToolStripItem)listBesselianElementsToolStripMenuItem).set_Name("listBesselianElementsToolStripMenuItem");
			((ToolStripItem)listBesselianElementsToolStripMenuItem).set_Size(new Size(255, 22));
			((ToolStripItem)listBesselianElementsToolStripMenuItem).set_Text("List Besselian elements");
			((ToolStripItem)listBesselianElementsToolStripMenuItem).add_Click((EventHandler)listBesselianElementsToolStripMenuItem_Click);
			((ToolStripItem)drawInBWToolStripMenuItem).set_Image((Image)Resources.ColorHS);
			((ToolStripItem)drawInBWToolStripMenuItem).set_Name("drawInBWToolStripMenuItem");
			((ToolStripItem)drawInBWToolStripMenuItem).set_Size(new Size(255, 22));
			((ToolStripItem)drawInBWToolStripMenuItem).set_Text("Draw in B&&W");
			((ToolStripItem)drawInBWToolStripMenuItem).add_Click((EventHandler)drawInBWToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator1).set_Name("toolStripSeparator1");
			((ToolStripItem)toolStripSeparator1).set_Size(new Size(252, 6));
			((ToolStripItem)copyToolStripMenuItem1).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem1).set_ImageTransparentColor(Color.Magenta);
			((ToolStripItem)copyToolStripMenuItem1).set_Name("copyToolStripMenuItem1");
			copyToolStripMenuItem1.set_ShortcutKeyDisplayString("Ctrl+C");
			((ToolStripItem)copyToolStripMenuItem1).set_Size(new Size(255, 22));
			((ToolStripItem)copyToolStripMenuItem1).set_Text("&Copy");
			((ToolStripItem)copyToolStripMenuItem1).add_Click((EventHandler)copyToolStripMenuItem1_Click);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Image((Image)Resources.PrintPreview);
			((ToolStripItem)printPreviewToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Name("printPreviewToolStripMenuItem");
			((ToolStripItem)printPreviewToolStripMenuItem).set_Size(new Size(255, 22));
			((ToolStripItem)printPreviewToolStripMenuItem).set_Text("Print preview");
			((ToolStripItem)printPreviewToolStripMenuItem).add_Click((EventHandler)printPreviewToolStripMenuItem_Click);
			((ToolStripItem)printToolStripMenuItem).set_Image((Image)Resources.Print);
			((ToolStripItem)printToolStripMenuItem).set_ImageTransparentColor(Color.Magenta);
			((ToolStripItem)printToolStripMenuItem).set_Name("printToolStripMenuItem");
			printToolStripMenuItem.set_ShortcutKeyDisplayString("Ctrl+P");
			((ToolStripItem)printToolStripMenuItem).set_Size(new Size(255, 22));
			((ToolStripItem)printToolStripMenuItem).set_Text("&Print");
			((ToolStripItem)printToolStripMenuItem).add_Click((EventHandler)printToolStripMenuItem_Click);
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveToolStripMenuItem).set_ImageTransparentColor(Color.Magenta);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			saveToolStripMenuItem.set_ShortcutKeyDisplayString("Ctrl+S");
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(255, 22));
			((ToolStripItem)saveToolStripMenuItem).set_Text("&Save");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
			((ToolStripItem)saveGraphicsToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveGraphicsToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveGraphicsToolStripMenuItem).set_Name("saveGraphicsToolStripMenuItem");
			saveGraphicsToolStripMenuItem.set_ShortcutKeys((Keys)131143);
			((ToolStripItem)saveGraphicsToolStripMenuItem).set_Size(new Size(255, 22));
			((ToolStripItem)saveGraphicsToolStripMenuItem).set_Text("Save Graphics");
			((ToolStripItem)saveGraphicsToolStripMenuItem).add_Click((EventHandler)saveGraphicsToolStripMenuItem_Click);
			((ToolStripItem)save10yrsPredictionsToolStripMenuItem).set_Image((Image)Resources.SaveAsWebPageHS);
			((ToolStripItem)save10yrsPredictionsToolStripMenuItem).set_Name("save10yrsPredictionsToolStripMenuItem");
			((ToolStripItem)save10yrsPredictionsToolStripMenuItem).set_Size(new Size(255, 22));
			((ToolStripItem)save10yrsPredictionsToolStripMenuItem).set_Text("Save predictions for all");
			((ToolStripItem)save10yrsPredictionsToolStripMenuItem).add_Click((EventHandler)save10yrsPredictionsToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem1).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem1).set_Name("helpToolStripMenuItem1");
			((ToolStripItem)helpToolStripMenuItem1).set_Size(new Size(60, 20));
			((ToolStripItem)helpToolStripMenuItem1).set_Text("&Help");
			((ToolStripItem)helpToolStripMenuItem1).add_Click((EventHandler)helpToolStripMenuItem1_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(53, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("E&xit");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label2).set_Location(new Point(25, 35));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(221, 60));
			((Control)label2).set_TabIndex(4);
			((Control)label2).set_Text("Select eclipse from the list\r\nof eclipses over the period\r\nof 10 years, starting at:\r\n");
			((Control)picShadow).set_BackColor(Color.LightGray);
			picShadow.set_BorderStyle((BorderStyle)1);
			((Control)picShadow).set_Location(new Point(15, 151));
			((Control)picShadow).set_Name("picShadow");
			((Control)picShadow).set_Size(new Size(284, 275));
			picShadow.set_TabIndex(7);
			picShadow.set_TabStop(false);
			picEarth.set_BorderStyle((BorderStyle)1);
			((Control)picEarth).set_Location(new Point(15, 445));
			((Control)picEarth).set_Name("picEarth");
			((Control)picEarth).set_Size(new Size(774, 134));
			picEarth.set_TabIndex(6);
			picEarth.set_TabStop(false);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(214, 101));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(91, 13));
			((Control)label1).set_TabIndex(8);
			((Control)label1).set_Text("(-13000 to 16990)");
			((ToolStripItem)findClosePlanetaryConjunctionsToolStripMenuItem).set_Image((Image)Resources.Stars3);
			((ToolStripItem)findClosePlanetaryConjunctionsToolStripMenuItem).set_Name("findClosePlanetaryConjunctionsToolStripMenuItem");
			((ToolStripItem)findClosePlanetaryConjunctionsToolStripMenuItem).set_Size(new Size(224, 20));
			((ToolStripItem)findClosePlanetaryConjunctionsToolStripMenuItem).set_Text("Find close planetary conjunctions    ");
			((ToolStripItem)findClosePlanetaryConjunctionsToolStripMenuItem).add_Click((EventHandler)findClosePlanetaryConjunctionsToolStripMenuItem_Click);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(806, 591));
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)picShadow);
			((Control)this).get_Controls().Add((Control)(object)picEarth);
			((Control)this).get_Controls().Add((Control)(object)txtMoonEclipse);
			((Control)this).get_Controls().Add((Control)(object)UpDownYear);
			((Control)this).get_Controls().Add((Control)(object)lstEclipses);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_Controls().Add((Control)(object)label2);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationEclipseLunar", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_FormBorderStyle((FormBorderStyle)2);
			((Form)this).set_Icon((Icon)componentResourceManager.GetObject("$this.Icon"));
			((Form)this).set_Location(Settings.Default.LocationEclipseLunar);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Form)this).set_MaximizeBox(false);
			((Control)this).set_Name("Lunar_Eclipses");
			((Control)this).set_Text("Lunar Eclipses");
			((Form)this).add_Load((EventHandler)Lunar_Eclipses_Load);
			((ISupportInitialize)UpDownYear).EndInit();
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((ISupportInitialize)picShadow).EndInit();
			((ISupportInitialize)picEarth).EndInit();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
