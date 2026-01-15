using System;
using System.Collections;
using System.ComponentModel;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Text;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult
{
	public class MutualConjunctions : Form
	{
		private readonly string AppPath;

		private const double Radian = 180.0 / Math.PI;

		private const double TwoPi = Math.PI * 2.0;

		private static bool CancelFlag;

		private static bool BWFlag;

		private static bool NewPlot;

		private static bool FormLoaded;

		private float ScaleFactor = 32f;

		private IContainer components;

		private Panel panel3;

		private Label label2;

		private Label label1;

		private NumericUpDown updnEndMonth;

		private NumericUpDown updnEndYear;

		private NumericUpDown updnStartMonth;

		private NumericUpDown updnStartYear;

		private NumericUpDown updnSeparation;

		private Label label3;

		private Button cmdFind;

		private ListBox lstResults;

		private PictureBox picConjunction;

		private Button cmdCancel;

		private GroupBox groupBox1;

		private RadioButton opt512;

		private RadioButton opt256;

		private RadioButton opt128;

		private RadioButton opt64;

		private RadioButton opt32;

		private RadioButton opt16;

		private RadioButton opt8;

		private RadioButton opt4;

		private RadioButton opt2;

		private ProgressBar progressBar1;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem withPredictionsToolStripMenuItem;

		private ToolStripMenuItem drawInBWToolStripMenuItem;

		private ToolStripMenuItem copyTextToolStripMenuItem;

		private ToolStripMenuItem copyGraphicToolStripMenuItem;

		private ToolStripMenuItem printTextToolStripMenuItem;

		private ToolStripMenuItem printGraphicToolStripMenuItem;

		private ToolStripMenuItem saveTextToolStripMenuItem;

		private ToolStripMenuItem saveGraphicToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator2;

		private ToolStripMenuItem printPreviewTextToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator1;

		private ToolStripMenuItem printPreviewGraphicToolStripMenuItem;

		private ToolStripMenuItem helpToolStripMenuItem;

		public MutualConjunctions()
		{
			InitializeComponent();
			AppPath = Utilities.AppPath;
		}

		private void MutualConjunctions_Load(object sender, EventArgs e)
		{
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			if (((Control)this).get_Top() < -30000)
			{
				((Control)this).set_Top(0);
			}
			if ((double)((Control)this).get_Left() + (double)((Control)this).get_Width() / 1.2 < (double)Screen.GetWorkingArea((Control)(object)this).Left)
			{
				((Control)this).set_Left(Screen.GetWorkingArea((Control)(object)this).Left);
			}
			updnStartYear.set_Value((decimal)DateTime.Now.Year);
			updnStartMonth.set_Value((decimal)DateTime.Now.Month);
			updnEndYear.set_Value((decimal)DateTime.Now.Year);
			updnEndMonth.set_Value(12m);
			FormLoaded = true;
		}

		private void MutualConjunctions_Resize(object sender, EventArgs e)
		{
			if (!((((Control)this).get_Width() < 100) | (((Control)this).get_Height() < 150)))
			{
				((Control)lstResults).set_Height(30 + (((Control)this).get_Height() - 218) / 4);
				((Control)picConjunction).set_Top(((Control)lstResults).get_Bottom() + 5);
				((Control)picConjunction).set_Width(((Control)this).get_Width() - 88);
				((Control)picConjunction).set_Height(((Control)this).get_Height() - ((Control)picConjunction).get_Top() - 45);
				DrawConjunction();
			}
		}

		private void cmdFind_Click(object sender, EventArgs e)
		{
			ComputeConjunctions();
		}

		private void ComputeConjunctions()
		{
			double[,] array = new double[3, 10];
			double[,] array2 = new double[3, 10];
			double[,] array3 = new double[3, 10];
			double[,] array4 = new double[3, 10];
			double[,] array5 = new double[3, 10];
			double[,] array6 = new double[3, 28];
			double[,] array7 = new double[3, 28];
			double[,] array8 = new double[3, 28];
			double num = 0.0;
			double num2 = 0.0;
			double day = 0.0;
			int Year = 0;
			int Month = 0;
			string text = "";
			if (!FormLoaded)
			{
				return;
			}
			double num3 = Utilities.JD_from_Date((int)updnStartYear.get_Value(), (int)updnStartMonth.get_Value(), 1.0);
			double num4 = Utilities.JD_from_Date((int)updnEndYear.get_Value(), (int)updnEndMonth.get_Value(), 31.0) - num3;
			double num5 = (double)updnSeparation.get_Value() * 60.0;
			if (num5 == 0.0)
			{
				num5 = 30.0;
			}
			text = "";
			if (updnStartYear.get_Value() < 1m)
			{
				text = "  ";
			}
			int k;
			double PA_atOrigin;
			for (int i = 0; i < 2; i++)
			{
				for (int j = 1; j <= 9; j++)
				{
					Utilities.PlanetGeocentric(num3 + 1.0 * (double)i, j, 1E-05, 0, out array[i, j], out array2[i, j], out array3[i, j]);
				}
				for (int j = 1; j <= 9; j++)
				{
					for (k = j; k <= 9; k++)
					{
						if (j == 3)
						{
							j = 4;
						}
						array6[i, PlanetPairNumber(j, k)] = array[i, j] - array[i, k];
						array7[i, PlanetPairNumber(j, k)] = array2[i, j] - array2[i, k];
						Utilities.Distance(array[i, j], array2[i, j], array[i, k], array2[i, k], out array8[i, PlanetPairNumber(j, k)], out PA_atOrigin);
					}
				}
			}
			lstResults.get_Items().Clear();
			lstResults.get_Items().Add((object)"    Mutual conjunctions of the Planets");
			lstResults.get_Items().Add((object)"[Date and times are for minimum separation]");
			lstResults.get_Items().Add((object)("    " + text + "             Center   Limb"));
			lstResults.get_Items().Add((object)("  Yr" + text + " Mth Dy   Hr   Sepn   Sepn  Type  Elon   Planets"));
			CancelFlag = false;
			progressBar1.set_Value(0);
			progressBar1.set_Maximum((int)(num4 + 1.0));
			((Control)cmdFind).set_Visible(false);
			((Control)progressBar1).set_Visible(true);
			for (double num6 = 2.0; num6 <= num4 + 1.0; num6 += 1.0)
			{
				Application.DoEvents();
				if (CancelFlag)
				{
					break;
				}
				progressBar1.set_Value((int)num6);
				num = num3 + num6;
				Utilities.Date_from_JD(num, out Year, out Month, out day);
				int j;
				for (j = 1; j <= 9; j++)
				{
					Utilities.PlanetGeocentric(num, j, 1E-05, 0, out array[2, j], out array2[2, j], out array3[2, j], out array4[2, j], out array5[2, j]);
				}
				for (int l = 0; l <= 27; l++)
				{
					PlanetsFromPairNumber(l, out j, out k);
					Utilities.Distance(array[2, j], array2[2, j], array[2, k], array2[2, k], out array8[2, l], out PA_atOrigin);
					double num7 = array8[0, l];
					double num8 = array8[1, l];
					double num9 = array8[2, l];
					if (!((Math.Abs(num9) < 0.15) & (Math.Abs(num7) < 0.15)) || !(num8 < num7 && num8 < num9))
					{
						continue;
					}
					double num10 = (num7 + num9 - 2.0 * num8) / 2.0;
					double num11 = (0.0 - (num9 - num7) / 2.0) / (2.0 * num10) * 1.0;
					double num12 = 0.0;
					int num13 = 0;
					_ = 11;
					double MinSep;
					do
					{
						num12 = T_estimate(num - 1.0 + num11, 0.2, j, k, out MinSep);
						num11 += num12;
						num13++;
					}
					while ((num13 < 10) & (Math.Abs(num12) > 0.0001));
					num11 -= Utilities.delta_T(Year, Month, day) / 86400.0;
					double num14 = 24.0 * (num11 - Math.Floor(num11));
					num11 = Math.Floor(num11);
					if (MinSep < num5)
					{
						double num15;
						for (num15 = array[1, 3] - array[1, j]; num15 > Math.PI; num15 -= Math.PI * 2.0)
						{
						}
						for (; num15 < -Math.PI; num15 += Math.PI * 2.0)
						{
						}
						string arg = "e";
						if (num15 > 0.0)
						{
							arg = "w";
						}
						double num16 = (array5[2, j] + array5[2, k]) / 2.0;
						double num17 = Math.Abs(array5[2, j] - array5[2, k]) / 2.0;
						double num18 = Math.Abs(8.794143836182533 / array3[2, j] - 8.794143836182533 / array3[2, k]);
						double num19 = num16 + num18;
						string value = ((MinSep > num16 + num18) ? "  Miss " : ((MinSep < num17) ? "  Total" : ((num18 > MinSep) ? "  Total" : ((MinSep < Math.Abs(num17 - num18)) ? "  Total" : ((!(MinSep < num16 + num18)) ? "  ?    " : "  Part ")))));
						Utilities.Distance(array[1, 3], array2[1, 3], array[1, j], array2[1, j], out var Distance, out PA_atOrigin);
						StringBuilder stringBuilder = new StringBuilder();
						stringBuilder.Append(Utilities.Date_from_JD(num - 1.0 + num11, 0));
						stringBuilder.AppendFormat("{0,5:F1}", num14);
						if (MinSep > 150.0)
						{
							stringBuilder.AppendFormat("{0,6:F1}'", MinSep / 60.0);
						}
						else
						{
							stringBuilder.AppendFormat("{0,6:F0}\"", MinSep);
						}
						if (MinSep > 150.0)
						{
							stringBuilder.AppendFormat("{0,6:F1}'", (MinSep - num19) / 60.0);
						}
						else
						{
							stringBuilder.AppendFormat("{0,6:F0}\"", MinSep - num19);
						}
						stringBuilder.Append(value);
						stringBuilder.AppendFormat("{0,4:F0}{1}", Distance * (180.0 / Math.PI), arg);
						stringBuilder.Append("   " + (Utilities.Planets[j] + string.Format(" ({0,1:f1})", array4[1, j])).PadRight(15) + Utilities.Planets[k] + string.Format(" ({0,1:f1})", array4[1, k]));
						lstResults.get_Items().Add((object)stringBuilder.ToString());
						num2 = num;
						if (lstResults.get_Items().get_Count() == 3)
						{
							((ListControl)lstResults).set_SelectedIndex(2);
						}
					}
				}
				for (j = 1; j <= 9; j++)
				{
					for (int m = 0; m < 2; m++)
					{
						array[m, j] = array[m + 1, j];
						array2[m, j] = array2[m + 1, j];
						array3[m, j] = array3[m + 1, j];
						array4[m, j] = array4[m + 1, j];
						array5[m, j] = array5[m + 1, j];
					}
				}
				for (int n = 0; n <= 27; n++)
				{
					for (int num20 = 0; num20 < 2; num20++)
					{
						array6[num20, n] = array6[num20 + 1, n];
						array7[num20, n] = array7[num20 + 1, n];
						array8[num20, n] = array8[num20 + 1, n];
					}
				}
				_ = num - num2;
				_ = 1.0;
			}
			CancelFlag = false;
			((Control)cmdFind).set_Visible(true);
			((Control)progressBar1).set_Visible(false);
		}

		private static double T_estimate(double JD, double JDstep, int Planet1, int Planet2, out double MinSep)
		{
			double num = PlanetSeparation(JD - JDstep, Planet1, Planet2);
			double num2 = PlanetSeparation(JD, Planet1, Planet2);
			double num3 = PlanetSeparation(JD + JDstep, Planet1, Planet2);
			double num4 = (num + num3 - 2.0 * num2) / 2.0;
			double num5 = (num3 - num) / 2.0;
			double num6 = (0.0 - num5) / (2.0 * num4);
			if (num6 < -1.0)
			{
				num6 = -1.0;
			}
			if (num6 > 1.0)
			{
				num6 = 1.0;
			}
			MinSep = num2 + num5 * num6 + num4 * num6 * num6;
			return num6 * JDstep;
		}

		private static double PlanetSeparation(double JD, int Planet1, int Planet2)
		{
			Utilities.PlanetGeocentric(JD, Planet1, 1E-06, 0, out var RA, out var Dec, out var _);
			Utilities.PlanetGeocentric(JD, Planet2, 1E-06, 0, out var RA2, out var Dec2, out var _);
			Utilities.Distance(RA, Dec, RA2, Dec2, out var Distance, out var _);
			return Distance * (180.0 / Math.PI) * 3600.0;
		}

		private static int PlanetPairNumber(int Planet, int SecondPlanet)
		{
			int num = 0;
			int num2 = 0;
			switch (Planet)
			{
			case 1:
				num = 0;
				break;
			case 2:
				num = 7;
				break;
			case 4:
				num = 13;
				break;
			case 5:
				num = 18;
				break;
			case 6:
				num = 22;
				break;
			case 7:
				num = 25;
				break;
			case 8:
				num = 27;
				break;
			}
			num2 = ((SecondPlanet > Planet) ? (SecondPlanet - Planet - 1) : 0);
			if (Planet < 4 && SecondPlanet > 3)
			{
				num2--;
			}
			return num + num2;
		}

		private static void PlanetsFromPairNumber(int PairNumber, out int Planet, out int SecondPlanet)
		{
			Planet = (SecondPlanet = 0);
			if (PairNumber < 7)
			{
				Planet = 1;
				SecondPlanet = PairNumber + 2;
				if (SecondPlanet > 2)
				{
					SecondPlanet++;
				}
			}
			else if (PairNumber < 13)
			{
				Planet = 2;
				SecondPlanet = PairNumber - 3;
			}
			else if (PairNumber < 18)
			{
				Planet = 4;
				SecondPlanet = PairNumber - 8;
			}
			else if (PairNumber < 22)
			{
				Planet = 5;
				SecondPlanet = PairNumber - 12;
			}
			else if (PairNumber < 25)
			{
				Planet = 6;
				SecondPlanet = PairNumber - 15;
			}
			else if (PairNumber < 27)
			{
				Planet = 7;
				SecondPlanet = PairNumber - 17;
			}
			else
			{
				Planet = 8;
				SecondPlanet = 9;
			}
		}

		private void cmdCancel_Click(object sender, EventArgs e)
		{
			CancelFlag = true;
		}

		private void DrawConjunction()
		{
			float MoonDiaKm = 0f;
			float Mag = 0f;
			double RA = 0.0;
			double Dec = 0.0;
			double GeocentricDistance = 0.0;
			double RA2 = 0.0;
			double RA3 = 0.0;
			double Dec2 = 0.0;
			double Dec3 = 0.0;
			double GeocentricDist = 0.0;
			double GeocentricDist2 = 0.0;
			double num = 0.0;
			double num2 = 0.0;
			double num3 = 0.0;
			double num4 = 0.0;
			double[] array = new double[5];
			double[] array2 = new double[5];
			string MoonName = "";
			if (!FormLoaded | (((ListControl)lstResults).get_SelectedIndex() < 0))
			{
				return;
			}
			string text = lstResults.get_SelectedItem().ToString();
			int num5 = 0;
			if (text.IndexOf("BC") > 0)
			{
				num5 = 2;
			}
			int num6 = int.Parse(text.Substring(0, 4));
			if (num5 > 0)
			{
				num6 = -num6 + 1;
			}
			string text2 = text.Substring(5 + num5, 3);
			int i;
			for (i = 1; i < 13 && !(text2 == Utilities.ShortMonths[i]); i++)
			{
			}
			int num7 = int.Parse(text.Substring(9 + num5, 2));
			int num8 = int.Parse(text.Substring(12 + num5, 2));
			double num9 = Utilities.JD_from_Date(num6, i, num7);
			double num10 = double.Parse(text.Substring(17 + num5, 5));
			if (text.Substring(22 + num5, 1) == "\"")
			{
				num10 /= 60.0;
			}
			if (NewPlot)
			{
				opt4.set_Checked(true);
				if (num10 > 2.0)
				{
					opt8.set_Checked(true);
				}
				if (num10 > 4.0)
				{
					opt16.set_Checked(true);
				}
				if (num10 > 8.0)
				{
					opt32.set_Checked(true);
				}
				if (num10 > 16.0)
				{
					opt64.set_Checked(true);
				}
				if (num10 > 32.0)
				{
					opt128.set_Checked(true);
				}
				if (num10 > 64.0)
				{
					opt256.set_Checked(true);
				}
				if (num10 > 128.0)
				{
					opt512.set_Checked(true);
				}
				NewPlot = false;
			}
			string text3 = text.Substring(45 + num5, 7).Trim();
			int num11 = text3.IndexOf("(");
			if (num11 > 0)
			{
				text3 = text3.Substring(0, num11).Trim();
			}
			string text4 = text.Substring(60 + num5).Trim();
			num11 = text4.IndexOf("(");
			if (num11 > 0)
			{
				text4 = text4.Substring(0, num11).Trim();
			}
			int j;
			for (j = 1; j < 10 && !(text3 == Utilities.Planets[j]); j++)
			{
			}
			int k;
			for (k = 1; k < 10 && !(text4 == Utilities.Planets[k]); k++)
			{
			}
			Image image = new Bitmap(((Control)picConjunction).get_Width(), ((Control)picConjunction).get_Height());
			Graphics graphics = Graphics.FromImage(image);
			if (Settings.Default.GraphicsSmoothed)
			{
				graphics.SmoothingMode = SmoothingMode.AntiAlias;
			}
			graphics.Clear(Color.White);
			Brush brush = Brushes.White;
			Pen pen = new Pen(Brushes.White);
			Font font = new Font("Times New Roman", 8f);
			if (BWFlag)
			{
				graphics.FillRectangle(Brushes.White, 0, 0, ((Control)picConjunction).get_Width(), ((Control)picConjunction).get_Height());
				brush = Brushes.Black;
				pen.Color = Color.Black;
			}
			else
			{
				graphics.FillRectangle(Brushes.Black, 0, 0, ((Control)picConjunction).get_Width(), ((Control)picConjunction).get_Height());
			}
			graphics.DrawString("Conjunction of " + text3.Trim() + " with " + text4.Trim() + " on " + text.Substring(0, 11 + num5), font, brush, 5f, 2f);
			float num12 = (float)((Control)picConjunction).get_Height() / ScaleFactor;
			float num13 = ((Control)picConjunction).get_Height() / 10;
			float num14 = ((Control)picConjunction).get_Width() / 2;
			float num15 = ((Control)picConjunction).get_Height() / 2;
			double num16 = Utilities.delta_T(num6, i, 15.0) / 3600.0;
			if (j == 1 && k == 2)
			{
				Utilities.PlanetGeocentric(num9 + (double)num8, j, 0.001, 0, out RA2, out Dec2, out GeocentricDist);
				Utilities.PlanetGeocentric(num9 + (double)num8, k, 0.001, 0, out RA3, out Dec3, out GeocentricDist2);
				if (GeocentricDist2 < GeocentricDist)
				{
					j = 2;
					k = 1;
				}
			}
			for (double num17 = -2.0; num17 <= 2.0; num17 += 1.0)
			{
				Utilities.PlanetGeocentric(num9 + ((double)num8 + num17 + num16) / 24.0, j, 0.0, 0, physicalFlag: true, out var x, out var y, out var z, out RA2, out Dec2, out GeocentricDist, out var Diameter_arcsec, out var Magnitude, out var Elongation, out var EW, out var PhaseAngle_deg, out var illumination, out var PAlimb_deg, out var Planetocentric_Latitude_deg, out var PAPole_deg, out var Planetocentric_SunLat_deg);
				Utilities.PlanetGeocentric(num9 + ((double)num8 + num17 + num16) / 24.0, k, 0.0, 0, physicalFlag: true, out x, out y, out z, out RA3, out Dec3, out GeocentricDist2, out var Diameter_arcsec2, out Magnitude, out Elongation, out EW, out var PhaseAngle_deg2, out illumination, out var PAlimb_deg2, out var Planetocentric_Latitude_deg2, out var PAPole_deg2, out Planetocentric_SunLat_deg);
				if (j == 5 || k == 5)
				{
					for (int l = 1; l <= 4; l++)
					{
						Satellites.SatelliteCoordinates(num9 + ((double)num8 + num17 + num16) / 24.0, 5, l, ViewedFromSun: false, ref MoonName, ref MoonDiaKm, ref array[l], ref array2[l], ref Mag);
						array[l] = array[l] * (180.0 / Math.PI) * 60.0 * Math.Cos(Dec2) * (double)num12;
						array2[l] = array2[l] * (180.0 / Math.PI) * 60.0 * (double)num12;
					}
				}
				if (j == 6 || k == 6)
				{
					Satellites.SatelliteCoordinates(num9 + ((double)num8 + num17 + num16) / 24.0, 6, 6, ViewedFromSun: false, ref MoonName, ref MoonDiaKm, ref array[1], ref array2[1], ref Mag);
					array[1] = array[1] * (180.0 / Math.PI) * 60.0 * Math.Cos(Dec2) * (double)num12;
					array2[1] = array2[1] * (180.0 / Math.PI) * 60.0 * (double)num12;
				}
				float num18 = (float)(180.0 / Math.PI * (RA2 - RA3) * 60.0 * Math.Cos(Dec2) * (double)num12);
				float num19 = (float)(180.0 / Math.PI * (Dec2 - Dec3) * 60.0 * (double)num12);
				if (num17 == 0.0)
				{
					num = num18;
					num2 = num19;
				}
				else if (num17 == 1.0)
				{
					num3 = (double)num18 - num;
					num4 = (double)num19 - num2;
				}
				PlotPlanet(graphics, k, num14, num15, (float)(Diameter_arcsec2 / 120.0 * (double)num12), PAPole_deg2 / (180.0 / Math.PI), PAlimb_deg2, PhaseAngle_deg2, Planetocentric_Latitude_deg2 / (180.0 / Math.PI), PrintFlag: false);
				PlotPlanet(graphics, j, num14 - num18, num15 - num19, (float)(Diameter_arcsec / 120.0 * (double)num12), PAPole_deg / (180.0 / Math.PI), PAlimb_deg, PhaseAngle_deg, Planetocentric_Latitude_deg / (180.0 / Math.PI), PrintFlag: false);
				Planetocentric_SunLat_deg = (double)num8 + num17;
				string s = Planetocentric_SunLat_deg.ToString("#hrs");
				graphics.DrawString(s, font, brush, num14 - num18 + (float)(Diameter_arcsec / 120.0 * (double)num12), num15 - num19 + (float)(Diameter_arcsec / 120.0 * (double)num12));
				float num20 = 0.8f + 0.8f * Convert.ToSingle(num17 == -2.0);
				if (k == 5)
				{
					for (int l = 1; l < 5; l++)
					{
						graphics.DrawEllipse(pen, (float)((double)num14 - array[l] - (double)num20), (float)((double)num15 - array2[l] - (double)num20), 2f * num20, 2f * num20);
					}
				}
				else if (j == 5)
				{
					for (int l = 1; l < 5; l++)
					{
						graphics.DrawEllipse(pen, (float)((double)(num14 - num18) - array[l] - (double)num20), (float)((double)(num15 - num19) - array2[l] - (double)num20), 2f * num20, 2f * num20);
					}
				}
				if (k == 6)
				{
					graphics.DrawEllipse(pen, (float)((double)num14 - array[1] - (double)num20), (float)((double)num15 - array2[1] - (double)num20), 2f * num20, 2f * num20);
				}
				else if (j == 6)
				{
					graphics.DrawEllipse(pen, (float)((double)(num14 - num18) - array[1] - (double)num20), (float)((double)(num15 - num19) - array2[1] - (double)num20), 2f * num20, 2f * num20);
				}
			}
			double num21 = Math.Sqrt(num3 * num3 + num4 * num4);
			_ = 3437.746770784939 * (num * num4 - num2 * num3) / num21;
			double num22 = (0.0 - (num * num3 + num2 * num4)) / num21 / num21;
			double num23 = Utilities.SiderealTime_deg(num9 + ((double)num8 + num22) / 24.0, Apparent: false);
			Utilities.QuickPlanet(num9, 3, EquinoxOfDate: true, out RA, out Dec, out GeocentricDistance);
			Maps.EarthGlobe(graphics, num13, (float)(1.1 * (double)num13), (float)((double)(2f * num15) - 1.3 * (double)num13), 180.0 / Math.PI * RA2 - num23, 180.0 / Math.PI * Dec2, 180.0 / Math.PI * RA - num23, 180.0 / Math.PI * Dec, 0.0, 0.0, ShowSunLit: true, SiteCentered: false, PlotCities: false, FullResolution: false, BWFlag, Mirrored: false);
			picConjunction.set_Image(image);
			graphics.Dispose();
		}

		private void opt2_CheckedChanged(object sender, EventArgs e)
		{
			ScaleFactor = 2f;
			if ((((ListControl)lstResults).get_SelectedIndex() > 1) & !NewPlot)
			{
				DrawConjunction();
			}
		}

		private void opt4_CheckedChanged(object sender, EventArgs e)
		{
			ScaleFactor = 4f;
			if ((((ListControl)lstResults).get_SelectedIndex() > 1) & !NewPlot)
			{
				DrawConjunction();
			}
		}

		private void opt8_CheckedChanged(object sender, EventArgs e)
		{
			ScaleFactor = 8f;
			if ((((ListControl)lstResults).get_SelectedIndex() > 1) & !NewPlot)
			{
				DrawConjunction();
			}
		}

		private void opt16_CheckedChanged(object sender, EventArgs e)
		{
			ScaleFactor = 16f;
			if ((((ListControl)lstResults).get_SelectedIndex() > 1) & !NewPlot)
			{
				DrawConjunction();
			}
		}

		private void opt32_CheckedChanged(object sender, EventArgs e)
		{
			ScaleFactor = 32f;
			if ((((ListControl)lstResults).get_SelectedIndex() > 1) & !NewPlot)
			{
				DrawConjunction();
			}
		}

		private void opt64_CheckedChanged(object sender, EventArgs e)
		{
			ScaleFactor = 64f;
			if ((((ListControl)lstResults).get_SelectedIndex() > 1) & !NewPlot)
			{
				DrawConjunction();
			}
		}

		private void opt128_CheckedChanged(object sender, EventArgs e)
		{
			ScaleFactor = 128f;
			if ((((ListControl)lstResults).get_SelectedIndex() > 1) & !NewPlot)
			{
				DrawConjunction();
			}
		}

		private void opt256_CheckedChanged(object sender, EventArgs e)
		{
			ScaleFactor = 256f;
			if ((((ListControl)lstResults).get_SelectedIndex() > 1) & !NewPlot)
			{
				DrawConjunction();
			}
		}

		private void opt512_CheckedChanged(object sender, EventArgs e)
		{
			ScaleFactor = 512f;
			if ((((ListControl)lstResults).get_SelectedIndex() > 1) & !NewPlot)
			{
				DrawConjunction();
			}
		}

		private void lstResults_SelectedIndexChanged(object sender, EventArgs e)
		{
			NewPlot = true;
			if (((ListControl)lstResults).get_SelectedIndex() > 3)
			{
				DrawConjunction();
			}
			NewPlot = false;
		}

		private static void PlotPlanet(Graphics formGraphics, int Planet, float PlanetX, float PlanetY, float rhoPlanet, double PAPole, double PALimb, double PhaseAngle, double Incl, bool PrintFlag)
		{
			float[] array = new float[6] { 0f, 1.239f, 1.526f, 1.95f, 2.03f, 2.269f };
			float num = 0f;
			float num2 = 0f;
			float num3 = 0f;
			Color[] array2 = new Color[10];
			Pen pen = new Pen(Color.Red);
			new SolidBrush(Color.White);
			ArrayList arrayList = new ArrayList();
			if (Settings.Default.GraphicsSmoothed)
			{
				formGraphics.SmoothingMode = SmoothingMode.AntiAlias;
			}
			Color color;
			Color color2;
			if (!BWFlag && !PrintFlag)
			{
				array2[1] = Color.Chocolate;
				array2[2] = Color.White;
				array2[4] = Color.OrangeRed;
				array2[5] = Color.Gold;
				array2[6] = Color.Goldenrod;
				array2[7] = Color.CadetBlue;
				array2[8] = Color.Aquamarine;
				array2[9] = Color.MediumSlateBlue;
				color = Color.Olive;
				color2 = Color.FromArgb(210, 105, 30);
			}
			else if (!BWFlag && PrintFlag)
			{
				for (int i = 1; i < 10; i++)
				{
					array2[i] = Color.Black;
				}
				color = Color.Black;
				color2 = Color.FromArgb(180, 180, 180);
			}
			else
			{
				for (int j = 1; j < 10; j++)
				{
					array2[j] = Color.Black;
				}
				color = Color.Black;
				color2 = Color.Gray;
			}
			float num4 = 0f;
			if (Planet == 5)
			{
				num4 = 0.064809f;
			}
			if (Planet == 6)
			{
				num4 = 0.10762f;
			}
			pen.Color = array2[Planet];
			if (num4 > 0f)
			{
				float num5 = (float)((double)(num4 / 2f) + (double)(num4 / 2f) * Math.Cos(2.0 * Incl));
				arrayList.Clear();
				for (int k = 0; k < 360; k += 3)
				{
					float num6 = (float)((double)rhoPlanet * ((double)(1f - num5 / 2f) + (double)(num5 / 2f) * Math.Cos((double)(2 * k) / (180.0 / Math.PI))));
					float num7 = (float)((double)num6 * Math.Cos((double)k / (180.0 / Math.PI)));
					float num8 = (float)((double)num6 * Math.Sin((double)k / (180.0 / Math.PI)));
					float num9 = (float)((double)PlanetX - (double)num7 * Math.Cos(PAPole) - (double)num8 * Math.Sin(PAPole));
					float num10 = (float)((double)PlanetY - (double)num8 * Math.Cos(PAPole) + (double)num7 * Math.Sin(PAPole));
					arrayList.Add(new PointF(num9, num10));
					num2 = num9;
					num3 = num10;
				}
				PointF[] points = (PointF[])arrayList.ToArray(arrayList[0]!.GetType());
				if (!BWFlag)
				{
					formGraphics.FillPolygon(new SolidBrush(array2[Planet]), points);
				}
				else
				{
					formGraphics.DrawPolygon(pen, points);
				}
			}
			else if (Planet > 6)
			{
				formGraphics.FillEllipse(new SolidBrush(array2[Planet]), PlanetX - rhoPlanet, PlanetY - rhoPlanet, 2f * rhoPlanet, 2f * rhoPlanet);
			}
			float num11 = (float)(180.0 - PALimb);
			if (num11 < 0f)
			{
				num11 += 360f;
			}
			float num12 = num11 + 180f;
			pen.Color = color2;
			formGraphics.DrawArc(pen, PlanetX - rhoPlanet, PlanetY - rhoPlanet, 2f * rhoPlanet, 2f * rhoPlanet, num12, num12 - num11);
			arrayList.Clear();
			for (float num13 = 0f; num13 <= 180f; num13 += 5f)
			{
				float num7 = (float)((double)rhoPlanet * Math.Sin((double)num13 / (180.0 / Math.PI)));
				float num8 = (float)((double)rhoPlanet * Math.Cos((double)num13 / (180.0 / Math.PI)));
				double num14 = (double)(90f - num12) / (180.0 / Math.PI);
				float num9 = (float)((double)PlanetX + (double)num7 * Math.Cos(num14) + (double)num8 * Math.Sin(num14));
				float num10 = (float)((double)PlanetY + (double)num8 * Math.Cos(num14) - (double)num7 * Math.Sin(num14));
				arrayList.Add(new PointF(num9, num10));
			}
			for (int l = 0; l <= 180; l += 5)
			{
				float num7 = (float)((double)rhoPlanet * Math.Sin((double)l / (180.0 / Math.PI)) * Math.Cos(PhaseAngle / (180.0 / Math.PI)));
				float num8 = (float)((double)rhoPlanet * Math.Cos((double)l / (180.0 / Math.PI)));
				double num15 = (double)(90f - num11) / (180.0 / Math.PI);
				float num9 = (float)((double)PlanetX + (double)num7 * Math.Cos(num15) + (double)num8 * Math.Sin(num15));
				float num10 = (float)((double)PlanetY + (double)num8 * Math.Cos(num15) - (double)num7 * Math.Sin(num15));
				arrayList.Add(new PointF(num9, num10));
			}
			if (arrayList.Count > 1)
			{
				PointF[] points = (PointF[])arrayList.ToArray(arrayList[0]!.GetType());
				if (!BWFlag)
				{
					formGraphics.FillPolygon(new SolidBrush(array2[Planet]), points);
				}
				else
				{
					pen.Color = array2[Planet];
					new SolidBrush(array2[Planet]);
					formGraphics.DrawPolygon(pen, points);
				}
			}
			if (Planet != 6)
			{
				return;
			}
			for (int m = 1; m <= 5; m++)
			{
				pen.Color = color;
				arrayList.Clear();
				for (int n = 0; n <= 360; n += 4)
				{
					float num16 = array[m] * rhoPlanet;
					float num7 = (float)((double)num16 * Math.Cos((double)n / (180.0 / Math.PI)));
					float num8 = (float)((double)num16 * Math.Sin((double)n / (180.0 / Math.PI)) * Math.Sin(Incl));
					float num17 = (float)Math.Sqrt(num7 * num7 + num8 * num8);
					float num9 = (float)((double)PlanetX - (double)num7 * Math.Cos(PAPole) - (double)num8 * Math.Sin(PAPole));
					float num10 = (float)((double)PlanetY - (double)num8 * Math.Cos(PAPole) + (double)num7 * Math.Sin(PAPole));
					if ((num17 > rhoPlanet && num > rhoPlanet) || n > 180 || (n == 0 && m > 0))
					{
						arrayList.Add(new PointF(num9, num10));
					}
					else
					{
						if (num17 > rhoPlanet && num <= rhoPlanet)
						{
							float num18 = (rhoPlanet - num) / (num17 - num);
							arrayList.Add(new PointF(num2 + num18 * (num9 - num2), num3 + num18 * (num10 - num3)));
							arrayList.Add(new PointF(num9, num10));
						}
						if (num17 <= rhoPlanet && num > rhoPlanet)
						{
							float num18 = (rhoPlanet - num) / (num17 - num);
							arrayList.Add(new PointF(num2 + num18 * (num9 - num2), num3 + num18 * (num10 - num3)));
							if (arrayList.Count > 1)
							{
								PointF[] points = (PointF[])arrayList.ToArray(arrayList[0]!.GetType());
								formGraphics.DrawLines(pen, points);
							}
							arrayList.Clear();
						}
					}
					num2 = num9;
					num3 = num10;
					num = num17;
				}
				if (arrayList.Count > 1)
				{
					PointF[] points = (PointF[])arrayList.ToArray(arrayList[0]!.GetType());
					formGraphics.DrawLines(pen, points);
				}
			}
		}

		private void updnStartYear_ValueChanged(object sender, EventArgs e)
		{
			if (updnStartYear.get_Value() > updnEndYear.get_Value())
			{
				updnEndYear.set_Value(updnStartYear.get_Value());
			}
		}

		private void updnEndYear_ValueChanged(object sender, EventArgs e)
		{
			if (updnStartYear.get_Value() > updnEndYear.get_Value())
			{
				updnStartYear.set_Value(updnEndYear.get_Value());
			}
		}

		private void drawInBWToolStripMenuItem_Click(object sender, EventArgs e)
		{
			BWFlag = !BWFlag;
			if (BWFlag)
			{
				((ToolStripItem)drawInBWToolStripMenuItem).set_Text("Draw in Colour");
			}
			else
			{
				((ToolStripItem)drawInBWToolStripMenuItem).set_Text("Draw in B&&W");
			}
			DrawConjunction();
		}

		private void copyTextToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(CollectEvents());
		}

		private void copyGraphicToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetImage(picConjunction.get_Image());
		}

		private void printPreviewTextToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.PrintPreviewText(CollectEvents());
		}

		private void printTextToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.PrintText(CollectEvents());
		}

		private void printPreviewGraphicToolStripMenuItem_Click(object sender, EventArgs e)
		{
		}

		private void printGraphicToolStripMenuItem_Click(object sender, EventArgs e)
		{
		}

		private void saveTextToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings.Default.Save_EphemerisData = Output.SaveAppendPredictionText(CollectEvents(), "Mutual planetary conjunctions", Settings.Default.Save_EphemerisData);
		}

		private void saveGraphicToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings.Default.Save_EphemerisData = Output.SaveGraphic(picConjunction.get_Image(), "Mutual planetary conjunctions", Settings.Default.Save_EphemerisData);
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
		}

		private string CollectEvents()
		{
			StringBuilder stringBuilder = new StringBuilder();
			int count = lstResults.get_Items().get_Count();
			for (int i = 0; i < count; i++)
			{
				stringBuilder.AppendLine(lstResults.get_Items().get_Item(i).ToString());
			}
			return stringBuilder.ToString();
		}

		private void updnStartYear_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnStartYear).Select(0, 10);
		}

		private void updnStartMonth_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnStartMonth).Select(0, 10);
		}

		private void updnEndYear_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnEndYear).Select(0, 10);
		}

		private void updnEndMonth_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnEndMonth).Select(0, 10);
		}

		private void updnSeparation_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnSeparation).Select(0, 10);
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Mutual conjunctions of the planets");
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
			//IL_00cc: Unknown result type (might be due to invalid IL or missing references)
			//IL_00d6: Expected O, but got Unknown
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
			//IL_010e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0118: Expected O, but got Unknown
			//IL_0119: Unknown result type (might be due to invalid IL or missing references)
			//IL_0123: Expected O, but got Unknown
			//IL_0124: Unknown result type (might be due to invalid IL or missing references)
			//IL_012e: Expected O, but got Unknown
			//IL_012f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0139: Expected O, but got Unknown
			//IL_013a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0144: Expected O, but got Unknown
			//IL_0145: Unknown result type (might be due to invalid IL or missing references)
			//IL_014f: Expected O, but got Unknown
			//IL_0150: Unknown result type (might be due to invalid IL or missing references)
			//IL_015a: Expected O, but got Unknown
			//IL_015b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0165: Expected O, but got Unknown
			//IL_0166: Unknown result type (might be due to invalid IL or missing references)
			//IL_0170: Expected O, but got Unknown
			//IL_0171: Unknown result type (might be due to invalid IL or missing references)
			//IL_017b: Expected O, but got Unknown
			//IL_017c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0186: Expected O, but got Unknown
			//IL_0187: Unknown result type (might be due to invalid IL or missing references)
			//IL_0191: Expected O, but got Unknown
			//IL_0192: Unknown result type (might be due to invalid IL or missing references)
			//IL_019c: Expected O, but got Unknown
			//IL_019d: Unknown result type (might be due to invalid IL or missing references)
			//IL_01a7: Expected O, but got Unknown
			//IL_01a8: Unknown result type (might be due to invalid IL or missing references)
			//IL_01b2: Expected O, but got Unknown
			//IL_01b3: Unknown result type (might be due to invalid IL or missing references)
			//IL_01bd: Expected O, but got Unknown
			//IL_17cc: Unknown result type (might be due to invalid IL or missing references)
			//IL_17d6: Expected O, but got Unknown
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(MutualConjunctions));
			panel3 = new Panel();
			label2 = new Label();
			label1 = new Label();
			updnEndMonth = new NumericUpDown();
			updnEndYear = new NumericUpDown();
			updnStartMonth = new NumericUpDown();
			updnStartYear = new NumericUpDown();
			updnSeparation = new NumericUpDown();
			label3 = new Label();
			cmdFind = new Button();
			lstResults = new ListBox();
			picConjunction = new PictureBox();
			cmdCancel = new Button();
			groupBox1 = new GroupBox();
			opt512 = new RadioButton();
			opt256 = new RadioButton();
			opt128 = new RadioButton();
			opt64 = new RadioButton();
			opt32 = new RadioButton();
			opt16 = new RadioButton();
			opt8 = new RadioButton();
			opt4 = new RadioButton();
			opt2 = new RadioButton();
			progressBar1 = new ProgressBar();
			menuStrip1 = new MenuStrip();
			withPredictionsToolStripMenuItem = new ToolStripMenuItem();
			drawInBWToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator2 = new ToolStripSeparator();
			copyTextToolStripMenuItem = new ToolStripMenuItem();
			printPreviewTextToolStripMenuItem = new ToolStripMenuItem();
			printTextToolStripMenuItem = new ToolStripMenuItem();
			saveTextToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator1 = new ToolStripSeparator();
			copyGraphicToolStripMenuItem = new ToolStripMenuItem();
			printPreviewGraphicToolStripMenuItem = new ToolStripMenuItem();
			printGraphicToolStripMenuItem = new ToolStripMenuItem();
			saveGraphicToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			((Control)panel3).SuspendLayout();
			((ISupportInitialize)updnEndMonth).BeginInit();
			((ISupportInitialize)updnEndYear).BeginInit();
			((ISupportInitialize)updnStartMonth).BeginInit();
			((ISupportInitialize)updnStartYear).BeginInit();
			((ISupportInitialize)updnSeparation).BeginInit();
			((ISupportInitialize)picConjunction).BeginInit();
			((Control)groupBox1).SuspendLayout();
			((Control)menuStrip1).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)panel3).get_Controls().Add((Control)(object)label2);
			((Control)panel3).get_Controls().Add((Control)(object)label1);
			((Control)panel3).get_Controls().Add((Control)(object)updnEndMonth);
			((Control)panel3).get_Controls().Add((Control)(object)updnEndYear);
			((Control)panel3).get_Controls().Add((Control)(object)label3);
			((Control)panel3).get_Controls().Add((Control)(object)updnStartMonth);
			((Control)panel3).get_Controls().Add((Control)(object)updnSeparation);
			((Control)panel3).get_Controls().Add((Control)(object)updnStartYear);
			((Control)panel3).set_Location(new Point(9, 34));
			((Control)panel3).set_Name("panel3");
			((Control)panel3).set_Size(new Size(213, 78));
			((Control)panel3).set_TabIndex(0);
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(5, 33));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(92, 13));
			((Control)label2).set_TabIndex(3);
			((Control)label2).set_Text("End Year && month");
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(5, 6));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(95, 13));
			((Control)label1).set_TabIndex(0);
			((Control)label1).set_Text("Start Year && month");
			((Control)updnEndMonth).set_Location(new Point(165, 31));
			updnEndMonth.set_Maximum(new decimal(new int[4] { 12, 0, 0, 0 }));
			updnEndMonth.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnEndMonth).set_Name("updnEndMonth");
			((Control)updnEndMonth).set_Size(new Size(39, 20));
			((Control)updnEndMonth).set_TabIndex(5);
			updnEndMonth.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnEndMonth).add_Enter((EventHandler)updnEndMonth_Enter);
			((Control)updnEndYear).set_Location(new Point(102, 31));
			updnEndYear.set_Maximum(new decimal(new int[4] { 17000, 0, 0, 0 }));
			updnEndYear.set_Minimum(new decimal(new int[4] { 13000, 0, 0, -2147483648 }));
			((Control)updnEndYear).set_Name("updnEndYear");
			((Control)updnEndYear).set_Size(new Size(55, 20));
			((Control)updnEndYear).set_TabIndex(4);
			updnEndYear.set_Value(new decimal(new int[4] { 2007, 0, 0, 0 }));
			updnEndYear.add_ValueChanged((EventHandler)updnEndYear_ValueChanged);
			((Control)updnEndYear).add_Enter((EventHandler)updnEndYear_Enter);
			((Control)updnStartMonth).set_Location(new Point(164, 4));
			updnStartMonth.set_Maximum(new decimal(new int[4] { 12, 0, 0, 0 }));
			updnStartMonth.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnStartMonth).set_Name("updnStartMonth");
			((Control)updnStartMonth).set_Size(new Size(40, 20));
			((Control)updnStartMonth).set_TabIndex(2);
			updnStartMonth.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnStartMonth).add_Enter((EventHandler)updnStartMonth_Enter);
			((Control)updnStartYear).set_Location(new Point(102, 4));
			updnStartYear.set_Maximum(new decimal(new int[4] { 17000, 0, 0, 0 }));
			updnStartYear.set_Minimum(new decimal(new int[4] { 13000, 0, 0, -2147483648 }));
			((Control)updnStartYear).set_Name("updnStartYear");
			((Control)updnStartYear).set_Size(new Size(55, 20));
			((Control)updnStartYear).set_TabIndex(1);
			updnStartYear.set_Value(new decimal(new int[4] { 2007, 0, 0, 0 }));
			updnStartYear.add_ValueChanged((EventHandler)updnStartYear_ValueChanged);
			((Control)updnStartYear).add_Enter((EventHandler)updnStartYear_Enter);
			((Control)updnSeparation).set_Anchor((AnchorStyles)1);
			((Control)updnSeparation).set_Location(new Point(153, 54));
			updnSeparation.set_Maximum(new decimal(new int[4] { 200, 0, 0, 0 }));
			((Control)updnSeparation).set_Name("updnSeparation");
			((Control)updnSeparation).set_Size(new Size(51, 20));
			((Control)updnSeparation).set_TabIndex(2);
			updnSeparation.set_Value(new decimal(new int[4] { 10, 0, 0, 0 }));
			((Control)updnSeparation).add_Enter((EventHandler)updnSeparation_Enter);
			((Control)label3).set_Anchor((AnchorStyles)1);
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Location(new Point(18, 58));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(129, 13));
			((Control)label3).set_TabIndex(1);
			((Control)label3).set_Text("Max Separation [arc-mins]");
			((Control)cmdFind).set_Location(new Point(75, 118));
			((Control)cmdFind).set_Name("cmdFind");
			((Control)cmdFind).set_Size(new Size(81, 30));
			((Control)cmdFind).set_TabIndex(3);
			((Control)cmdFind).set_Text("&Find");
			((ButtonBase)cmdFind).set_UseVisualStyleBackColor(true);
			((Control)cmdFind).add_Click((EventHandler)cmdFind_Click);
			((Control)lstResults).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstResults).set_FormattingEnabled(true);
			lstResults.set_ItemHeight(14);
			((Control)lstResults).set_Location(new Point(239, 34));
			((Control)lstResults).set_Name("lstResults");
			((Control)lstResults).set_Size(new Size(557, 130));
			((Control)lstResults).set_TabIndex(4);
			lstResults.add_SelectedIndexChanged((EventHandler)lstResults_SelectedIndexChanged);
			picConjunction.set_BorderStyle((BorderStyle)2);
			((Control)picConjunction).set_Location(new Point(68, 172));
			((Control)picConjunction).set_Name("picConjunction");
			((Control)picConjunction).set_Size(new Size(728, 413));
			picConjunction.set_TabIndex(26);
			picConjunction.set_TabStop(false);
			((Control)cmdCancel).set_Location(new Point(75, 118));
			((Control)cmdCancel).set_Name("cmdCancel");
			((Control)cmdCancel).set_Size(new Size(81, 30));
			((Control)cmdCancel).set_TabIndex(6);
			((Control)cmdCancel).set_Text("Cancel");
			((ButtonBase)cmdCancel).set_UseVisualStyleBackColor(true);
			((Control)cmdCancel).add_Click((EventHandler)cmdCancel_Click);
			((Control)groupBox1).set_Anchor((AnchorStyles)4);
			((Control)groupBox1).get_Controls().Add((Control)(object)opt512);
			((Control)groupBox1).get_Controls().Add((Control)(object)opt256);
			((Control)groupBox1).get_Controls().Add((Control)(object)opt128);
			((Control)groupBox1).get_Controls().Add((Control)(object)opt64);
			((Control)groupBox1).get_Controls().Add((Control)(object)opt32);
			((Control)groupBox1).get_Controls().Add((Control)(object)opt16);
			((Control)groupBox1).get_Controls().Add((Control)(object)opt8);
			((Control)groupBox1).get_Controls().Add((Control)(object)opt4);
			((Control)groupBox1).get_Controls().Add((Control)(object)opt2);
			((Control)groupBox1).set_Location(new Point(4, 265));
			((Control)groupBox1).set_Name("groupBox1");
			((Control)groupBox1).set_Size(new Size(58, 227));
			((Control)groupBox1).set_TabIndex(5);
			groupBox1.set_TabStop(false);
			((Control)groupBox1).set_Text("v.Scale");
			((Control)opt512).set_AutoSize(true);
			((Control)opt512).set_Location(new Point(4, 199));
			((Control)opt512).set_Name("opt512");
			((Control)opt512).set_Size(new Size(46, 17));
			((Control)opt512).set_TabIndex(8);
			opt512.set_TabStop(true);
			((Control)opt512).set_Text("8.4d");
			((ButtonBase)opt512).set_UseVisualStyleBackColor(true);
			opt512.add_CheckedChanged((EventHandler)opt512_CheckedChanged);
			((Control)opt256).set_AutoSize(true);
			((Control)opt256).set_Location(new Point(4, 177));
			((Control)opt256).set_Name("opt256");
			((Control)opt256).set_Size(new Size(46, 17));
			((Control)opt256).set_TabIndex(7);
			opt256.set_TabStop(true);
			((Control)opt256).set_Text("4.2d");
			((ButtonBase)opt256).set_UseVisualStyleBackColor(true);
			opt256.add_CheckedChanged((EventHandler)opt256_CheckedChanged);
			((Control)opt128).set_AutoSize(true);
			((Control)opt128).set_Location(new Point(4, 155));
			((Control)opt128).set_Name("opt128");
			((Control)opt128).set_Size(new Size(46, 17));
			((Control)opt128).set_TabIndex(6);
			opt128.set_TabStop(true);
			((Control)opt128).set_Text("2.1d");
			((ButtonBase)opt128).set_UseVisualStyleBackColor(true);
			opt128.add_CheckedChanged((EventHandler)opt128_CheckedChanged);
			((Control)opt64).set_AutoSize(true);
			((Control)opt64).set_Location(new Point(4, 133));
			((Control)opt64).set_Name("opt64");
			((Control)opt64).set_Size(new Size(39, 17));
			((Control)opt64).set_TabIndex(5);
			opt64.set_TabStop(true);
			((Control)opt64).set_Text("64'");
			((ButtonBase)opt64).set_UseVisualStyleBackColor(true);
			opt64.add_CheckedChanged((EventHandler)opt64_CheckedChanged);
			((Control)opt32).set_AutoSize(true);
			opt32.set_Checked(true);
			((Control)opt32).set_Location(new Point(4, 111));
			((Control)opt32).set_Name("opt32");
			((Control)opt32).set_Size(new Size(39, 17));
			((Control)opt32).set_TabIndex(4);
			opt32.set_TabStop(true);
			((Control)opt32).set_Text("32'");
			((ButtonBase)opt32).set_UseVisualStyleBackColor(true);
			opt32.add_CheckedChanged((EventHandler)opt32_CheckedChanged);
			((Control)opt16).set_AutoSize(true);
			((Control)opt16).set_Location(new Point(4, 89));
			((Control)opt16).set_Name("opt16");
			((Control)opt16).set_Size(new Size(39, 17));
			((Control)opt16).set_TabIndex(3);
			opt16.set_TabStop(true);
			((Control)opt16).set_Text("16'");
			((ButtonBase)opt16).set_UseVisualStyleBackColor(true);
			opt16.add_CheckedChanged((EventHandler)opt16_CheckedChanged);
			((Control)opt8).set_AutoSize(true);
			((Control)opt8).set_Location(new Point(4, 67));
			((Control)opt8).set_Name("opt8");
			((Control)opt8).set_Size(new Size(33, 17));
			((Control)opt8).set_TabIndex(2);
			opt8.set_TabStop(true);
			((Control)opt8).set_Text("8'");
			((ButtonBase)opt8).set_UseVisualStyleBackColor(true);
			opt8.add_CheckedChanged((EventHandler)opt8_CheckedChanged);
			((Control)opt4).set_AutoSize(true);
			((Control)opt4).set_Location(new Point(4, 45));
			((Control)opt4).set_Name("opt4");
			((Control)opt4).set_Size(new Size(33, 17));
			((Control)opt4).set_TabIndex(1);
			opt4.set_TabStop(true);
			((Control)opt4).set_Text("4'");
			((ButtonBase)opt4).set_UseVisualStyleBackColor(true);
			opt4.add_CheckedChanged((EventHandler)opt4_CheckedChanged);
			((Control)opt2).set_AutoSize(true);
			((Control)opt2).set_Location(new Point(4, 23));
			((Control)opt2).set_Name("opt2");
			((Control)opt2).set_Size(new Size(33, 17));
			((Control)opt2).set_TabIndex(0);
			opt2.set_TabStop(true);
			((Control)opt2).set_Text("2'");
			((ButtonBase)opt2).set_UseVisualStyleBackColor(true);
			opt2.add_CheckedChanged((EventHandler)opt2_CheckedChanged);
			((Control)progressBar1).set_Location(new Point(9, 155));
			((Control)progressBar1).set_Name("progressBar1");
			((Control)progressBar1).set_Size(new Size(213, 10));
			((Control)progressBar1).set_TabIndex(7);
			((Control)progressBar1).set_Visible(false);
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)withPredictionsToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(808, 24));
			((Control)menuStrip1).set_TabIndex(30);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withPredictionsToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[11]
			{
				(ToolStripItem)drawInBWToolStripMenuItem,
				(ToolStripItem)toolStripSeparator2,
				(ToolStripItem)copyTextToolStripMenuItem,
				(ToolStripItem)printPreviewTextToolStripMenuItem,
				(ToolStripItem)printTextToolStripMenuItem,
				(ToolStripItem)saveTextToolStripMenuItem,
				(ToolStripItem)toolStripSeparator1,
				(ToolStripItem)copyGraphicToolStripMenuItem,
				(ToolStripItem)printPreviewGraphicToolStripMenuItem,
				(ToolStripItem)printGraphicToolStripMenuItem,
				(ToolStripItem)saveGraphicToolStripMenuItem
			});
			((ToolStripItem)withPredictionsToolStripMenuItem).set_Name("withPredictionsToolStripMenuItem");
			((ToolStripItem)withPredictionsToolStripMenuItem).set_Size(new Size(119, 20));
			((ToolStripItem)withPredictionsToolStripMenuItem).set_Text("with Predictions...  ");
			((ToolStripItem)drawInBWToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)drawInBWToolStripMenuItem).set_Name("drawInBWToolStripMenuItem");
			((ToolStripItem)drawInBWToolStripMenuItem).set_Size(new Size(220, 22));
			((ToolStripItem)drawInBWToolStripMenuItem).set_Text("Draw in B&&W");
			((ToolStripItem)drawInBWToolStripMenuItem).add_Click((EventHandler)drawInBWToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator2).set_Name("toolStripSeparator2");
			((ToolStripItem)toolStripSeparator2).set_Size(new Size(217, 6));
			((ToolStripItem)copyTextToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyTextToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyTextToolStripMenuItem).set_Name("copyTextToolStripMenuItem");
			copyTextToolStripMenuItem.set_ShortcutKeys((Keys)131139);
			((ToolStripItem)copyTextToolStripMenuItem).set_Size(new Size(220, 22));
			((ToolStripItem)copyTextToolStripMenuItem).set_Text("Copy text");
			((ToolStripItem)copyTextToolStripMenuItem).add_Click((EventHandler)copyTextToolStripMenuItem_Click);
			((ToolStripItem)printPreviewTextToolStripMenuItem).set_Image((Image)Resources.PrintPreview);
			((ToolStripItem)printPreviewTextToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printPreviewTextToolStripMenuItem).set_Name("printPreviewTextToolStripMenuItem");
			((ToolStripItem)printPreviewTextToolStripMenuItem).set_Size(new Size(220, 22));
			((ToolStripItem)printPreviewTextToolStripMenuItem).set_Text("Print preview text");
			((ToolStripItem)printPreviewTextToolStripMenuItem).add_Click((EventHandler)printPreviewTextToolStripMenuItem_Click);
			((ToolStripItem)printTextToolStripMenuItem).set_Image((Image)Resources.Print);
			((ToolStripItem)printTextToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printTextToolStripMenuItem).set_Name("printTextToolStripMenuItem");
			printTextToolStripMenuItem.set_ShortcutKeys((Keys)131152);
			((ToolStripItem)printTextToolStripMenuItem).set_Size(new Size(220, 22));
			((ToolStripItem)printTextToolStripMenuItem).set_Text("Print text");
			((ToolStripItem)printTextToolStripMenuItem).add_Click((EventHandler)printTextToolStripMenuItem_Click);
			((ToolStripItem)saveTextToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveTextToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveTextToolStripMenuItem).set_Name("saveTextToolStripMenuItem");
			saveTextToolStripMenuItem.set_ShortcutKeys((Keys)131155);
			((ToolStripItem)saveTextToolStripMenuItem).set_Size(new Size(220, 22));
			((ToolStripItem)saveTextToolStripMenuItem).set_Text("Save Text");
			((ToolStripItem)saveTextToolStripMenuItem).add_Click((EventHandler)saveTextToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator1).set_Name("toolStripSeparator1");
			((ToolStripItem)toolStripSeparator1).set_Size(new Size(217, 6));
			((ToolStripItem)copyGraphicToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyGraphicToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyGraphicToolStripMenuItem).set_Name("copyGraphicToolStripMenuItem");
			copyGraphicToolStripMenuItem.set_ShortcutKeys((Keys)196675);
			((ToolStripItem)copyGraphicToolStripMenuItem).set_Size(new Size(220, 22));
			((ToolStripItem)copyGraphicToolStripMenuItem).set_Text("Copy Graphic");
			((ToolStripItem)copyGraphicToolStripMenuItem).add_Click((EventHandler)copyGraphicToolStripMenuItem_Click);
			((ToolStripItem)printPreviewGraphicToolStripMenuItem).set_Enabled(false);
			((ToolStripItem)printPreviewGraphicToolStripMenuItem).set_Image((Image)Resources.PrintPreview);
			((ToolStripItem)printPreviewGraphicToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printPreviewGraphicToolStripMenuItem).set_Name("printPreviewGraphicToolStripMenuItem");
			((ToolStripItem)printPreviewGraphicToolStripMenuItem).set_Size(new Size(220, 22));
			((ToolStripItem)printPreviewGraphicToolStripMenuItem).set_Text("Print preview graphic");
			((ToolStripItem)printPreviewGraphicToolStripMenuItem).add_Click((EventHandler)printPreviewGraphicToolStripMenuItem_Click);
			((ToolStripItem)printGraphicToolStripMenuItem).set_Enabled(false);
			((ToolStripItem)printGraphicToolStripMenuItem).set_Image((Image)Resources.Print);
			((ToolStripItem)printGraphicToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printGraphicToolStripMenuItem).set_Name("printGraphicToolStripMenuItem");
			printGraphicToolStripMenuItem.set_ShortcutKeys((Keys)196688);
			((ToolStripItem)printGraphicToolStripMenuItem).set_Size(new Size(220, 22));
			((ToolStripItem)printGraphicToolStripMenuItem).set_Text("Print Graphic");
			((ToolStripItem)printGraphicToolStripMenuItem).add_Click((EventHandler)printGraphicToolStripMenuItem_Click);
			((ToolStripItem)saveGraphicToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveGraphicToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveGraphicToolStripMenuItem).set_Name("saveGraphicToolStripMenuItem");
			saveGraphicToolStripMenuItem.set_ShortcutKeys((Keys)196691);
			((ToolStripItem)saveGraphicToolStripMenuItem).set_Size(new Size(220, 22));
			((ToolStripItem)saveGraphicToolStripMenuItem).set_Text("Save Graphic");
			((ToolStripItem)saveGraphicToolStripMenuItem).add_Click((EventHandler)saveGraphicToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(72, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("&Help    ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(53, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("E&xit");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(808, 597));
			((Control)this).get_Controls().Add((Control)(object)progressBar1);
			((Control)this).get_Controls().Add((Control)(object)groupBox1);
			((Control)this).get_Controls().Add((Control)(object)picConjunction);
			((Control)this).get_Controls().Add((Control)(object)lstResults);
			((Control)this).get_Controls().Add((Control)(object)panel3);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_Controls().Add((Control)(object)cmdFind);
			((Control)this).get_Controls().Add((Control)(object)cmdCancel);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationEphemConjunctions", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_Icon((Icon)componentResourceManager.GetObject("$this.Icon"));
			((Form)this).set_Location(Settings.Default.LocationEphemConjunctions);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_MinimumSize(new Size(690, 630));
			((Control)this).set_Name("MutualConjunctions");
			((Control)this).set_Text("Mutual conjunctions (appulses) of the planets");
			((Form)this).add_Load((EventHandler)MutualConjunctions_Load);
			((Control)this).add_Resize((EventHandler)MutualConjunctions_Resize);
			((Control)panel3).ResumeLayout(false);
			((Control)panel3).PerformLayout();
			((ISupportInitialize)updnEndMonth).EndInit();
			((ISupportInitialize)updnEndYear).EndInit();
			((ISupportInitialize)updnStartMonth).EndInit();
			((ISupportInitialize)updnStartYear).EndInit();
			((ISupportInitialize)updnSeparation).EndInit();
			((ISupportInitialize)picConjunction).EndInit();
			((Control)groupBox1).ResumeLayout(false);
			((Control)groupBox1).PerformLayout();
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
