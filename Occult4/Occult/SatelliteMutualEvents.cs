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
	public class SatelliteMutualEvents : Form
	{
		private PlanetViewer PlanetConfiguration;

		private readonly string AppPath;

		private static string EventMain;

		private static double JD;

		private static double dT = 0.0;

		private static float ScaleLength;

		private static int Planet;

		private static int ScaleTime = 1;

		private static int ScaleMagnify = 1;

		private static bool BreakFlag = false;

		private const double Radian = 180.0 / Math.PI;

		private const string Header = "Year  M  D  h  m  s  Event Type     Ph   Dur dMag  %Ill   Sep  PA   MinD";

		private static ArrayList Times = new ArrayList();

		internal static double[] ShadowBrightnessForGraphic = new double[101];

		private int NumPointsAcrossShadow;

		private IContainer components;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem exitToolStripMenuItem;

		private ListBox lstEvents;

		private RadioButton optJupiter;

		private RadioButton optSaturn;

		private RadioButton optUranus;

		private NumericUpDown updnStartYear;

		private NumericUpDown updnStartMonth;

		private NumericUpDown updnEndMonth;

		private NumericUpDown updnEndYear;

		private Button cmdCompute;

		private Button cmdCancel;

		private ListBox lstTimes;

		private PictureBox picEvents;

		private ToolStripMenuItem fileToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator1;

		private ToolStripMenuItem printToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator2;

		private ToolStripMenuItem copyEventsToolStripMenuItem;

		private ToolStripMenuItem copyGraphicToolStripMenuItem;

		private ToolStripMenuItem saveGraphicToolStripMenuItem;

		private ToolStripMenuItem helpToolStripMenuItem;

		private NumericUpDown updnLongitude;

		private NumericUpDown updnLatitude;

		private CheckBox chkSiteLimited;

		private Panel panel2;

		private Label label1;

		private Label label2;

		private Panel panel3;

		private Label lblHeader;

		private ProgressBar pbar;

		private GroupBox groupBox1;

		private RadioButton radioButton4;

		private RadioButton radioButton3;

		private RadioButton radioButton2;

		private RadioButton radioButton1;

		private GroupBox groupBox2;

		private RadioButton optTx4;

		private RadioButton optTx3;

		private RadioButton optTx2;

		private RadioButton optTx1;

		private ToolStripMenuItem copyEventsContactTimesToolStripMenuItem;

		private ToolStripMenuItem saveEventsContactTimesToolStripMenuItem;

		private Label label3;

		private Label label4;

		private Label label5;

		private GroupBox groupBox3;

		private Label label6;

		private ToolStripMenuItem printPreviewToolStripMenuItem;

		private ComboBox cmbMissDist;

		private Label label7;

		private CheckBox chkIncludeInvisible;

		private Label label8;

		private Label label9;

		private Label label10;

		private ContextMenuStrip contextMenuStrip1;

		private ToolStripMenuItem displayGraphicOfPlanetSatellitesToolStripMenuItem;

		private ToolStripMenuItem placeEventInVDTimerToolStripMenuItem;

		private ToolStripMenuItem showVDubTimerToolStripMenuItem;

		private PictureBox picShadow;

		private Label label11;

		private ToolStripMenuItem copyEclipseShadowGraphicToolStripMenuItem;

		private ToolStripMenuItem saveEclipseShadowGraphicToolStripMenuItem;

		private Panel panel1;

		private PictureBox picLightCurve;

		private ToolStripMenuItem displayLightCurveLargeToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator3;

		private ToolStripMenuItem copyLightCurveToolStripMenuItem;

		private ToolStripMenuItem saveLightCurveAsCSVToolStripMenuItem;

		private ToolTip toolTip1;

		private Label lblAlt;

		private Label label12;

		private Label label13;

		private ToolStripSeparator toolStripSeparator4;

		private ToolStripMenuItem SaveEclipseShadowProfileAsCSVToolStripMenuItem;

		public SatelliteMutualEvents()
		{
			InitializeComponent();
			AppPath = Utilities.AppPath;
			updnStartYear.set_Value((decimal)DateTime.Now.Year);
			NumericUpDown obj = updnStartMonth;
			decimal value;
			updnEndMonth.set_Value(value = DateTime.Now.Month);
			obj.set_Value(value);
			updnEndYear.set_Value((decimal)DateTime.Now.Year);
			((Control)lblHeader).set_Text("Year  M  D  h  m  s  Event Type     Ph   Dur dMag  %Ill   Sep  PA   MinD");
			((Control)lblAlt).set_Visible(chkSiteLimited.get_Checked());
			if (!decimal.TryParse(Settings.Default.Site_Longitude_dd_d, out var result))
			{
				result = default(decimal);
			}
			updnLongitude.set_Value(result);
			if (!decimal.TryParse(Settings.Default.Site_Latitude_dd_d, out result))
			{
				result = default(decimal);
			}
			updnLatitude.set_Value(result);
		}

		private void SatelliteMutualEvents_Load(object sender, EventArgs e)
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
			((ListControl)cmbMissDist).set_SelectedIndex(0);
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
		}

		private void cmdCompute_Click(object sender, EventArgs e)
		{
			//IL_00de: Unknown result type (might be due to invalid IL or missing references)
			string[] Events = new string[20];
			string[] EventTimes = new string[20];
			double num = Utilities.JD_from_Date((int)updnStartYear.get_Value(), (int)updnStartMonth.get_Value(), 1.0);
			double num2 = Utilities.JD_from_Date((int)updnEndYear.get_Value(), (int)updnEndMonth.get_Value(), 31.0) + 1.0;
			double siteLongitude = (double)updnLongitude.get_Value() / (180.0 / Math.PI);
			double siteLatitude = (double)updnLatitude.get_Value() / (180.0 / Math.PI);
			double[] array = new double[9] { 0.0, 0.1, 0.2, 0.5, 1.0, 1.2, 1.5, 2.0, 5.0 };
			int num3 = (int)(num2 - num);
			if (num3 < 0)
			{
				MessageBox.Show("The end date is earlier than the start date", "Invalid date range", (MessageBoxButtons)0, (MessageBoxIcon)48);
				return;
			}
			if (optJupiter.get_Checked())
			{
				Planet = 5;
			}
			else if (optSaturn.get_Checked())
			{
				Planet = 6;
			}
			else
			{
				Planet = 7;
			}
			Satellites.IncludeHiddenEvents = chkIncludeInvisible.get_Checked();
			lstEvents.get_Items().Clear();
			Times.Clear();
			BreakFlag = false;
			((Control)cmdCompute).set_Visible(false);
			pbar.set_Minimum(0);
			pbar.set_Value(0);
			pbar.set_Maximum(num3);
			((Control)pbar).set_Visible(true);
			for (int i = 0; i < num3; i++)
			{
				pbar.set_Value(i);
				Satellites.FindMutualEvents(num + (double)i, Planet, Eclipses: true, Occultations: true, array[((ListControl)cmbMissDist).get_SelectedIndex()], chkSiteLimited.get_Checked(), siteLongitude, siteLatitude, out var NumberOfEvents, ref Events, ref EventTimes);
				for (int j = 0; j <= NumberOfEvents - 1; j++)
				{
					lstEvents.get_Items().Add((object)Events[j]);
					Times.Add(EventTimes[j]);
					if (lstEvents.get_Items().get_Count() == 1)
					{
						((ListControl)lstEvents).set_SelectedIndex(0);
					}
				}
				Application.DoEvents();
				if (BreakFlag)
				{
					break;
				}
			}
			BreakFlag = false;
			pbar.set_Value(0);
			((Control)pbar).set_Visible(false);
			((Control)cmdCompute).set_Visible(true);
		}

		private void cmdCancel_Click(object sender, EventArgs e)
		{
			BreakFlag = true;
		}

		private void lstEvents_SelectedIndexChanged(object sender, EventArgs e)
		{
			string text = Times[((ListControl)lstEvents).get_SelectedIndex()]!.ToString();
			FillEventTimesBox(text.PadRight(70));
			EventMain = lstEvents.get_SelectedItem().ToString();
			DrawGraphic();
		}

		private void FillEventTimesBox(string ContactTimes)
		{
			lstTimes.get_Items().Clear();
			lstTimes.get_Items().Add((object)"         h  m  s");
			lstTimes.get_Items().Add((object)("  T1 = " + ContactTimes.Substring(0, 10)));
			lstTimes.get_Items().Add((object)("  T2 = " + ContactTimes.Substring(10, 10)));
			lstTimes.get_Items().Add((object)("  T3 = " + ContactTimes.Substring(20, 10)));
			lstTimes.get_Items().Add((object)("TMax = " + ContactTimes.Substring(30, 10)));
			lstTimes.get_Items().Add((object)("  T5 = " + ContactTimes.Substring(40, 10)));
			lstTimes.get_Items().Add((object)("  T6 = " + ContactTimes.Substring(50, 10)));
			lstTimes.get_Items().Add((object)("  T7 = " + ContactTimes.Substring(60, 10)));
		}

		private void DrawGraphic()
		{
			string[] array = new string[9] { "", "(I)", "(II)", "(III)", "(IV)", "(V)", "(VI)", "(VII)", "(VIII)" };
			float num = 1f;
			float num2 = 0f;
			int moon = 0;
			int moon2 = 0;
			NumPointsAcrossShadow = ShadowBrightnessForGraphic.Length;
			Pen pen = new Pen(Color.Black);
			Pen pen2 = new Pen(Color.FromArgb(50, 0, 0, 0));
			Font font = new Font("Times New Roman", 8f);
			float num3 = ((Control)picEvents).get_Height();
			float num4 = ((Control)picEvents).get_Width();
			float num5 = num4 / 2f;
			float num6 = num3 / 2f;
			Bitmap image = new Bitmap((int)num4, (int)num3);
			Graphics graphics = Graphics.FromImage(image);
			if (Settings.Default.GraphicsSmoothed)
			{
				graphics.SmoothingMode = SmoothingMode.AntiAlias;
			}
			graphics.Clear(Color.White);
			float num7 = num3 / 6f;
			float num8 = num4 / 11f;
			int year = int.Parse(EventMain.Substring(0, 4));
			int month = int.Parse(EventMain.Substring(5, 2));
			double num9 = double.Parse(EventMain.Substring(8, 2));
			double num10 = double.Parse(EventMain.Substring(11, 2)) + (double.Parse(EventMain.Substring(14, 2)) + double.Parse(EventMain.Substring(17, 2)) / 60.0) / 60.0;
			dT = Utilities.delta_T(year, month, num9);
			num9 += (num10 + dT / 3600.0) / 24.0;
			if (EventMain.Substring(10, 1) == "-")
			{
				num9 = double.Parse(EventMain.Substring(8, 2)) + (0.0 - num10 + dT / 3600.0) / 24.0;
			}
			double num11 = double.Parse(EventMain.Substring(39, 5)) / 60.0;
			if (num11 < 4.0)
			{
				num11 = 4.0;
			}
			string text = EventMain.Substring(21, 16);
			switch (Planet)
			{
			case 5:
				num = num7;
				ScaleLength = 2f;
				break;
			case 6:
				num = 5f * num7;
				ScaleLength = 0.4f;
				break;
			case 7:
				num = 10f * num7;
				ScaleLength = 0.2f;
				break;
			}
			for (int i = 1; i < 9; i++)
			{
				int num12 = text.IndexOf(array[i]);
				if (num12 == 0)
				{
					moon = i;
				}
				else if (num12 > 1)
				{
					moon2 = i;
				}
				if (Planet == 6 && i == 6 && num12 > 0)
				{
					num = 2.5f * num7;
					ScaleLength = 0.8f;
				}
			}
			num /= (float)ScaleMagnify;
			ScaleLength *= ScaleMagnify;
			string s = ScaleLength.ToString("0.0") + "\"";
			bool flag = text.IndexOf("ecl") >= 0;
			JD = Utilities.JD_from_Date(year, month, num9);
			double num13 = num11 / 7.0 / 1440.0 * (double)ScaleTime;
			ToolStripMenuItem obj = saveEclipseShadowGraphicToolStripMenuItem;
			ToolStripMenuItem saveEclipseShadowProfileAsCSVToolStripMenuItem = SaveEclipseShadowProfileAsCSVToolStripMenuItem;
			bool flag2;
			((ToolStripItem)copyEclipseShadowGraphicToolStripMenuItem).set_Enabled(flag2 = flag);
			bool enabled;
			((ToolStripItem)saveEclipseShadowProfileAsCSVToolStripMenuItem).set_Enabled(enabled = flag2);
			((ToolStripItem)obj).set_Enabled(enabled);
			string EventName;
			int MoonInFront;
			int MoonAtRear;
			double MoonAtRearRadius;
			double MoonInFrontRadius;
			double PenumbralRadius;
			double UmbralRadius;
			double dx;
			double dy;
			double dz;
			double Sepn;
			double PlanetSepn;
			double PlanetPA;
			double DistanceToPlanet;
			float Moon1Mag;
			float Moon2Mag;
			if (flag)
			{
				Satellites.RelativeMoonPositions(JD, Planet, moon, moon2, flag, out EventName, out MoonInFront, out MoonAtRear, out MoonAtRearRadius, out MoonInFrontRadius, out PenumbralRadius, out UmbralRadius, out dx, out dy, out dz, out Sepn, out PlanetSepn, out PlanetPA, out DistanceToPlanet, out Moon1Mag, out Moon2Mag);
				Satellites.SunToMoonRatio = 1.0 - UmbralRadius / MoonInFrontRadius;
				Satellites.CalculateShadowBrightness(ref ShadowBrightnessForGraphic);
			}
			for (int j = -5; j <= 5; j++)
			{
				Satellites.RelativeMoonPositions(JD + (double)j * num13, Planet, moon, moon2, flag, out EventName, out MoonInFront, out MoonAtRear, out MoonAtRearRadius, out MoonInFrontRadius, out PenumbralRadius, out UmbralRadius, out dx, out dy, out dz, out Sepn, out PlanetSepn, out PlanetPA, out DistanceToPlanet, out Moon1Mag, out Moon2Mag);
				if (j == -5)
				{
					graphics.FillRectangle(Brushes.White, 0f, 0f, num4, num3);
					pen.Color = Color.Blue;
					graphics.DrawLine(pen, 0f, num6, num4, num6);
				}
				if (UmbralRadius < 0.0)
				{
					UmbralRadius = 0.0;
				}
				pen.Color = Color.Black;
				float num14 = (float)(MoonAtRearRadius * (double)num);
				graphics.FillEllipse(new SolidBrush(Color.FromArgb(200, 255, 255, 180)), num5 + (float)j * num8 - num14, num6 - num14, 2f * num14, 2f * num14);
				graphics.DrawEllipse(pen, num5 + (float)j * num8 - num14, num6 - num14, 2f * num14, 2f * num14);
				if (flag)
				{
					num14 = (float)(PenumbralRadius * (double)num);
					float num15;
					if (UmbralRadius < 0.0)
					{
						num15 = num14 / 4f;
					}
					num15 = (float)PenumbralRadius * num / 10f;
					num2 = num14;
					for (int k = 0; k < 10; k++)
					{
						num2 = num14 - (float)k * num15;
						int num16 = (int)(180.0 * ShadowBrightnessForGraphic[NumPointsAcrossShadow - NumPointsAcrossShadow / 10 * k - NumPointsAcrossShadow / 20]);
						graphics.FillEllipse(new SolidBrush(Color.FromArgb(100, num16, num16, num16)), num5 + (float)j * num8 - (float)dx * num - num2, num6 - (float)dy * num - num2, 2f * num2, 2f * num2);
					}
					graphics.DrawEllipse(pen2, num5 + (float)j * num8 - (float)dx * num - num14, num6 - (float)dy * num - num14, 2f * num14, 2f * num14);
				}
				num14 = (float)(UmbralRadius * (double)num);
				if (flag)
				{
					graphics.FillEllipse(new SolidBrush(Color.FromArgb(130, 139, 69, 19)), num5 + (float)j * num8 - (float)dx * num - num14, num6 - (float)dy * num - num14, 2f * num14, 2f * num14);
				}
				else
				{
					graphics.FillEllipse(new SolidBrush(Color.FromArgb(200, 245, 222, 179)), num5 + (float)j * num8 - (float)dx * num - num14, num6 - (float)dy * num - num14, 2f * num14, 2f * num14);
					graphics.DrawEllipse(pen, num5 + (float)j * num8 - (float)dx * num - num14, num6 - (float)dy * num - num14, 2f * num14, 2f * num14);
				}
				pen.DashStyle = DashStyle.Dash;
				pen.Color = Color.Chocolate;
				graphics.DrawLine(pen, num5 + (float)j * num8, num6, num5 + (float)j * num8 - (float)dx * num, num6 - (float)dy * num);
				pen.DashStyle = DashStyle.Solid;
				graphics.DrawString(Utilities.DEGtoDMS(num10 + (double)j * num13 * 24.0, 2, 0, MinutesOnly: false), font, Brushes.DarkGreen, num5 + (float)j * num8 - 20f, num3 - 16f);
			}
			graphics.DrawLine(pen, 60f, 10f, 60f + num * ScaleLength, 10f);
			graphics.DrawLine(pen, 60, 6, 60, 14);
			graphics.DrawLine(pen, 60f + num * ScaleLength, 6f, 60f + num * ScaleLength, 14f);
			graphics.DrawString(s, font, Brushes.Chocolate, 60f + num * ScaleLength / 2f - 8f, 12f);
			graphics.DrawString("N", font, Brushes.Black, num5, 5f);
			graphics.DrawString("<E", font, Brushes.Black, 5f, 5f);
			graphics.DrawString("W>", font, Brushes.Black, num4 - 24f, 5f);
			picEvents.set_Image((Image)image);
			graphics.Dispose();
			float num17 = 1f;
			int num18 = 0;
			num3 = ((Control)picShadow).get_Height();
			num4 = ((Control)picShadow).get_Width();
			image = new Bitmap((int)num4, (int)num3);
			num3 -= 1f;
			num4 -= 1f;
			graphics = Graphics.FromImage(image);
			if (Settings.Default.GraphicsSmoothed)
			{
				graphics.SmoothingMode = SmoothingMode.AntiAlias;
			}
			graphics.Clear(Color.FromArgb(10, 50, 50, 50));
			pen.Color = Color.Black;
			graphics.DrawRectangle(pen, 0f, 0f, num4, num3);
			pen.DashPattern = new float[2] { 0.5f, 6f };
			if (flag)
			{
				float num19 = num3 * 0.1f;
				float num20 = num3 * 0.78f;
				float num21 = num4 * 0.1f;
				float num22 = num4 * 0.9f;
				float num23 = (num22 - num21) / 100f;
				float num24 = num20 - num19;
				graphics.FillRectangle(Brushes.BlanchedAlmond, num21, 1f, num22 - num21, num3 - 2f);
				pen.Color = Color.DarkGreen;
				pen.DashStyle = DashStyle.Solid;
				graphics.DrawLine(pen, num4 / 2f - 5f, num19, num4 / 2f + 5f, num19);
				for (int l = 1; l < 10; l++)
				{
					num17 = ((l != 5) ? 1f : 2f);
					graphics.DrawLine(pen, num4 / 2f - num17, num19 + (float)l / 10f * (num20 - num19), num4 / 2f + num17, num19 + (float)l / 10f * (num20 - num19));
				}
				pen.Color = Color.DarkRed;
				graphics.DrawLine(pen, 0f, num20, num4, num20);
				pen.Color = Color.Black;
				pen.Width = 2f;
				pen.DashStyle = DashStyle.Solid;
				graphics.DrawLine(pen, 0f, num19, num21, num19);
				graphics.DrawLine(pen, num22, num19, num4, num19);
				float y;
				float x = (y = 0f);
				for (int m = 0; m < 2 * (NumPointsAcrossShadow - 1); m++)
				{
					num18 = NumPointsAcrossShadow - 1 - m;
					float num25 = num21 + (float)(m / 2) * num23;
					float num26 = num20 - (float)((double)num24 * ShadowBrightnessForGraphic[Math.Abs(num18)]);
					if (m > 0)
					{
						graphics.DrawLine(pen, x, y, num25, num26);
					}
					x = num25;
					y = num26;
				}
				graphics.DrawString(string.Format("mag = {0,4:f3}", 1.0 / Satellites.SunToMoonRatio), new Font("Arial", 8f), Brushes.Black, num4 * 0.17f, num20);
			}
			else
			{
				pen.DashStyle = DashStyle.Solid;
				pen.Color = Color.Red;
				graphics.DrawLine(pen, 0f, 0f, num4, num3);
				graphics.DrawLine(pen, 0f, num3, num4, 0f);
			}
			picShadow.set_Image((Image)image);
			graphics.Dispose();
			Satellites.LightCurveStartTime_UT = JDtoNearestMinute(JD - dT / 86400.0 - 4.5 * num13, RoundUp: false);
			Satellites.LightCurveEndTime_UT = JDtoNearestMinute(JD - dT / 86400.0 + 5.5 * num13, RoundUp: false);
			Satellites.LightCurveStepInterval = 5.0;
			if (num13 > 0.0008)
			{
				Satellites.LightCurveStepInterval = 10.0;
			}
			if (num13 > 0.0016)
			{
				Satellites.LightCurveStepInterval = 15.0;
			}
			if (num13 > 0.0024)
			{
				Satellites.LightCurveStepInterval = 20.0;
			}
			if (num13 > 0.0036)
			{
				Satellites.LightCurveStepInterval = 30.0;
			}
			Satellites.LightCurveCount = (int)Math.Floor((Satellites.LightCurveEndTime_UT - Satellites.LightCurveStartTime_UT + 1E-07) / Satellites.LightCurveStepInterval * 86400.000001) + 1;
			int num27 = 0;
			Satellites.LightCurve = new double[Satellites.LightCurveCount + 1];
			Satellites.EventName = (new string[3] { "Jupiter", "Saturn", "Uranus" })[Planet - 5] + " : " + EventMain.Substring(0, 35) + " - predicted light curve";
			for (int n = 0; n <= Satellites.LightCurveCount; n++)
			{
				Satellites.RelativeMoonPositions(Satellites.LightCurveStartTime_UT + (dT + (double)n * Satellites.LightCurveStepInterval) / 86400.0, Planet, moon, moon2, flag, out EventName, out MoonInFront, out MoonAtRear, out MoonAtRearRadius, out MoonInFrontRadius, out PenumbralRadius, out UmbralRadius, out dx, out dy, out dz, out Sepn, out PlanetSepn, out PlanetPA, out DistanceToPlanet, out Moon1Mag, out Moon2Mag);
				Satellites.LightCurve[num27] = Satellites.GetBrightness(Planet, flag, MoonAtRearRadius, MoonInFrontRadius, UmbralRadius, PenumbralRadius, UmbralRadius, Math.Sqrt(dx * dx + dy * dy), MoonInFront, MoonAtRear);
				num27++;
			}
			Satellites.DrawLightCurve(picLightCurve, ShowLabels: false, 1.0);
			if (Satellites.MutualLightCurve != null)
			{
				((Form)Satellites.MutualLightCurve).Activate();
				Satellites.DrawLightCurve(Satellites.MutualLightCurve.picMutual, ShowLabels: true, (double)Satellites.MutualLightCurve.trackBarScale.get_Value() / 100.0);
			}
		}

		private double JDtoNearestMinute(double JD, bool RoundUp)
		{
			Utilities.Date_from_JD(JD, out var Year, out var Month, out var day);
			int num = (int)Math.Floor(day);
			double num2 = (int)Math.Floor(1440.0 * (day - (double)num));
			if (RoundUp)
			{
				num2 += 1.0;
			}
			return Utilities.JD_from_Date(Year, Month, (double)num + num2 / 1440.0);
		}

		private void radioButton1_CheckedChanged(object sender, EventArgs e)
		{
			if (radioButton1.get_Checked())
			{
				ScaleMagnify = 1;
			}
			DrawGraphic();
		}

		private void radioButton2_CheckedChanged(object sender, EventArgs e)
		{
			if (radioButton2.get_Checked())
			{
				ScaleMagnify = 2;
			}
			DrawGraphic();
		}

		private void radioButton3_CheckedChanged(object sender, EventArgs e)
		{
			if (radioButton3.get_Checked())
			{
				ScaleMagnify = 3;
			}
			DrawGraphic();
		}

		private void radioButton4_CheckedChanged(object sender, EventArgs e)
		{
			if (radioButton4.get_Checked())
			{
				ScaleMagnify = 4;
			}
			DrawGraphic();
		}

		private void optTx1_CheckedChanged(object sender, EventArgs e)
		{
			if (optTx1.get_Checked())
			{
				ScaleTime = 1;
			}
			DrawGraphic();
		}

		private void optTx2_CheckedChanged(object sender, EventArgs e)
		{
			if (optTx2.get_Checked())
			{
				ScaleTime = 2;
			}
			DrawGraphic();
		}

		private void optTx3_CheckedChanged(object sender, EventArgs e)
		{
			if (optTx3.get_Checked())
			{
				ScaleTime = 5;
			}
			DrawGraphic();
		}

		private void optTx4_CheckedChanged(object sender, EventArgs e)
		{
			if (optTx4.get_Checked())
			{
				ScaleTime = 10;
			}
			DrawGraphic();
		}

		private void copyEventsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(CollectEvents(IncludeContactTimes: false));
		}

		private void copyEventsContactTimesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(CollectEvents(IncludeContactTimes: true));
		}

		private void copyLightCurveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(CollectLightCurve());
		}

		private void copyGraphicToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetImage(picEvents.get_Image());
		}

		private void copyEclipseShadowGraphicToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetImage(picShadow.get_Image());
		}

		private void saveGraphicToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings.Default.Save_EphemerisData = Output.SaveGraphic(picEvents.get_Image(), Utilities.Date_from_JD(JD, 0) + " Mutual Events of " + Utilities.Planets[Planet], Settings.Default.Save_EphemerisData);
		}

		private void saveEclipseShadowGraphicToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings.Default.Save_EphemerisData = Output.SaveGraphic(picShadow.get_Image(), Utilities.Date_from_JD(JD, 0) + " Shadow for Mutual Events of " + Utilities.Planets[Planet], Settings.Default.Save_EphemerisData);
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings.Default.Save_EphemerisData = Output.SaveAppendPredictionText(CollectEvents(IncludeContactTimes: false) + "\r\n", "Mutual Events of " + Utilities.Planets[Planet], Settings.Default.Save_EphemerisData);
		}

		private void saveEventsContactTimesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings.Default.Save_EphemerisData = Output.SaveAppendPredictionText(CollectEvents(IncludeContactTimes: true) + "\r\n", "Mutual Events of " + Utilities.Planets[Planet], Settings.Default.Save_EphemerisData);
		}

		private void saveLightCurveAsCSVToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings.Default.Save_EphemerisData = Output.SavePredictionTextasCSV(CollectLightCurve(), EventMain.Substring(0, 36).Trim() + "PredLightCurve", Settings.Default.Save_EphemerisData);
		}

		private string CollectLightCurve()
		{
			StringBuilder stringBuilder = new StringBuilder();
			for (int i = 0; i < Satellites.LightCurveCount; i++)
			{
				stringBuilder.Append(Utilities.DateTime_from_JD(Satellites.LightCurveStartTime_UT + (double)i * Satellites.LightCurveStepInterval / 86400.0, ToSecs: true).Substring(11) + ",");
				stringBuilder.AppendFormat("{0,6:f4}\r\n", Satellites.LightCurve[i]);
			}
			return stringBuilder.ToString();
		}

		private string CollectEvents(bool IncludeContactTimes)
		{
			StringBuilder stringBuilder = new StringBuilder();
			int count = lstEvents.get_Items().get_Count();
			if (IncludeContactTimes)
			{
				if (chkSiteLimited.get_Checked())
				{
					stringBuilder.AppendFormat("Visible at Longitude {0,1:f0}°, Latitude {1,1:f0}°\r\n", updnLongitude.get_Value(), updnLatitude.get_Value());
				}
				stringBuilder.Append("Year  M  D  h  m  s  Event Type     Ph   Dur dMag  %Ill   Sep  PA   MinD");
				if (chkSiteLimited.get_Checked())
				{
					stringBuilder.Append("  Alt  ");
				}
				else
				{
					stringBuilder.Append("       ");
				}
				for (int i = 0; i < 7; i++)
				{
					stringBuilder.Append("   h  m  s");
				}
				stringBuilder.AppendLine("");
			}
			else
			{
				stringBuilder.AppendLine("Year  M  D  h  m  s  Event Type     Ph   Dur dMag  %Ill   Sep  PA   MinD");
			}
			for (int j = 0; j < count; j++)
			{
				if (IncludeContactTimes)
				{
					stringBuilder.Append(lstEvents.get_Items().get_Item(j));
					stringBuilder.Append(" | ");
					stringBuilder.AppendLine(Times[j]!.ToString());
				}
				else
				{
					stringBuilder.Append(lstEvents.get_Items().get_Item(j));
					stringBuilder.AppendLine("");
				}
			}
			return stringBuilder.ToString();
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

		private void printToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.PrintText(CollectEvents(IncludeContactTimes: true));
		}

		private void printPreviewToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.PrintPreviewText(CollectEvents(IncludeContactTimes: true));
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

		private void updnLongitude_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnLongitude).Select(0, 10);
		}

		private void updnLatitude_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnLatitude).Select(0, 10);
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Mutual eclipses and occultations of satellites");
		}

		private void optJupiter_CheckedChanged(object sender, EventArgs e)
		{
			((ListControl)cmbMissDist).set_SelectedIndex(0);
		}

		private void optSaturn_CheckedChanged(object sender, EventArgs e)
		{
			((ListControl)cmbMissDist).set_SelectedIndex(0);
		}

		private void optUranus_CheckedChanged(object sender, EventArgs e)
		{
			((ListControl)cmbMissDist).set_SelectedIndex(0);
		}

		private void lstEvents_MouseClick(object sender, MouseEventArgs e)
		{
		}

		internal void ShowPlanetViewer()
		{
			try
			{
				((Control)PlanetConfiguration).Show();
			}
			catch
			{
				PlanetConfiguration = new PlanetViewer();
				((Control)PlanetConfiguration).Show();
			}
			RadioButton obj2 = PlanetConfiguration.optJupiter;
			RadioButton obj3 = PlanetConfiguration.optSaturn;
			bool flag;
			PlanetConfiguration.optUranus.set_Checked(flag = false);
			bool @checked;
			obj3.set_Checked(@checked = flag);
			obj2.set_Checked(@checked);
			if (optJupiter.get_Checked())
			{
				PlanetConfiguration.optJupiter.set_Checked(true);
			}
			else if (optSaturn.get_Checked())
			{
				PlanetConfiguration.optSaturn.set_Checked(true);
			}
			else if (optUranus.get_Checked())
			{
				PlanetConfiguration.optUranus.set_Checked(true);
			}
			PlanetConfiguration.EnablePlanetSelection(Enable: false);
			PlanetConfiguration.updnYear.set_Value(decimal.Parse(EventMain.Substring(0, 4)));
			PlanetConfiguration.updnMonth.set_Value(decimal.Parse(EventMain.Substring(5, 2)));
			PlanetConfiguration.updnDay.set_Value(decimal.Parse(EventMain.Substring(8, 2)));
			PlanetConfiguration.updnHour.set_Value(decimal.Parse(EventMain.Substring(11, 2)) + (decimal)(double.Parse(EventMain.Substring(14, 2)) / 60.0));
			PlanetConfiguration.chkMagnify.set_Checked(false);
			PlanetConfiguration.chkMagnify2.set_Checked(false);
			PlanetConfiguration.ShowStarPath = false;
			PlanetConfiguration.chkFaintMoons.set_Checked(false);
			((Control)PlanetConfiguration).Focus();
			PlanetConfiguration.DrawPlanetsAndMoons();
		}

		private void lstEvents_MouseDoubleClick(object sender, MouseEventArgs e)
		{
			ShowPlanetViewer();
		}

		private void displayGraphicOfPlanetSatellitesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ShowPlanetViewer();
		}

		private void placeEventInVDTimerToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_01a3: Unknown result type (might be due to invalid IL or missing references)
			string text = "";
			string? text2 = lstEvents.get_SelectedItem().ToString();
			string text3 = text2!.Substring(21, 16);
			int year = int.Parse(text2!.Substring(0, 4));
			int month = int.Parse(text2!.Substring(5, 2));
			int day = int.Parse(text2!.Substring(8, 2));
			int hour = int.Parse(text2!.Substring(11, 2));
			int minute = int.Parse(text2!.Substring(14, 2));
			int second = int.Parse(text2!.Substring(17, 2));
			int num = int.Parse(text2!.Substring(39, 5));
			if (num < 240)
			{
				num = 240;
			}
			DateTime dateTime = new DateTime(year, month, day, hour, minute, second).ToLocalTime().AddSeconds((double)(-num) * 0.7 - 60.0);
			DateTime dateTime2 = new DateTime(year, month, day, hour, minute, second).ToLocalTime().AddSeconds((double)num * 0.7 + 60.0);
			DateTime now = DateTime.Now;
			if (dateTime < now)
			{
				text = "The event { " + text3.Trim() + " } has already started";
			}
			if (dateTime2 < now)
			{
				text = "The event { " + text3.Trim() + " } has already finished";
			}
			if (dateTime > now.AddDays(3.0))
			{
				text = "The event { " + text3.Trim() + " } is more than 72 hours in the future";
			}
			if (text.Length > 0)
			{
				MessageBox.Show(text + "\r\nThe event will not be added to VirtualDub Timer", "Event not added", (MessageBoxButtons)0);
				return;
			}
			LunarOccultations.Show_RecordingTimer();
			LunarOccultations.VDTimer.chkEnableTimer.set_Checked(false);
			LunarOccultations.VDTimer.AddAnEvent_External(dateTime, dateTime2, text3, 0);
		}

		private void showVDubTimerToolStripMenuItem_Click(object sender, EventArgs e)
		{
			LunarOccultations.Show_RecordingTimer();
		}

		private void updnStartMonth_ValueChanged(object sender, EventArgs e)
		{
			if (updnStartYear.get_Value() == updnEndYear.get_Value() && updnStartMonth.get_Value() > updnEndMonth.get_Value())
			{
				updnEndMonth.set_Value(updnStartMonth.get_Value());
			}
		}

		private void updnEndMonth_ValueChanged(object sender, EventArgs e)
		{
			if (updnStartYear.get_Value() == updnEndYear.get_Value() && updnStartMonth.get_Value() > updnEndMonth.get_Value())
			{
				updnStartMonth.set_Value(updnEndMonth.get_Value());
			}
		}

		private void displayLightCurveLargeToolStripMenuItem_Click(object sender, EventArgs e)
		{
			DisplayLightCurve();
		}

		private void picLightCurve_DoubleClick(object sender, EventArgs e)
		{
			DisplayLightCurve();
		}

		private void contextMenuStrip1_Opening(object sender, CancelEventArgs e)
		{
		}

		private void DisplayLightCurve()
		{
			Satellites.MutualLightCurve_Show();
			Satellites.DrawLightCurve(Satellites.MutualLightCurve.picMutual, ShowLabels: true, 1.0);
		}

		private void chkSiteLimited_CheckedChanged(object sender, EventArgs e)
		{
			((Control)lblAlt).set_Visible(chkSiteLimited.get_Checked());
		}

		private void SaveEclipseShadowProfileAsCSVToolStripMenuItem_Click(object sender, EventArgs e)
		{
			StringBuilder stringBuilder = new StringBuilder();
			for (int i = 0; i < 2 * (NumPointsAcrossShadow - 1); i++)
			{
				int value = NumPointsAcrossShadow - 1 - i;
				stringBuilder.AppendFormat("{0,3:f4}, ", i);
				stringBuilder.AppendFormat("{0,6:f4}\r\n", ShadowBrightnessForGraphic[Math.Abs(value)]);
			}
			Settings.Default.Save_EphemerisData = Output.SavePredictionTextasCSV(stringBuilder.ToString(), EventMain.Substring(0, 36).Trim() + "ShadowCurve", Settings.Default.Save_EphemerisData);
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
			//IL_0135: Unknown result type (might be due to invalid IL or missing references)
			//IL_013f: Expected O, but got Unknown
			//IL_0140: Unknown result type (might be due to invalid IL or missing references)
			//IL_014a: Expected O, but got Unknown
			//IL_014b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0155: Expected O, but got Unknown
			//IL_0156: Unknown result type (might be due to invalid IL or missing references)
			//IL_0160: Expected O, but got Unknown
			//IL_0161: Unknown result type (might be due to invalid IL or missing references)
			//IL_016b: Expected O, but got Unknown
			//IL_016c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0176: Expected O, but got Unknown
			//IL_0177: Unknown result type (might be due to invalid IL or missing references)
			//IL_0181: Expected O, but got Unknown
			//IL_0182: Unknown result type (might be due to invalid IL or missing references)
			//IL_018c: Expected O, but got Unknown
			//IL_018d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0197: Expected O, but got Unknown
			//IL_0198: Unknown result type (might be due to invalid IL or missing references)
			//IL_01a2: Expected O, but got Unknown
			//IL_01a3: Unknown result type (might be due to invalid IL or missing references)
			//IL_01ad: Expected O, but got Unknown
			//IL_01ae: Unknown result type (might be due to invalid IL or missing references)
			//IL_01b8: Expected O, but got Unknown
			//IL_01b9: Unknown result type (might be due to invalid IL or missing references)
			//IL_01c3: Expected O, but got Unknown
			//IL_01c4: Unknown result type (might be due to invalid IL or missing references)
			//IL_01ce: Expected O, but got Unknown
			//IL_01cf: Unknown result type (might be due to invalid IL or missing references)
			//IL_01d9: Expected O, but got Unknown
			//IL_01da: Unknown result type (might be due to invalid IL or missing references)
			//IL_01e4: Expected O, but got Unknown
			//IL_01e5: Unknown result type (might be due to invalid IL or missing references)
			//IL_01ef: Expected O, but got Unknown
			//IL_01f0: Unknown result type (might be due to invalid IL or missing references)
			//IL_01fa: Expected O, but got Unknown
			//IL_01fb: Unknown result type (might be due to invalid IL or missing references)
			//IL_0205: Expected O, but got Unknown
			//IL_0206: Unknown result type (might be due to invalid IL or missing references)
			//IL_0210: Expected O, but got Unknown
			//IL_0211: Unknown result type (might be due to invalid IL or missing references)
			//IL_021b: Expected O, but got Unknown
			//IL_021c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0226: Expected O, but got Unknown
			//IL_0227: Unknown result type (might be due to invalid IL or missing references)
			//IL_0231: Expected O, but got Unknown
			//IL_0232: Unknown result type (might be due to invalid IL or missing references)
			//IL_023c: Expected O, but got Unknown
			//IL_023d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0247: Expected O, but got Unknown
			//IL_0248: Unknown result type (might be due to invalid IL or missing references)
			//IL_0252: Expected O, but got Unknown
			//IL_0253: Unknown result type (might be due to invalid IL or missing references)
			//IL_025d: Expected O, but got Unknown
			//IL_025e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0268: Expected O, but got Unknown
			//IL_0269: Unknown result type (might be due to invalid IL or missing references)
			//IL_0273: Expected O, but got Unknown
			//IL_0274: Unknown result type (might be due to invalid IL or missing references)
			//IL_027e: Expected O, but got Unknown
			//IL_027f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0289: Expected O, but got Unknown
			//IL_028a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0294: Expected O, but got Unknown
			//IL_0295: Unknown result type (might be due to invalid IL or missing references)
			//IL_029f: Expected O, but got Unknown
			//IL_02a0: Unknown result type (might be due to invalid IL or missing references)
			//IL_02aa: Expected O, but got Unknown
			//IL_02ab: Unknown result type (might be due to invalid IL or missing references)
			//IL_02b5: Expected O, but got Unknown
			//IL_02b6: Unknown result type (might be due to invalid IL or missing references)
			//IL_02c0: Expected O, but got Unknown
			//IL_02c1: Unknown result type (might be due to invalid IL or missing references)
			//IL_02cb: Expected O, but got Unknown
			//IL_02cc: Unknown result type (might be due to invalid IL or missing references)
			//IL_02d6: Expected O, but got Unknown
			//IL_02d7: Unknown result type (might be due to invalid IL or missing references)
			//IL_02e1: Expected O, but got Unknown
			//IL_02e2: Unknown result type (might be due to invalid IL or missing references)
			//IL_02ec: Expected O, but got Unknown
			//IL_02ed: Unknown result type (might be due to invalid IL or missing references)
			//IL_02f7: Expected O, but got Unknown
			//IL_02f8: Unknown result type (might be due to invalid IL or missing references)
			//IL_0302: Expected O, but got Unknown
			//IL_0303: Unknown result type (might be due to invalid IL or missing references)
			//IL_030d: Expected O, but got Unknown
			//IL_030e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0318: Expected O, but got Unknown
			//IL_0319: Unknown result type (might be due to invalid IL or missing references)
			//IL_0323: Expected O, but got Unknown
			//IL_0324: Unknown result type (might be due to invalid IL or missing references)
			//IL_032e: Expected O, but got Unknown
			//IL_032f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0339: Expected O, but got Unknown
			//IL_033a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0344: Expected O, but got Unknown
			//IL_034b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0355: Expected O, but got Unknown
			//IL_0356: Unknown result type (might be due to invalid IL or missing references)
			//IL_0360: Expected O, but got Unknown
			//IL_0361: Unknown result type (might be due to invalid IL or missing references)
			//IL_036b: Expected O, but got Unknown
			//IL_036c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0376: Expected O, but got Unknown
			//IL_0eb1: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ebb: Expected O, but got Unknown
			//IL_0edf: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ee9: Expected O, but got Unknown
			//IL_1804: Unknown result type (might be due to invalid IL or missing references)
			//IL_180e: Expected O, but got Unknown
			//IL_2cc3: Unknown result type (might be due to invalid IL or missing references)
			//IL_2ccd: Expected O, but got Unknown
			components = new Container();
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(SatelliteMutualEvents));
			menuStrip1 = new MenuStrip();
			fileToolStripMenuItem = new ToolStripMenuItem();
			copyEventsToolStripMenuItem = new ToolStripMenuItem();
			copyEventsContactTimesToolStripMenuItem = new ToolStripMenuItem();
			copyGraphicToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator = new ToolStripSeparator();
			printPreviewToolStripMenuItem = new ToolStripMenuItem();
			printToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator1 = new ToolStripSeparator();
			saveToolStripMenuItem = new ToolStripMenuItem();
			saveEventsContactTimesToolStripMenuItem = new ToolStripMenuItem();
			saveGraphicToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator4 = new ToolStripSeparator();
			copyLightCurveToolStripMenuItem = new ToolStripMenuItem();
			saveLightCurveAsCSVToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator2 = new ToolStripSeparator();
			copyEclipseShadowGraphicToolStripMenuItem = new ToolStripMenuItem();
			saveEclipseShadowGraphicToolStripMenuItem = new ToolStripMenuItem();
			SaveEclipseShadowProfileAsCSVToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator3 = new ToolStripSeparator();
			displayLightCurveLargeToolStripMenuItem = new ToolStripMenuItem();
			showVDubTimerToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			lstEvents = new ListBox();
			contextMenuStrip1 = new ContextMenuStrip(components);
			displayGraphicOfPlanetSatellitesToolStripMenuItem = new ToolStripMenuItem();
			placeEventInVDTimerToolStripMenuItem = new ToolStripMenuItem();
			optJupiter = new RadioButton();
			optSaturn = new RadioButton();
			optUranus = new RadioButton();
			updnStartYear = new NumericUpDown();
			updnStartMonth = new NumericUpDown();
			updnEndMonth = new NumericUpDown();
			updnEndYear = new NumericUpDown();
			cmdCompute = new Button();
			cmdCancel = new Button();
			lstTimes = new ListBox();
			updnLongitude = new NumericUpDown();
			updnLatitude = new NumericUpDown();
			chkSiteLimited = new CheckBox();
			panel2 = new Panel();
			label1 = new Label();
			label2 = new Label();
			panel3 = new Panel();
			lblHeader = new Label();
			pbar = new ProgressBar();
			groupBox1 = new GroupBox();
			radioButton4 = new RadioButton();
			radioButton3 = new RadioButton();
			radioButton2 = new RadioButton();
			radioButton1 = new RadioButton();
			groupBox2 = new GroupBox();
			optTx4 = new RadioButton();
			optTx3 = new RadioButton();
			optTx2 = new RadioButton();
			optTx1 = new RadioButton();
			label3 = new Label();
			label4 = new Label();
			label5 = new Label();
			groupBox3 = new GroupBox();
			picEvents = new PictureBox();
			label6 = new Label();
			cmbMissDist = new ComboBox();
			label7 = new Label();
			chkIncludeInvisible = new CheckBox();
			label8 = new Label();
			label9 = new Label();
			label10 = new Label();
			picShadow = new PictureBox();
			label11 = new Label();
			panel1 = new Panel();
			picLightCurve = new PictureBox();
			toolTip1 = new ToolTip(components);
			lblAlt = new Label();
			label12 = new Label();
			label13 = new Label();
			((Control)menuStrip1).SuspendLayout();
			((Control)contextMenuStrip1).SuspendLayout();
			((ISupportInitialize)updnStartYear).BeginInit();
			((ISupportInitialize)updnStartMonth).BeginInit();
			((ISupportInitialize)updnEndMonth).BeginInit();
			((ISupportInitialize)updnEndYear).BeginInit();
			((ISupportInitialize)updnLongitude).BeginInit();
			((ISupportInitialize)updnLatitude).BeginInit();
			((Control)panel2).SuspendLayout();
			((Control)panel3).SuspendLayout();
			((Control)groupBox1).SuspendLayout();
			((Control)groupBox2).SuspendLayout();
			((Control)groupBox3).SuspendLayout();
			((ISupportInitialize)picEvents).BeginInit();
			((ISupportInitialize)picShadow).BeginInit();
			((Control)panel1).SuspendLayout();
			((ISupportInitialize)picLightCurve).BeginInit();
			((Control)this).SuspendLayout();
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[4]
			{
				(ToolStripItem)fileToolStripMenuItem,
				(ToolStripItem)showVDubTimerToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(809, 24));
			((Control)menuStrip1).set_TabIndex(12);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)fileToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[19]
			{
				(ToolStripItem)copyEventsToolStripMenuItem,
				(ToolStripItem)copyEventsContactTimesToolStripMenuItem,
				(ToolStripItem)copyGraphicToolStripMenuItem,
				(ToolStripItem)toolStripSeparator,
				(ToolStripItem)printPreviewToolStripMenuItem,
				(ToolStripItem)printToolStripMenuItem,
				(ToolStripItem)toolStripSeparator1,
				(ToolStripItem)saveToolStripMenuItem,
				(ToolStripItem)saveEventsContactTimesToolStripMenuItem,
				(ToolStripItem)saveGraphicToolStripMenuItem,
				(ToolStripItem)toolStripSeparator4,
				(ToolStripItem)copyLightCurveToolStripMenuItem,
				(ToolStripItem)saveLightCurveAsCSVToolStripMenuItem,
				(ToolStripItem)toolStripSeparator2,
				(ToolStripItem)copyEclipseShadowGraphicToolStripMenuItem,
				(ToolStripItem)saveEclipseShadowGraphicToolStripMenuItem,
				(ToolStripItem)SaveEclipseShadowProfileAsCSVToolStripMenuItem,
				(ToolStripItem)toolStripSeparator3,
				(ToolStripItem)displayLightCurveLargeToolStripMenuItem
			});
			((ToolStripItem)fileToolStripMenuItem).set_Name("fileToolStripMenuItem");
			((ToolStripItem)fileToolStripMenuItem).set_Size(new Size(116, 20));
			((ToolStripItem)fileToolStripMenuItem).set_Text("&with Predictions... ");
			((ToolStripItem)copyEventsToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyEventsToolStripMenuItem).set_ImageTransparentColor(Color.Magenta);
			((ToolStripItem)copyEventsToolStripMenuItem).set_Name("copyEventsToolStripMenuItem");
			copyEventsToolStripMenuItem.set_ShortcutKeys((Keys)131139);
			((ToolStripItem)copyEventsToolStripMenuItem).set_Size(new Size(301, 22));
			((ToolStripItem)copyEventsToolStripMenuItem).set_Text("Copy events");
			((ToolStripItem)copyEventsToolStripMenuItem).add_Click((EventHandler)copyEventsToolStripMenuItem_Click);
			((ToolStripItem)copyEventsContactTimesToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyEventsContactTimesToolStripMenuItem).set_ImageTransparentColor(Color.Magenta);
			((ToolStripItem)copyEventsContactTimesToolStripMenuItem).set_Name("copyEventsContactTimesToolStripMenuItem");
			copyEventsContactTimesToolStripMenuItem.set_ShortcutKeys((Keys)196675);
			((ToolStripItem)copyEventsContactTimesToolStripMenuItem).set_Size(new Size(301, 22));
			((ToolStripItem)copyEventsContactTimesToolStripMenuItem).set_Text("Copy events && contact times");
			((ToolStripItem)copyEventsContactTimesToolStripMenuItem).add_Click((EventHandler)copyEventsContactTimesToolStripMenuItem_Click);
			((ToolStripItem)copyGraphicToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyGraphicToolStripMenuItem).set_ImageTransparentColor(Color.Magenta);
			((ToolStripItem)copyGraphicToolStripMenuItem).set_Name("copyGraphicToolStripMenuItem");
			copyGraphicToolStripMenuItem.set_ShortcutKeys((Keys)393283);
			((ToolStripItem)copyGraphicToolStripMenuItem).set_Size(new Size(301, 22));
			((ToolStripItem)copyGraphicToolStripMenuItem).set_Text("Copy event graphic");
			((ToolStripItem)copyGraphicToolStripMenuItem).add_Click((EventHandler)copyGraphicToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator).set_Name("toolStripSeparator");
			((ToolStripItem)toolStripSeparator).set_Size(new Size(298, 6));
			((ToolStripItem)printPreviewToolStripMenuItem).set_Image((Image)Resources.PrintPreview);
			((ToolStripItem)printPreviewToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Name("printPreviewToolStripMenuItem");
			((ToolStripItem)printPreviewToolStripMenuItem).set_Size(new Size(301, 22));
			((ToolStripItem)printPreviewToolStripMenuItem).set_Text("Print Preview");
			((ToolStripItem)printPreviewToolStripMenuItem).add_Click((EventHandler)printPreviewToolStripMenuItem_Click);
			((ToolStripItem)printToolStripMenuItem).set_Image((Image)componentResourceManager.GetObject("printToolStripMenuItem.Image"));
			((ToolStripItem)printToolStripMenuItem).set_ImageTransparentColor(Color.Magenta);
			((ToolStripItem)printToolStripMenuItem).set_Name("printToolStripMenuItem");
			printToolStripMenuItem.set_ShortcutKeys((Keys)131152);
			((ToolStripItem)printToolStripMenuItem).set_Size(new Size(301, 22));
			((ToolStripItem)printToolStripMenuItem).set_Text("&Print");
			((ToolStripItem)printToolStripMenuItem).add_Click((EventHandler)printToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator1).set_Name("toolStripSeparator1");
			((ToolStripItem)toolStripSeparator1).set_Size(new Size(298, 6));
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)componentResourceManager.GetObject("saveToolStripMenuItem.Image"));
			((ToolStripItem)saveToolStripMenuItem).set_ImageTransparentColor(Color.Magenta);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			saveToolStripMenuItem.set_ShortcutKeys((Keys)131155);
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(301, 22));
			((ToolStripItem)saveToolStripMenuItem).set_Text("&Save events");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
			((ToolStripItem)saveEventsContactTimesToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveEventsContactTimesToolStripMenuItem).set_ImageTransparentColor(Color.Magenta);
			((ToolStripItem)saveEventsContactTimesToolStripMenuItem).set_Name("saveEventsContactTimesToolStripMenuItem");
			saveEventsContactTimesToolStripMenuItem.set_ShortcutKeys((Keys)196691);
			((ToolStripItem)saveEventsContactTimesToolStripMenuItem).set_Size(new Size(301, 22));
			((ToolStripItem)saveEventsContactTimesToolStripMenuItem).set_Text("Save events && contact times");
			((ToolStripItem)saveEventsContactTimesToolStripMenuItem).add_Click((EventHandler)saveEventsContactTimesToolStripMenuItem_Click);
			((ToolStripItem)saveGraphicToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveGraphicToolStripMenuItem).set_ImageTransparentColor(Color.Magenta);
			((ToolStripItem)saveGraphicToolStripMenuItem).set_Name("saveGraphicToolStripMenuItem");
			saveGraphicToolStripMenuItem.set_ShortcutKeys((Keys)393299);
			((ToolStripItem)saveGraphicToolStripMenuItem).set_Size(new Size(301, 22));
			((ToolStripItem)saveGraphicToolStripMenuItem).set_Text("Save event graphic");
			((ToolStripItem)saveGraphicToolStripMenuItem).add_Click((EventHandler)saveGraphicToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator4).set_Name("toolStripSeparator4");
			((ToolStripItem)toolStripSeparator4).set_Size(new Size(298, 6));
			((ToolStripItem)copyLightCurveToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyLightCurveToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyLightCurveToolStripMenuItem).set_Name("copyLightCurveToolStripMenuItem");
			((ToolStripItem)copyLightCurveToolStripMenuItem).set_Size(new Size(301, 22));
			((ToolStripItem)copyLightCurveToolStripMenuItem).set_Text("Light curve - Copy as CSV data");
			((ToolStripItem)copyLightCurveToolStripMenuItem).add_Click((EventHandler)copyLightCurveToolStripMenuItem_Click);
			((ToolStripItem)saveLightCurveAsCSVToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveLightCurveAsCSVToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveLightCurveAsCSVToolStripMenuItem).set_Name("saveLightCurveAsCSVToolStripMenuItem");
			((ToolStripItem)saveLightCurveAsCSVToolStripMenuItem).set_Size(new Size(301, 22));
			((ToolStripItem)saveLightCurveAsCSVToolStripMenuItem).set_Text("Light curve - Save as CSV");
			((ToolStripItem)saveLightCurveAsCSVToolStripMenuItem).add_Click((EventHandler)saveLightCurveAsCSVToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator2).set_Name("toolStripSeparator2");
			((ToolStripItem)toolStripSeparator2).set_Size(new Size(298, 6));
			((ToolStripItem)copyEclipseShadowGraphicToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyEclipseShadowGraphicToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyEclipseShadowGraphicToolStripMenuItem).set_Name("copyEclipseShadowGraphicToolStripMenuItem");
			((ToolStripItem)copyEclipseShadowGraphicToolStripMenuItem).set_Size(new Size(301, 22));
			((ToolStripItem)copyEclipseShadowGraphicToolStripMenuItem).set_Text("Eclipse shadow profile - Copy image");
			((ToolStripItem)copyEclipseShadowGraphicToolStripMenuItem).add_Click((EventHandler)copyEclipseShadowGraphicToolStripMenuItem_Click);
			((ToolStripItem)saveEclipseShadowGraphicToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveEclipseShadowGraphicToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveEclipseShadowGraphicToolStripMenuItem).set_Name("saveEclipseShadowGraphicToolStripMenuItem");
			((ToolStripItem)saveEclipseShadowGraphicToolStripMenuItem).set_Size(new Size(301, 22));
			((ToolStripItem)saveEclipseShadowGraphicToolStripMenuItem).set_Text("Eclipse shadow profile - Save image");
			((ToolStripItem)saveEclipseShadowGraphicToolStripMenuItem).add_Click((EventHandler)saveEclipseShadowGraphicToolStripMenuItem_Click);
			((ToolStripItem)SaveEclipseShadowProfileAsCSVToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)SaveEclipseShadowProfileAsCSVToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)SaveEclipseShadowProfileAsCSVToolStripMenuItem).set_Name("SaveEclipseShadowProfileAsCSVToolStripMenuItem");
			((ToolStripItem)SaveEclipseShadowProfileAsCSVToolStripMenuItem).set_Size(new Size(301, 22));
			((ToolStripItem)SaveEclipseShadowProfileAsCSVToolStripMenuItem).set_Text("Eclipse shadow profile - Save as CSV");
			((ToolStripItem)SaveEclipseShadowProfileAsCSVToolStripMenuItem).add_Click((EventHandler)SaveEclipseShadowProfileAsCSVToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator3).set_Name("toolStripSeparator3");
			((ToolStripItem)toolStripSeparator3).set_Size(new Size(298, 6));
			((ToolStripItem)displayLightCurveLargeToolStripMenuItem).set_Image((Image)Resources.Fresnel);
			((ToolStripItem)displayLightCurveLargeToolStripMenuItem).set_Name("displayLightCurveLargeToolStripMenuItem");
			((ToolStripItem)displayLightCurveLargeToolStripMenuItem).set_Size(new Size(301, 22));
			((ToolStripItem)displayLightCurveLargeToolStripMenuItem).set_Text("Display light curve (Large)");
			((ToolStripItem)displayLightCurveLargeToolStripMenuItem).add_Click((EventHandler)displayLightCurveLargeToolStripMenuItem_Click);
			((ToolStripItem)showVDubTimerToolStripMenuItem).set_Image((Image)Resources.CLOCK02);
			((ToolStripItem)showVDubTimerToolStripMenuItem).set_Name("showVDubTimerToolStripMenuItem");
			((ToolStripItem)showVDubTimerToolStripMenuItem).set_Size(new Size(165, 20));
			((ToolStripItem)showVDubTimerToolStripMenuItem).set_Text("show Recording Timer    ");
			((ToolStripItem)showVDubTimerToolStripMenuItem).add_Click((EventHandler)showVDubTimerToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(60, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("&Help");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(54, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("E&xit");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((Control)lstEvents).set_ContextMenuStrip(contextMenuStrip1);
			((Control)lstEvents).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstEvents).set_FormattingEnabled(true);
			lstEvents.set_ItemHeight(14);
			((Control)lstEvents).set_Location(new Point(4, 107));
			((Control)lstEvents).set_Name("lstEvents");
			((Control)lstEvents).set_Size(new Size(560, 298));
			((Control)lstEvents).set_TabIndex(9);
			lstEvents.add_MouseClick(new MouseEventHandler(lstEvents_MouseClick));
			lstEvents.add_SelectedIndexChanged((EventHandler)lstEvents_SelectedIndexChanged);
			((Control)lstEvents).add_MouseDoubleClick(new MouseEventHandler(lstEvents_MouseDoubleClick));
			((ToolStrip)contextMenuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)displayGraphicOfPlanetSatellitesToolStripMenuItem,
				(ToolStripItem)placeEventInVDTimerToolStripMenuItem
			});
			((Control)contextMenuStrip1).set_Name("contextMenuStrip1");
			((Control)contextMenuStrip1).set_Size(new Size(267, 48));
			((ToolStripDropDown)contextMenuStrip1).add_Opening((CancelEventHandler)contextMenuStrip1_Opening);
			((ToolStripItem)displayGraphicOfPlanetSatellitesToolStripMenuItem).set_Name("displayGraphicOfPlanetSatellitesToolStripMenuItem");
			((ToolStripItem)displayGraphicOfPlanetSatellitesToolStripMenuItem).set_Size(new Size(266, 22));
			((ToolStripItem)displayGraphicOfPlanetSatellitesToolStripMenuItem).set_Text("Display graphic of planet && satellites");
			((ToolStripItem)displayGraphicOfPlanetSatellitesToolStripMenuItem).add_Click((EventHandler)displayGraphicOfPlanetSatellitesToolStripMenuItem_Click);
			((ToolStripItem)placeEventInVDTimerToolStripMenuItem).set_Name("placeEventInVDTimerToolStripMenuItem");
			((ToolStripItem)placeEventInVDTimerToolStripMenuItem).set_Size(new Size(266, 22));
			((ToolStripItem)placeEventInVDTimerToolStripMenuItem).set_Text("Place event in Recording Timer");
			((ToolStripItem)placeEventInVDTimerToolStripMenuItem).add_Click((EventHandler)placeEventInVDTimerToolStripMenuItem_Click);
			((Control)optJupiter).set_AutoSize(true);
			optJupiter.set_Checked(true);
			((Control)optJupiter).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optJupiter).set_Location(new Point(9, 5));
			((Control)optJupiter).set_Name("optJupiter");
			((Control)optJupiter).set_Size(new Size(63, 17));
			((Control)optJupiter).set_TabIndex(0);
			optJupiter.set_TabStop(true);
			((Control)optJupiter).set_Text("Jupiter");
			((ButtonBase)optJupiter).set_UseVisualStyleBackColor(true);
			optJupiter.add_CheckedChanged((EventHandler)optJupiter_CheckedChanged);
			((Control)optSaturn).set_AutoSize(true);
			((Control)optSaturn).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optSaturn).set_Location(new Point(9, 21));
			((Control)optSaturn).set_Name("optSaturn");
			((Control)optSaturn).set_Size(new Size(62, 17));
			((Control)optSaturn).set_TabIndex(1);
			optSaturn.set_TabStop(true);
			((Control)optSaturn).set_Text("Saturn");
			((ButtonBase)optSaturn).set_UseVisualStyleBackColor(true);
			optSaturn.add_CheckedChanged((EventHandler)optSaturn_CheckedChanged);
			((Control)optUranus).set_AutoSize(true);
			((Control)optUranus).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optUranus).set_Location(new Point(9, 37));
			((Control)optUranus).set_Name("optUranus");
			((Control)optUranus).set_Size(new Size(65, 17));
			((Control)optUranus).set_TabIndex(2);
			optUranus.set_TabStop(true);
			((Control)optUranus).set_Text("Uranus");
			((ButtonBase)optUranus).set_UseVisualStyleBackColor(true);
			optUranus.add_CheckedChanged((EventHandler)optUranus_CheckedChanged);
			((Control)updnStartYear).set_Location(new Point(106, 4));
			updnStartYear.set_Maximum(new decimal(new int[4] { 3000, 0, 0, 0 }));
			updnStartYear.set_Minimum(new decimal(new int[4] { 1000, 0, 0, 0 }));
			((Control)updnStartYear).set_Name("updnStartYear");
			((Control)updnStartYear).set_Size(new Size(51, 20));
			((Control)updnStartYear).set_TabIndex(2);
			updnStartYear.set_Value(new decimal(new int[4] { 1000, 0, 0, 0 }));
			updnStartYear.add_ValueChanged((EventHandler)updnStartYear_ValueChanged);
			((Control)updnStartYear).add_Enter((EventHandler)updnStartYear_Enter);
			((Control)updnStartMonth).set_Location(new Point(164, 4));
			updnStartMonth.set_Maximum(new decimal(new int[4] { 12, 0, 0, 0 }));
			updnStartMonth.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnStartMonth).set_Name("updnStartMonth");
			((Control)updnStartMonth).set_Size(new Size(40, 20));
			((Control)updnStartMonth).set_TabIndex(4);
			updnStartMonth.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			updnStartMonth.add_ValueChanged((EventHandler)updnStartMonth_ValueChanged);
			((Control)updnStartMonth).add_Enter((EventHandler)updnStartMonth_Enter);
			((Control)updnEndMonth).set_Location(new Point(165, 31));
			updnEndMonth.set_Maximum(new decimal(new int[4] { 12, 0, 0, 0 }));
			updnEndMonth.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnEndMonth).set_Name("updnEndMonth");
			((Control)updnEndMonth).set_Size(new Size(39, 20));
			((Control)updnEndMonth).set_TabIndex(5);
			updnEndMonth.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			updnEndMonth.add_ValueChanged((EventHandler)updnEndMonth_ValueChanged);
			((Control)updnEndMonth).add_Enter((EventHandler)updnEndMonth_Enter);
			((Control)updnEndYear).set_Location(new Point(106, 31));
			updnEndYear.set_Maximum(new decimal(new int[4] { 3000, 0, 0, 0 }));
			updnEndYear.set_Minimum(new decimal(new int[4] { 1000, 0, 0, 0 }));
			((Control)updnEndYear).set_Name("updnEndYear");
			((Control)updnEndYear).set_Size(new Size(51, 20));
			((Control)updnEndYear).set_TabIndex(3);
			updnEndYear.set_Value(new decimal(new int[4] { 1000, 0, 0, 0 }));
			updnEndYear.add_ValueChanged((EventHandler)updnEndYear_ValueChanged);
			((Control)updnEndYear).add_Enter((EventHandler)updnEndYear_Enter);
			((Control)cmdCompute).set_Location(new Point(716, 33));
			((Control)cmdCompute).set_Name("cmdCompute");
			((Control)cmdCompute).set_Size(new Size(70, 32));
			((Control)cmdCompute).set_TabIndex(3);
			((Control)cmdCompute).set_Text("Compute");
			((ButtonBase)cmdCompute).set_UseVisualStyleBackColor(true);
			((Control)cmdCompute).add_Click((EventHandler)cmdCompute_Click);
			((Control)cmdCancel).set_Location(new Point(716, 33));
			((Control)cmdCancel).set_Name("cmdCancel");
			((Control)cmdCancel).set_Size(new Size(70, 32));
			((Control)cmdCancel).set_TabIndex(4);
			((Control)cmdCancel).set_Text("Cancel");
			((ButtonBase)cmdCancel).set_UseVisualStyleBackColor(true);
			((Control)cmdCancel).add_Click((EventHandler)cmdCancel_Click);
			((Control)lstTimes).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstTimes).set_FormattingEnabled(true);
			lstTimes.set_ItemHeight(14);
			((Control)lstTimes).set_Location(new Point(569, 107));
			((Control)lstTimes).set_Name("lstTimes");
			((Control)lstTimes).set_Size(new Size(127, 130));
			((Control)lstTimes).set_TabIndex(5);
			((Control)updnLongitude).set_Location(new Point(100, 30));
			updnLongitude.set_Maximum(new decimal(new int[4] { 180, 0, 0, 0 }));
			updnLongitude.set_Minimum(new decimal(new int[4] { 180, 0, 0, -2147483648 }));
			((Control)updnLongitude).set_Name("updnLongitude");
			((Control)updnLongitude).set_Size(new Size(52, 20));
			((Control)updnLongitude).set_TabIndex(2);
			((Control)updnLongitude).add_Enter((EventHandler)updnLongitude_Enter);
			((Control)updnLatitude).set_Location(new Point(158, 30));
			updnLatitude.set_Maximum(new decimal(new int[4] { 90, 0, 0, 0 }));
			updnLatitude.set_Minimum(new decimal(new int[4] { 90, 0, 0, -2147483648 }));
			((Control)updnLatitude).set_Name("updnLatitude");
			((Control)updnLatitude).set_Size(new Size(40, 20));
			((Control)updnLatitude).set_TabIndex(4);
			((Control)updnLatitude).add_Enter((EventHandler)updnLatitude_Enter);
			((Control)chkSiteLimited).set_AutoSize(true);
			chkSiteLimited.set_Checked(Settings.Default.MutualEvents_LimitToLocal);
			((Control)chkSiteLimited).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "MutualEvents_LimitToLocal", true, (DataSourceUpdateMode)1));
			((Control)chkSiteLimited).set_Location(new Point(6, 24));
			((Control)chkSiteLimited).set_Name("chkSiteLimited");
			((Control)chkSiteLimited).set_Size(new Size(94, 30));
			((Control)chkSiteLimited).set_TabIndex(0);
			((Control)chkSiteLimited).set_Text("Limit to events\r\nvisible at:");
			((ButtonBase)chkSiteLimited).set_TextAlign(ContentAlignment.MiddleRight);
			((ButtonBase)chkSiteLimited).set_UseVisualStyleBackColor(true);
			chkSiteLimited.add_CheckedChanged((EventHandler)chkSiteLimited_CheckedChanged);
			((Control)panel2).get_Controls().Add((Control)(object)optSaturn);
			((Control)panel2).get_Controls().Add((Control)(object)optJupiter);
			((Control)panel2).get_Controls().Add((Control)(object)optUranus);
			((Control)panel2).set_Location(new Point(4, 34));
			((Control)panel2).set_Name("panel2");
			((Control)panel2).set_Size(new Size(76, 56));
			((Control)panel2).set_TabIndex(0);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(5, 6));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(95, 13));
			((Control)label1).set_TabIndex(0);
			((Control)label1).set_Text("Start Year && month");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(5, 33));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(92, 13));
			((Control)label2).set_TabIndex(1);
			((Control)label2).set_Text("End Year && month");
			((Control)panel3).get_Controls().Add((Control)(object)label2);
			((Control)panel3).get_Controls().Add((Control)(object)label1);
			((Control)panel3).get_Controls().Add((Control)(object)updnEndMonth);
			((Control)panel3).get_Controls().Add((Control)(object)updnEndYear);
			((Control)panel3).get_Controls().Add((Control)(object)updnStartMonth);
			((Control)panel3).get_Controls().Add((Control)(object)updnStartYear);
			((Control)panel3).set_Location(new Point(104, 34));
			((Control)panel3).set_Name("panel3");
			((Control)panel3).set_Size(new Size(213, 57));
			((Control)panel3).set_TabIndex(1);
			((Control)lblHeader).set_AutoSize(true);
			((Control)lblHeader).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblHeader).set_Location(new Point(6, 93));
			((Control)lblHeader).set_Name("lblHeader");
			((Control)lblHeader).set_Size(new Size(49, 14));
			((Control)lblHeader).set_TabIndex(8);
			((Control)lblHeader).set_Text("label3");
			((Control)pbar).set_Location(new Point(701, 68));
			((Control)pbar).set_Name("pbar");
			((Control)pbar).set_Size(new Size(101, 10));
			((Control)pbar).set_TabIndex(7);
			((Control)pbar).set_Visible(false);
			((Control)groupBox1).get_Controls().Add((Control)(object)radioButton4);
			((Control)groupBox1).get_Controls().Add((Control)(object)radioButton3);
			((Control)groupBox1).get_Controls().Add((Control)(object)radioButton2);
			((Control)groupBox1).get_Controls().Add((Control)(object)radioButton1);
			((Control)groupBox1).set_Location(new Point(582, 352));
			((Control)groupBox1).set_Name("groupBox1");
			((Control)groupBox1).set_Size(new Size(199, 35));
			((Control)groupBox1).set_TabIndex(10);
			groupBox1.set_TabStop(false);
			((Control)groupBox1).set_Text("Increase plot scale");
			((Control)radioButton4).set_AutoSize(true);
			((Control)radioButton4).set_Location(new Point(155, 16));
			((Control)radioButton4).set_Name("radioButton4");
			((Control)radioButton4).set_Size(new Size(42, 17));
			((Control)radioButton4).set_TabIndex(3);
			((Control)radioButton4).set_Text("x10");
			((ButtonBase)radioButton4).set_UseVisualStyleBackColor(true);
			radioButton4.add_CheckedChanged((EventHandler)radioButton4_CheckedChanged);
			((Control)radioButton3).set_AutoSize(true);
			((Control)radioButton3).set_Location(new Point(106, 16));
			((Control)radioButton3).set_Name("radioButton3");
			((Control)radioButton3).set_Size(new Size(36, 17));
			((Control)radioButton3).set_TabIndex(2);
			((Control)radioButton3).set_Text("x5");
			((ButtonBase)radioButton3).set_UseVisualStyleBackColor(true);
			radioButton3.add_CheckedChanged((EventHandler)radioButton3_CheckedChanged);
			((Control)radioButton2).set_AutoSize(true);
			((Control)radioButton2).set_Location(new Point(57, 16));
			((Control)radioButton2).set_Name("radioButton2");
			((Control)radioButton2).set_Size(new Size(36, 17));
			((Control)radioButton2).set_TabIndex(1);
			((Control)radioButton2).set_Text("x2");
			((ButtonBase)radioButton2).set_UseVisualStyleBackColor(true);
			radioButton2.add_CheckedChanged((EventHandler)radioButton2_CheckedChanged);
			((Control)radioButton1).set_AutoSize(true);
			radioButton1.set_Checked(true);
			((Control)radioButton1).set_Location(new Point(8, 16));
			((Control)radioButton1).set_Name("radioButton1");
			((Control)radioButton1).set_Size(new Size(36, 17));
			((Control)radioButton1).set_TabIndex(0);
			radioButton1.set_TabStop(true);
			((Control)radioButton1).set_Text("x1");
			((ButtonBase)radioButton1).set_UseVisualStyleBackColor(true);
			radioButton1.add_CheckedChanged((EventHandler)radioButton1_CheckedChanged);
			((Control)groupBox2).get_Controls().Add((Control)(object)optTx4);
			((Control)groupBox2).get_Controls().Add((Control)(object)optTx3);
			((Control)groupBox2).get_Controls().Add((Control)(object)optTx2);
			((Control)groupBox2).get_Controls().Add((Control)(object)optTx1);
			((Control)groupBox2).set_Location(new Point(582, 387));
			((Control)groupBox2).set_Name("groupBox2");
			((Control)groupBox2).set_Size(new Size(199, 35));
			((Control)groupBox2).set_TabIndex(11);
			groupBox2.set_TabStop(false);
			((Control)groupBox2).set_Text("Increase plot time-span");
			((Control)optTx4).set_AutoSize(true);
			((Control)optTx4).set_Location(new Point(155, 16));
			((Control)optTx4).set_Name("optTx4");
			((Control)optTx4).set_Size(new Size(36, 17));
			((Control)optTx4).set_TabIndex(3);
			((Control)optTx4).set_Text("x4");
			((ButtonBase)optTx4).set_UseVisualStyleBackColor(true);
			optTx4.add_CheckedChanged((EventHandler)optTx4_CheckedChanged);
			((Control)optTx3).set_AutoSize(true);
			((Control)optTx3).set_Location(new Point(106, 16));
			((Control)optTx3).set_Name("optTx3");
			((Control)optTx3).set_Size(new Size(36, 17));
			((Control)optTx3).set_TabIndex(2);
			((Control)optTx3).set_Text("x3");
			((ButtonBase)optTx3).set_UseVisualStyleBackColor(true);
			optTx3.add_CheckedChanged((EventHandler)optTx3_CheckedChanged);
			((Control)optTx2).set_AutoSize(true);
			((Control)optTx2).set_Location(new Point(57, 16));
			((Control)optTx2).set_Name("optTx2");
			((Control)optTx2).set_Size(new Size(36, 17));
			((Control)optTx2).set_TabIndex(1);
			((Control)optTx2).set_Text("x2");
			((ButtonBase)optTx2).set_UseVisualStyleBackColor(true);
			optTx2.add_CheckedChanged((EventHandler)optTx2_CheckedChanged);
			((Control)optTx1).set_AutoSize(true);
			optTx1.set_Checked(true);
			((Control)optTx1).set_Location(new Point(8, 16));
			((Control)optTx1).set_Name("optTx1");
			((Control)optTx1).set_Size(new Size(36, 17));
			((Control)optTx1).set_TabIndex(0);
			optTx1.set_TabStop(true);
			((Control)optTx1).set_Text("x1");
			((ButtonBase)optTx1).set_UseVisualStyleBackColor(true);
			optTx1.add_CheckedChanged((EventHandler)optTx1_CheckedChanged);
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Location(new Point(97, 13));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(54, 13));
			((Control)label3).set_TabIndex(1);
			((Control)label3).set_Text("Longitude");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Location(new Point(154, 13));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(45, 13));
			((Control)label4).set_TabIndex(3);
			((Control)label4).set_Text("Latitude");
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Location(new Point(592, 94));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(71, 13));
			((Control)label5).set_TabIndex(6);
			((Control)label5).set_Text("Contact times");
			((Control)groupBox3).get_Controls().Add((Control)(object)chkSiteLimited);
			((Control)groupBox3).get_Controls().Add((Control)(object)label3);
			((Control)groupBox3).get_Controls().Add((Control)(object)label4);
			((Control)groupBox3).get_Controls().Add((Control)(object)updnLongitude);
			((Control)groupBox3).get_Controls().Add((Control)(object)updnLatitude);
			((Control)groupBox3).set_Location(new Point(330, 33));
			((Control)groupBox3).set_Name("groupBox3");
			((Control)groupBox3).set_Size(new Size(207, 58));
			((Control)groupBox3).set_TabIndex(2);
			groupBox3.set_TabStop(false);
			((Control)groupBox3).set_Text("Limit to site location");
			((Control)picEvents).set_BackColor(Color.White);
			picEvents.set_BorderStyle((BorderStyle)2);
			((Control)picEvents).set_Location(new Point(4, 438));
			((Control)picEvents).set_Name("picEvents");
			((Control)picEvents).set_Size(new Size(801, 133));
			picEvents.set_TabIndex(12);
			picEvents.set_TabStop(false);
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Location(new Point(4, 423));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(759, 13));
			((Control)label6).set_TabIndex(13);
			((Control)label6).set_Text("Plot of the relative positions of the two moons, or moon plus eclipsing shadow, over the period of the selected event.   For eclipses, umbra + penumbra is shown.");
			cmbMissDist.set_DropDownStyle((ComboBoxStyle)2);
			((ListControl)cmbMissDist).set_FormattingEnabled(true);
			cmbMissDist.get_Items().AddRange(new object[5] { "0.0\"", "0.1\"", "0.2\"", "0.5\"", "1.0\"" });
			((Control)cmbMissDist).set_Location(new Point(556, 40));
			cmbMissDist.set_MaxDropDownItems(9);
			((Control)cmbMissDist).set_Name("cmbMissDist");
			((Control)cmbMissDist).set_Size(new Size(44, 21));
			((Control)cmbMissDist).set_TabIndex(14);
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Location(new Point(603, 44));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(71, 13));
			((Control)label7).set_TabIndex(15);
			((Control)label7).set_Text("Miss distance");
			((Control)chkIncludeInvisible).set_AutoSize(true);
			((Control)chkIncludeInvisible).set_ForeColor(Color.Maroon);
			((Control)chkIncludeInvisible).set_Location(new Point(556, 70));
			((Control)chkIncludeInvisible).set_Name("chkIncludeInvisible");
			((Control)chkIncludeInvisible).set_Size(new Size(131, 17));
			((Control)chkIncludeInvisible).set_TabIndex(16);
			((Control)chkIncludeInvisible).set_Text("Include hidden events");
			((ButtonBase)chkIncludeInvisible).set_UseVisualStyleBackColor(true);
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Italic, GraphicsUnit.Point, 0));
			((Control)label8).set_Location(new Point(7, 405));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(554, 13));
			((Control)label8).set_TabIndex(17);
			((Control)label8).set_Text("Right-click a highlighted line to display a graphic of the planet and its satellites, or put the event into Recording Timer");
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label9).set_ForeColor(Color.MediumBlue);
			((Control)label9).set_Location(new Point(3, 2));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(232, 39));
			((Control)label9).set_TabIndex(18);
			((Control)label9).set_Text("Ph codes: Difficult events\r\n             e:  occulting satellite in planet's shadow\r\n              t:  satellite in transit\r\n");
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label10).set_ForeColor(Color.Maroon);
			((Control)label10).set_Location(new Point(37, 40));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(191, 52));
			((Control)label10).set_TabIndex(19);
			((Control)label10).set_Text("    Hidden events\r\n f:  occulted satellite in planet's shadow\r\ng:  both satelllites in planet's shadow\r\nh:  satellite behind planet\r\n");
			((Control)picShadow).set_Location(new Point(701, 179));
			((Control)picShadow).set_Name("picShadow");
			((Control)picShadow).set_Size(new Size(101, 58));
			picShadow.set_TabIndex(20);
			picShadow.set_TabStop(false);
			((Control)label11).set_AutoSize(true);
			((Control)label11).set_Location(new Point(721, 84));
			((Control)label11).set_Name("label11");
			((Control)label11).set_Size(new Size(60, 13));
			((Control)label11).set_TabIndex(21);
			((Control)label11).set_Text("Light curve");
			label11.set_TextAlign(ContentAlignment.TopCenter);
			panel1.set_BorderStyle((BorderStyle)1);
			((Control)panel1).get_Controls().Add((Control)(object)label10);
			((Control)panel1).get_Controls().Add((Control)(object)label9);
			((Control)panel1).set_Location(new Point(569, 244));
			((Control)panel1).set_Name("panel1");
			((Control)panel1).set_Size(new Size(236, 98));
			((Control)panel1).set_TabIndex(22);
			((Control)picLightCurve).set_Location(new Point(701, 107));
			((Control)picLightCurve).set_Name("picLightCurve");
			((Control)picLightCurve).set_Size(new Size(101, 58));
			picLightCurve.set_TabIndex(23);
			picLightCurve.set_TabStop(false);
			toolTip1.SetToolTip((Control)(object)picLightCurve, "Double-click to display full image");
			((Control)picLightCurve).add_DoubleClick((EventHandler)picLightCurve_DoubleClick);
			toolTip1.set_AutomaticDelay(100);
			toolTip1.set_AutoPopDelay(5000);
			toolTip1.set_InitialDelay(100);
			toolTip1.set_ReshowDelay(20);
			((Control)lblAlt).set_AutoSize(true);
			((Control)lblAlt).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblAlt).set_Location(new Point(522, 93));
			((Control)lblAlt).set_Name("lblAlt");
			((Control)lblAlt).set_Size(new Size(28, 14));
			((Control)lblAlt).set_TabIndex(24);
			((Control)lblAlt).set_Text("Alt");
			((Control)label12).set_AutoSize(true);
			((Control)label12).set_Font(new Font("Microsoft Sans Serif", 6.75f, FontStyle.Italic, GraphicsUnit.Point, 0));
			((Control)label12).set_Location(new Point(707, 95));
			((Control)label12).set_Name("label12");
			((Control)label12).set_Size(new Size(89, 12));
			((Control)label12).set_TabIndex(25);
			((Control)label12).set_Text("double-click for large");
			((Control)label13).set_AutoSize(true);
			((Control)label13).set_Location(new Point(703, 166));
			((Control)label13).set_Name("label13");
			((Control)label13).set_Size(new Size(97, 13));
			((Control)label13).set_TabIndex(26);
			((Control)label13).set_Text("Shadow brightness");
			label13.set_TextAlign(ContentAlignment.TopCenter);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(809, 577));
			((Control)this).get_Controls().Add((Control)(object)picShadow);
			((Control)this).get_Controls().Add((Control)(object)picLightCurve);
			((Control)this).get_Controls().Add((Control)(object)panel1);
			((Control)this).get_Controls().Add((Control)(object)label11);
			((Control)this).get_Controls().Add((Control)(object)chkIncludeInvisible);
			((Control)this).get_Controls().Add((Control)(object)label7);
			((Control)this).get_Controls().Add((Control)(object)cmbMissDist);
			((Control)this).get_Controls().Add((Control)(object)label6);
			((Control)this).get_Controls().Add((Control)(object)groupBox3);
			((Control)this).get_Controls().Add((Control)(object)label5);
			((Control)this).get_Controls().Add((Control)(object)groupBox2);
			((Control)this).get_Controls().Add((Control)(object)groupBox1);
			((Control)this).get_Controls().Add((Control)(object)pbar);
			((Control)this).get_Controls().Add((Control)(object)lblHeader);
			((Control)this).get_Controls().Add((Control)(object)panel3);
			((Control)this).get_Controls().Add((Control)(object)panel2);
			((Control)this).get_Controls().Add((Control)(object)picEvents);
			((Control)this).get_Controls().Add((Control)(object)lstTimes);
			((Control)this).get_Controls().Add((Control)(object)cmdCompute);
			((Control)this).get_Controls().Add((Control)(object)lstEvents);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_Controls().Add((Control)(object)cmdCancel);
			((Control)this).get_Controls().Add((Control)(object)label8);
			((Control)this).get_Controls().Add((Control)(object)lblAlt);
			((Control)this).get_Controls().Add((Control)(object)label12);
			((Control)this).get_Controls().Add((Control)(object)label13);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationSatelliteMutuals", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_FormBorderStyle((FormBorderStyle)2);
			((Form)this).set_Icon((Icon)componentResourceManager.GetObject("$this.Icon"));
			((Form)this).set_Location(Settings.Default.LocationSatelliteMutuals);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Form)this).set_MaximizeBox(false);
			((Control)this).set_Name("SatelliteMutualEvents");
			((Control)this).set_Text("Mutual eclipses and occultations of planetary satellites");
			((Form)this).add_Load((EventHandler)SatelliteMutualEvents_Load);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)contextMenuStrip1).ResumeLayout(false);
			((ISupportInitialize)updnStartYear).EndInit();
			((ISupportInitialize)updnStartMonth).EndInit();
			((ISupportInitialize)updnEndMonth).EndInit();
			((ISupportInitialize)updnEndYear).EndInit();
			((ISupportInitialize)updnLongitude).EndInit();
			((ISupportInitialize)updnLatitude).EndInit();
			((Control)panel2).ResumeLayout(false);
			((Control)panel2).PerformLayout();
			((Control)panel3).ResumeLayout(false);
			((Control)panel3).PerformLayout();
			((Control)groupBox1).ResumeLayout(false);
			((Control)groupBox1).PerformLayout();
			((Control)groupBox2).ResumeLayout(false);
			((Control)groupBox2).PerformLayout();
			((Control)groupBox3).ResumeLayout(false);
			((Control)groupBox3).PerformLayout();
			((ISupportInitialize)picEvents).EndInit();
			((ISupportInitialize)picShadow).EndInit();
			((Control)panel1).ResumeLayout(false);
			((Control)panel1).PerformLayout();
			((ISupportInitialize)picLightCurve).EndInit();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
