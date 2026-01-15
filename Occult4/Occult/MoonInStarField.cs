using System;
using System.Collections;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Drawing.Imaging;
using System.IO;
using System.Reflection;
using System.Windows.Forms;
using GifCreator;
using Occult.Properties;

namespace Occult
{
	public class MoonInStarField : Form
	{
		private readonly string AppPath;

		private const double Radian = 180.0 / Math.PI;

		private const double TwoPi = Math.PI * 2.0;

		private static int[] XZIndex = new int[361];

		private static bool CancelFlag = false;

		private static bool formCreated = false;

		private static bool AnimatedGIF = false;

		private IContainer components;

		private PictureBox picMoon;

		private ComboBox cmbLunarDayStart;

		private ComboBox cmbLunarMonthStart;

		private NumericUpDown updnLunarYearStart;

		private NumericUpDown updnLatitude;

		private NumericUpDown updnLongitude;

		private Label label5;

		private Label label4;

		private NumericUpDown updnHour;

		private NumericUpDown updnMinute;

		private Label label1;

		private Label label2;

		private Label label3;

		private Label label6;

		private Label label7;

		private RadioButton opt07;

		private RadioButton opt4;

		private RadioButton opt2;

		private RadioButton opt1;

		private NumericUpDown updnDuration;

		private NumericUpDown updnInterval;

		private Label label8;

		private Label label9;

		private Label label10;

		private Label label11;

		private Label label12;

		private MenuStrip menuStrip1;

		private Button cmdPlot;

		private Button cmdCancel;

		private Label txtHourOut;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripMenuItem animatedGIFToolStripMenuItem;

		private ToolStripMenuItem animatedGIFSettingsToolStripMenuItem;

		private ToolStripMenuItem createAnimatedGIFToolStripMenuItem;

		private Button cmdHome;

		private RadioButton opt10;

		private ComboBox cmbMag;

		private Label label13;

		public MoonInStarField()
		{
			InitializeComponent();
			AppPath = Utilities.AppPath;
		}

		private void MoonInStarField_Load(object sender, EventArgs e)
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
			formCreated = false;
			updnLunarYearStart.set_Value((decimal)DateTime.Now.ToUniversalTime().Year);
			((ListControl)cmbLunarMonthStart).set_SelectedIndex(DateTime.Now.ToUniversalTime().Month - 1);
			((ListControl)cmbLunarDayStart).set_SelectedIndex(DateTime.Now.ToUniversalTime().Day - 1);
			updnHour.set_Value((decimal)DateTime.Now.ToUniversalTime().Hour);
			updnMinute.set_Value((decimal)DateTime.Now.ToUniversalTime().Minute);
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
			((ListControl)cmbMag).set_SelectedIndex(0);
			formCreated = true;
		}

		private void cmdPlot_Click(object sender, EventArgs e)
		{
			PlotMoonField();
		}

		private void PlotMoonField()
		{
			if (!formCreated || !((Control)cmdPlot).get_Visible())
			{
				return;
			}
			CancelFlag = false;
			((Control)cmdPlot).set_Visible(false);
			float num = 0f;
			List<string> list = new List<string>();
			int num2 = 10000;
			ArrayList arrayList = new ArrayList();
			Font font = new Font("Courier New", 8f);
			Font font2 = new Font("Times New Roman", 7f, FontStyle.Regular);
			Brush yellow = Brushes.Yellow;
			Brush yellow2 = Brushes.Yellow;
			Brush black = Brushes.Black;
			Brush orange = Brushes.Orange;
			double num3 = (double)updnDuration.get_Value();
			double num4 = (double)updnInterval.get_Value() / 60.0;
			if (num4 < 0.001)
			{
				num3 = 0.0;
			}
			int year = (int)updnLunarYearStart.get_Value();
			int month = ((ListControl)cmbLunarMonthStart).get_SelectedIndex() + 1;
			double day = ((ListControl)cmbLunarDayStart).get_SelectedIndex() + 1;
			double num5 = (double)updnHour.get_Value();
			double num6 = (double)updnMinute.get_Value();
			double num7 = Utilities.JD_from_Date(year, month, day);
			double num8 = (double)updnLongitude.get_Value() / (180.0 / Math.PI);
			double num9 = (double)updnLatitude.get_Value() / (180.0 / Math.PI);
			if (AnimatedGIF)
			{
				string path = AppPath + "\\Predictions\\AnimatedGIF";
				if (!Directory.Exists(path))
				{
					Directory.CreateDirectory(path);
				}
				string[] files = Directory.GetFiles(path, "*.gif");
				foreach (string path2 in files)
				{
					try
					{
						File.Delete(path2);
					}
					catch
					{
					}
				}
			}
			StreamReader streamReader = new StreamReader(AppPath + "\\Resource Files\\XZ80.inx");
			for (int j = 0; j <= 360; j++)
			{
				XZIndex[j] = Convert.ToInt32(streamReader.ReadLine()) - 1;
			}
			streamReader.Close();
			FileStream fileStream = new FileStream(AppPath + "\\Resource Files\\XZ80.dat", FileMode.Open, FileAccess.Read);
			BinaryReader readXZ = new BinaryReader(fileStream);
			for (double num10 = 0.0; num10 <= num3; num10 += num4)
			{
				double num11 = num6 + num10;
				double num12 = num5;
				while (num11 >= 60.0)
				{
					num11 -= 60.0;
					num12 += 1.0;
				}
				((Control)txtHourOut).set_Text(Utilities.DateTime_from_JD(num7 + num12 / 24.0 + num11 / 1440.0));
				Utilities.TopocentricMoon(num7 + num12 / 24.0 + num11 / 1440.0, num8, num9, 0.0, Altitude_is_MSL: true, 0, out var RA, out var Dec, out var MoonRadius_Radians, out var _, out var _, out var _, out var _, out var _);
				double num13 = 0.0;
				double num14 = 0.0;
				double RA2;
				double Dec2;
				for (int k = 0; k <= 2; k++)
				{
					RA2 = RA + num13;
					Dec2 = Dec + num14;
					Utilities.ApparentStarPosition(ref RA2, ref Dec2, 0.0, 0.0, 2000, num7, use2006values_Not1976: false);
					num13 = num13 + RA - RA2;
					if (Math.Abs(num13) > 3.0)
					{
						num13 -= Math.PI * 2.0 * (double)Math.Sign(num13);
					}
					num14 = num14 + Dec - Dec2;
				}
				RA2 = RA + num13;
				Dec2 = Dec + num14;
				Utilities.QuickPlanet(num7 + num12 / 24.0 + num11 / 1440.0, 3, EquinoxOfDate: true, out var RA3, out var Dec3, out var _);
				double num15 = Utilities.SiderealTime_deg(num7 + num12 / 24.0 + num11 / 1440.0, Apparent: false) / (180.0 / Math.PI);
				double num16 = 180.0 / Math.PI * Math.Sin(Math.Sin(Dec) * Math.Sin(num9) + Math.Cos(num9) * Math.Cos(num15 + num8 - RA) * Math.Cos(Dec));
				double num17 = 180.0 / Math.PI * Math.Sin(Math.Sin(Dec3) * Math.Sin(num9) + Math.Cos(num9) * Math.Cos(num15 + num8 - RA3) * Math.Cos(Dec3));
				float num18 = (float)((Control)picMoon).get_Width() / 2f;
				float num19 = (float)((Control)picMoon).get_Height() / 2f;
				double num20 = 15.0;
				double num21 = 11.0;
				if (((ListControl)cmbMag).get_SelectedIndex() > 0)
				{
					num20 = 11 - ((ListControl)cmbMag).get_SelectedIndex();
					num21 = 11 - ((ListControl)cmbMag).get_SelectedIndex();
				}
				float num22 = (opt07.get_Checked() ? 0.7f : (opt1.get_Checked() ? 1f : (opt2.get_Checked() ? 2f : ((!opt4.get_Checked()) ? 10f : 4f))));
				float num23 = (float)((Control)picMoon).get_Height() / num22;
				double num24 = num23 * num22;
				int width = ((Control)picMoon).get_Width();
				int height = ((Control)picMoon).get_Height();
				Bitmap image = new Bitmap(width, height);
				Graphics graphics = Graphics.FromImage(image);
				if (Settings.Default.GraphicsSmoothed)
				{
					graphics.SmoothingMode = SmoothingMode.AntiAlias;
				}
				if (num16 < -0.5)
				{
					graphics.Clear(Color.FromArgb(80, 40, 0));
					graphics.DrawString("Moon below horizon", font, Brushes.Orange, 3f, 3f);
					graphics.DrawString("N", font, orange, num18, 3f);
					graphics.DrawString("E", font, orange, 3f, num19);
					graphics.DrawString("Occult " + Assembly.GetExecutingAssembly().GetName(copiedName: false).Version, font2, orange, 12f, height - 16);
				}
				else if (num17 > -10.0 && num17 < 0.0)
				{
					graphics.Clear(Color.FromArgb((int)(2.0 * Math.Pow(num17 + 10.0, 2.0)), (int)(2.0 * Math.Pow(num17 + 10.0, 2.0)), (int)(20.0 * (num17 + 10.0))));
					graphics.DrawString("Twilight", font, Brushes.Yellow, 3f, 3f);
					graphics.DrawString("N", font, yellow2, num18, 3f);
					graphics.DrawString("E", font, yellow2, 3f, num19);
					graphics.DrawString("Occult " + Assembly.GetExecutingAssembly().GetName(copiedName: false).Version, font2, yellow2, 12f, height - 16);
				}
				else if (num17 >= 0.0)
				{
					graphics.Clear(Color.FromArgb(200, 200, 200));
					graphics.DrawString("Daytime", font, Brushes.Black, 3f, 3f);
					graphics.DrawString("N", font, black, num18, 3f);
					graphics.DrawString("E", font, black, 3f, num19);
					graphics.DrawString("Occult " + Assembly.GetExecutingAssembly().GetName(copiedName: false).Version, font2, black, 12f, height - 16);
				}
				else
				{
					graphics.Clear(Color.Black);
					graphics.DrawString("NightTime", font, Brushes.Yellow, 3f, 3f);
					graphics.DrawString("N", font, yellow, num18, 3f);
					graphics.DrawString("E", font, yellow, 3f, num19);
					graphics.DrawString("Occult " + Assembly.GetExecutingAssembly().GetName(copiedName: false).Version, font2, yellow, 12f, height - 16);
				}
				double num25 = RA2 * (180.0 / Math.PI) - (double)num22 / Math.Cos(Dec2);
				if (num25 < 0.0)
				{
					num25 += 360.0;
				}
				double num26 = RA2 * (180.0 / Math.PI) + (double)num22 / Math.Cos(Dec2);
				if (num26 > 360.0)
				{
					num26 -= 360.0;
				}
				double num27 = Dec2 - (double)num22 / 1.9 / (180.0 / Math.PI);
				double num28 = Dec2 + (double)num22 / 1.9 / (180.0 / Math.PI);
				int num29 = XZIndex[(int)(num25 - 0.5)];
				int num30 = XZIndex[(int)(num26 + 1.0)];
				int num31 = XZIndex[360];
				int num32 = num29;
				int num33 = num30;
				if (num29 > num30)
				{
					num32 = 1;
					num33 = num31;
				}
				double num34 = Math.Pow(MoonRadius_Radians * (180.0 / Math.PI) * (double)num23, 2.0);
				for (int m = num32; m <= num33; m++)
				{
					if (num30 < num29 && m == num30)
					{
						m = num29;
					}
					XZ80Q.ReadStarEntry(fileStream, readXZ, m);
					double mv = XZ80Q.Mv;
					if (mv < num20 && ((XZ80Q.Dec_rad < num28) & (XZ80Q.Dec_rad > num27)))
					{
						double rA_rad = XZ80Q.RA_rad;
						double dec_rad = XZ80Q.Dec_rad;
						if (mv > num21 - 5.0)
						{
							num = (float)(num21 - mv) * 1.8f + 2f;
						}
						if (mv <= num21 - 5.0)
						{
							num = (float)(num21 - mv) * 1.4f + 4f;
						}
						if ((double)num < 1.5)
						{
							num = 1.5f;
						}
						double num35 = Math.Atan(Math.Tan(dec_rad) / Math.Cos(rA_rad - RA2));
						double num36 = 180.0 / Math.PI * (double)num23 * Math.Cos(num35) * Math.Tan(rA_rad - RA2) / Math.Cos(num35 - Dec2);
						double num37 = 180.0 / Math.PI * (double)num23 * Math.Tan(num35 - Dec2);
						if (num36 * num36 + num37 * num37 > num34 && ((Math.Abs(num36) < num24) & (Math.Abs(num37) < num24)))
						{
							float num38 = (float)((double)num18 - num36);
							float num39 = (float)((double)num19 - num37);
							graphics.FillEllipse(Brushes.White, num38 - num / 2f, num39 - num / 2f, num, num);
						}
					}
				}
				num = (float)(MoonRadius_Radians * (180.0 / Math.PI) * (double)num23);
				graphics.FillEllipse(Brushes.Gray, num18 - num, num19 - num, 2f * num, 2f * num);
				double num40 = Math.Cos(Dec3) * Math.Sin(RA3 - RA);
				double num41 = Math.Sin(Dec3) * Math.Cos(Dec) - Math.Cos(Dec3) * Math.Sin(Dec) * Math.Cos(RA3 - RA);
				double x = Math.Sin(Dec3) * Math.Sin(Dec) + Math.Cos(Dec3) * Math.Cos(Dec) * Math.Cos(RA3 - RA);
				double num42 = Math.Atan2(num40, num41) * (180.0 / Math.PI) + 90.0;
				double d = Math.Atan2(Math.Sqrt(num40 * num40 + num41 * num41), x);
				double num43 = num42 + 90.0;
				double num44 = num42 - 90.0;
				while (num43 > 360.0 || num44 > 360.0)
				{
					num43 -= 180.0;
					num44 -= 180.0;
					double num45 = num43;
					num43 = num44;
					num44 = num45;
				}
				while (num43 < 0.0 || num44 < 0.0)
				{
					num43 += 180.0;
					num44 += 180.0;
					double num46 = num43;
					num43 = num44;
					num44 = num46;
				}
				num43 /= 180.0 / Math.PI;
				num44 /= 180.0 / Math.PI;
				arrayList.Clear();
				for (int j = 0; j <= 180; j += 3)
				{
					double num47 = num43 + Math.Abs(num44 - num43) / 180.0 * (double)j;
					float num38 = (float)((double)num18 - MoonRadius_Radians * (180.0 / Math.PI) * (double)num23 * Math.Cos(num47));
					float num39 = (float)((double)num19 + MoonRadius_Radians * (180.0 / Math.PI) * (double)num23 * Math.Sin(num47));
					arrayList.Add(new PointF(num38, num39));
				}
				for (int j = 0; j <= 180; j += 5)
				{
					double num48 = MoonRadius_Radians * (180.0 / Math.PI) * (double)num23 * Math.Sin((double)j / (180.0 / Math.PI)) * Math.Cos(d);
					double num49 = MoonRadius_Radians * (180.0 / Math.PI) * (double)num23 * Math.Cos((double)j / (180.0 / Math.PI));
					double num50 = Math.PI / 2.0 - num43;
					float num38 = (float)((double)num18 + num48 * Math.Cos(num50) + num49 * Math.Sin(num50));
					float num39 = (float)((double)num19 - num49 * Math.Cos(num50) + num48 * Math.Sin(num50));
					arrayList.Add(new PointF(num38, num39));
				}
				PointF[] points = (PointF[])arrayList.ToArray(arrayList[0]!.GetType());
				graphics.FillPolygon(new SolidBrush(Color.Yellow), points);
				picMoon.set_Image((Image)image);
				if (AnimatedGIF)
				{
					string text = Utilities.AppPath + "\\Predictions\\AnimatedGIF\\" + num2 + ".gif";
					try
					{
						picMoon.get_Image().Save(text, ImageFormat.Gif);
						list.Add(text);
					}
					catch
					{
					}
					num2++;
				}
				Application.DoEvents();
				graphics.Dispose();
				if (CancelFlag)
				{
					break;
				}
			}
			fileStream.Close();
			((Control)cmdPlot).set_Visible(true);
			if (AnimatedGIF)
			{
				global::GifCreator.GifCreator.Create_Animated_Gif(list, "MoonInStars_" + updnLunarYearStart.get_Value() + cmbLunarMonthStart.get_Items().get_Item(((ListControl)cmbLunarMonthStart).get_SelectedIndex()).ToString() + cmbLunarDayStart.get_Items().get_Item(((ListControl)cmbLunarDayStart).get_SelectedIndex()).ToString() + ".gif");
			}
		}

		private void cmdCancel_Click(object sender, EventArgs e)
		{
			CancelFlag = true;
		}

		private void opt07_CheckedChanged(object sender, EventArgs e)
		{
			if (opt07.get_Checked())
			{
				PlotMoonField();
			}
		}

		private void opt1_CheckedChanged(object sender, EventArgs e)
		{
			if (opt1.get_Checked())
			{
				PlotMoonField();
			}
		}

		private void opt2_CheckedChanged(object sender, EventArgs e)
		{
			if (opt2.get_Checked())
			{
				PlotMoonField();
			}
		}

		private void opt4_CheckedChanged(object sender, EventArgs e)
		{
			if (opt4.get_Checked())
			{
				PlotMoonField();
			}
		}

		private void opt10_CheckedChanged(object sender, EventArgs e)
		{
			if (opt10.get_Checked())
			{
				PlotMoonField();
			}
		}

		private void MoonInStarField_Resize(object sender, EventArgs e)
		{
			if (!((((Control)this).get_Width() < 700) | (((Control)this).get_Height() < 550)))
			{
				((Control)picMoon).set_Width(((Control)this).get_Width() - 25);
				((Control)picMoon).set_Height(((Control)this).get_Height() - 152);
				if (updnDuration.get_Value() == 0m)
				{
					PlotMoonField();
				}
			}
		}

		private void MoonInStarField_FormClosing(object sender, FormClosingEventArgs e)
		{
			((Component)this).Dispose();
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Lunar Occultations - Moon in star field");
		}

		private void animatedGIFSettingsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0006: Unknown result type (might be due to invalid IL or missing references)
			GIFsettings gIFsettings = new GIFsettings();
			((Form)gIFsettings).ShowDialog();
			((Component)(object)gIFsettings).Dispose();
		}

		private void createAnimatedGIFToolStripMenuItem_Click(object sender, EventArgs e)
		{
			AnimatedGIF = true;
			PlotMoonField();
			AnimatedGIF = false;
		}

		private void cmdHome_Click(object sender, EventArgs e)
		{
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

		private void cmbMag_SelectedIndexChanged(object sender, EventArgs e)
		{
			PlotMoonField();
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
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_000b: Expected O, but got Unknown
			//IL_000c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0016: Expected O, but got Unknown
			//IL_0017: Unknown result type (might be due to invalid IL or missing references)
			//IL_0021: Expected O, but got Unknown
			//IL_0022: Unknown result type (might be due to invalid IL or missing references)
			//IL_002c: Expected O, but got Unknown
			//IL_002d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0037: Expected O, but got Unknown
			//IL_0038: Unknown result type (might be due to invalid IL or missing references)
			//IL_0042: Expected O, but got Unknown
			//IL_0043: Unknown result type (might be due to invalid IL or missing references)
			//IL_004d: Expected O, but got Unknown
			//IL_004e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0058: Expected O, but got Unknown
			//IL_0059: Unknown result type (might be due to invalid IL or missing references)
			//IL_0063: Expected O, but got Unknown
			//IL_0064: Unknown result type (might be due to invalid IL or missing references)
			//IL_006e: Expected O, but got Unknown
			//IL_006f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0079: Expected O, but got Unknown
			//IL_007a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0084: Expected O, but got Unknown
			//IL_0085: Unknown result type (might be due to invalid IL or missing references)
			//IL_008f: Expected O, but got Unknown
			//IL_0090: Unknown result type (might be due to invalid IL or missing references)
			//IL_009a: Expected O, but got Unknown
			//IL_009b: Unknown result type (might be due to invalid IL or missing references)
			//IL_00a5: Expected O, but got Unknown
			//IL_00a6: Unknown result type (might be due to invalid IL or missing references)
			//IL_00b0: Expected O, but got Unknown
			//IL_00b1: Unknown result type (might be due to invalid IL or missing references)
			//IL_00bb: Expected O, but got Unknown
			//IL_00bc: Unknown result type (might be due to invalid IL or missing references)
			//IL_00c6: Expected O, but got Unknown
			//IL_00c7: Unknown result type (might be due to invalid IL or missing references)
			//IL_00d1: Expected O, but got Unknown
			//IL_00d2: Unknown result type (might be due to invalid IL or missing references)
			//IL_00dc: Expected O, but got Unknown
			//IL_00dd: Unknown result type (might be due to invalid IL or missing references)
			//IL_00e7: Expected O, but got Unknown
			//IL_00e8: Unknown result type (might be due to invalid IL or missing references)
			//IL_00f2: Expected O, but got Unknown
			//IL_00f3: Unknown result type (might be due to invalid IL or missing references)
			//IL_00fd: Expected O, but got Unknown
			//IL_00fe: Unknown result type (might be due to invalid IL or missing references)
			//IL_0108: Expected O, but got Unknown
			//IL_0109: Unknown result type (might be due to invalid IL or missing references)
			//IL_0113: Expected O, but got Unknown
			//IL_0114: Unknown result type (might be due to invalid IL or missing references)
			//IL_011e: Expected O, but got Unknown
			//IL_011f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0129: Expected O, but got Unknown
			//IL_012a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0134: Expected O, but got Unknown
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
			//IL_18ee: Unknown result type (might be due to invalid IL or missing references)
			//IL_18f8: Expected O, but got Unknown
			//IL_195c: Unknown result type (might be due to invalid IL or missing references)
			//IL_1966: Expected O, but got Unknown
			picMoon = new PictureBox();
			cmbLunarDayStart = new ComboBox();
			cmbLunarMonthStart = new ComboBox();
			updnLunarYearStart = new NumericUpDown();
			updnLatitude = new NumericUpDown();
			updnLongitude = new NumericUpDown();
			label5 = new Label();
			label4 = new Label();
			updnHour = new NumericUpDown();
			updnMinute = new NumericUpDown();
			label1 = new Label();
			label2 = new Label();
			label3 = new Label();
			label6 = new Label();
			label7 = new Label();
			opt07 = new RadioButton();
			opt4 = new RadioButton();
			opt2 = new RadioButton();
			opt1 = new RadioButton();
			updnDuration = new NumericUpDown();
			updnInterval = new NumericUpDown();
			label8 = new Label();
			label9 = new Label();
			label10 = new Label();
			label11 = new Label();
			label12 = new Label();
			menuStrip1 = new MenuStrip();
			animatedGIFToolStripMenuItem = new ToolStripMenuItem();
			animatedGIFSettingsToolStripMenuItem = new ToolStripMenuItem();
			createAnimatedGIFToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			cmdPlot = new Button();
			cmdCancel = new Button();
			txtHourOut = new Label();
			cmdHome = new Button();
			opt10 = new RadioButton();
			cmbMag = new ComboBox();
			label13 = new Label();
			((ISupportInitialize)picMoon).BeginInit();
			((ISupportInitialize)updnLunarYearStart).BeginInit();
			((ISupportInitialize)updnLatitude).BeginInit();
			((ISupportInitialize)updnLongitude).BeginInit();
			((ISupportInitialize)updnHour).BeginInit();
			((ISupportInitialize)updnMinute).BeginInit();
			((ISupportInitialize)updnDuration).BeginInit();
			((ISupportInitialize)updnInterval).BeginInit();
			((Control)menuStrip1).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)picMoon).set_BackColor(Color.Black);
			picMoon.set_BorderStyle((BorderStyle)2);
			((Control)picMoon).set_Location(new Point(4, 110));
			((Control)picMoon).set_Name("picMoon");
			((Control)picMoon).set_Size(new Size(709, 448));
			picMoon.set_TabIndex(0);
			picMoon.set_TabStop(false);
			((Control)cmbLunarDayStart).set_Anchor((AnchorStyles)1);
			cmbLunarDayStart.set_DropDownStyle((ComboBoxStyle)2);
			((ListControl)cmbLunarDayStart).set_FormattingEnabled(true);
			cmbLunarDayStart.get_Items().AddRange(new object[31]
			{
				"1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
				"11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
				"21", "22", "23", "24", "25", "26", "27", "28", "29", "30",
				"31"
			});
			((Control)cmbLunarDayStart).set_Location(new Point(112, 42));
			cmbLunarDayStart.set_MaxDropDownItems(31);
			((Control)cmbLunarDayStart).set_Name("cmbLunarDayStart");
			((Control)cmbLunarDayStart).set_Size(new Size(40, 21));
			((Control)cmbLunarDayStart).set_TabIndex(43);
			((Control)cmbLunarMonthStart).set_Anchor((AnchorStyles)1);
			cmbLunarMonthStart.set_DropDownStyle((ComboBoxStyle)2);
			((ListControl)cmbLunarMonthStart).set_FormattingEnabled(true);
			cmbLunarMonthStart.get_Items().AddRange(new object[12]
			{
				"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct",
				"Nov", "Dec"
			});
			((Control)cmbLunarMonthStart).set_Location(new Point(69, 42));
			cmbLunarMonthStart.set_MaxDropDownItems(12);
			((Control)cmbLunarMonthStart).set_Name("cmbLunarMonthStart");
			((Control)cmbLunarMonthStart).set_Size(new Size(43, 21));
			((Control)cmbLunarMonthStart).set_TabIndex(42);
			((Control)updnLunarYearStart).set_Anchor((AnchorStyles)1);
			((Control)updnLunarYearStart).set_Location(new Point(18, 43));
			updnLunarYearStart.set_Maximum(new decimal(new int[4] { 9999, 0, 0, 0 }));
			updnLunarYearStart.set_Minimum(new decimal(new int[4] { 5000, 0, 0, -2147483648 }));
			((Control)updnLunarYearStart).set_Name("updnLunarYearStart");
			((Control)updnLunarYearStart).set_Size(new Size(51, 20));
			((Control)updnLunarYearStart).set_TabIndex(41);
			updnLunarYearStart.set_Value(new decimal(new int[4] { 1800, 0, 0, 0 }));
			((Control)updnLatitude).set_Anchor((AnchorStyles)1);
			updnLatitude.set_DecimalPlaces(1);
			((Control)updnLatitude).set_Location(new Point(610, 43));
			updnLatitude.set_Maximum(new decimal(new int[4] { 90, 0, 0, 0 }));
			updnLatitude.set_Minimum(new decimal(new int[4] { 90, 0, 0, -2147483648 }));
			((Control)updnLatitude).set_Name("updnLatitude");
			((Control)updnLatitude).set_Size(new Size(54, 20));
			((Control)updnLatitude).set_TabIndex(47);
			((Control)updnLongitude).set_Anchor((AnchorStyles)1);
			updnLongitude.set_DecimalPlaces(1);
			((Control)updnLongitude).set_Location(new Point(540, 43));
			updnLongitude.set_Maximum(new decimal(new int[4] { 180, 0, 0, 0 }));
			updnLongitude.set_Minimum(new decimal(new int[4] { 180, 0, 0, -2147483648 }));
			((Control)updnLongitude).set_Name("updnLongitude");
			((Control)updnLongitude).set_Size(new Size(57, 20));
			((Control)updnLongitude).set_TabIndex(45);
			((Control)label5).set_Anchor((AnchorStyles)1);
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Location(new Point(610, 28));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(45, 13));
			((Control)label5).set_TabIndex(46);
			((Control)label5).set_Text("Latitude");
			((Control)label4).set_Anchor((AnchorStyles)1);
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Location(new Point(540, 28));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(54, 13));
			((Control)label4).set_TabIndex(44);
			((Control)label4).set_Text("Longitude");
			((Control)updnHour).set_Anchor((AnchorStyles)1);
			((Control)updnHour).set_Location(new Point(153, 43));
			updnHour.set_Maximum(new decimal(new int[4] { 23, 0, 0, 0 }));
			((Control)updnHour).set_Name("updnHour");
			((Control)updnHour).set_Size(new Size(39, 20));
			((Control)updnHour).set_TabIndex(48);
			((Control)updnMinute).set_Anchor((AnchorStyles)1);
			((Control)updnMinute).set_Location(new Point(193, 43));
			updnMinute.set_Maximum(new decimal(new int[4] { 59, 0, 0, 0 }));
			((Control)updnMinute).set_Name("updnMinute");
			((Control)updnMinute).set_Size(new Size(41, 20));
			((Control)updnMinute).set_TabIndex(49);
			((Control)label1).set_Anchor((AnchorStyles)1);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(21, 28));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(29, 13));
			((Control)label1).set_TabIndex(50);
			((Control)label1).set_Text("Year");
			((Control)label2).set_Anchor((AnchorStyles)1);
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(66, 28));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(37, 13));
			((Control)label2).set_TabIndex(51);
			((Control)label2).set_Text("Month");
			((Control)label3).set_Anchor((AnchorStyles)1);
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Location(new Point(112, 28));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(26, 13));
			((Control)label3).set_TabIndex(52);
			((Control)label3).set_Text("Day");
			((Control)label6).set_Anchor((AnchorStyles)1);
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Location(new Point(151, 28));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(30, 13));
			((Control)label6).set_TabIndex(53);
			((Control)label6).set_Text("Hour");
			((Control)label7).set_Anchor((AnchorStyles)1);
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Location(new Point(192, 28));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(27, 13));
			((Control)label7).set_TabIndex(54);
			((Control)label7).set_Text("Min.");
			((Control)opt07).set_Anchor((AnchorStyles)1);
			((Control)opt07).set_AutoSize(true);
			((Control)opt07).set_Location(new Point(29, 69));
			((Control)opt07).set_Name("opt07");
			((Control)opt07).set_Size(new Size(83, 17));
			((Control)opt07).set_TabIndex(56);
			((Control)opt07).set_Text("0.7 deg field");
			((ButtonBase)opt07).set_UseVisualStyleBackColor(true);
			opt07.add_CheckedChanged((EventHandler)opt07_CheckedChanged);
			((Control)opt4).set_Anchor((AnchorStyles)1);
			((Control)opt4).set_AutoSize(true);
			((Control)opt4).set_Location(new Point(281, 69));
			((Control)opt4).set_Name("opt4");
			((Control)opt4).set_Size(new Size(74, 17));
			((Control)opt4).set_TabIndex(57);
			((Control)opt4).set_Text("4 deg field");
			((ButtonBase)opt4).set_UseVisualStyleBackColor(true);
			opt4.add_CheckedChanged((EventHandler)opt4_CheckedChanged);
			((Control)opt2).set_Anchor((AnchorStyles)1);
			((Control)opt2).set_AutoSize(true);
			((Control)opt2).set_Location(new Point(200, 69));
			((Control)opt2).set_Name("opt2");
			((Control)opt2).set_Size(new Size(74, 17));
			((Control)opt2).set_TabIndex(58);
			((Control)opt2).set_Text("2 deg field");
			((ButtonBase)opt2).set_UseVisualStyleBackColor(true);
			opt2.add_CheckedChanged((EventHandler)opt2_CheckedChanged);
			((Control)opt1).set_Anchor((AnchorStyles)1);
			((Control)opt1).set_AutoSize(true);
			opt1.set_Checked(true);
			((Control)opt1).set_Location(new Point(119, 69));
			((Control)opt1).set_Name("opt1");
			((Control)opt1).set_Size(new Size(74, 17));
			((Control)opt1).set_TabIndex(59);
			opt1.set_TabStop(true);
			((Control)opt1).set_Text("1 deg field");
			((ButtonBase)opt1).set_UseVisualStyleBackColor(true);
			opt1.add_CheckedChanged((EventHandler)opt1_CheckedChanged);
			((Control)updnDuration).set_Anchor((AnchorStyles)1);
			updnDuration.set_Increment(new decimal(new int[4] { 10, 0, 0, 0 }));
			((Control)updnDuration).set_Location(new Point(303, 43));
			updnDuration.set_Maximum(new decimal(new int[4] { 900000, 0, 0, 0 }));
			((Control)updnDuration).set_Name("updnDuration");
			((Control)updnDuration).set_Size(new Size(60, 20));
			((Control)updnDuration).set_TabIndex(60);
			((Control)updnInterval).set_Anchor((AnchorStyles)1);
			((Control)updnInterval).set_Location(new Point(414, 43));
			updnInterval.set_Maximum(new decimal(new int[4] { 86400, 0, 0, 0 }));
			((Control)updnInterval).set_Name("updnInterval");
			((Control)updnInterval).set_Size(new Size(54, 20));
			((Control)updnInterval).set_TabIndex(61);
			updnInterval.set_Value(new decimal(new int[4] { 60, 0, 0, 0 }));
			((Control)label8).set_Anchor((AnchorStyles)1);
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Location(new Point(254, 46));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(47, 13));
			((Control)label8).set_TabIndex(62);
			((Control)label8).set_Text("Duration");
			((Control)label9).set_Anchor((AnchorStyles)1);
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Location(new Point(370, 46));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(42, 13));
			((Control)label9).set_TabIndex(63);
			((Control)label9).set_Text("Interval");
			((Control)label10).set_Anchor((AnchorStyles)1);
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Location(new Point(302, 28));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(32, 13));
			((Control)label10).set_TabIndex(64);
			((Control)label10).set_Text("Mins.");
			((Control)label11).set_Anchor((AnchorStyles)1);
			((Control)label11).set_AutoSize(true);
			((Control)label11).set_Location(new Point(415, 28));
			((Control)label11).set_Name("label11");
			((Control)label11).set_Size(new Size(34, 13));
			((Control)label11).set_TabIndex(65);
			((Control)label11).set_Text("Secs.");
			((Control)label12).set_Anchor((AnchorStyles)1);
			((Control)label12).set_AutoSize(true);
			((Control)label12).set_Location(new Point(510, 47));
			((Control)label12).set_Name("label12");
			((Control)label12).set_Size(new Size(25, 13));
			((Control)label12).set_TabIndex(66);
			((Control)label12).set_Text("Site");
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)animatedGIFToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(718, 24));
			((Control)menuStrip1).set_TabIndex(55);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)animatedGIFToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)animatedGIFSettingsToolStripMenuItem,
				(ToolStripItem)createAnimatedGIFToolStripMenuItem
			});
			((ToolStripItem)animatedGIFToolStripMenuItem).set_Name("animatedGIFToolStripMenuItem");
			((ToolStripItem)animatedGIFToolStripMenuItem).set_Size(new Size(114, 20));
			((ToolStripItem)animatedGIFToolStripMenuItem).set_Text("Animated-GIF...    ");
			((ToolStripItem)animatedGIFSettingsToolStripMenuItem).set_Name("animatedGIFSettingsToolStripMenuItem");
			animatedGIFSettingsToolStripMenuItem.set_ShortcutKeys((Keys)131137);
			((ToolStripItem)animatedGIFSettingsToolStripMenuItem).set_Size(new Size(234, 22));
			((ToolStripItem)animatedGIFSettingsToolStripMenuItem).set_Text("Animated-GIF settings");
			((ToolStripItem)animatedGIFSettingsToolStripMenuItem).add_Click((EventHandler)animatedGIFSettingsToolStripMenuItem_Click);
			((ToolStripItem)createAnimatedGIFToolStripMenuItem).set_Name("createAnimatedGIFToolStripMenuItem");
			createAnimatedGIFToolStripMenuItem.set_ShortcutKeys((Keys)131143);
			((ToolStripItem)createAnimatedGIFToolStripMenuItem).set_Size(new Size(234, 22));
			((ToolStripItem)createAnimatedGIFToolStripMenuItem).set_Text("Create animated-GIF");
			((ToolStripItem)createAnimatedGIFToolStripMenuItem).add_Click((EventHandler)createAnimatedGIFToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(60, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("&Help");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((Control)cmdPlot).set_Anchor((AnchorStyles)1);
			((Control)cmdPlot).set_Location(new Point(612, 80));
			((Control)cmdPlot).set_Name("cmdPlot");
			((Control)cmdPlot).set_Size(new Size(75, 27));
			((Control)cmdPlot).set_TabIndex(67);
			((Control)cmdPlot).set_Text("Plot");
			((ButtonBase)cmdPlot).set_UseVisualStyleBackColor(true);
			((Control)cmdPlot).add_Click((EventHandler)cmdPlot_Click);
			((Control)cmdCancel).set_Anchor((AnchorStyles)1);
			((Control)cmdCancel).set_Location(new Point(612, 80));
			((Control)cmdCancel).set_Name("cmdCancel");
			((Control)cmdCancel).set_Size(new Size(75, 27));
			((Control)cmdCancel).set_TabIndex(68);
			((Control)cmdCancel).set_Text("Cancel");
			((ButtonBase)cmdCancel).set_UseVisualStyleBackColor(true);
			((Control)cmdCancel).add_Click((EventHandler)cmdCancel_Click);
			((Control)txtHourOut).set_Anchor((AnchorStyles)1);
			((Control)txtHourOut).set_AutoSize(true);
			((Control)txtHourOut).set_Location(new Point(302, 94));
			((Control)txtHourOut).set_Name("txtHourOut");
			((Control)txtHourOut).set_Size(new Size(13, 13));
			((Control)txtHourOut).set_TabIndex(69);
			((Control)txtHourOut).set_Text("0");
			((Control)cmdHome).set_Anchor((AnchorStyles)1);
			((Control)cmdHome).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdHome).set_Location(new Point(670, 43));
			((Control)cmdHome).set_Name("cmdHome");
			((Control)cmdHome).set_Size(new Size(38, 20));
			((Control)cmdHome).set_TabIndex(71);
			((Control)cmdHome).set_Text("Home");
			((ButtonBase)cmdHome).set_UseVisualStyleBackColor(true);
			((Control)cmdHome).add_Click((EventHandler)cmdHome_Click);
			((Control)opt10).set_Anchor((AnchorStyles)1);
			((Control)opt10).set_AutoSize(true);
			((Control)opt10).set_Location(new Point(362, 69));
			((Control)opt10).set_Name("opt10");
			((Control)opt10).set_Size(new Size(80, 17));
			((Control)opt10).set_TabIndex(72);
			((Control)opt10).set_Text("10 deg field");
			((ButtonBase)opt10).set_UseVisualStyleBackColor(true);
			opt10.add_CheckedChanged((EventHandler)opt10_CheckedChanged);
			((Control)cmbMag).set_Anchor((AnchorStyles)1);
			((ListControl)cmbMag).set_FormattingEnabled(true);
			cmbMag.get_Items().AddRange(new object[7] { "All", "10.0", "9.0", "8.0", "7.0", "6.0", "5.0" });
			((Control)cmbMag).set_Location(new Point(493, 67));
			((Control)cmbMag).set_Name("cmbMag");
			((Control)cmbMag).set_Size(new Size(44, 21));
			((Control)cmbMag).set_TabIndex(73);
			cmbMag.add_SelectedIndexChanged((EventHandler)cmbMag_SelectedIndexChanged);
			((Control)label13).set_Anchor((AnchorStyles)1);
			((Control)label13).set_AutoSize(true);
			((Control)label13).set_Location(new Point(541, 71));
			((Control)label13).set_Name("label13");
			((Control)label13).set_Size(new Size(48, 13));
			((Control)label13).set_TabIndex(74);
			((Control)label13).set_Text("Mag limit");
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(718, 564));
			((Control)this).get_Controls().Add((Control)(object)label13);
			((Control)this).get_Controls().Add((Control)(object)cmbMag);
			((Control)this).get_Controls().Add((Control)(object)opt10);
			((Control)this).get_Controls().Add((Control)(object)cmdHome);
			((Control)this).get_Controls().Add((Control)(object)txtHourOut);
			((Control)this).get_Controls().Add((Control)(object)cmdPlot);
			((Control)this).get_Controls().Add((Control)(object)label12);
			((Control)this).get_Controls().Add((Control)(object)label11);
			((Control)this).get_Controls().Add((Control)(object)label10);
			((Control)this).get_Controls().Add((Control)(object)label9);
			((Control)this).get_Controls().Add((Control)(object)label8);
			((Control)this).get_Controls().Add((Control)(object)updnInterval);
			((Control)this).get_Controls().Add((Control)(object)updnDuration);
			((Control)this).get_Controls().Add((Control)(object)opt1);
			((Control)this).get_Controls().Add((Control)(object)opt2);
			((Control)this).get_Controls().Add((Control)(object)opt4);
			((Control)this).get_Controls().Add((Control)(object)opt07);
			((Control)this).get_Controls().Add((Control)(object)label7);
			((Control)this).get_Controls().Add((Control)(object)label6);
			((Control)this).get_Controls().Add((Control)(object)label3);
			((Control)this).get_Controls().Add((Control)(object)label2);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)updnMinute);
			((Control)this).get_Controls().Add((Control)(object)updnHour);
			((Control)this).get_Controls().Add((Control)(object)updnLatitude);
			((Control)this).get_Controls().Add((Control)(object)updnLongitude);
			((Control)this).get_Controls().Add((Control)(object)label5);
			((Control)this).get_Controls().Add((Control)(object)label4);
			((Control)this).get_Controls().Add((Control)(object)cmbLunarDayStart);
			((Control)this).get_Controls().Add((Control)(object)cmbLunarMonthStart);
			((Control)this).get_Controls().Add((Control)(object)updnLunarYearStart);
			((Control)this).get_Controls().Add((Control)(object)picMoon);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_Controls().Add((Control)(object)cmdCancel);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationMoonInStarField", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_Location(Settings.Default.LocationMoonInStarField);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_Name("MoonInStarField");
			((Control)this).set_Text("Moon in star field");
			((Form)this).add_Load((EventHandler)MoonInStarField_Load);
			((Form)this).add_FormClosing(new FormClosingEventHandler(MoonInStarField_FormClosing));
			((Control)this).add_Resize((EventHandler)MoonInStarField_Resize);
			((ISupportInitialize)picMoon).EndInit();
			((ISupportInitialize)updnLunarYearStart).EndInit();
			((ISupportInitialize)updnLatitude).EndInit();
			((ISupportInitialize)updnLongitude).EndInit();
			((ISupportInitialize)updnHour).EndInit();
			((ISupportInitialize)updnMinute).EndInit();
			((ISupportInitialize)updnDuration).EndInit();
			((ISupportInitialize)updnInterval).EndInit();
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
