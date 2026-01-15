using System;
using System.ComponentModel;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult
{
	public class MoonPhases : Form
	{
		private readonly string AppPath;

		private const double Radian = 180.0 / Math.PI;

		private static string Prediction;

		private static string PredPerigee;

		private static string PredApogee;

		private bool IsComputing;

		private static int IsUT;

		private IContainer components;

		private NumericUpDown updnYear;

		private Label label1;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem withPredictionToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem printToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private TextBox txtOut;

		private ToolStripMenuItem printPreviewToolStripMenuItem;

		private ToolStripMenuItem helpToolStripMenuItem;

		private CheckBox chkHighPrecision;

		private CheckBox chkUT;

		private Button cmdCompute;

		public MoonPhases()
		{
			InitializeComponent();
			AppPath = Utilities.AppPath;
			updnYear.set_Value((decimal)DateTime.Now.Year);
		}

		private void cmdCompute_Click(object sender, EventArgs e)
		{
			Compute();
		}

		internal void Compute()
		{
			if (!IsComputing)
			{
				IsComputing = true;
				IsUT = 1;
				if (chkUT.get_Checked())
				{
					IsUT = 0;
				}
				Phases();
				ApogeePerigee();
				((Control)txtOut).set_Text(Prediction + PredPerigee + PredApogee + MoonPhysical());
				IsComputing = false;
			}
		}

		private void Phases()
		{
			double num = 0.0;
			double num2 = 0.0;
			double num3 = 0.0;
			bool flag = true;
			double num4 = Utilities.JD_from_Date((int)updnYear.get_Value(), 1, 1.0);
			double num5 = Utilities.JD_from_Date((int)(updnYear.get_Value() + 1m), 1, 1.0);
			double num6 = Utilities.delta_T((int)updnYear.get_Value(), 7, 1.0) / 86400.0;
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.AppendLine("".PadRight(20) + "Phases of the Moon " + updnYear.get_Value() + "\r\n");
			stringBuilder.AppendLine("    New Moon    First Quarter      Full Moon             Last Quarter");
			stringBuilder.AppendLine("      d  h            d  h            d  h    ⊕dist        d  h      ");
			for (double num7 = num4; num7 <= num5; num7 += 1.0)
			{
				Utilities.QuickMoon(num7, out var RA, out var Dec, out var Parallax, out var MoonLongitude, out var MoonLatitude);
				Utilities.QuickLongLatMercuryVenusEarth(num7, 3, out var Longitude, out var _, out var _);
				double num8 = ((MoonLongitude - Longitude) * (180.0 / Math.PI) + 180.0) % 360.0;
				if (num8 < 0.0)
				{
					num8 += 360.0;
				}
				if (num7 > num4)
				{
					if (num8 < num2)
					{
						num2 -= 360.0;
					}
					int num9 = 0;
					if (num2 < 0.0 && num8 > 0.0)
					{
						num9 = 1;
						num3 = num2 / (num2 - num8);
					}
					else if (num2 < 90.0 && num8 > 90.0)
					{
						num9 = 2;
						num3 = (num2 - 90.0) / (num2 - num8);
					}
					else if (num2 < 180.0 && num8 > 180.0)
					{
						num9 = 3;
						num3 = (num2 - 180.0) / (num2 - num8);
						Utilities.QuickMoon(num7 - 1.0 + num3, out RA, out Dec, out Parallax, out MoonLongitude, out MoonLatitude);
						num = 1.0 / Math.Sin(Parallax);
					}
					else if (num2 < 270.0 && num8 > 270.0)
					{
						num9 = 4;
						num3 = (num2 - 270.0) / (num2 - num8);
					}
					if (num9 > 0)
					{
						if (flag)
						{
							flag = false;
							for (int i = 1; i < num9; i++)
							{
								stringBuilder.Append("".PadRight(16));
								if (i == 3)
								{
									stringBuilder.Append("".PadRight(8));
								}
							}
						}
						stringBuilder.Append(Utilities.Date_from_JD(num7 - 1.0, 0).Substring(4));
						if (num9 == 3)
						{
							stringBuilder.AppendFormat("{0,5:F1} ({1,5:F2})    ", (num3 - num6) * 24.0, num);
						}
						else
						{
							stringBuilder.AppendFormat("{0,5:F1}    ", (num3 - num6) * 24.0);
						}
						if (num9 == 4)
						{
							stringBuilder.AppendLine("");
						}
					}
				}
				num2 = num8;
			}
			Prediction = stringBuilder.ToString() + "\r\n\r\n";
		}

		private void ApogeePerigee()
		{
			double num = 0.0;
			double num2 = 0.0;
			double num3 = 0.0;
			double num4 = Utilities.JD_from_Date((int)updnYear.get_Value(), 1, 1.0);
			double num5 = Utilities.JD_from_Date((int)(updnYear.get_Value() + 1m), 1, 1.0);
			double num6 = Utilities.delta_T((int)updnYear.get_Value(), 7, 1.0) / 86400.0;
			StringBuilder stringBuilder = new StringBuilder();
			StringBuilder stringBuilder2 = new StringBuilder();
			int num7 = 0;
			int num8 = 0;
			stringBuilder.AppendLine("".PadRight(20) + "Moon at Perigee " + updnYear.get_Value() + "\r\n");
			stringBuilder.Append("      d  h      ");
			stringBuilder.Append("      d  h      ");
			stringBuilder.Append("      d  h      ");
			stringBuilder.AppendLine("      d  h      ");
			stringBuilder2.AppendLine("".PadRight(20) + "Moon at Apogee " + updnYear.get_Value() + "\r\n");
			stringBuilder2.Append("      d  h      ");
			stringBuilder2.Append("      d  h      ");
			stringBuilder2.Append("      d  h      ");
			stringBuilder2.AppendLine("      d  h      ");
			double Parallax = (num2 = 0.0);
			for (int i = -2; i < 368; i++)
			{
				double num9 = num4 + (double)i;
				Utilities.QuickMoon(num9, out var _, out var _, out Parallax, out var _, out var _);
				double num10 = Parallax - num2;
				if (i >= 0 && Math.Sign(num10) != Math.Sign(num))
				{
					num3 = num / (num - num10);
					double num11 = num9;
					num3 -= 0.5;
					if (num3 < 0.0)
					{
						num3 += 1.0;
						num11 -= 1.0;
					}
					if (num11 >= num4 && num11 < num5)
					{
						if (num > 0.0)
						{
							stringBuilder.Append(Utilities.Date_from_JD(num11 - 1.0, 0).Substring(4));
							stringBuilder.AppendFormat("{0,3:F0}      ", (num3 - num6) * 24.0);
							num7++;
							if (num7 % 4 == 0)
							{
								stringBuilder.AppendLine("");
							}
						}
						else
						{
							stringBuilder2.Append(Utilities.Date_from_JD(num11 - 1.0, 0).Substring(4));
							stringBuilder2.AppendFormat("{0,3:F0}      ", (num3 - num6) * 24.0);
							num8++;
							if (num8 % 4 == 0)
							{
								stringBuilder2.AppendLine("");
							}
						}
					}
				}
				num = num10;
				num2 = Parallax;
			}
			PredPerigee = stringBuilder.ToString() + "\r\n\r\n";
			PredApogee = stringBuilder2.ToString() + "\r\n\r\n";
		}

		private string MoonPhysical()
		{
			string[] array = new string[94];
			string text = " Date    l    b   Axis  Coln  Lat %ill ";
			string text2 = "         o    o     o     o    o       ";
			if (chkHighPrecision.get_Checked())
			{
				text = " Date    l      b     Axis    Coln    Lat   SunAxis %ill ";
				text2 = "         o      o       o       o      o       o         ";
			}
			string text3 = "".PadRight(20) + "Physical Ephemeris for the Moon " + updnYear.get_Value();
			double num = Utilities.delta_T((int)updnYear.get_Value(), 7, 1.0) / 86400.0;
			StringBuilder stringBuilder = new StringBuilder();
			for (int i = 0; i <= 1; i++)
			{
				for (int j = 0; j < 94; j++)
				{
					array[j] = "";
				}
				stringBuilder.AppendLine("\r\n\r\n" + text3);
				stringBuilder.AppendLine("\r\n" + text + text);
				stringBuilder.AppendLine(text2 + text2);
				double num2;
				double num3;
				if (i == 0)
				{
					num2 = Utilities.JD_from_Date((int)updnYear.get_Value(), 1, 1.0);
					num3 = Utilities.JD_from_Date((int)updnYear.get_Value(), 7, 1.0);
				}
				else
				{
					num2 = Utilities.JD_from_Date((int)updnYear.get_Value(), 7, 1.0);
					num3 = Utilities.JD_from_Date((int)updnYear.get_Value(), 12, 31.0);
				}
				for (double num4 = num2; num4 <= num3 + 1.0; num4 += 2.0)
				{
					Utilities.MeanEclipticOfDate(num4, Use1976Value: true);
					StringBuilder stringBuilder2 = new StringBuilder();
					int num5 = (int)(num4 - num2) / 2;
					Utilities.GeocentricMoon(num4 - (double)IsUT * num, out var RA, out var Dec, out var PiMoon, out var l, out var b, out var C);
					Utilities.PlanetGeocentric(num4 + (double)(1 - IsUT) * num, 3, 1E-06, 0, out var RA2, out var Dec2, out var GeocentricDist);
					Utilities.Librations(num4 + (double)(1 - IsUT) * num, RA, Dec, PiMoon, GetEarthSelenographic: true, GetSunSelenographic: true, RA2, Dec2, GeocentricDist, out l, out b, out C, out var lSun_deg, out var bSun_deg, out var CSun_deg, out var SunMinusMoonLongitude);
					lSun_deg = 90.0 - lSun_deg;
					if (lSun_deg < 0.0)
					{
						lSun_deg += 360.0;
					}
					if (lSun_deg >= 360.0)
					{
						lSun_deg -= 360.0;
					}
					double num6 = 50.0 * (1.0 - Math.Cos(SunMinusMoonLongitude));
					Utilities.Date_from_JD(num4, out var _, out var Month, out var day);
					if (day < 3.0)
					{
						stringBuilder2.Append(Utilities.ShortMonths[Month]);
					}
					else
					{
						stringBuilder2.Append("   ");
					}
					stringBuilder2.AppendFormat("{0,3:f0}", day);
					if (chkHighPrecision.get_Checked())
					{
						stringBuilder2.AppendFormat("{0,7:f3}", l);
						stringBuilder2.AppendFormat("{0,7:f3}", b);
						stringBuilder2.AppendFormat("{0,8:f3}", C);
						stringBuilder2.AppendFormat("{0,8:f3}", lSun_deg);
						stringBuilder2.AppendFormat("{0,7:f3}", bSun_deg);
						stringBuilder2.AppendFormat("{0,8:f3}", CSun_deg);
					}
					else
					{
						stringBuilder2.AppendFormat("{0,5:f1}", l);
						stringBuilder2.AppendFormat("{0,5:f1}", b);
						stringBuilder2.AppendFormat("{0,6:f1}", C);
						stringBuilder2.AppendFormat("{0,6:f1}", lSun_deg);
						stringBuilder2.AppendFormat("{0,5:f1}", bSun_deg);
					}
					stringBuilder2.AppendFormat("{0,4:f0}  ", num6);
					array[num5] = stringBuilder2.ToString();
				}
				for (int k = 0; k < 47; k++)
				{
					if (k % 5 == 0 && k > 0)
					{
						stringBuilder.AppendLine("");
					}
					stringBuilder.AppendLine(array[k] + array[k + 45 + i]);
				}
			}
			return stringBuilder.ToString();
		}

		private void copyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(((Control)txtOut).get_Text());
		}

		private void printPreviewToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.PrintPreviewText(((Control)txtOut).get_Text());
		}

		private void printToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.PrintText(((Control)txtOut).get_Text());
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings.Default.Save_EphemerisData = Output.SaveAppendPredictionText(((Control)txtOut).get_Text(), "Moon Phases " + updnYear.get_Value(), Settings.Default.Save_EphemerisData);
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
		}

		private void updnYear_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnYear).Select(0, 10);
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Moon Phases");
		}

		private void MoonPhases_Load(object sender, EventArgs e)
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
		}

		private void chkHighPrecision_Resize(object sender, EventArgs e)
		{
			if (((Control)this).get_Width() > 60)
			{
				((Control)txtOut).set_Width(((Control)this).get_Width() - 33);
			}
			if (((Control)this).get_Height() > 200)
			{
				((Control)txtOut).set_Height(((Control)this).get_Height() - 102);
			}
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
			//IL_082e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0838: Expected O, but got Unknown
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(MoonPhases));
			updnYear = new NumericUpDown();
			label1 = new Label();
			menuStrip1 = new MenuStrip();
			withPredictionToolStripMenuItem = new ToolStripMenuItem();
			copyToolStripMenuItem = new ToolStripMenuItem();
			printPreviewToolStripMenuItem = new ToolStripMenuItem();
			printToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			txtOut = new TextBox();
			chkHighPrecision = new CheckBox();
			chkUT = new CheckBox();
			cmdCompute = new Button();
			((ISupportInitialize)updnYear).BeginInit();
			((Control)menuStrip1).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)updnYear).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnYear).set_Location(new Point(126, 34));
			updnYear.set_Maximum(new decimal(new int[4] { 17000, 0, 0, 0 }));
			updnYear.set_Minimum(new decimal(new int[4] { 13000, 0, 0, -2147483648 }));
			((Control)updnYear).set_Name("updnYear");
			((Control)updnYear).set_Size(new Size(55, 20));
			((Control)updnYear).set_TabIndex(0);
			((Control)updnYear).add_Enter((EventHandler)updnYear_Enter);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(87, 37));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(29, 13));
			((Control)label1).set_TabIndex(1);
			((Control)label1).set_Text("Year");
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)withPredictionToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(617, 24));
			((Control)menuStrip1).set_TabIndex(2);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withPredictionToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[4]
			{
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)printPreviewToolStripMenuItem,
				(ToolStripItem)printToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem
			});
			((ToolStripItem)withPredictionToolStripMenuItem).set_Name("withPredictionToolStripMenuItem");
			((ToolStripItem)withPredictionToolStripMenuItem).set_Size(new Size(114, 20));
			((ToolStripItem)withPredictionToolStripMenuItem).set_Text("with Prediction...  ");
			((ToolStripItem)copyToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyToolStripMenuItem).set_Name("copyToolStripMenuItem");
			copyToolStripMenuItem.set_ShortcutKeys((Keys)131139);
			((ToolStripItem)copyToolStripMenuItem).set_Size(new Size(144, 22));
			((ToolStripItem)copyToolStripMenuItem).set_Text("&Copy");
			((ToolStripItem)copyToolStripMenuItem).add_Click((EventHandler)copyToolStripMenuItem_Click);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Image((Image)Resources.PrintPreview);
			((ToolStripItem)printPreviewToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Name("printPreviewToolStripMenuItem");
			((ToolStripItem)printPreviewToolStripMenuItem).set_Size(new Size(144, 22));
			((ToolStripItem)printPreviewToolStripMenuItem).set_Text("Print preview");
			((ToolStripItem)printPreviewToolStripMenuItem).add_Click((EventHandler)printPreviewToolStripMenuItem_Click);
			((ToolStripItem)printToolStripMenuItem).set_Image((Image)Resources.Print);
			((ToolStripItem)printToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printToolStripMenuItem).set_Name("printToolStripMenuItem");
			printToolStripMenuItem.set_ShortcutKeys((Keys)131152);
			((ToolStripItem)printToolStripMenuItem).set_Size(new Size(144, 22));
			((ToolStripItem)printToolStripMenuItem).set_Text("&Print");
			((ToolStripItem)printToolStripMenuItem).add_Click((EventHandler)printToolStripMenuItem_Click);
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			saveToolStripMenuItem.set_ShortcutKeys((Keys)131155);
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(144, 22));
			((ToolStripItem)saveToolStripMenuItem).set_Text("&Save");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
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
			((Control)txtOut).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtOut).set_Location(new Point(11, 60));
			((TextBoxBase)txtOut).set_Multiline(true);
			((Control)txtOut).set_Name("txtOut");
			((TextBoxBase)txtOut).set_ReadOnly(true);
			txtOut.set_ScrollBars((ScrollBars)3);
			((Control)txtOut).set_Size(new Size(594, 582));
			((Control)txtOut).set_TabIndex(3);
			((Control)chkHighPrecision).set_AutoSize(true);
			((Control)chkHighPrecision).set_Location(new Point(198, 36));
			((Control)chkHighPrecision).set_Name("chkHighPrecision");
			((Control)chkHighPrecision).set_Size(new Size(93, 17));
			((Control)chkHighPrecision).set_TabIndex(4);
			((Control)chkHighPrecision).set_Text("High precision");
			((ButtonBase)chkHighPrecision).set_UseVisualStyleBackColor(true);
			((Control)chkUT).set_AutoSize(true);
			chkUT.set_Checked(true);
			chkUT.set_CheckState((CheckState)1);
			((Control)chkUT).set_Location(new Point(309, 36));
			((Control)chkUT).set_Name("chkUT");
			((Control)chkUT).set_Size(new Size(113, 17));
			((Control)chkUT).set_TabIndex(5);
			((Control)chkUT).set_Text("UT (instead of TT)");
			((ButtonBase)chkUT).set_UseVisualStyleBackColor(true);
			((Control)cmdCompute).set_Location(new Point(440, 34));
			((Control)cmdCompute).set_Name("cmdCompute");
			((Control)cmdCompute).set_Size(new Size(61, 20));
			((Control)cmdCompute).set_TabIndex(6);
			((Control)cmdCompute).set_Text("Compute");
			((ButtonBase)cmdCompute).set_UseVisualStyleBackColor(true);
			((Control)cmdCompute).add_Click((EventHandler)cmdCompute_Click);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(617, 654));
			((Control)this).get_Controls().Add((Control)(object)cmdCompute);
			((Control)this).get_Controls().Add((Control)(object)chkUT);
			((Control)this).get_Controls().Add((Control)(object)chkHighPrecision);
			((Control)this).get_Controls().Add((Control)(object)txtOut);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)updnYear);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationEphemMoonPhase", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_Icon((Icon)componentResourceManager.GetObject("$this.Icon"));
			((Form)this).set_Location(Settings.Default.LocationEphemMoonPhase);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Form)this).set_MaximizeBox(false);
			((Control)this).set_Name("MoonPhases");
			((Control)this).set_Text("Moon phases, apogee and perigee, physical ephemeris");
			((Form)this).add_Load((EventHandler)MoonPhases_Load);
			((Control)this).add_Resize((EventHandler)chkHighPrecision_Resize);
			((ISupportInitialize)updnYear).EndInit();
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
