using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Ephemerides
{
	public class Asteroids : Form
	{
		private const double Radian = 180.0 / Math.PI;

		private bool CancelFlag;

		private bool FormLoaded;

		private List<AsteroidElements> ElementsList = Elements.MainAsteroids.AstElements;

		private IContainer components;

		private ListBox lstAsteroids;

		private NumericUpDown updnStartMonth;

		private NumericUpDown updnStartYear;

		private Label label5;

		private Label label3;

		private Label label2;

		private Label label1;

		private NumericUpDown updnDay;

		private NumericUpDown updnMonth;

		private NumericUpDown updnYear;

		private Label label7;

		private NumericUpDown updnInterval;

		private Label label6;

		private NumericUpDown updnDuration;

		private ListBox lstEphem;

		private CheckBox chkShowElements;

		private Label label4;

		private NumericUpDown updnHour;

		private Label label8;

		private Label label9;

		private Label label10;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem withEphemerisToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem printToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private ToolStripMenuItem printPreviewToolStripMenuItem;

		private Label label12;

		private NumericUpDown updnStartDay;

		private TextBox txtSearchID;

		private Label label11;

		private Button cmdFind;

		private Label label13;

		private CheckBox chkApparent;

		private CheckBox chkHighPrec;

		public Asteroids()
		{
			InitializeComponent();
		}

		private void Asteroids_Load(object sender, EventArgs e)
		{
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			updnStartYear.set_Value((decimal)DateTime.Now.ToUniversalTime().Year);
			updnStartMonth.set_Value((decimal)DateTime.Now.ToUniversalTime().Month);
			updnStartDay.set_Value((decimal)DateTime.Now.ToUniversalTime().Day);
			updnYear.set_Value((decimal)DateTime.Now.ToUniversalTime().Year);
			updnMonth.set_Value((decimal)DateTime.Now.ToUniversalTime().Month);
			updnDay.set_Value((decimal)DateTime.Now.ToUniversalTime().Day);
			updnHour.set_Value((decimal)DateTime.Now.ToUniversalTime().Hour);
			ReadAsteroids();
			FormLoaded = true;
		}

		private void ReadAsteroids()
		{
			if (ElementsList.Count < 1)
			{
				Elements.MainAsteroids.Fill_AllAsteroids();
			}
			lstAsteroids.get_Items().Clear();
			for (int i = 0; i < ElementsList.Count; i++)
			{
				lstAsteroids.get_Items().Add((object)(ElementsList[i].IDNumber.ToString().PadLeft(7) + " " + ElementsList[i].IDName.ToString()));
			}
		}

		private void Asteroids_FormClosed(object sender, FormClosedEventArgs e)
		{
			((Component)this).Dispose();
		}

		private void lstAsteroids_SelectedIndexChanged(object sender, EventArgs e)
		{
			IntegrateElements();
		}

		private void IntegrateElements()
		{
			if (((ListControl)lstAsteroids).get_SelectedIndex() >= 0)
			{
				if (ElementsList.Count < 1)
				{
					Elements.MainAsteroids.Fill_AllAsteroids();
				}
				MinorPlanetOccultationElements.Integrate(ElementsList, ((ListControl)lstAsteroids).get_SelectedIndex(), IntegrateJD(), IntegrateJD(), AddNotIntegratedFlag: true);
				ComputeEphemeris();
			}
		}

		private double IntegrateJD()
		{
			return Math.Floor(Utilities.JD_from_Date((int)updnStartYear.get_Value(), (int)updnStartMonth.get_Value(), (int)updnStartDay.get_Value())) + 0.5;
		}

		private void ComputeEphemeris()
		{
			if (!FormLoaded)
			{
				return;
			}
			int num = (int)updnInterval.get_Value();
			int num2 = (int)updnDuration.get_Value();
			int num3 = 0;
			int count = MinorPlanetOccultationElements.ElementsAt1Day.Count;
			if (count < 1)
			{
				return;
			}
			MinorPlanetOccultationElements.ElementsAt1Day[count - 1].ToString();
			double num4 = MinorPlanetOccultationElements.ElementsAt1Day[count - 1].IDNumber;
			string iDName = MinorPlanetOccultationElements.ElementsAt1Day[count - 1].IDName;
			int epochYear = MinorPlanetOccultationElements.ElementsAt1Day[count - 1].EpochYear;
			int epochMonth = MinorPlanetOccultationElements.ElementsAt1Day[count - 1].EpochMonth;
			double epochDay = MinorPlanetOccultationElements.ElementsAt1Day[count - 1].EpochDay;
			double osculatingJD = MinorPlanetOccultationElements.ElementsAt1Day[count - 1].OsculatingJD;
			double num5 = MinorPlanetOccultationElements.ElementsAt1Day[count - 1].Meananomaly / (180.0 / Math.PI);
			double num6 = MinorPlanetOccultationElements.ElementsAt1Day[count - 1].Perihelion / (180.0 / Math.PI);
			double num7 = MinorPlanetOccultationElements.ElementsAt1Day[count - 1].Node / (180.0 / Math.PI);
			double num8 = MinorPlanetOccultationElements.ElementsAt1Day[count - 1].I / (180.0 / Math.PI);
			double e = MinorPlanetOccultationElements.ElementsAt1Day[count - 1].E;
			double num9 = MinorPlanetOccultationElements.ElementsAt1Day[count - 1].A * (1.0 - e);
			double h = MinorPlanetOccultationElements.ElementsAt1Day[count - 1].H0;
			double g_phaseCoeff = MinorPlanetOccultationElements.ElementsAt1Day[count - 1].G_phaseCoeff;
			string text = MinorPlanetOccultationElements.ElementsAt1Day[count - 1].OrbitSource + " " + MinorPlanetOccultationElements.ElementsAt1Day[count - 1].OrbitDate;
			double num10 = Utilities.JD_from_Date((int)updnYear.get_Value(), (int)updnMonth.get_Value(), (double)updnDay.get_Value());
			double num11 = (double)updnHour.get_Value();
			lstEphem.get_Items().Clear();
			lstEphem.get_Items().Add((object)("Ephemeris of " + num4 + " " + iDName.ToString() + num11.ToString(" at #0 hrs UTC") + "   (J2000)"));
			lstEphem.get_Items().Add((object)"");
			if (chkShowElements.get_Checked())
			{
				lstEphem.get_Items().Add((object)(string.Format("T {0, 4:F0} {1,3} {2,7:F4} TT", epochYear, Utilities.ShortMonths[epochMonth], epochDay) + "     Orbit : " + text));
				lstEphem.get_Items().Add((object)string.Format("MA {0,10:f6}°", num5 * (180.0 / Math.PI)));
				lstEphem.get_Items().Add((object)string.Format("q {0,10:F7}    Peri. {1,10:F6}°", num9, num6 * (180.0 / Math.PI)));
				if (e < 0.95)
				{
					lstEphem.get_Items().Add((object)string.Format("a {0,10:F7}    Node  {1,10:F6}°", num9 / (1.0 - e), num7 * (180.0 / Math.PI)));
				}
				else
				{
					lstEphem.get_Items().Add((object)string.Format("z {0,10:F7}    Node  {1,10:F6}°", (1.0 - e) / num9, num7 * (180.0 / Math.PI)));
				}
				lstEphem.get_Items().Add((object)string.Format("e {0,10:F7}    Incl. {1,10:F6}°", e, num8 * (180.0 / Math.PI)));
				lstEphem.get_Items().Add((object)"");
			}
			if (Math.Abs(num10 + (double)(num2 / 2) - osculatingJD) > 150.0)
			{
				lstEphem.get_Items().Add((object)"*** WARNING. Ephemeris is of poor accuracy. Set integration date to");
				lstEphem.get_Items().Add((object)"***          correspond to the middle of the Ephemeris period.");
				lstEphem.get_Items().Add((object)"");
			}
			if (chkHighPrec.get_Checked())
			{
				lstEphem.get_Items().Add((object)"   y  m   d   h  m   s         o  '   \"        delta  RSun   Elong  Phase    Mag   \"/min  PA");
			}
			else
			{
				lstEphem.get_Items().Add((object)"   y  m   d   h  m   s      o  '   \"     delta  RSun   Elong  Phase    Mag   \"/min  PA");
			}
			for (int i = 0; i <= num2; i += num)
			{
				Application.DoEvents();
				if (!CancelFlag)
				{
					double UT1UTC;
					double x;
					double y;
					double num12 = Utilities.delta_T(num10 + (double)i, out UT1UTC, out x, out y);
					Utilities.PositionfromElements(num10 + (double)i + num11 / 24.0 + num12 / 86400.0, 0.0, 0.0, 0.0, 0.0, osculatingJD, num5, num9, e, num6, num7, num8, h, g_phaseCoeff, 5.0, 0.0, out var RA, out var Dec, out var RadiusVector, out var AstrometricGeocentricDistance, out var Magnitude, out var Elongation, out var PhaseAngle);
					if (chkApparent.get_Checked())
					{
						Utilities.ApparentStarPosition(ref RA, ref Dec, 0.0, 0.0, 2000, num10 + (double)i + num11 / 24.0, use2006values_Not1976: false);
					}
					StringBuilder stringBuilder = new StringBuilder();
					stringBuilder.Append(Utilities.Date_from_JD(num10 + (double)i, 0) + "  ");
					if (chkHighPrec.get_Checked())
					{
						stringBuilder.Append(Utilities.DEGtoDMS(RA * (180.0 / Math.PI) / 15.0, 2, 5, MinutesOnly: false) + "  ");
						stringBuilder.Append(Utilities.DEGtoDMS(Dec * (180.0 / Math.PI), 3, 4, MinutesOnly: false) + "  ");
					}
					else
					{
						stringBuilder.Append(Utilities.DEGtoDMS(RA * (180.0 / Math.PI) / 15.0, 2, 2, MinutesOnly: false) + "  ");
						stringBuilder.Append(Utilities.DEGtoDMS(Dec * (180.0 / Math.PI), 3, 1, MinutesOnly: false) + "  ");
					}
					stringBuilder.AppendFormat("{0,7:F3}", AstrometricGeocentricDistance);
					stringBuilder.AppendFormat("{0,7:F3}", RadiusVector);
					stringBuilder.AppendFormat("{0,7:F1}", Elongation * (180.0 / Math.PI));
					stringBuilder.AppendFormat("{0,7:F1}", PhaseAngle * (180.0 / Math.PI));
					stringBuilder.AppendFormat("{0,7:F1}", Magnitude);
					Utilities.PositionfromElements(num10 + (double)i + num11 / 24.0 + 1.0 / 144.0, 0.0, 0.0, 0.0, 0.0, osculatingJD, num5, num9, e, num6, num7, num8, h, g_phaseCoeff, 5.0, 0.0, out var RA2, out var Dec2, out RadiusVector, out AstrometricGeocentricDistance, out Magnitude, out Elongation, out PhaseAngle);
					Utilities.Distance(RA, Dec, RA2, Dec2, out var Distance, out var PA_atOrigin);
					stringBuilder.AppendFormat(" {0,7:F2}", Distance * (180.0 / Math.PI) * 360.0);
					stringBuilder.AppendFormat(" {0,5:F1}", PA_atOrigin * (180.0 / Math.PI));
					lstEphem.get_Items().Add((object)stringBuilder.ToString());
					num3++;
					if (num3 % 5 == 0)
					{
						lstEphem.get_Items().Add((object)"");
					}
					continue;
				}
				break;
			}
		}

		private void copyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(CollectEvents());
		}

		private void printPreviewToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.PrintPreviewText(CollectEvents());
		}

		private void printToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.PrintText(CollectEvents());
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (((ListControl)lstAsteroids).get_SelectedIndex() >= 0)
			{
				Settings.Default.Save_EphemerisData = Output.SaveAppendPredictionText(CollectEvents(), "Ephemeris of " + lstAsteroids.get_Items().get_Item(((ListControl)lstAsteroids).get_SelectedIndex()).ToString()!.Trim(), Settings.Default.Save_EphemerisData);
			}
		}

		private string CollectEvents()
		{
			StringBuilder stringBuilder = new StringBuilder();
			int count = lstEphem.get_Items().get_Count();
			for (int i = 0; i < count; i++)
			{
				stringBuilder.AppendLine(lstEphem.get_Items().get_Item(i).ToString());
			}
			stringBuilder.AppendLine("");
			return stringBuilder.ToString();
		}

		private void updnDuration_ValueChanged(object sender, EventArgs e)
		{
			ComputeEphemeris();
		}

		private void updnInterval_ValueChanged(object sender, EventArgs e)
		{
			ComputeEphemeris();
		}

		private void updnYear_ValueChanged(object sender, EventArgs e)
		{
			ComputeEphemeris();
		}

		private void updnMonth_ValueChanged(object sender, EventArgs e)
		{
			ComputeEphemeris();
		}

		private void updnDay_ValueChanged(object sender, EventArgs e)
		{
			ComputeEphemeris();
		}

		private void updnHour_ValueChanged(object sender, EventArgs e)
		{
			ComputeEphemeris();
		}

		private void updnStartYear_ValueChanged(object sender, EventArgs e)
		{
			IntegrateElements();
		}

		private void updnStartMonth_ValueChanged(object sender, EventArgs e)
		{
			IntegrateElements();
		}

		private void updnStartDay_ValueChanged(object sender, EventArgs e)
		{
			IntegrateElements();
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Asteroid ephemeris");
		}

		private void chkShowElements_CheckedChanged(object sender, EventArgs e)
		{
			ComputeEphemeris();
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private void cmdFind_Click(object sender, EventArgs e)
		{
			//IL_0064: Unknown result type (might be due to invalid IL or missing references)
			int num = 0;
			string text = ((Control)txtSearchID).get_Text().Trim();
			if (text.Length != 0)
			{
				int result = -1;
				if (!int.TryParse(text, out result))
				{
					result = -1;
				}
				num = ((result <= 0) ? Elements.MainAsteroids.GetAsteroidRecord_fromName(text) : Elements.MainAsteroids.GetAsteroidRecord_fromNumber(result));
				if (num == -1)
				{
					MessageBox.Show("The asteroid " + text + " is not in the\r\nfile of asteroid elements", "No elements", (MessageBoxButtons)0, (MessageBoxIcon)16);
				}
				else
				{
					((ListControl)lstAsteroids).set_SelectedIndex(num);
				}
			}
		}

		private void chkHighPrec_CheckedChanged(object sender, EventArgs e)
		{
			ComputeEphemeris();
		}

		private void chkApparent_CheckedChanged(object sender, EventArgs e)
		{
			ComputeEphemeris();
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
			//IL_0932: Unknown result type (might be due to invalid IL or missing references)
			//IL_093c: Expected O, but got Unknown
			//IL_09d3: Unknown result type (might be due to invalid IL or missing references)
			//IL_09dd: Expected O, but got Unknown
			//IL_0a87: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a91: Expected O, but got Unknown
			//IL_16a1: Unknown result type (might be due to invalid IL or missing references)
			//IL_16ab: Expected O, but got Unknown
			lstAsteroids = new ListBox();
			updnStartMonth = new NumericUpDown();
			updnStartYear = new NumericUpDown();
			label5 = new Label();
			label3 = new Label();
			label2 = new Label();
			label1 = new Label();
			updnDay = new NumericUpDown();
			updnMonth = new NumericUpDown();
			updnYear = new NumericUpDown();
			label7 = new Label();
			label6 = new Label();
			lstEphem = new ListBox();
			chkShowElements = new CheckBox();
			updnInterval = new NumericUpDown();
			updnDuration = new NumericUpDown();
			label4 = new Label();
			updnHour = new NumericUpDown();
			label8 = new Label();
			label9 = new Label();
			label10 = new Label();
			menuStrip1 = new MenuStrip();
			withEphemerisToolStripMenuItem = new ToolStripMenuItem();
			copyToolStripMenuItem = new ToolStripMenuItem();
			printPreviewToolStripMenuItem = new ToolStripMenuItem();
			printToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			label12 = new Label();
			updnStartDay = new NumericUpDown();
			txtSearchID = new TextBox();
			label11 = new Label();
			cmdFind = new Button();
			label13 = new Label();
			chkApparent = new CheckBox();
			chkHighPrec = new CheckBox();
			((ISupportInitialize)updnStartMonth).BeginInit();
			((ISupportInitialize)updnStartYear).BeginInit();
			((ISupportInitialize)updnDay).BeginInit();
			((ISupportInitialize)updnMonth).BeginInit();
			((ISupportInitialize)updnYear).BeginInit();
			((ISupportInitialize)updnInterval).BeginInit();
			((ISupportInitialize)updnDuration).BeginInit();
			((ISupportInitialize)updnHour).BeginInit();
			((Control)menuStrip1).SuspendLayout();
			((ISupportInitialize)updnStartDay).BeginInit();
			((Control)this).SuspendLayout();
			((Control)lstAsteroids).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstAsteroids).set_FormattingEnabled(true);
			lstAsteroids.set_ItemHeight(14);
			((Control)lstAsteroids).set_Location(new Point(12, 148));
			((Control)lstAsteroids).set_Name("lstAsteroids");
			((Control)lstAsteroids).set_Size(new Size(187, 382));
			((Control)lstAsteroids).set_TabIndex(0);
			lstAsteroids.add_SelectedIndexChanged((EventHandler)lstAsteroids_SelectedIndexChanged);
			((Control)updnStartMonth).set_Location(new Point(151, 53));
			updnStartMonth.set_Maximum(new decimal(new int[4] { 12, 0, 0, 0 }));
			updnStartMonth.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnStartMonth).set_Name("updnStartMonth");
			((Control)updnStartMonth).set_Size(new Size(36, 20));
			((Control)updnStartMonth).set_TabIndex(6);
			updnStartMonth.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			updnStartMonth.add_ValueChanged((EventHandler)updnStartMonth_ValueChanged);
			((Control)updnStartYear).set_Location(new Point(98, 53));
			updnStartYear.set_Maximum(new decimal(new int[4] { 9000, 0, 0, 0 }));
			updnStartYear.set_Minimum(new decimal(new int[4] { 9000, 0, 0, -2147483648 }));
			((Control)updnStartYear).set_Name("updnStartYear");
			((Control)updnStartYear).set_Size(new Size(46, 20));
			((Control)updnStartYear).set_TabIndex(5);
			updnStartYear.set_Value(new decimal(new int[4] { 2007, 0, 0, 0 }));
			updnStartYear.add_ValueChanged((EventHandler)updnStartYear_ValueChanged);
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Location(new Point(243, 55));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(82, 13));
			((Control)label5).set_TabIndex(7);
			((Control)label5).set_Text("Ephemeris from:");
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Location(new Point(433, 39));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(26, 13));
			((Control)label3).set_TabIndex(12);
			((Control)label3).set_Text("Day");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(390, 39));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(37, 13));
			((Control)label2).set_TabIndex(10);
			((Control)label2).set_Text("Month");
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(334, 39));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(29, 13));
			((Control)label1).set_TabIndex(8);
			((Control)label1).set_Text("Year");
			((Control)updnDay).set_Location(new Point(433, 53));
			updnDay.set_Maximum(new decimal(new int[4] { 31, 0, 0, 0 }));
			updnDay.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnDay).set_Name("updnDay");
			((Control)updnDay).set_Size(new Size(35, 20));
			((Control)updnDay).set_TabIndex(13);
			updnDay.set_Value(new decimal(new int[4] { 28, 0, 0, 0 }));
			updnDay.add_ValueChanged((EventHandler)updnDay_ValueChanged);
			((Control)updnMonth).set_Location(new Point(390, 53));
			updnMonth.set_Maximum(new decimal(new int[4] { 12, 0, 0, 0 }));
			updnMonth.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnMonth).set_Name("updnMonth");
			((Control)updnMonth).set_Size(new Size(35, 20));
			((Control)updnMonth).set_TabIndex(11);
			updnMonth.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			updnMonth.add_ValueChanged((EventHandler)updnMonth_ValueChanged);
			((Control)updnYear).set_Location(new Point(327, 53));
			updnYear.set_Maximum(new decimal(new int[4] { 19999, 0, 0, 0 }));
			updnYear.set_Minimum(new decimal(new int[4] { 19999, 0, 0, -2147483648 }));
			((Control)updnYear).set_Name("updnYear");
			((Control)updnYear).set_Size(new Size(55, 20));
			((Control)updnYear).set_TabIndex(9);
			updnYear.add_ValueChanged((EventHandler)updnYear_ValueChanged);
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Location(new Point(631, 39));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(73, 13));
			((Control)label7).set_TabIndex(16);
			((Control)label7).set_Text("Interval (days)");
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Location(new Point(535, 39));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(78, 13));
			((Control)label6).set_TabIndex(14);
			((Control)label6).set_Text("Duration (days)");
			((Control)lstEphem).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstEphem).set_FormattingEnabled(true);
			lstEphem.set_ItemHeight(14);
			((Control)lstEphem).set_Location(new Point(221, 82));
			((Control)lstEphem).set_Name("lstEphem");
			((Control)lstEphem).set_Size(new Size(640, 452));
			((Control)lstEphem).set_TabIndex(18);
			((Control)chkShowElements).set_AutoSize(true);
			chkShowElements.set_Checked(Settings.Default.ShowAsteroidElements);
			chkShowElements.set_CheckState((CheckState)1);
			((Control)chkShowElements).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "ShowAsteroidElements", true, (DataSourceUpdateMode)1));
			((Control)chkShowElements).set_Location(new Point(737, 65));
			((Control)chkShowElements).set_Name("chkShowElements");
			((Control)chkShowElements).set_Size(new Size(98, 17));
			((Control)chkShowElements).set_TabIndex(21);
			((Control)chkShowElements).set_Text("Show elements");
			((ButtonBase)chkShowElements).set_UseVisualStyleBackColor(true);
			chkShowElements.add_CheckedChanged((EventHandler)chkShowElements_CheckedChanged);
			((Control)updnInterval).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "AsteroidEphemInterval", true, (DataSourceUpdateMode)1));
			((Control)updnInterval).set_Location(new Point(646, 53));
			updnInterval.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnInterval).set_Name("updnInterval");
			((Control)updnInterval).set_Size(new Size(35, 20));
			((Control)updnInterval).set_TabIndex(17);
			updnInterval.set_Value(Settings.Default.AsteroidEphemInterval);
			updnInterval.add_ValueChanged((EventHandler)updnInterval_ValueChanged);
			((Control)updnDuration).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "AsteroidEphemDuration", true, (DataSourceUpdateMode)1));
			((Control)updnDuration).set_Location(new Point(548, 53));
			updnDuration.set_Maximum(new decimal(new int[4] { 900, 0, 0, 0 }));
			updnDuration.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnDuration).set_Name("updnDuration");
			((Control)updnDuration).set_Size(new Size(51, 20));
			((Control)updnDuration).set_TabIndex(15);
			updnDuration.set_Value(Settings.Default.AsteroidEphemDuration);
			updnDuration.add_ValueChanged((EventHandler)updnDuration_ValueChanged);
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Location(new Point(476, 39));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(30, 13));
			((Control)label4).set_TabIndex(22);
			((Control)label4).set_Text("Hour");
			((Control)updnHour).set_Location(new Point(476, 53));
			updnHour.set_Maximum(new decimal(new int[4] { 23, 0, 0, 0 }));
			((Control)updnHour).set_Name("updnHour");
			((Control)updnHour).set_Size(new Size(35, 20));
			((Control)updnHour).set_TabIndex(23);
			updnHour.add_ValueChanged((EventHandler)updnHour_ValueChanged);
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Location(new Point(152, 37));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(37, 13));
			((Control)label8).set_TabIndex(25);
			((Control)label8).set_Text("Month");
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Location(new Point(104, 37));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(29, 13));
			((Control)label9).set_TabIndex(24);
			((Control)label9).set_Text("Year");
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Location(new Point(5, 57));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(87, 13));
			((Control)label10).set_TabIndex(26);
			((Control)label10).set_Text("Integrate orbit to:");
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)withEphemerisToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(866, 24));
			((Control)menuStrip1).set_TabIndex(27);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withEphemerisToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[4]
			{
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)printPreviewToolStripMenuItem,
				(ToolStripItem)printToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem
			});
			((ToolStripItem)withEphemerisToolStripMenuItem).set_Name("withEphemerisToolStripMenuItem");
			((ToolStripItem)withEphemerisToolStripMenuItem).set_Size(new Size(121, 20));
			((ToolStripItem)withEphemerisToolStripMenuItem).set_Text("with Ephemeris...    ");
			((ToolStripItem)copyToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyToolStripMenuItem).set_Name("copyToolStripMenuItem");
			((ToolStripItem)copyToolStripMenuItem).set_Size(new Size(143, 22));
			((ToolStripItem)copyToolStripMenuItem).set_Text("Copy");
			((ToolStripItem)copyToolStripMenuItem).add_Click((EventHandler)copyToolStripMenuItem_Click);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Image((Image)Resources.PrintPreview);
			((ToolStripItem)printPreviewToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Name("printPreviewToolStripMenuItem");
			((ToolStripItem)printPreviewToolStripMenuItem).set_Size(new Size(143, 22));
			((ToolStripItem)printPreviewToolStripMenuItem).set_Text("Print Preview");
			((ToolStripItem)printPreviewToolStripMenuItem).add_Click((EventHandler)printPreviewToolStripMenuItem_Click);
			((ToolStripItem)printToolStripMenuItem).set_Image((Image)Resources.Print);
			((ToolStripItem)printToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printToolStripMenuItem).set_Name("printToolStripMenuItem");
			((ToolStripItem)printToolStripMenuItem).set_Size(new Size(143, 22));
			((ToolStripItem)printToolStripMenuItem).set_Text("Print ");
			((ToolStripItem)printToolStripMenuItem).add_Click((EventHandler)printToolStripMenuItem_Click);
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(143, 22));
			((ToolStripItem)saveToolStripMenuItem).set_Text("Save");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(72, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("Help    ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(63, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("Exit   ");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((Control)label12).set_AutoSize(true);
			((Control)label12).set_Location(new Point(195, 39));
			((Control)label12).set_Name("label12");
			((Control)label12).set_Size(new Size(26, 13));
			((Control)label12).set_TabIndex(29);
			((Control)label12).set_Text("Day");
			((Control)updnStartDay).set_Location(new Point(195, 53));
			updnStartDay.set_Maximum(new decimal(new int[4] { 31, 0, 0, 0 }));
			updnStartDay.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnStartDay).set_Name("updnStartDay");
			((Control)updnStartDay).set_Size(new Size(35, 20));
			((Control)updnStartDay).set_TabIndex(30);
			updnStartDay.set_Value(new decimal(new int[4] { 28, 0, 0, 0 }));
			updnStartDay.add_ValueChanged((EventHandler)updnStartDay_ValueChanged);
			((Control)txtSearchID).set_Location(new Point(89, 98));
			((Control)txtSearchID).set_Name("txtSearchID");
			((Control)txtSearchID).set_Size(new Size(67, 20));
			((Control)txtSearchID).set_TabIndex(31);
			((Control)label11).set_AutoSize(true);
			((Control)label11).set_Location(new Point(5, 95));
			((Control)label11).set_Name("label11");
			((Control)label11).set_Size(new Size(83, 26));
			((Control)label11).set_TabIndex(32);
			((Control)label11).set_Text("Find asterod by \r\nnumber or name");
			((Control)cmdFind).set_Location(new Point(161, 98));
			((Control)cmdFind).set_Name("cmdFind");
			((Control)cmdFind).set_Size(new Size(38, 21));
			((Control)cmdFind).set_TabIndex(33);
			((Control)cmdFind).set_Text("Find");
			((ButtonBase)cmdFind).set_UseVisualStyleBackColor(true);
			((Control)cmdFind).add_Click((EventHandler)cmdFind_Click);
			((Control)label13).set_AutoSize(true);
			((Control)label13).set_Location(new Point(46, 133));
			((Control)label13).set_Name("label13");
			((Control)label13).set_Size(new Size(119, 13));
			((Control)label13).set_TabIndex(34);
			((Control)label13).set_Text("S e l e c t   a s t e r o i d");
			((Control)chkApparent).set_AutoSize(true);
			((Control)chkApparent).set_Location(new Point(737, 27));
			((Control)chkApparent).set_Name("chkApparent");
			((Control)chkApparent).set_Size(new Size(113, 17));
			((Control)chkApparent).set_TabIndex(35);
			((Control)chkApparent).set_Text("Apparent positions");
			((ButtonBase)chkApparent).set_UseVisualStyleBackColor(true);
			chkApparent.add_CheckedChanged((EventHandler)chkApparent_CheckedChanged);
			((Control)chkHighPrec).set_AutoSize(true);
			((Control)chkHighPrec).set_Location(new Point(737, 46));
			((Control)chkHighPrec).set_Name("chkHighPrec");
			((Control)chkHighPrec).set_Size(new Size(93, 17));
			((Control)chkHighPrec).set_TabIndex(36);
			((Control)chkHighPrec).set_Text("High precision");
			((ButtonBase)chkHighPrec).set_UseVisualStyleBackColor(true);
			chkHighPrec.add_CheckedChanged((EventHandler)chkHighPrec_CheckedChanged);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(866, 542));
			((Control)this).get_Controls().Add((Control)(object)chkHighPrec);
			((Control)this).get_Controls().Add((Control)(object)chkApparent);
			((Control)this).get_Controls().Add((Control)(object)label13);
			((Control)this).get_Controls().Add((Control)(object)cmdFind);
			((Control)this).get_Controls().Add((Control)(object)label11);
			((Control)this).get_Controls().Add((Control)(object)txtSearchID);
			((Control)this).get_Controls().Add((Control)(object)label12);
			((Control)this).get_Controls().Add((Control)(object)updnStartDay);
			((Control)this).get_Controls().Add((Control)(object)label10);
			((Control)this).get_Controls().Add((Control)(object)label8);
			((Control)this).get_Controls().Add((Control)(object)label9);
			((Control)this).get_Controls().Add((Control)(object)label4);
			((Control)this).get_Controls().Add((Control)(object)updnHour);
			((Control)this).get_Controls().Add((Control)(object)chkShowElements);
			((Control)this).get_Controls().Add((Control)(object)lstEphem);
			((Control)this).get_Controls().Add((Control)(object)label7);
			((Control)this).get_Controls().Add((Control)(object)updnInterval);
			((Control)this).get_Controls().Add((Control)(object)label6);
			((Control)this).get_Controls().Add((Control)(object)updnDuration);
			((Control)this).get_Controls().Add((Control)(object)label5);
			((Control)this).get_Controls().Add((Control)(object)label3);
			((Control)this).get_Controls().Add((Control)(object)label2);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)updnDay);
			((Control)this).get_Controls().Add((Control)(object)updnMonth);
			((Control)this).get_Controls().Add((Control)(object)updnYear);
			((Control)this).get_Controls().Add((Control)(object)updnStartMonth);
			((Control)this).get_Controls().Add((Control)(object)updnStartYear);
			((Control)this).get_Controls().Add((Control)(object)lstAsteroids);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_Name("Asteroids");
			((Control)this).set_Text("Asteroids");
			((Form)this).add_FormClosed(new FormClosedEventHandler(Asteroids_FormClosed));
			((Form)this).add_Load((EventHandler)Asteroids_Load);
			((ISupportInitialize)updnStartMonth).EndInit();
			((ISupportInitialize)updnStartYear).EndInit();
			((ISupportInitialize)updnDay).EndInit();
			((ISupportInitialize)updnMonth).EndInit();
			((ISupportInitialize)updnYear).EndInit();
			((ISupportInitialize)updnInterval).EndInit();
			((ISupportInitialize)updnDuration).EndInit();
			((ISupportInitialize)updnHour).EndInit();
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((ISupportInitialize)updnStartDay).EndInit();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
