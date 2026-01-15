using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Ephemerides
{
	public class ObjectAltitude : Form
	{
		private const double Radian = 180.0 / Math.PI;

		private IContainer components;

		private GroupBox groupBox1;

		private NumericUpDown updnTimeZone;

		private NumericUpDown updnLatitude;

		private NumericUpDown updnLongitude;

		private Label label5;

		private Label label4;

		private Label label6;

		public NumericUpDown updnHour;

		public NumericUpDown updnDay;

		public NumericUpDown updnMonth;

		public NumericUpDown updnYear;

		public NumericUpDown updnMinute;

		private GroupBox groupBox2;

		private Label label8;

		private Label label7;

		private Label label3;

		private Label label2;

		private Label label1;

		private GroupBox groupBox3;

		private Label label15;

		private Label label13;

		private Label label9;

		private Label label10;

		private Label label11;

		private Label label12;

		private TextBox txtRS1;

		private TextBox txtRM1;

		private TextBox txtRH1;

		private TextBox txtDS1;

		private TextBox txtDM1;

		private TextBox txtDD1;

		private Label label14;

		private Label label16;

		private Button cmdAlt;

		private Button cmdRiseSet;

		private Label lblRise;

		private Label lblSet;

		private MenuStrip menuStrip1;

		private RadioButton opt0;

		private RadioButton opt10;

		private RadioButton opt5;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private RadioButton opt20;

		private RadioButton opt15;

		private Label lblAltAz;

		public ObjectAltitude()
		{
			InitializeComponent();
		}

		private void ObjectAltitude_Load(object sender, EventArgs e)
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
			if (!decimal.TryParse(Settings.Default.Site_TimeZone_Hrs, out result))
			{
				result = default(decimal);
			}
			updnTimeZone.set_Value(result);
			updnYear.set_Value((decimal)DateTime.Now.ToUniversalTime().Year);
			updnMonth.set_Value((decimal)DateTime.Now.ToUniversalTime().Month);
			updnDay.set_Value((decimal)DateTime.Now.ToUniversalTime().Day);
			updnHour.set_Value((decimal)DateTime.Now.ToUniversalTime().Hour);
		}

		private void cmdAlt_Click(object sender, EventArgs e)
		{
			double RA = 0.0;
			double Dec = 0.0;
			double jD = Utilities.JD_from_Date((int)updnYear.get_Value(), (int)updnMonth.get_Value(), (double)updnDay.get_Value()) + (double)updnHour.get_Value() / 24.0 + (double)updnMinute.get_Value() / 1440.0;
			GetPosition(jD, out RA, out Dec);
			Utilities.AltAz(jD, RA, Dec, (double)updnLongitude.get_Value() / (180.0 / Math.PI), (double)updnLatitude.get_Value() / (180.0 / Math.PI), out var Altitude, out var Azimuth);
			Altitude *= 180.0 / Math.PI;
			double num = Utilities.Refraction_deg(Altitude, 1016.0, 5.0);
			string text = Utilities.DEGtoDMS(Altitude + num, 2, 0, MinutesOnly: false, IncludeLeadingZeros: false, IncludePlusSymbol: false, IncludeDMS: true, IncludeHMS: false);
			string text2 = Utilities.DEGtoDMS(Azimuth * (180.0 / Math.PI), 3, 0, MinutesOnly: true, IncludeLeadingZeros: false, IncludePlusSymbol: false, IncludeDMS: true, IncludeHMS: false);
			((Control)lblAltAz).set_Text("Altitude = " + text + "   (incl. refraction)\r\nAzimuth = " + text2);
		}

		private void cmdRiseSet_Click(object sender, EventArgs e)
		{
			double RA = 0.0;
			double Dec = 0.0;
			double TRise_hrs = 0.0;
			double TSet_hrs = 0.0;
			double jD = Utilities.JD_from_Date((int)updnYear.get_Value(), (int)updnMonth.get_Value(), (double)updnDay.get_Value());
			GetPosition(jD, out RA, out Dec);
			Utilities.ApparentStarPosition(ref RA, ref Dec, 0.0, 0.0, 2000, jD, use2006values_Not1976: false);
			double altitude_deg = -0.6;
			if (opt5.get_Checked())
			{
				altitude_deg = 4.8;
			}
			if (opt10.get_Checked())
			{
				altitude_deg = 9.9;
			}
			if (opt15.get_Checked())
			{
				altitude_deg = 15.0;
			}
			if (opt20.get_Checked())
			{
				altitude_deg = 20.0;
			}
			Utilities.RiseSet_LocalTime(altitude_deg, RA, Dec, jD, (double)updnLongitude.get_Value(), (double)updnLatitude.get_Value(), (float)updnTimeZone.get_Value(), out TRise_hrs, out TSet_hrs);
			if (TRise_hrs == TSet_hrs)
			{
				if (TRise_hrs == 12.0)
				{
					Label obj = lblRise;
					string text;
					((Control)lblSet).set_Text(text = "Object does not rise");
					((Control)obj).set_Text(text);
				}
				else
				{
					Label obj2 = lblRise;
					string text;
					((Control)lblSet).set_Text(text = "Object does not set");
					((Control)obj2).set_Text(text);
				}
			}
			else if (TRise_hrs < TSet_hrs)
			{
				((Control)lblRise).set_Text("Local Rise time = " + Utilities.DEGtoDMS(TRise_hrs, 2, 0, MinutesOnly: true, IncludeLeadingZeros: false, IncludePlusSymbol: false, IncludeDMS: false, IncludeHMS: true));
				((Control)lblSet).set_Text("Local Set time = " + Utilities.DEGtoDMS(TSet_hrs, 2, 0, MinutesOnly: true, IncludeLeadingZeros: false, IncludePlusSymbol: false, IncludeDMS: false, IncludeHMS: true));
			}
			else
			{
				((Control)lblRise).set_Text("Local Set time = " + Utilities.DEGtoDMS(TSet_hrs, 2, 0, MinutesOnly: true, IncludeLeadingZeros: false, IncludePlusSymbol: false, IncludeDMS: false, IncludeHMS: true));
				((Control)lblSet).set_Text("Local Rise time = " + Utilities.DEGtoDMS(TRise_hrs, 2, 0, MinutesOnly: true, IncludeLeadingZeros: false, IncludePlusSymbol: false, IncludeDMS: false, IncludeHMS: true));
			}
		}

		private void GetPosition(double JD, out double RA, out double Dec)
		{
			RA = 0.0;
			Dec = 0.0;
			if (int.TryParse(((Control)txtRH1).get_Text(), out var result) && int.TryParse(((Control)txtRM1).get_Text(), out var result2) && double.TryParse(((Control)txtRS1).get_Text(), out var result3) && int.TryParse(((Control)txtDD1).get_Text(), out var result4) && int.TryParse(((Control)txtDM1).get_Text(), out var result5) && double.TryParse(((Control)txtDS1).get_Text(), out var result6))
			{
				RA = ((double)result + (double)result2 / 60.0 + result3 / 3600.0) * 15.0 / (180.0 / Math.PI);
				Dec = ((double)Math.Abs(result4) + (double)result5 / 60.0 + result6 / 3600.0) / (180.0 / Math.PI);
				if (((Control)txtDD1).get_Text().Contains("-"))
				{
					Dec = 0.0 - Dec;
				}
			}
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Object altitude");
		}

		private void txtRH1_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtRH1).SelectAll();
		}

		private void txtRM1_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtRM1).SelectAll();
		}

		private void txtRS1_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtRS1).SelectAll();
		}

		private void txtDD1_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtDD1).SelectAll();
		}

		private void txtDM1_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtDM1).SelectAll();
		}

		private void txtDS1_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtDS1).SelectAll();
		}

		private void updnLongitude_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnLongitude).Select(0, 5);
		}

		private void updnLatitude_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnLatitude).Select(0, 5);
		}

		private void updnTimeZone_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnTimeZone).Select(0, 5);
		}

		private void updnYear_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnYear).Select(0, 5);
		}

		private void updnMonth_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnMonth).Select(0, 5);
		}

		private void updnDay_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnDay).Select(0, 5);
		}

		private void updnHour_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnHour).Select(0, 5);
		}

		private void updnMinute_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnMinute).Select(0, 5);
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
			//IL_1b52: Unknown result type (might be due to invalid IL or missing references)
			//IL_1b5c: Expected O, but got Unknown
			groupBox1 = new GroupBox();
			updnTimeZone = new NumericUpDown();
			updnLatitude = new NumericUpDown();
			updnLongitude = new NumericUpDown();
			label5 = new Label();
			label4 = new Label();
			label6 = new Label();
			updnHour = new NumericUpDown();
			updnDay = new NumericUpDown();
			updnMonth = new NumericUpDown();
			updnYear = new NumericUpDown();
			updnMinute = new NumericUpDown();
			groupBox2 = new GroupBox();
			label8 = new Label();
			label7 = new Label();
			label3 = new Label();
			label2 = new Label();
			label1 = new Label();
			groupBox3 = new GroupBox();
			label15 = new Label();
			label13 = new Label();
			label9 = new Label();
			label10 = new Label();
			label11 = new Label();
			label12 = new Label();
			txtRS1 = new TextBox();
			txtRM1 = new TextBox();
			txtRH1 = new TextBox();
			txtDS1 = new TextBox();
			txtDM1 = new TextBox();
			txtDD1 = new TextBox();
			label14 = new Label();
			label16 = new Label();
			cmdAlt = new Button();
			cmdRiseSet = new Button();
			lblRise = new Label();
			lblSet = new Label();
			menuStrip1 = new MenuStrip();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			opt0 = new RadioButton();
			opt10 = new RadioButton();
			opt5 = new RadioButton();
			opt20 = new RadioButton();
			opt15 = new RadioButton();
			lblAltAz = new Label();
			((Control)groupBox1).SuspendLayout();
			((ISupportInitialize)updnTimeZone).BeginInit();
			((ISupportInitialize)updnLatitude).BeginInit();
			((ISupportInitialize)updnLongitude).BeginInit();
			((ISupportInitialize)updnHour).BeginInit();
			((ISupportInitialize)updnDay).BeginInit();
			((ISupportInitialize)updnMonth).BeginInit();
			((ISupportInitialize)updnYear).BeginInit();
			((ISupportInitialize)updnMinute).BeginInit();
			((Control)groupBox2).SuspendLayout();
			((Control)groupBox3).SuspendLayout();
			((Control)menuStrip1).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)groupBox1).get_Controls().Add((Control)(object)updnTimeZone);
			((Control)groupBox1).get_Controls().Add((Control)(object)updnLatitude);
			((Control)groupBox1).get_Controls().Add((Control)(object)updnLongitude);
			((Control)groupBox1).get_Controls().Add((Control)(object)label5);
			((Control)groupBox1).get_Controls().Add((Control)(object)label4);
			((Control)groupBox1).get_Controls().Add((Control)(object)label6);
			((Control)groupBox1).set_Location(new Point(40, 124));
			((Control)groupBox1).set_Name("groupBox1");
			((Control)groupBox1).set_Size(new Size(225, 52));
			((Control)groupBox1).set_TabIndex(3);
			groupBox1.set_TabStop(false);
			((Control)groupBox1).set_Text("Observer location");
			updnTimeZone.set_DecimalPlaces(1);
			updnTimeZone.set_Increment(new decimal(new int[4] { 5, 0, 0, 65536 }));
			((Control)updnTimeZone).set_Location(new Point(144, 29));
			updnTimeZone.set_Maximum(new decimal(new int[4] { 14, 0, 0, 0 }));
			updnTimeZone.set_Minimum(new decimal(new int[4] { 12, 0, 0, -2147483648 }));
			((Control)updnTimeZone).set_Name("updnTimeZone");
			((Control)updnTimeZone).set_Size(new Size(48, 20));
			((Control)updnTimeZone).set_TabIndex(9);
			((Control)updnTimeZone).add_Enter((EventHandler)updnTimeZone_Enter);
			updnLatitude.set_DecimalPlaces(1);
			((Control)updnLatitude).set_Location(new Point(79, 29));
			updnLatitude.set_Maximum(new decimal(new int[4] { 90, 0, 0, 0 }));
			updnLatitude.set_Minimum(new decimal(new int[4] { 90, 0, 0, -2147483648 }));
			((Control)updnLatitude).set_Name("updnLatitude");
			((Control)updnLatitude).set_Size(new Size(51, 20));
			((Control)updnLatitude).set_TabIndex(8);
			((Control)updnLatitude).add_Enter((EventHandler)updnLatitude_Enter);
			updnLongitude.set_DecimalPlaces(1);
			((Control)updnLongitude).set_Location(new Point(9, 30));
			updnLongitude.set_Maximum(new decimal(new int[4] { 180, 0, 0, 0 }));
			updnLongitude.set_Minimum(new decimal(new int[4] { 180, 0, 0, -2147483648 }));
			((Control)updnLongitude).set_Name("updnLongitude");
			((Control)updnLongitude).set_Size(new Size(54, 20));
			((Control)updnLongitude).set_TabIndex(7);
			((Control)updnLongitude).add_Enter((EventHandler)updnLongitude_Enter);
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Location(new Point(76, 14));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(45, 13));
			((Control)label5).set_TabIndex(2);
			((Control)label5).set_Text("Latitude");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Location(new Point(6, 14));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(54, 13));
			((Control)label4).set_TabIndex(0);
			((Control)label4).set_Text("Longitude");
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Location(new Point(137, 14));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(81, 13));
			((Control)label6).set_TabIndex(4);
			((Control)label6).set_Text("Time Zone [hrs]");
			label6.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)updnHour).set_Anchor((AnchorStyles)1);
			((Control)updnHour).set_Location(new Point(162, 30));
			updnHour.set_Maximum(new decimal(new int[4] { 23, 0, 0, 0 }));
			((Control)updnHour).set_Name("updnHour");
			((Control)updnHour).set_Size(new Size(36, 20));
			((Control)updnHour).set_TabIndex(13);
			((Control)updnHour).add_Enter((EventHandler)updnHour_Enter);
			((Control)updnDay).set_Anchor((AnchorStyles)1);
			((Control)updnDay).set_Location(new Point(118, 30));
			updnDay.set_Maximum(new decimal(new int[4] { 31, 0, 0, 0 }));
			updnDay.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnDay).set_Name("updnDay");
			((Control)updnDay).set_Size(new Size(36, 20));
			((Control)updnDay).set_TabIndex(12);
			updnDay.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnDay).add_Enter((EventHandler)updnDay_Enter);
			((Control)updnMonth).set_Anchor((AnchorStyles)1);
			((Control)updnMonth).set_Location(new Point(73, 30));
			updnMonth.set_Maximum(new decimal(new int[4] { 12, 0, 0, 0 }));
			updnMonth.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnMonth).set_Name("updnMonth");
			((Control)updnMonth).set_Size(new Size(37, 20));
			((Control)updnMonth).set_TabIndex(11);
			updnMonth.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnMonth).add_Enter((EventHandler)updnMonth_Enter);
			((Control)updnYear).set_Anchor((AnchorStyles)1);
			((Control)updnYear).set_Location(new Point(10, 30));
			updnYear.set_Maximum(new decimal(new int[4] { 17000, 0, 0, 0 }));
			updnYear.set_Minimum(new decimal(new int[4] { 13000, 0, 0, -2147483648 }));
			((Control)updnYear).set_Name("updnYear");
			((Control)updnYear).set_Size(new Size(55, 20));
			((Control)updnYear).set_TabIndex(10);
			updnYear.set_Value(new decimal(new int[4] { 2006, 0, 0, 0 }));
			((Control)updnYear).add_Enter((EventHandler)updnYear_Enter);
			((Control)updnMinute).set_Anchor((AnchorStyles)1);
			updnMinute.set_DecimalPlaces(1);
			((Control)updnMinute).set_Location(new Point(206, 30));
			updnMinute.set_Maximum(new decimal(new int[4] { 599, 0, 0, 65536 }));
			((Control)updnMinute).set_Name("updnMinute");
			((Control)updnMinute).set_Size(new Size(53, 20));
			((Control)updnMinute).set_TabIndex(14);
			((Control)updnMinute).add_Enter((EventHandler)updnMinute_Enter);
			((Control)groupBox2).get_Controls().Add((Control)(object)label8);
			((Control)groupBox2).get_Controls().Add((Control)(object)label7);
			((Control)groupBox2).get_Controls().Add((Control)(object)label3);
			((Control)groupBox2).get_Controls().Add((Control)(object)label2);
			((Control)groupBox2).get_Controls().Add((Control)(object)label1);
			((Control)groupBox2).get_Controls().Add((Control)(object)updnMinute);
			((Control)groupBox2).get_Controls().Add((Control)(object)updnHour);
			((Control)groupBox2).get_Controls().Add((Control)(object)updnDay);
			((Control)groupBox2).get_Controls().Add((Control)(object)updnMonth);
			((Control)groupBox2).get_Controls().Add((Control)(object)updnYear);
			((Control)groupBox2).set_Location(new Point(12, 182));
			((Control)groupBox2).set_Name("groupBox2");
			((Control)groupBox2).set_Size(new Size(281, 55));
			((Control)groupBox2).set_TabIndex(10);
			groupBox2.set_TabStop(false);
			((Control)groupBox2).set_Text("Date and time  -  UT");
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Location(new Point(70, 16));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(37, 13));
			((Control)label8).set_TabIndex(14);
			((Control)label8).set_Text("Month");
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Location(new Point(204, 16));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(39, 13));
			((Control)label7).set_TabIndex(13);
			((Control)label7).set_Text("Minute");
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Location(new Point(160, 16));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(30, 13));
			((Control)label3).set_TabIndex(12);
			((Control)label3).set_Text("Hour");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(118, 16));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(26, 13));
			((Control)label2).set_TabIndex(11);
			((Control)label2).set_Text("Day");
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(18, 15));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(29, 13));
			((Control)label1).set_TabIndex(10);
			((Control)label1).set_Text("Year");
			((Control)groupBox3).get_Controls().Add((Control)(object)label15);
			((Control)groupBox3).get_Controls().Add((Control)(object)label13);
			((Control)groupBox3).get_Controls().Add((Control)(object)label9);
			((Control)groupBox3).get_Controls().Add((Control)(object)label10);
			((Control)groupBox3).get_Controls().Add((Control)(object)label11);
			((Control)groupBox3).get_Controls().Add((Control)(object)label12);
			((Control)groupBox3).get_Controls().Add((Control)(object)txtRS1);
			((Control)groupBox3).get_Controls().Add((Control)(object)txtRM1);
			((Control)groupBox3).get_Controls().Add((Control)(object)txtRH1);
			((Control)groupBox3).get_Controls().Add((Control)(object)txtDS1);
			((Control)groupBox3).get_Controls().Add((Control)(object)txtDM1);
			((Control)groupBox3).get_Controls().Add((Control)(object)txtDD1);
			((Control)groupBox3).get_Controls().Add((Control)(object)label14);
			((Control)groupBox3).get_Controls().Add((Control)(object)label16);
			((Control)groupBox3).set_Location(new Point(46, 29));
			((Control)groupBox3).set_Name("groupBox3");
			((Control)groupBox3).set_Size(new Size(212, 89));
			((Control)groupBox3).set_TabIndex(11);
			groupBox3.set_TabStop(false);
			((Control)groupBox3).set_Text("Object's location (J2000)");
			((Control)label15).set_AutoSize(true);
			((Control)label15).set_Location(new Point(30, 65));
			((Control)label15).set_Name("label15");
			((Control)label15).set_Size(new Size(60, 13));
			((Control)label15).set_TabIndex(48);
			((Control)label15).set_Text("Declination");
			((Control)label13).set_AutoSize(true);
			((Control)label13).set_Location(new Point(17, 32));
			((Control)label13).set_Name("label13");
			((Control)label13).set_Size(new Size(79, 13));
			((Control)label13).set_TabIndex(47);
			((Control)label13).set_Text("Right Acension");
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Location(new Point(108, 49));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(13, 13));
			((Control)label9).set_TabIndex(44);
			((Control)label9).set_Text("o");
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Location(new Point(170, 13));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(12, 13));
			((Control)label10).set_TabIndex(43);
			((Control)label10).set_Text("s");
			((Control)label11).set_AutoSize(true);
			((Control)label11).set_Location(new Point(136, 13));
			((Control)label11).set_Name("label11");
			((Control)label11).set_Size(new Size(15, 13));
			((Control)label11).set_TabIndex(42);
			((Control)label11).set_Text("m");
			((Control)label12).set_AutoSize(true);
			((Control)label12).set_Location(new Point(108, 13));
			((Control)label12).set_Name("label12");
			((Control)label12).set_Size(new Size(13, 13));
			((Control)label12).set_TabIndex(41);
			((Control)label12).set_Text("h");
			((Control)txtRS1).set_Location(new Point(163, 28));
			((Control)txtRS1).set_Name("txtRS1");
			((Control)txtRS1).set_Size(new Size(39, 20));
			((Control)txtRS1).set_TabIndex(3);
			((Control)txtRS1).set_Text("0");
			((Control)txtRS1).add_Enter((EventHandler)txtRS1_Enter);
			((Control)txtRM1).set_Location(new Point(131, 28));
			((Control)txtRM1).set_Name("txtRM1");
			((Control)txtRM1).set_Size(new Size(23, 20));
			((Control)txtRM1).set_TabIndex(2);
			((Control)txtRM1).set_Text("0");
			((Control)txtRM1).add_Enter((EventHandler)txtRM1_Enter);
			((Control)txtRH1).set_Location(new Point(101, 28));
			((Control)txtRH1).set_Name("txtRH1");
			((Control)txtRH1).set_Size(new Size(23, 20));
			((Control)txtRH1).set_TabIndex(1);
			((Control)txtRH1).set_Text("0");
			((Control)txtRH1).add_Enter((EventHandler)txtRH1_Enter);
			((Control)txtDS1).set_Location(new Point(163, 62));
			((Control)txtDS1).set_Name("txtDS1");
			((Control)txtDS1).set_Size(new Size(32, 20));
			((Control)txtDS1).set_TabIndex(6);
			((Control)txtDS1).set_Text("0");
			((Control)txtDS1).add_Enter((EventHandler)txtDS1_Enter);
			((Control)txtDM1).set_Location(new Point(131, 62));
			((Control)txtDM1).set_Name("txtDM1");
			((Control)txtDM1).set_Size(new Size(23, 20));
			((Control)txtDM1).set_TabIndex(5);
			((Control)txtDM1).set_Text("0");
			((Control)txtDM1).add_Enter((EventHandler)txtDM1_Enter);
			((Control)txtDD1).set_Location(new Point(96, 62));
			((Control)txtDD1).set_Name("txtDD1");
			((Control)txtDD1).set_Size(new Size(28, 20));
			((Control)txtDD1).set_TabIndex(4);
			((Control)txtDD1).set_Text("0");
			((Control)txtDD1).add_Enter((EventHandler)txtDD1_Enter);
			((Control)label14).set_AutoSize(true);
			((Control)label14).set_Location(new Point(142, 54));
			((Control)label14).set_Name("label14");
			((Control)label14).set_Size(new Size(9, 13));
			((Control)label14).set_TabIndex(45);
			((Control)label14).set_Text("'");
			((Control)label16).set_AutoSize(true);
			((Control)label16).set_Location(new Point(172, 54));
			((Control)label16).set_Name("label16");
			((Control)label16).set_Size(new Size(12, 13));
			((Control)label16).set_TabIndex(46);
			((Control)label16).set_Text("\"");
			((Control)cmdAlt).set_Location(new Point(16, 252));
			((Control)cmdAlt).set_Name("cmdAlt");
			((Control)cmdAlt).set_Size(new Size(73, 38));
			((Control)cmdAlt).set_TabIndex(15);
			((Control)cmdAlt).set_Text("&Altitude");
			((ButtonBase)cmdAlt).set_UseVisualStyleBackColor(true);
			((Control)cmdAlt).add_Click((EventHandler)cmdAlt_Click);
			((Control)cmdRiseSet).set_Location(new Point(17, 325));
			((Control)cmdRiseSet).set_Name("cmdRiseSet");
			((Control)cmdRiseSet).set_Size(new Size(73, 38));
			((Control)cmdRiseSet).set_TabIndex(16);
			((Control)cmdRiseSet).set_Text("&Rise/Set times");
			((ButtonBase)cmdRiseSet).set_UseVisualStyleBackColor(true);
			((Control)cmdRiseSet).add_Click((EventHandler)cmdRiseSet_Click);
			((Control)lblRise).set_AutoSize(true);
			((Control)lblRise).set_Location(new Point(155, 331));
			((Control)lblRise).set_Name("lblRise");
			((Control)lblRise).set_Size(new Size(91, 13));
			((Control)lblRise).set_TabIndex(15);
			((Control)lblRise).set_Text("Local Rise time = ");
			((Control)lblSet).set_AutoSize(true);
			((Control)lblSet).set_Location(new Point(155, 348));
			((Control)lblSet).set_Name("lblSet");
			((Control)lblSet).set_Size(new Size(86, 13));
			((Control)lblSet).set_TabIndex(16);
			((Control)lblSet).set_Text("Local Set time = ");
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(305, 24));
			((Control)menuStrip1).set_TabIndex(17);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(69, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("&Help   ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(54, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("E&xit");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((Control)opt0).set_AutoSize(true);
			opt0.set_Checked(true);
			((Control)opt0).set_Location(new Point(96, 302));
			((Control)opt0).set_Name("opt0");
			((Control)opt0).set_Size(new Size(52, 17));
			((Control)opt0).set_TabIndex(17);
			opt0.set_TabStop(true);
			((Control)opt0).set_Text("0 deg");
			((ButtonBase)opt0).set_UseVisualStyleBackColor(true);
			((Control)opt10).set_AutoSize(true);
			((Control)opt10).set_Location(new Point(96, 336));
			((Control)opt10).set_Name("opt10");
			((Control)opt10).set_Size(new Size(58, 17));
			((Control)opt10).set_TabIndex(19);
			((Control)opt10).set_Text("10 deg");
			((ButtonBase)opt10).set_UseVisualStyleBackColor(true);
			((Control)opt5).set_AutoSize(true);
			((Control)opt5).set_Location(new Point(96, 319));
			((Control)opt5).set_Name("opt5");
			((Control)opt5).set_Size(new Size(52, 17));
			((Control)opt5).set_TabIndex(18);
			((Control)opt5).set_Text("5 deg");
			((ButtonBase)opt5).set_UseVisualStyleBackColor(true);
			((Control)opt20).set_AutoSize(true);
			((Control)opt20).set_Location(new Point(96, 370));
			((Control)opt20).set_Name("opt20");
			((Control)opt20).set_Size(new Size(58, 17));
			((Control)opt20).set_TabIndex(21);
			((Control)opt20).set_Text("20 deg");
			((ButtonBase)opt20).set_UseVisualStyleBackColor(true);
			((Control)opt15).set_AutoSize(true);
			((Control)opt15).set_Location(new Point(96, 353));
			((Control)opt15).set_Name("opt15");
			((Control)opt15).set_Size(new Size(58, 17));
			((Control)opt15).set_TabIndex(20);
			((Control)opt15).set_Text("15 deg");
			((ButtonBase)opt15).set_UseVisualStyleBackColor(true);
			((Control)lblAltAz).set_AutoSize(true);
			((Control)lblAltAz).set_Location(new Point(107, 258));
			((Control)lblAltAz).set_Name("lblAltAz");
			((Control)lblAltAz).set_Size(new Size(66, 26));
			((Control)lblAltAz).set_TabIndex(23);
			((Control)lblAltAz).set_Text("Altitude =     \r\nAzimuth = ");
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(305, 398));
			((Control)this).get_Controls().Add((Control)(object)lblAltAz);
			((Control)this).get_Controls().Add((Control)(object)opt15);
			((Control)this).get_Controls().Add((Control)(object)opt20);
			((Control)this).get_Controls().Add((Control)(object)opt5);
			((Control)this).get_Controls().Add((Control)(object)opt10);
			((Control)this).get_Controls().Add((Control)(object)opt0);
			((Control)this).get_Controls().Add((Control)(object)lblSet);
			((Control)this).get_Controls().Add((Control)(object)lblRise);
			((Control)this).get_Controls().Add((Control)(object)cmdRiseSet);
			((Control)this).get_Controls().Add((Control)(object)cmdAlt);
			((Control)this).get_Controls().Add((Control)(object)groupBox3);
			((Control)this).get_Controls().Add((Control)(object)groupBox2);
			((Control)this).get_Controls().Add((Control)(object)groupBox1);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationEphemAltitude", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_FormBorderStyle((FormBorderStyle)2);
			((Form)this).set_Location(Settings.Default.LocationEphemAltitude);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Form)this).set_MaximizeBox(false);
			((Form)this).set_MinimizeBox(false);
			((Control)this).set_Name("ObjectAltitude");
			((Control)this).set_Text("Object Altitude");
			((Form)this).add_Load((EventHandler)ObjectAltitude_Load);
			((Control)groupBox1).ResumeLayout(false);
			((Control)groupBox1).PerformLayout();
			((ISupportInitialize)updnTimeZone).EndInit();
			((ISupportInitialize)updnLatitude).EndInit();
			((ISupportInitialize)updnLongitude).EndInit();
			((ISupportInitialize)updnHour).EndInit();
			((ISupportInitialize)updnDay).EndInit();
			((ISupportInitialize)updnMonth).EndInit();
			((ISupportInitialize)updnYear).EndInit();
			((ISupportInitialize)updnMinute).EndInit();
			((Control)groupBox2).ResumeLayout(false);
			((Control)groupBox2).PerformLayout();
			((Control)groupBox3).ResumeLayout(false);
			((Control)groupBox3).PerformLayout();
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
