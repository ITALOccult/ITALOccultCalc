using System;
using System.ComponentModel;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Ephemerides
{
	public class OrbitSizes : Form
	{
		private static bool GettingRadius = true;

		private IContainer components;

		private NumericUpDown updnMainDia;

		private Label label1;

		private Label label2;

		private NumericUpDown updnSatDia;

		private Label label3;

		private NumericUpDown updnDensity;

		private Label label4;

		private Label label5;

		private NumericUpDown updnPeriodHrs;

		private NumericUpDown updnRadius;

		private Label label6;

		private Label label7;

		private Label label8;

		private TextBox txtUser;

		private TextBox txt4;

		private TextBox txt3;

		private TextBox txt6;

		private TextBox txt271;

		private TextBox txt2;

		private TextBox txt138;

		private TextBox txt1;

		private Label label9;

		private Label label10;

		private Label label11;

		private Label label12;

		private Label lblUserDensity;

		private Label label14;

		private Label label15;

		private Label label16;

		private Label label17;

		private TextBox txt532;

		private Label lblSoln;

		private Label label13;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem copyResultsToolStripMenuItem;

		private ToolStripMenuItem helpToolStripMenuItem;

		private PictureBox pictureBox1;

		private PictureBox pictureBox2;

		private PictureBox pictureBox3;

		private Label label18;

		private Label label19;

		private Panel panel1;

		public OrbitSizes()
		{
			InitializeComponent();
		}

		private void updnMainDia_ValueChanged(object sender, EventArgs e)
		{
			if (GettingRadius)
			{
				GetOrbitRadius();
			}
			else
			{
				GetOrbitPeriod();
			}
		}

		private void updnSatDia_ValueChanged(object sender, EventArgs e)
		{
			if (GettingRadius)
			{
				GetOrbitRadius();
			}
			else
			{
				GetOrbitPeriod();
			}
		}

		private void updnDensity_ValueChanged(object sender, EventArgs e)
		{
			if (GettingRadius)
			{
				GetOrbitRadius();
			}
			else
			{
				GetOrbitPeriod();
			}
		}

		private void updnRadius_ValueChanged(object sender, EventArgs e)
		{
			GettingRadius = true;
			GetOrbitPeriod();
		}

		private void updnRadius_MouseClick(object sender, MouseEventArgs e)
		{
			GettingRadius = true;
			GetOrbitPeriod();
		}

		private void updnPeriodHrs_ValueChanged(object sender, EventArgs e)
		{
			GettingRadius = false;
			GetOrbitRadius();
		}

		private void updnPeriodHrs_MouseClick(object sender, MouseEventArgs e)
		{
			GettingRadius = false;
			GetOrbitRadius();
		}

		private void GetOrbitRadius()
		{
			GettingRadius = true;
			double num = (double)updnDensity.get_Value();
			double period_Days = (double)updnPeriodHrs.get_Value() / 24.0;
			((Control)lblUserDensity).set_Text(string.Format("{0,1:f2}", num));
			((Control)lblSoln).set_Text("Radius\r\nkm");
			((Control)txtUser).set_Text(string.Format("{0,1:f2}", Utilities.OrbitRadius_from_Period((double)updnMainDia.get_Value(), (double)updnSatDia.get_Value(), num, period_Days)));
			((Control)txt1).set_Text(string.Format("{0,1:f2}", Utilities.OrbitRadius_from_Period((double)updnMainDia.get_Value(), (double)updnSatDia.get_Value(), 1.0, period_Days)));
			((Control)txt138).set_Text(string.Format("{0,1:f2}", Utilities.OrbitRadius_from_Period((double)updnMainDia.get_Value(), (double)updnSatDia.get_Value(), 1.38, period_Days)));
			((Control)txt2).set_Text(string.Format("{0,1:f2}", Utilities.OrbitRadius_from_Period((double)updnMainDia.get_Value(), (double)updnSatDia.get_Value(), 2.0, period_Days)));
			((Control)txt271).set_Text(string.Format("{0,1:f2}", Utilities.OrbitRadius_from_Period((double)updnMainDia.get_Value(), (double)updnSatDia.get_Value(), 2.71, period_Days)));
			((Control)txt3).set_Text(string.Format("{0,1:f2}", Utilities.OrbitRadius_from_Period((double)updnMainDia.get_Value(), (double)updnSatDia.get_Value(), 3.0, period_Days)));
			((Control)txt4).set_Text(string.Format("{0,1:f2}", Utilities.OrbitRadius_from_Period((double)updnMainDia.get_Value(), (double)updnSatDia.get_Value(), 4.0, period_Days)));
			((Control)txt532).set_Text(string.Format("{0,1:f2}", Utilities.OrbitRadius_from_Period((double)updnMainDia.get_Value(), (double)updnSatDia.get_Value(), 5.32, period_Days)));
			((Control)txt6).set_Text(string.Format("{0,1:f2}", Utilities.OrbitRadius_from_Period((double)updnMainDia.get_Value(), (double)updnSatDia.get_Value(), 6.0, period_Days)));
			TextBox obj = txtUser;
			TextBox obj2 = txt1;
			TextBox obj3 = txt138;
			TextBox obj4 = txt2;
			TextBox obj5 = txt271;
			TextBox obj6 = txt3;
			TextBox obj7 = txt4;
			TextBox obj8 = txt532;
			Color moccasin;
			((Control)txt6).set_BackColor(moccasin = Color.Moccasin);
			Color color;
			((Control)obj8).set_BackColor(color = moccasin);
			Color color2;
			((Control)obj7).set_BackColor(color2 = color);
			Color color3;
			((Control)obj6).set_BackColor(color3 = color2);
			Color color4;
			((Control)obj5).set_BackColor(color4 = color3);
			Color color5;
			((Control)obj4).set_BackColor(color5 = color4);
			Color color6;
			((Control)obj3).set_BackColor(color6 = color5);
			Color backColor;
			((Control)obj2).set_BackColor(backColor = color6);
			((Control)obj).set_BackColor(backColor);
		}

		private void GetOrbitPeriod()
		{
			GettingRadius = false;
			double density_g_cc = (double)updnDensity.get_Value();
			double radius_km = (double)updnRadius.get_Value();
			((Control)lblUserDensity).set_Text(string.Format("{0,1:f2}", updnDensity.get_Value()));
			((Control)lblSoln).set_Text("Period\r\nhrs");
			((Control)txtUser).set_Text(string.Format("{0,1:f2}", Utilities.OrbitPeriod_from_Radius((double)updnMainDia.get_Value(), (double)updnSatDia.get_Value(), density_g_cc, radius_km) * 24.0));
			((Control)txt1).set_Text(string.Format("{0,1:f2}", Utilities.OrbitPeriod_from_Radius((double)updnMainDia.get_Value(), (double)updnSatDia.get_Value(), 1.0, radius_km) * 24.0));
			((Control)txt138).set_Text(string.Format("{0,1:f2}", Utilities.OrbitPeriod_from_Radius((double)updnMainDia.get_Value(), (double)updnSatDia.get_Value(), 1.38, radius_km) * 24.0));
			((Control)txt2).set_Text(string.Format("{0,1:f2}", Utilities.OrbitPeriod_from_Radius((double)updnMainDia.get_Value(), (double)updnSatDia.get_Value(), 2.0, radius_km) * 24.0));
			((Control)txt271).set_Text(string.Format("{0,1:f2}", Utilities.OrbitPeriod_from_Radius((double)updnMainDia.get_Value(), (double)updnSatDia.get_Value(), 2.71, radius_km) * 24.0));
			((Control)txt3).set_Text(string.Format("{0,1:f2}", Utilities.OrbitPeriod_from_Radius((double)updnMainDia.get_Value(), (double)updnSatDia.get_Value(), 3.0, radius_km) * 24.0));
			((Control)txt4).set_Text(string.Format("{0,1:f2}", Utilities.OrbitPeriod_from_Radius((double)updnMainDia.get_Value(), (double)updnSatDia.get_Value(), 4.0, radius_km) * 24.0));
			((Control)txt532).set_Text(string.Format("{0,1:f2}", Utilities.OrbitPeriod_from_Radius((double)updnMainDia.get_Value(), (double)updnSatDia.get_Value(), 5.32, radius_km) * 24.0));
			((Control)txt6).set_Text(string.Format("{0,1:f2}", Utilities.OrbitPeriod_from_Radius((double)updnMainDia.get_Value(), (double)updnSatDia.get_Value(), 6.0, radius_km) * 24.0));
			TextBox obj = txtUser;
			TextBox obj2 = txt1;
			TextBox obj3 = txt138;
			TextBox obj4 = txt2;
			TextBox obj5 = txt271;
			TextBox obj6 = txt3;
			TextBox obj7 = txt4;
			TextBox obj8 = txt532;
			Color paleTurquoise;
			((Control)txt6).set_BackColor(paleTurquoise = Color.PaleTurquoise);
			Color color;
			((Control)obj8).set_BackColor(color = paleTurquoise);
			Color color2;
			((Control)obj7).set_BackColor(color2 = color);
			Color color3;
			((Control)obj6).set_BackColor(color3 = color2);
			Color color4;
			((Control)obj5).set_BackColor(color4 = color3);
			Color color5;
			((Control)obj4).set_BackColor(color5 = color4);
			Color color6;
			((Control)obj3).set_BackColor(color6 = color5);
			Color backColor;
			((Control)obj2).set_BackColor(backColor = color6);
			((Control)obj).set_BackColor(backColor);
		}

		private void OrbitSizes_FormClosed(object sender, FormClosedEventArgs e)
		{
			((Component)this).Dispose();
		}

		private void OrbitSizes_Load(object sender, EventArgs e)
		{
			GetOrbitRadius();
		}

		private void copyResultsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.AppendFormat("Diameter of main body : {0,1:f2}\r\n", updnMainDia.get_Value());
			stringBuilder.AppendFormat("Diameter of Satellite : {0,1:f2}\r\n", updnSatDia.get_Value());
			stringBuilder.AppendFormat("User density : {0,1:f2}\r\n", updnDensity.get_Value());
			stringBuilder.AppendFormat("Diameter of main body : {0,1:f2}\r\n\r\n", updnMainDia.get_Value());
			string text = "km";
			if (GettingRadius)
			{
				stringBuilder.AppendFormat("Assuming a period of : {0,1:f2} hours\r\nThe orbit radius (km) for different densities is:\r\n", updnPeriodHrs.get_Value());
			}
			else
			{
				text = "hrs";
				stringBuilder.AppendFormat("Assuming an orbit radius : {0,1:f2} km\r\nThe orbital period (hours) for different densities is:\r\n", updnRadius.get_Value());
			}
			stringBuilder.AppendFormat("{0}  {1} {2}\r\n", ((Control)lblUserDensity).get_Text(), ((Control)txtUser).get_Text(), text);
			stringBuilder.AppendFormat("1.00  {0} {1}\r\n", ((Control)txt1).get_Text(), text);
			stringBuilder.AppendFormat("1.38  {0} {1}   C-type asteroids\r\n", ((Control)txt138).get_Text(), text);
			stringBuilder.AppendFormat("2.00  {0} {1}\r\n", ((Control)txt2).get_Text(), text);
			stringBuilder.AppendFormat("2.71  {0} {1}   S-type asteroids\r\n", ((Control)txt271).get_Text(), text);
			stringBuilder.AppendFormat("3.00  {0} {1}\r\n", ((Control)txt3).get_Text(), text);
			stringBuilder.AppendFormat("4.00  {0} {1}\r\n", ((Control)txt4).get_Text(), text);
			stringBuilder.AppendFormat("5.32  {0} {1}   M-type asteroids\r\n", ((Control)txt532).get_Text(), text);
			stringBuilder.AppendFormat("6.00  {0} {1}\r\n", ((Control)txt6).get_Text(), text);
			Clipboard.SetText(stringBuilder.ToString());
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Binary orbit size");
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
			//IL_01be: Unknown result type (might be due to invalid IL or missing references)
			//IL_01c8: Expected O, but got Unknown
			//IL_01c9: Unknown result type (might be due to invalid IL or missing references)
			//IL_01d3: Expected O, but got Unknown
			//IL_01d4: Unknown result type (might be due to invalid IL or missing references)
			//IL_01de: Expected O, but got Unknown
			//IL_0898: Unknown result type (might be due to invalid IL or missing references)
			//IL_08a2: Expected O, but got Unknown
			//IL_0995: Unknown result type (might be due to invalid IL or missing references)
			//IL_099f: Expected O, but got Unknown
			//IL_1b20: Unknown result type (might be due to invalid IL or missing references)
			//IL_1b2a: Expected O, but got Unknown
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(OrbitSizes));
			updnMainDia = new NumericUpDown();
			label1 = new Label();
			label2 = new Label();
			updnSatDia = new NumericUpDown();
			label3 = new Label();
			updnDensity = new NumericUpDown();
			label4 = new Label();
			label5 = new Label();
			updnPeriodHrs = new NumericUpDown();
			updnRadius = new NumericUpDown();
			label6 = new Label();
			label7 = new Label();
			label8 = new Label();
			txtUser = new TextBox();
			txt4 = new TextBox();
			txt3 = new TextBox();
			txt6 = new TextBox();
			txt271 = new TextBox();
			txt2 = new TextBox();
			txt138 = new TextBox();
			txt1 = new TextBox();
			label9 = new Label();
			label10 = new Label();
			label11 = new Label();
			label12 = new Label();
			lblUserDensity = new Label();
			label14 = new Label();
			label15 = new Label();
			label16 = new Label();
			label17 = new Label();
			txt532 = new TextBox();
			lblSoln = new Label();
			label13 = new Label();
			menuStrip1 = new MenuStrip();
			copyResultsToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			pictureBox1 = new PictureBox();
			pictureBox2 = new PictureBox();
			pictureBox3 = new PictureBox();
			label18 = new Label();
			label19 = new Label();
			panel1 = new Panel();
			((ISupportInitialize)updnMainDia).BeginInit();
			((ISupportInitialize)updnSatDia).BeginInit();
			((ISupportInitialize)updnDensity).BeginInit();
			((ISupportInitialize)updnPeriodHrs).BeginInit();
			((ISupportInitialize)updnRadius).BeginInit();
			((Control)menuStrip1).SuspendLayout();
			((ISupportInitialize)pictureBox1).BeginInit();
			((ISupportInitialize)pictureBox2).BeginInit();
			((ISupportInitialize)pictureBox3).BeginInit();
			((Control)panel1).SuspendLayout();
			((Control)this).SuspendLayout();
			updnMainDia.set_DecimalPlaces(2);
			updnMainDia.set_Increment(new decimal(new int[4] { 1, 0, 0, 65536 }));
			((Control)updnMainDia).set_Location(new Point(18, 58));
			updnMainDia.set_Maximum(new decimal(new int[4] { 20000, 0, 0, 0 }));
			updnMainDia.set_Minimum(new decimal(new int[4] { 1, 0, 0, 131072 }));
			((Control)updnMainDia).set_Name("updnMainDia");
			((Control)updnMainDia).set_Size(new Size(56, 20));
			((Control)updnMainDia).set_TabIndex(0);
			((UpDownBase)updnMainDia).set_TextAlign((HorizontalAlignment)2);
			updnMainDia.set_Value(new decimal(new int[4] { 20, 0, 0, 0 }));
			updnMainDia.add_ValueChanged((EventHandler)updnMainDia_ValueChanged);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(10, 29));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(72, 26));
			((Control)label1).set_TabIndex(1);
			((Control)label1).set_Text("Diameter (km)\r\nMain body");
			label1.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(86, 29));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(72, 26));
			((Control)label2).set_TabIndex(3);
			((Control)label2).set_Text("Diameter (km)\r\nSatellite");
			label2.set_TextAlign(ContentAlignment.MiddleCenter);
			updnSatDia.set_DecimalPlaces(2);
			updnSatDia.set_Increment(new decimal(new int[4] { 1, 0, 0, 65536 }));
			((Control)updnSatDia).set_Location(new Point(94, 58));
			updnSatDia.set_Maximum(new decimal(new int[4] { 20000, 0, 0, 0 }));
			updnSatDia.set_Minimum(new decimal(new int[4] { 1, 0, 0, 131072 }));
			((Control)updnSatDia).set_Name("updnSatDia");
			((Control)updnSatDia).set_Size(new Size(56, 20));
			((Control)updnSatDia).set_TabIndex(2);
			((UpDownBase)updnSatDia).set_TextAlign((HorizontalAlignment)2);
			updnSatDia.set_Value(new decimal(new int[4] { 20, 0, 0, 0 }));
			updnSatDia.add_ValueChanged((EventHandler)updnSatDia_ValueChanged);
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_ForeColor(Color.Crimson);
			((Control)label3).set_Location(new Point(175, 29));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(46, 26));
			((Control)label3).set_TabIndex(5);
			((Control)label3).set_Text("Set user\r\nDensity");
			((Control)updnDensity).set_BackColor(Color.MistyRose);
			updnDensity.set_DecimalPlaces(2);
			updnDensity.set_Increment(new decimal(new int[4] { 5, 0, 0, 131072 }));
			((Control)updnDensity).set_Location(new Point(170, 58));
			updnDensity.set_Maximum(new decimal(new int[4] { 20, 0, 0, 0 }));
			updnDensity.set_Minimum(new decimal(new int[4] { 1, 0, 0, 65536 }));
			((Control)updnDensity).set_Name("updnDensity");
			((Control)updnDensity).set_Size(new Size(56, 20));
			((Control)updnDensity).set_TabIndex(4);
			((UpDownBase)updnDensity).set_TextAlign((HorizontalAlignment)2);
			updnDensity.set_Value(new decimal(new int[4] { 15, 0, 0, 65536 }));
			updnDensity.add_ValueChanged((EventHandler)updnDensity_ValueChanged);
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_ForeColor(Color.Navy);
			((Control)label4).set_Location(new Point(333, 29));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(64, 26));
			((Control)label4).set_TabIndex(7);
			((Control)label4).set_Text("Set the orbit\r\nradius (km)");
			label4.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_ForeColor(Color.SaddleBrown);
			((Control)label5).set_Location(new Point(243, 29));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(74, 26));
			((Control)label5).set_TabIndex(9);
			((Control)label5).set_Text("Set the Period\r\n(hours)");
			label5.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)updnPeriodHrs).set_BackColor(Color.Moccasin);
			updnPeriodHrs.set_DecimalPlaces(2);
			updnPeriodHrs.set_Increment(new decimal(new int[4] { 1, 0, 0, 65536 }));
			((Control)updnPeriodHrs).set_Location(new Point(252, 58));
			updnPeriodHrs.set_Maximum(new decimal(new int[4] { 800, 0, 0, 0 }));
			updnPeriodHrs.set_Minimum(new decimal(new int[4] { 1, 0, 0, 65536 }));
			((Control)updnPeriodHrs).set_Name("updnPeriodHrs");
			((Control)updnPeriodHrs).set_Size(new Size(56, 20));
			((Control)updnPeriodHrs).set_TabIndex(8);
			((UpDownBase)updnPeriodHrs).set_TextAlign((HorizontalAlignment)2);
			updnPeriodHrs.set_Value(new decimal(new int[4] { 15, 0, 0, 0 }));
			updnPeriodHrs.add_ValueChanged((EventHandler)updnPeriodHrs_ValueChanged);
			((Control)updnPeriodHrs).add_MouseClick(new MouseEventHandler(updnPeriodHrs_MouseClick));
			((Control)updnRadius).set_BackColor(Color.PaleTurquoise);
			updnRadius.set_DecimalPlaces(1);
			updnRadius.set_Increment(new decimal(new int[4] { 5, 0, 0, 65536 }));
			((Control)updnRadius).set_Location(new Point(337, 58));
			updnRadius.set_Minimum(new decimal(new int[4] { 5, 0, 0, 65536 }));
			((Control)updnRadius).set_Name("updnRadius");
			((Control)updnRadius).set_Size(new Size(56, 20));
			((Control)updnRadius).set_TabIndex(10);
			((UpDownBase)updnRadius).set_TextAlign((HorizontalAlignment)2);
			updnRadius.set_Value(new decimal(new int[4] { 20, 0, 0, 0 }));
			updnRadius.add_ValueChanged((EventHandler)updnRadius_ValueChanged);
			((Control)updnRadius).add_MouseClick(new MouseEventHandler(updnRadius_MouseClick));
			((Control)label6).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label6).set_ForeColor(Color.Indigo);
			((Control)label6).set_Location(new Point(4, 194));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(269, 69));
			((Control)label6).set_TabIndex(11);
			((Control)label6).set_Text(componentResourceManager.GetString("label6.Text"));
			((Control)label7).set_ForeColor(Color.DarkRed);
			((Control)label7).set_Location(new Point(4, 288));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(269, 66));
			((Control)label7).set_TabIndex(12);
			((Control)label7).set_Text(componentResourceManager.GetString("label7.Text"));
			((Control)label8).set_ForeColor(Color.DarkGreen);
			((Control)label8).set_Location(new Point(4, 400));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(269, 70));
			((Control)label8).set_TabIndex(13);
			((Control)label8).set_Text(componentResourceManager.GetString("label8.Text"));
			((Control)txtUser).set_ForeColor(Color.DarkBlue);
			((Control)txtUser).set_Location(new Point(61, 5));
			((Control)txtUser).set_Name("txtUser");
			((Control)txtUser).set_Size(new Size(49, 20));
			((Control)txtUser).set_TabIndex(14);
			((Control)txt4).set_Location(new Point(62, 254));
			((Control)txt4).set_Name("txt4");
			((Control)txt4).set_Size(new Size(49, 20));
			((Control)txt4).set_TabIndex(15);
			((Control)txt3).set_Location(new Point(61, 229));
			((Control)txt3).set_Name("txt3");
			((Control)txt3).set_Size(new Size(49, 20));
			((Control)txt3).set_TabIndex(16);
			((Control)txt6).set_Location(new Point(61, 351));
			((Control)txt6).set_Name("txt6");
			((Control)txt6).set_Size(new Size(49, 20));
			((Control)txt6).set_TabIndex(17);
			((Control)txt271).set_Location(new Point(62, 188));
			((Control)txt271).set_Name("txt271");
			((Control)txt271).set_Size(new Size(49, 20));
			((Control)txt271).set_TabIndex(18);
			((Control)txt2).set_Location(new Point(62, 143));
			((Control)txt2).set_Name("txt2");
			((Control)txt2).set_Size(new Size(49, 20));
			((Control)txt2).set_TabIndex(19);
			((Control)txt138).set_Location(new Point(62, 95));
			((Control)txt138).set_Name("txt138");
			((Control)txt138).set_Size(new Size(49, 20));
			((Control)txt138).set_TabIndex(20);
			((Control)txt1).set_Location(new Point(62, 39));
			((Control)txt1).set_Name("txt1");
			((Control)txt1).set_Size(new Size(49, 20));
			((Control)txt1).set_TabIndex(21);
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label9).set_ForeColor(Color.DarkBlue);
			((Control)label9).set_Location(new Point(4, 97));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(40, 17));
			((Control)label9).set_TabIndex(22);
			((Control)label9).set_Text("1.38");
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label10).set_ForeColor(Color.DarkBlue);
			((Control)label10).set_Location(new Point(4, 190));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(40, 17));
			((Control)label10).set_TabIndex(23);
			((Control)label10).set_Text("2.71");
			((Control)label11).set_AutoSize(true);
			((Control)label11).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label11).set_ForeColor(Color.DarkBlue);
			((Control)label11).set_Location(new Point(4, 304));
			((Control)label11).set_Name("label11");
			((Control)label11).set_Size(new Size(40, 17));
			((Control)label11).set_TabIndex(24);
			((Control)label11).set_Text("5.32");
			((Control)label12).set_AutoSize(true);
			((Control)label12).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label12).set_ForeColor(Color.DarkGreen);
			((Control)label12).set_Location(new Point(4, 41));
			((Control)label12).set_Name("label12");
			((Control)label12).set_Size(new Size(40, 17));
			((Control)label12).set_TabIndex(25);
			((Control)label12).set_Text("1.00");
			((Control)lblUserDensity).set_AutoSize(true);
			((Control)lblUserDensity).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblUserDensity).set_ForeColor(Color.Crimson);
			((Control)lblUserDensity).set_Location(new Point(3, 7));
			((Control)lblUserDensity).set_Name("lblUserDensity");
			((Control)lblUserDensity).set_Size(new Size(42, 17));
			((Control)lblUserDensity).set_TabIndex(26);
			((Control)lblUserDensity).set_Text("User");
			((Control)label14).set_AutoSize(true);
			((Control)label14).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label14).set_ForeColor(Color.DarkGreen);
			((Control)label14).set_Location(new Point(4, 257));
			((Control)label14).set_Name("label14");
			((Control)label14).set_Size(new Size(40, 17));
			((Control)label14).set_TabIndex(27);
			((Control)label14).set_Text("4.00");
			((Control)label15).set_AutoSize(true);
			((Control)label15).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label15).set_ForeColor(Color.DarkGreen);
			((Control)label15).set_Location(new Point(4, 232));
			((Control)label15).set_Name("label15");
			((Control)label15).set_Size(new Size(40, 17));
			((Control)label15).set_TabIndex(28);
			((Control)label15).set_Text("3.00");
			((Control)label16).set_AutoSize(true);
			((Control)label16).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label16).set_ForeColor(Color.DarkGreen);
			((Control)label16).set_Location(new Point(4, 145));
			((Control)label16).set_Name("label16");
			((Control)label16).set_Size(new Size(40, 17));
			((Control)label16).set_TabIndex(29);
			((Control)label16).set_Text("2.00");
			((Control)label17).set_AutoSize(true);
			((Control)label17).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label17).set_ForeColor(Color.DarkGreen);
			((Control)label17).set_Location(new Point(4, 353));
			((Control)label17).set_Name("label17");
			((Control)label17).set_Size(new Size(40, 17));
			((Control)label17).set_TabIndex(30);
			((Control)label17).set_Text("6.00");
			((Control)txt532).set_Location(new Point(61, 302));
			((Control)txt532).set_Name("txt532");
			((Control)txt532).set_Size(new Size(49, 20));
			((Control)txt532).set_TabIndex(31);
			((Control)lblSoln).set_AutoSize(true);
			((Control)lblSoln).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblSoln).set_ForeColor(Color.DarkBlue);
			((Control)lblSoln).set_Location(new Point(348, 90));
			((Control)lblSoln).set_Name("lblSoln");
			((Control)lblSoln).set_Size(new Size(58, 34));
			((Control)lblSoln).set_TabIndex(32);
			((Control)lblSoln).set_Text("Radius\r\nkm");
			lblSoln.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)label13).set_AutoSize(true);
			((Control)label13).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label13).set_ForeColor(Color.DarkRed);
			((Control)label13).set_Location(new Point(310, 61));
			((Control)label13).set_Name("label13");
			((Control)label13).set_Size(new Size(25, 13));
			((Control)label13).set_TabIndex(33);
			((Control)label13).set_Text("OR");
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)copyResultsToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(415, 24));
			((Control)menuStrip1).set_TabIndex(34);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripItem)copyResultsToolStripMenuItem).set_Name("copyResultsToolStripMenuItem");
			((ToolStripItem)copyResultsToolStripMenuItem).set_Size(new Size(126, 20));
			((ToolStripItem)copyResultsToolStripMenuItem).set_Text("Copy results              ");
			((ToolStripItem)copyResultsToolStripMenuItem).add_Click((EventHandler)copyResultsToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(60, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("Help");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			pictureBox1.set_Image((Image)Resources.arrow_Next_16xLG_color);
			((Control)pictureBox1).set_Location(new Point(266, 222));
			((Control)pictureBox1).set_Name("pictureBox1");
			((Control)pictureBox1).set_Size(new Size(14, 17));
			pictureBox1.set_TabIndex(35);
			pictureBox1.set_TabStop(false);
			pictureBox2.set_Image((Image)Resources.arrow_Next_16xLG_color);
			((Control)pictureBox2).set_Location(new Point(261, 315));
			((Control)pictureBox2).set_Name("pictureBox2");
			((Control)pictureBox2).set_Size(new Size(14, 17));
			pictureBox2.set_TabIndex(36);
			pictureBox2.set_TabStop(false);
			pictureBox3.set_Image((Image)Resources.arrow_Next_16xLG_color);
			((Control)pictureBox3).set_Location(new Point(261, 429));
			((Control)pictureBox3).set_Name("pictureBox3");
			((Control)pictureBox3).set_Size(new Size(14, 17));
			pictureBox3.set_TabIndex(37);
			pictureBox3.set_TabStop(false);
			((Control)label18).set_AutoSize(true);
			((Control)label18).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label18).set_ForeColor(Color.DarkSlateGray);
			((Control)label18).set_Location(new Point(4, 93));
			((Control)label18).set_Name("label18");
			((Control)label18).set_Size(new Size(272, 65));
			((Control)label18).set_TabIndex(38);
			((Control)label18).set_Text(componentResourceManager.GetString("label18.Text"));
			label18.set_TextAlign(ContentAlignment.MiddleLeft);
			((Control)label19).set_AutoSize(true);
			((Control)label19).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label19).set_ForeColor(Color.DarkGreen);
			((Control)label19).set_Location(new Point(285, 104));
			((Control)label19).set_Name("label19");
			((Control)label19).set_Size(new Size(62, 17));
			((Control)label19).set_TabIndex(39);
			((Control)label19).set_Text("Density");
			((Control)panel1).set_BackColor(Color.Ivory);
			panel1.set_BorderStyle((BorderStyle)2);
			((Control)panel1).get_Controls().Add((Control)(object)txt532);
			((Control)panel1).get_Controls().Add((Control)(object)label17);
			((Control)panel1).get_Controls().Add((Control)(object)label16);
			((Control)panel1).get_Controls().Add((Control)(object)label15);
			((Control)panel1).get_Controls().Add((Control)(object)label14);
			((Control)panel1).get_Controls().Add((Control)(object)lblUserDensity);
			((Control)panel1).get_Controls().Add((Control)(object)label12);
			((Control)panel1).get_Controls().Add((Control)(object)label11);
			((Control)panel1).get_Controls().Add((Control)(object)label10);
			((Control)panel1).get_Controls().Add((Control)(object)label9);
			((Control)panel1).get_Controls().Add((Control)(object)txt1);
			((Control)panel1).get_Controls().Add((Control)(object)txt138);
			((Control)panel1).get_Controls().Add((Control)(object)txt2);
			((Control)panel1).get_Controls().Add((Control)(object)txt271);
			((Control)panel1).get_Controls().Add((Control)(object)txt6);
			((Control)panel1).get_Controls().Add((Control)(object)txt3);
			((Control)panel1).get_Controls().Add((Control)(object)txt4);
			((Control)panel1).get_Controls().Add((Control)(object)txtUser);
			((Control)panel1).set_Location(new Point(287, 123));
			((Control)panel1).set_Name("panel1");
			((Control)panel1).set_Size(new Size(122, 381));
			((Control)panel1).set_TabIndex(40);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(415, 510));
			((Control)this).get_Controls().Add((Control)(object)panel1);
			((Control)this).get_Controls().Add((Control)(object)label18);
			((Control)this).get_Controls().Add((Control)(object)pictureBox3);
			((Control)this).get_Controls().Add((Control)(object)pictureBox2);
			((Control)this).get_Controls().Add((Control)(object)pictureBox1);
			((Control)this).get_Controls().Add((Control)(object)lblSoln);
			((Control)this).get_Controls().Add((Control)(object)label8);
			((Control)this).get_Controls().Add((Control)(object)label7);
			((Control)this).get_Controls().Add((Control)(object)label6);
			((Control)this).get_Controls().Add((Control)(object)updnRadius);
			((Control)this).get_Controls().Add((Control)(object)label5);
			((Control)this).get_Controls().Add((Control)(object)updnPeriodHrs);
			((Control)this).get_Controls().Add((Control)(object)label4);
			((Control)this).get_Controls().Add((Control)(object)label3);
			((Control)this).get_Controls().Add((Control)(object)updnDensity);
			((Control)this).get_Controls().Add((Control)(object)label2);
			((Control)this).get_Controls().Add((Control)(object)updnSatDia);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)updnMainDia);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_Controls().Add((Control)(object)label19);
			((Control)this).get_Controls().Add((Control)(object)label13);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_Name("OrbitSizes");
			((Control)this).set_Text("Estimate binary orbit size or period");
			((Form)this).add_FormClosed(new FormClosedEventHandler(OrbitSizes_FormClosed));
			((Form)this).add_Load((EventHandler)OrbitSizes_Load);
			((ISupportInitialize)updnMainDia).EndInit();
			((ISupportInitialize)updnSatDia).EndInit();
			((ISupportInitialize)updnDensity).EndInit();
			((ISupportInitialize)updnPeriodHrs).EndInit();
			((ISupportInitialize)updnRadius).EndInit();
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((ISupportInitialize)pictureBox1).EndInit();
			((ISupportInitialize)pictureBox2).EndInit();
			((ISupportInitialize)pictureBox3).EndInit();
			((Control)panel1).ResumeLayout(false);
			((Control)panel1).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
