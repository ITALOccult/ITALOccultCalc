using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Asteroids
{
	public class Relativity : Form
	{
		private const double Radian = 180.0 / Math.PI;

		private IContainer components;

		private Label label15;

		private Label label13;

		private Label label4;

		private Label label3;

		private Label label2;

		private Label label1;

		private Label label5;

		private Label label6;

		private Label label8;

		private Label label7;

		private Label label9;

		internal TextBox txtRS1;

		internal TextBox txtRM1;

		internal TextBox txtRH1;

		internal TextBox txtDS1;

		internal TextBox txtDM1;

		internal TextBox txtDD1;

		internal NumericUpDown updnDay;

		internal NumericUpDown updnMonth;

		internal NumericUpDown updnYear;

		private Label label10;

		private TextBox txtRelRA;

		private TextBox txtRelDec;

		private Button cmdCompute;

		private Label label11;

		internal TextBox txtDistance;

		private Label label12;

		private Label label14;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private Label label16;

		private TextBox txtDecStar;

		private TextBox txtRAstar;

		private Label label17;

		private TextBox txtTotal;

		private Label label18;

		private Label label19;

		private TextBox txtElongation;

		private TextBox txtdDecAst;

		private TextBox txtRAast;

		private Label label20;

		private TextBox txtRAast_sec;

		private TextBox txtRAstar_sec;

		private Label label21;

		private TextBox txtRelRA_sec;

		private Label label22;

		private Label label23;

		private Label label24;

		public Relativity()
		{
			InitializeComponent();
		}

		private void cmdCompute_Click(object sender, EventArgs e)
		{
			Compute();
		}

		internal void Compute()
		{
			if (!double.TryParse(((Control)txtDistance).get_Text(), out var result))
			{
				result = 3.0;
			}
			if (!int.TryParse(((Control)txtRH1).get_Text(), out var result2))
			{
				result2 = 0;
			}
			if (!int.TryParse(((Control)txtRM1).get_Text(), out var result3))
			{
				result3 = 0;
			}
			if (!double.TryParse(((Control)txtRS1).get_Text(), out var result4))
			{
				result4 = 0.0;
			}
			if (!int.TryParse(((Control)txtDD1).get_Text().Replace("-", ""), out var result5))
			{
				result5 = 0;
			}
			if (!int.TryParse(((Control)txtDM1).get_Text(), out var result6))
			{
				result6 = 0;
			}
			if (!double.TryParse(((Control)txtDS1).get_Text(), out var result7))
			{
				result7 = 0.0;
			}
			double num = ((double)result2 + (double)result3 / 60.0 + result4 / 3600.0) * 15.0 / (180.0 / Math.PI);
			double num2 = ((double)Math.Abs(result5) + (double)result6 / 60.0 + result7 / 3600.0) / (180.0 / Math.PI);
			if (((Control)txtDD1).get_Text().Contains("-"))
			{
				num2 = 0.0 - num2;
			}
			double jD = Utilities.JD_from_Date((int)updnYear.get_Value(), (int)updnMonth.get_Value(), (double)updnDay.get_Value());
			Utilities.Relativistic_Differential_Correction(jD, num, num2, result, out var dRA_ComparedToStar, out var dDec_ComparedToStar);
			((Control)txtRelRA).set_Text($"{dRA_ComparedToStar * (180.0 / Math.PI) * 3600.0 * 1000.0 * Math.Cos(num2):+#0.0;-#0.0} mas");
			((Control)txtRelRA_sec).set_Text($"{dRA_ComparedToStar * (180.0 / Math.PI) * 3600.0 * 1000.0 / 15.0:+#0.00;-#0.00} mts");
			((Control)txtRelDec).set_Text($"{dDec_ComparedToStar * (180.0 / Math.PI) * 3600.0 * 1000.0:+#0.0;-#0.0} mas");
			Utilities.Relativistic_Correction(jD, num, num2, result, out dRA_ComparedToStar, out dDec_ComparedToStar, out var Elongation_deg);
			((Control)txtRAast).set_Text($"{dRA_ComparedToStar * (180.0 / Math.PI) * 3600.0 * 1000.0 * Math.Cos(num2):+#0.0;-#0.0} mas");
			((Control)txtRAast_sec).set_Text($"{dRA_ComparedToStar * (180.0 / Math.PI) * 3600.0 * 1000.0 / 15.0:+#0.00;-#0.00} mts");
			((Control)txtdDecAst).set_Text($"{dDec_ComparedToStar * (180.0 / Math.PI) * 3600.0 * 1000.0:+#0.0;-#0.0} mas");
			double num3 = Utilities.Relativistic_Correction(jD, num, num2, out dRA_ComparedToStar, out dDec_ComparedToStar, out Elongation_deg);
			((Control)txtRAstar).set_Text($"{dRA_ComparedToStar * (180.0 / Math.PI) * 3600.0 * 1000.0 * Math.Cos(num2):+#0.0;-#0.0} mas");
			((Control)txtRAstar_sec).set_Text($"{dRA_ComparedToStar * (180.0 / Math.PI) * 3600.0 * 1000.0 / 15.0:+#0.00;-#0.00} mts");
			((Control)txtDecStar).set_Text($"{dDec_ComparedToStar * (180.0 / Math.PI) * 3600.0 * 1000.0:+#0.0;-#0.0} mas");
			((Control)txtElongation).set_Text($"{Elongation_deg:F1}°");
			((Control)txtTotal).set_Text($"{num3 * 1000.0:F1} mas");
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Relativity");
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private void Relativity_Load(object sender, EventArgs e)
		{
			((Control)this).set_Text(((Control)this).get_Text() + " : v." + Utilities.OccultVersion_Short);
			updnYear.set_Value((decimal)DateTime.UtcNow.Year);
			updnMonth.set_Value((decimal)DateTime.UtcNow.Month);
			updnDay.set_Value((decimal)DateTime.UtcNow.Day + (decimal)DateTime.UtcNow.Hour / 60m);
			Utilities.PlanetGeocentric(Utilities.JD_from_Date((int)updnYear.get_Value(), (int)updnMonth.get_Value(), (double)updnDay.get_Value()), 3, 1E-05, 0, out var RA, out var Dec, out var _);
			string text = Utilities.DEGtoDMS(RA * (180.0 / Math.PI) / 15.0 + 0.1, 2, 1, MinutesOnly: false);
			string text2 = Utilities.DEGtoDMS(Dec * (180.0 / Math.PI), 3, 0, MinutesOnly: false);
			((Control)txtRH1).set_Text(text.Substring(0, 2));
			((Control)txtRM1).set_Text(text.Substring(3, 2));
			((Control)txtRS1).set_Text(text.Substring(6));
			((Control)txtDD1).set_Text(text2.Substring(0, 3));
			((Control)txtDM1).set_Text(text2.Substring(4, 2));
			((Control)txtDS1).set_Text(text2.Substring(7));
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
			//IL_01df: Unknown result type (might be due to invalid IL or missing references)
			//IL_01e9: Expected O, but got Unknown
			//IL_01ea: Unknown result type (might be due to invalid IL or missing references)
			//IL_01f4: Expected O, but got Unknown
			//IL_01f5: Unknown result type (might be due to invalid IL or missing references)
			//IL_01ff: Expected O, but got Unknown
			//IL_0200: Unknown result type (might be due to invalid IL or missing references)
			//IL_020a: Expected O, but got Unknown
			//IL_020b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0215: Expected O, but got Unknown
			//IL_0216: Unknown result type (might be due to invalid IL or missing references)
			//IL_0220: Expected O, but got Unknown
			//IL_0221: Unknown result type (might be due to invalid IL or missing references)
			//IL_022b: Expected O, but got Unknown
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(Relativity));
			label15 = new Label();
			label13 = new Label();
			label4 = new Label();
			label3 = new Label();
			label2 = new Label();
			label1 = new Label();
			txtRS1 = new TextBox();
			txtRM1 = new TextBox();
			txtRH1 = new TextBox();
			txtDS1 = new TextBox();
			txtDM1 = new TextBox();
			txtDD1 = new TextBox();
			label5 = new Label();
			label6 = new Label();
			label8 = new Label();
			label7 = new Label();
			label9 = new Label();
			updnDay = new NumericUpDown();
			updnMonth = new NumericUpDown();
			updnYear = new NumericUpDown();
			label10 = new Label();
			txtRelRA = new TextBox();
			txtRelDec = new TextBox();
			cmdCompute = new Button();
			txtDistance = new TextBox();
			label11 = new Label();
			label12 = new Label();
			label14 = new Label();
			menuStrip1 = new MenuStrip();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			label16 = new Label();
			txtDecStar = new TextBox();
			txtRAstar = new TextBox();
			label17 = new Label();
			txtTotal = new TextBox();
			label18 = new Label();
			label19 = new Label();
			txtElongation = new TextBox();
			txtdDecAst = new TextBox();
			txtRAast = new TextBox();
			label20 = new Label();
			txtRAast_sec = new TextBox();
			txtRAstar_sec = new TextBox();
			label21 = new Label();
			txtRelRA_sec = new TextBox();
			label22 = new Label();
			label23 = new Label();
			label24 = new Label();
			((ISupportInitialize)updnDay).BeginInit();
			((ISupportInitialize)updnMonth).BeginInit();
			((ISupportInitialize)updnYear).BeginInit();
			((Control)menuStrip1).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)label15).set_AutoSize(true);
			((Control)label15).set_Location(new Point(226, 48));
			((Control)label15).set_Name("label15");
			((Control)label15).set_Size(new Size(60, 13));
			((Control)label15).set_TabIndex(8);
			((Control)label15).set_Text("Declination");
			((Control)label13).set_AutoSize(true);
			((Control)label13).set_Location(new Point(9, 49));
			((Control)label13).set_Name("label13");
			((Control)label13).set_Size(new Size(79, 13));
			((Control)label13).set_TabIndex(1);
			((Control)label13).set_Text("Right Acension");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Location(new Point(303, 32));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(13, 13));
			((Control)label4).set_TabIndex(9);
			((Control)label4).set_Text("o");
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Location(new Point(162, 30));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(12, 13));
			((Control)label3).set_TabIndex(6);
			((Control)label3).set_Text("s");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(128, 30));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(15, 13));
			((Control)label2).set_TabIndex(4);
			((Control)label2).set_Text("m");
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(100, 30));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(13, 13));
			((Control)label1).set_TabIndex(2);
			((Control)label1).set_Text("h");
			((Control)txtRS1).set_Location(new Point(155, 45));
			((Control)txtRS1).set_Name("txtRS1");
			((Control)txtRS1).set_Size(new Size(50, 20));
			((Control)txtRS1).set_TabIndex(7);
			((Control)txtRS1).set_Text("0");
			((Control)txtRM1).set_Location(new Point(123, 45));
			((Control)txtRM1).set_Name("txtRM1");
			((Control)txtRM1).set_Size(new Size(23, 20));
			((Control)txtRM1).set_TabIndex(5);
			((Control)txtRM1).set_Text("0");
			((Control)txtRH1).set_Location(new Point(93, 45));
			((Control)txtRH1).set_Name("txtRH1");
			((Control)txtRH1).set_Size(new Size(23, 20));
			((Control)txtRH1).set_TabIndex(3);
			((Control)txtRH1).set_Text("0");
			((Control)txtDS1).set_Location(new Point(359, 45));
			((Control)txtDS1).set_Name("txtDS1");
			((Control)txtDS1).set_Size(new Size(47, 20));
			((Control)txtDS1).set_TabIndex(14);
			((Control)txtDS1).set_Text("0");
			((Control)txtDM1).set_Location(new Point(327, 45));
			((Control)txtDM1).set_Name("txtDM1");
			((Control)txtDM1).set_Size(new Size(23, 20));
			((Control)txtDM1).set_TabIndex(12);
			((Control)txtDM1).set_Text("0");
			((Control)txtDD1).set_Location(new Point(292, 45));
			((Control)txtDD1).set_Name("txtDD1");
			((Control)txtDD1).set_Size(new Size(28, 20));
			((Control)txtDD1).set_TabIndex(10);
			((Control)txtDD1).set_Text("0");
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Location(new Point(338, 37));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(9, 13));
			((Control)label5).set_TabIndex(11);
			((Control)label5).set_Text("'");
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Location(new Point(368, 37));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(12, 13));
			((Control)label6).set_TabIndex(13);
			((Control)label6).set_Text("\"");
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Location(new Point(90, 80));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(37, 13));
			((Control)label8).set_TabIndex(17);
			((Control)label8).set_Text("Month");
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Location(new Point(138, 80));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(26, 13));
			((Control)label7).set_TabIndex(19);
			((Control)label7).set_Text("Day");
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Location(new Point(38, 79));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(29, 13));
			((Control)label9).set_TabIndex(15);
			((Control)label9).set_Text("Year");
			((Control)updnDay).set_Anchor((AnchorStyles)1);
			updnDay.set_DecimalPlaces(2);
			((Control)updnDay).set_Location(new Point(138, 94));
			updnDay.set_Maximum(new decimal(new int[4] { 33, 0, 0, 0 }));
			updnDay.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnDay).set_Name("updnDay");
			((Control)updnDay).set_Size(new Size(47, 20));
			((Control)updnDay).set_TabIndex(20);
			updnDay.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnMonth).set_Anchor((AnchorStyles)1);
			((Control)updnMonth).set_Location(new Point(93, 94));
			updnMonth.set_Maximum(new decimal(new int[4] { 12, 0, 0, 0 }));
			updnMonth.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnMonth).set_Name("updnMonth");
			((Control)updnMonth).set_Size(new Size(37, 20));
			((Control)updnMonth).set_TabIndex(18);
			updnMonth.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnYear).set_Anchor((AnchorStyles)1);
			((Control)updnYear).set_Location(new Point(36, 94));
			updnYear.set_Maximum(new decimal(new int[4] { 4000, 0, 0, 0 }));
			updnYear.set_Minimum(new decimal(new int[4] { 2000, 0, 0, -2147483648 }));
			((Control)updnYear).set_Name("updnYear");
			((Control)updnYear).set_Size(new Size(49, 20));
			((Control)updnYear).set_TabIndex(16);
			updnYear.set_Value(new decimal(new int[4] { 2019, 0, 0, 0 }));
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Location(new Point(23, 297));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(128, 26));
			((Control)label10).set_TabIndex(42);
			((Control)label10).set_Text("Deflection of the asteroid \r\nrelative to the star");
			label10.set_TextAlign(ContentAlignment.TopCenter);
			((Control)txtRelRA).set_Font(new Font("Courier New", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtRelRA).set_Location(new Point(153, 300));
			((Control)txtRelRA).set_Name("txtRelRA");
			((Control)txtRelRA).set_Size(new Size(73, 20));
			((Control)txtRelRA).set_TabIndex(43);
			((Control)txtRelRA).set_Text("0");
			((Control)txtRelDec).set_Font(new Font("Courier New", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtRelDec).set_Location(new Point(339, 300));
			((Control)txtRelDec).set_Name("txtRelDec");
			((Control)txtRelDec).set_Size(new Size(66, 20));
			((Control)txtRelDec).set_TabIndex(46);
			((Control)txtRelDec).set_Text("0");
			((Control)cmdCompute).set_Location(new Point(142, 134));
			((Control)cmdCompute).set_Name("cmdCompute");
			((Control)cmdCompute).set_Size(new Size(131, 34));
			((Control)cmdCompute).set_TabIndex(24);
			((Control)cmdCompute).set_Text("Compute deflections");
			((ButtonBase)cmdCompute).set_UseVisualStyleBackColor(true);
			((Control)cmdCompute).add_Click((EventHandler)cmdCompute_Click);
			((Control)txtDistance).set_Location(new Point(334, 91));
			((Control)txtDistance).set_Name("txtDistance");
			((Control)txtDistance).set_Size(new Size(55, 20));
			((Control)txtDistance).set_TabIndex(22);
			((Control)txtDistance).set_Text("1.5");
			((Control)label11).set_AutoSize(true);
			((Control)label11).set_Location(new Point(226, 94));
			((Control)label11).set_Name("label11");
			((Control)label11).set_Size(new Size(102, 13));
			((Control)label11).set_TabIndex(21);
			((Control)label11).set_Text("Distance to Asteroid");
			((Control)label12).set_AutoSize(true);
			((Control)label12).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label12).set_Location(new Point(342, 231));
			((Control)label12).set_Name("label12");
			((Control)label12).set_Size(new Size(64, 13));
			((Control)label12).set_TabIndex(35);
			((Control)label12).set_Text("Dec (mas)");
			((Control)label14).set_AutoSize(true);
			((Control)label14).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label14).set_Location(new Point(156, 231));
			((Control)label14).set_Name("label14");
			((Control)label14).set_Size(new Size(66, 13));
			((Control)label14).set_TabIndex(30);
			((Control)label14).set_Text("R.A. (mas)");
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(415, 24));
			((Control)menuStrip1).set_TabIndex(0);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(72, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("Help    ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(54, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("Exit");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((Control)label16).set_AutoSize(true);
			((Control)label16).set_Location(new Point(389, 94));
			((Control)label16).set_Name("label16");
			((Control)label16).set_Size(new Size(22, 13));
			((Control)label16).set_TabIndex(23);
			((Control)label16).set_Text("AU");
			((Control)txtDecStar).set_Font(new Font("Courier New", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtDecStar).set_Location(new Point(339, 246));
			((Control)txtDecStar).set_Name("txtDecStar");
			((Control)txtDecStar).set_Size(new Size(66, 20));
			((Control)txtDecStar).set_TabIndex(36);
			((Control)txtDecStar).set_Text("0");
			((Control)txtRAstar).set_Font(new Font("Courier New", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtRAstar).set_Location(new Point(153, 246));
			((Control)txtRAstar).set_Name("txtRAstar");
			((Control)txtRAstar).set_Size(new Size(73, 20));
			((Control)txtRAstar).set_TabIndex(31);
			((Control)txtRAstar).set_Text("0");
			((Control)label17).set_AutoSize(true);
			((Control)label17).set_Location(new Point(46, 250));
			((Control)label17).set_Name("label17");
			((Control)label17).set_Size(new Size(105, 13));
			((Control)label17).set_TabIndex(29);
			((Control)label17).set_Text("Deflection of the star");
			((Control)txtTotal).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtTotal).set_Location(new Point(164, 190));
			((Control)txtTotal).set_Name("txtTotal");
			((Control)txtTotal).set_Size(new Size(73, 20));
			((Control)txtTotal).set_TabIndex(26);
			((Control)txtTotal).set_Text("0");
			((Control)label18).set_AutoSize(true);
			((Control)label18).set_Location(new Point(13, 194));
			((Control)label18).set_Name("label18");
			((Control)label18).set_Size(new Size(149, 13));
			((Control)label18).set_TabIndex(25);
			((Control)label18).set_Text("Total deflection of a star (mas)");
			((Control)label19).set_AutoSize(true);
			((Control)label19).set_Location(new Point(253, 194));
			((Control)label19).set_Name("label19");
			((Control)label19).set_Size(new Size(84, 13));
			((Control)label19).set_TabIndex(27);
			((Control)label19).set_Text("Solar Elongation");
			((Control)txtElongation).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtElongation).set_Location(new Point(339, 190));
			((Control)txtElongation).set_Name("txtElongation");
			((Control)txtElongation).set_Size(new Size(66, 20));
			((Control)txtElongation).set_TabIndex(28);
			((Control)txtdDecAst).set_Font(new Font("Courier New", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtdDecAst).set_Location(new Point(339, 273));
			((Control)txtdDecAst).set_Name("txtdDecAst");
			((Control)txtdDecAst).set_Size(new Size(66, 20));
			((Control)txtdDecAst).set_TabIndex(41);
			((Control)txtdDecAst).set_Text("0");
			((Control)txtRAast).set_Font(new Font("Courier New", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtRAast).set_Location(new Point(153, 273));
			((Control)txtRAast).set_Name("txtRAast");
			((Control)txtRAast).set_Size(new Size(73, 20));
			((Control)txtRAast).set_TabIndex(38);
			((Control)txtRAast).set_Text("0");
			((Control)label20).set_AutoSize(true);
			((Control)label20).set_Location(new Point(25, 277));
			((Control)label20).set_Name("label20");
			((Control)label20).set_Size(new Size(125, 13));
			((Control)label20).set_TabIndex(37);
			((Control)label20).set_Text("Deflection of the asteroid");
			((Control)txtRAast_sec).set_Font(new Font("Courier New", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtRAast_sec).set_Location(new Point(239, 273));
			((Control)txtRAast_sec).set_Name("txtRAast_sec");
			((Control)txtRAast_sec).set_Size(new Size(73, 20));
			((Control)txtRAast_sec).set_TabIndex(40);
			((Control)txtRAast_sec).set_Text("0");
			((Control)txtRAstar_sec).set_Font(new Font("Courier New", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtRAstar_sec).set_Location(new Point(239, 246));
			((Control)txtRAstar_sec).set_Name("txtRAstar_sec");
			((Control)txtRAstar_sec).set_Size(new Size(73, 20));
			((Control)txtRAstar_sec).set_TabIndex(34);
			((Control)txtRAstar_sec).set_Text("0");
			((Control)label21).set_AutoSize(true);
			((Control)label21).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label21).set_Location(new Point(242, 231));
			((Control)label21).set_Name("label21");
			((Control)label21).set_Size(new Size(63, 13));
			((Control)label21).set_TabIndex(33);
			((Control)label21).set_Text("R.A. (mts)");
			((Control)txtRelRA_sec).set_Font(new Font("Courier New", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtRelRA_sec).set_Location(new Point(239, 300));
			((Control)txtRelRA_sec).set_Name("txtRelRA_sec");
			((Control)txtRelRA_sec).set_Size(new Size(73, 20));
			((Control)txtRelRA_sec).set_TabIndex(45);
			((Control)txtRelRA_sec).set_Text("0");
			((Control)label22).set_AutoSize(true);
			((Control)label22).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label22).set_Location(new Point(225, 248));
			((Control)label22).set_Name("label22");
			((Control)label22).set_Size(new Size(16, 17));
			((Control)label22).set_TabIndex(32);
			((Control)label22).set_Text("=");
			((Control)label23).set_AutoSize(true);
			((Control)label23).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label23).set_Location(new Point(225, 275));
			((Control)label23).set_Name("label23");
			((Control)label23).set_Size(new Size(16, 17));
			((Control)label23).set_TabIndex(39);
			((Control)label23).set_Text("=");
			((Control)label24).set_AutoSize(true);
			((Control)label24).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label24).set_Location(new Point(225, 302));
			((Control)label24).set_Name("label24");
			((Control)label24).set_Size(new Size(16, 17));
			((Control)label24).set_TabIndex(44);
			((Control)label24).set_Text("=");
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(415, 334));
			((Control)this).get_Controls().Add((Control)(object)txtRAast_sec);
			((Control)this).get_Controls().Add((Control)(object)txtRAstar_sec);
			((Control)this).get_Controls().Add((Control)(object)label21);
			((Control)this).get_Controls().Add((Control)(object)txtRelRA_sec);
			((Control)this).get_Controls().Add((Control)(object)txtdDecAst);
			((Control)this).get_Controls().Add((Control)(object)txtRAast);
			((Control)this).get_Controls().Add((Control)(object)label20);
			((Control)this).get_Controls().Add((Control)(object)label19);
			((Control)this).get_Controls().Add((Control)(object)txtElongation);
			((Control)this).get_Controls().Add((Control)(object)label18);
			((Control)this).get_Controls().Add((Control)(object)txtTotal);
			((Control)this).get_Controls().Add((Control)(object)txtDecStar);
			((Control)this).get_Controls().Add((Control)(object)txtRAstar);
			((Control)this).get_Controls().Add((Control)(object)label17);
			((Control)this).get_Controls().Add((Control)(object)label16);
			((Control)this).get_Controls().Add((Control)(object)label12);
			((Control)this).get_Controls().Add((Control)(object)label14);
			((Control)this).get_Controls().Add((Control)(object)txtDistance);
			((Control)this).get_Controls().Add((Control)(object)label11);
			((Control)this).get_Controls().Add((Control)(object)cmdCompute);
			((Control)this).get_Controls().Add((Control)(object)txtRelDec);
			((Control)this).get_Controls().Add((Control)(object)txtRelRA);
			((Control)this).get_Controls().Add((Control)(object)label10);
			((Control)this).get_Controls().Add((Control)(object)label8);
			((Control)this).get_Controls().Add((Control)(object)label7);
			((Control)this).get_Controls().Add((Control)(object)label9);
			((Control)this).get_Controls().Add((Control)(object)updnDay);
			((Control)this).get_Controls().Add((Control)(object)updnMonth);
			((Control)this).get_Controls().Add((Control)(object)updnYear);
			((Control)this).get_Controls().Add((Control)(object)label15);
			((Control)this).get_Controls().Add((Control)(object)label13);
			((Control)this).get_Controls().Add((Control)(object)label4);
			((Control)this).get_Controls().Add((Control)(object)label3);
			((Control)this).get_Controls().Add((Control)(object)label2);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)txtRS1);
			((Control)this).get_Controls().Add((Control)(object)txtRM1);
			((Control)this).get_Controls().Add((Control)(object)txtRH1);
			((Control)this).get_Controls().Add((Control)(object)txtDS1);
			((Control)this).get_Controls().Add((Control)(object)txtDM1);
			((Control)this).get_Controls().Add((Control)(object)txtDD1);
			((Control)this).get_Controls().Add((Control)(object)label5);
			((Control)this).get_Controls().Add((Control)(object)label6);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_Controls().Add((Control)(object)label22);
			((Control)this).get_Controls().Add((Control)(object)label24);
			((Control)this).get_Controls().Add((Control)(object)label23);
			((Form)this).set_FormBorderStyle((FormBorderStyle)2);
			((Form)this).set_Icon((Icon)componentResourceManager.GetObject("$this.Icon"));
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_Name("Relativity");
			((Control)this).set_Text("Relativistic offset from Sun's gravity");
			((Form)this).add_Load((EventHandler)Relativity_Load);
			((ISupportInitialize)updnDay).EndInit();
			((ISupportInitialize)updnMonth).EndInit();
			((ISupportInitialize)updnYear).EndInit();
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
