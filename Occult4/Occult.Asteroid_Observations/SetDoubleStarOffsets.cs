using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;

namespace Occult.Asteroid_Observations
{
	public class SetDoubleStarOffsets : Form
	{
		private decimal NorthX;

		private decimal SouthX;

		private decimal NorthY;

		private decimal SouthY;

		private decimal MidX;

		private decimal MidY;

		private decimal OffsetX;

		private decimal OffsetY;

		private IContainer components;

		private Label label1;

		private Label label2;

		private Button cmdNorth;

		private Button cmdSouth;

		private Button cmdSetOffsets;

		private Label lblNX;

		private Label lblSY;

		private Label lblSX;

		private Label lblNY;

		private Label lblOffX;

		private Label lblOffY;

		private Label lblCX;

		private Label lblCY;

		private Panel panel1;

		private Label label3;

		private Panel panel2;

		private Label label4;

		private Panel panel3;

		private Panel panel4;

		private Panel panel5;

		private GroupBox groupBox1;

		private Label label5;

		public SetDoubleStarOffsets()
		{
			InitializeComponent();
			CheckBox chkX = Data_and_Plots.PlotForm.chkX;
			bool @checked;
			Data_and_Plots.PlotForm.chkY.set_Checked(@checked = true);
			chkX.set_Checked(@checked);
			CheckBox chkA = Data_and_Plots.PlotForm.chkA;
			Data_and_Plots.PlotForm.chkB.set_Checked(@checked = false);
			chkA.set_Checked(@checked);
			Data_and_Plots.PlotForm.optPrimary.set_Checked(true);
			CheckBox chkCompanionPA = Data_and_Plots.PlotForm.chkCompanionPA;
			CheckBox chkCompanionSep = Data_and_Plots.PlotForm.chkCompanionSep;
			bool flag;
			Data_and_Plots.PlotForm.chkCircle.set_Checked(flag = true);
			chkCompanionSep.set_Checked(@checked = flag);
			chkCompanionPA.set_Checked(@checked);
			Data_and_Plots.DoublesOffset_X[1] = (Data_and_Plots.DoublesOffset_X[2] = 0.0);
			Data_and_Plots.DoublesOffset_Y[1] = (Data_and_Plots.DoublesOffset_Y[2] = 0.0);
			Data_and_Plots.DoublesOffset_X[3] = (Data_and_Plots.DoublesOffset_X[4] = 0.0);
			Data_and_Plots.DoublesOffset_Y[3] = (Data_and_Plots.DoublesOffset_Y[4] = 0.0);
			Data_and_Plots.PlotForm.SetLabelsForDoubleOffsets();
			NorthX = (SouthX = (NorthY = (SouthY = 0m)));
			SetMeans();
			Data_and_Plots.PlotForm.FlashTimerDouble_StartEnd();
		}

		private void cmdClose_Click(object sender, EventArgs e)
		{
			CheckBox chkX = Data_and_Plots.PlotForm.chkX;
			bool @checked;
			Data_and_Plots.PlotForm.chkY.set_Checked(@checked = false);
			chkX.set_Checked(@checked);
			CheckBox chkA = Data_and_Plots.PlotForm.chkA;
			Data_and_Plots.PlotForm.chkB.set_Checked(@checked = false);
			chkA.set_Checked(@checked);
			Data_and_Plots.PlotForm.optPrimary.set_Checked(true);
			CheckBox chkCompanionPA = Data_and_Plots.PlotForm.chkCompanionPA;
			Data_and_Plots.PlotForm.chkCompanionSep.set_Checked(@checked = true);
			chkCompanionPA.set_Checked(@checked);
			Data_and_Plots.PlotForm.FlashTimerDouble_StartEnd();
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private void SetDoubleStarOffsets_FormClosing(object sender, FormClosingEventArgs e)
		{
			RadioButton optSecondary = Data_and_Plots.PlotForm.optSecondary;
			RadioButton optBoth = Data_and_Plots.PlotForm.optBoth;
			CheckBox chkCompanionPA = Data_and_Plots.PlotForm.chkCompanionPA;
			CheckBox chkCompanionSep = Data_and_Plots.PlotForm.chkCompanionSep;
			bool flag;
			((Control)Data_and_Plots.PlotForm.panelDouble).set_Enabled(flag = true);
			bool flag2;
			((Control)chkCompanionSep).set_Enabled(flag2 = flag);
			bool flag3;
			((Control)chkCompanionPA).set_Enabled(flag3 = flag2);
			bool enabled;
			((Control)optBoth).set_Enabled(enabled = flag3);
			((Control)optSecondary).set_Enabled(enabled);
			CheckBox chkX = Data_and_Plots.PlotForm.chkX;
			Data_and_Plots.PlotForm.chkY.set_Checked(enabled = false);
			chkX.set_Checked(enabled);
			Data_and_Plots.PlotForm.optSecondary.set_Checked(true);
		}

		private void cmdNorth_Click(object sender, EventArgs e)
		{
			NorthX = Data_and_Plots.PlotForm.updnX.get_Value();
			NorthY = Data_and_Plots.PlotForm.updnY.get_Value();
			((Control)lblNX).set_Text(string.Format("North X = {0,1:f1}", NorthX));
			((Control)lblNY).set_Text(string.Format("North Y = {0,1:f1}", NorthY));
			SetMeans();
		}

		private void cmdSouth_Click(object sender, EventArgs e)
		{
			SouthX = Data_and_Plots.PlotForm.updnX.get_Value();
			SouthY = Data_and_Plots.PlotForm.updnY.get_Value();
			((Control)lblSX).set_Text(string.Format("South X = {0,1:f1}", SouthX));
			((Control)lblSY).set_Text(string.Format("South Y = {0,1:f1}", SouthY));
			SetMeans();
		}

		private void cmdSetOffsets_Click(object sender, EventArgs e)
		{
			SetMeans();
			Data_and_Plots.PlotForm.SetDoubleStarOffsetValues(MidX, MidY, OffsetX, OffsetY);
			Data_and_Plots.PlotForm.FlashTimerDouble_StartEnd();
			CheckBox chkX = Data_and_Plots.PlotForm.chkX;
			bool @checked;
			Data_and_Plots.PlotForm.chkY.set_Checked(@checked = false);
			chkX.set_Checked(@checked);
			CheckBox chkA = Data_and_Plots.PlotForm.chkA;
			Data_and_Plots.PlotForm.chkB.set_Checked(@checked = false);
			chkA.set_Checked(@checked);
			CheckBox chkCompanionPA = Data_and_Plots.PlotForm.chkCompanionPA;
			Data_and_Plots.PlotForm.chkCompanionSep.set_Checked(@checked = true);
			chkCompanionPA.set_Checked(@checked);
			Data_and_Plots.PlotForm.optSecondary.set_Checked(true);
			Data_and_Plots.PlotForm.FlashTimerDouble_StartEnd();
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private void cmdClear_Click(object sender, EventArgs e)
		{
			Data_and_Plots.DoublesOffset_X[1] = (Data_and_Plots.DoublesOffset_X[2] = 0.0);
			Data_and_Plots.DoublesOffset_Y[1] = (Data_and_Plots.DoublesOffset_Y[2] = 0.0);
			Data_and_Plots.DoublesOffset_X[3] = (Data_and_Plots.DoublesOffset_X[4] = 0.0);
			Data_and_Plots.DoublesOffset_Y[3] = (Data_and_Plots.DoublesOffset_Y[4] = 0.0);
			Data_and_Plots.PlotForm.SetLabelsForDoubleOffsets();
			NorthX = (SouthX = (NorthY = (SouthY = 0m)));
			SetMeans();
			Data_and_Plots.PlotForm.FlashTimerDouble_StartEnd();
		}

		private void SetMeans()
		{
			MidX = (NorthX + SouthX) / 2m;
			MidY = (NorthY + SouthY) / 2m;
			OffsetX = (NorthX - SouthX) / 2m;
			OffsetY = (NorthY - SouthY) / 2m;
			((Control)lblCX).set_Text(string.Format("Centre X = {0,1:f1}", MidX));
			((Control)lblCY).set_Text(string.Format("Centre Y = {0,1:f1}", MidY));
			((Control)lblOffX).set_Text(string.Format("Offset X = ±{0,1:f1}", OffsetX));
			((Control)lblOffY).set_Text(string.Format("Offset Y = ±{0,1:f1}", OffsetY));
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
			//IL_0d8b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d95: Expected O, but got Unknown
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(SetDoubleStarOffsets));
			label1 = new Label();
			label2 = new Label();
			cmdNorth = new Button();
			cmdSouth = new Button();
			cmdSetOffsets = new Button();
			lblNX = new Label();
			lblSY = new Label();
			lblSX = new Label();
			lblNY = new Label();
			lblOffX = new Label();
			lblOffY = new Label();
			lblCX = new Label();
			lblCY = new Label();
			panel1 = new Panel();
			label3 = new Label();
			panel2 = new Panel();
			label4 = new Label();
			panel3 = new Panel();
			panel4 = new Panel();
			panel5 = new Panel();
			groupBox1 = new GroupBox();
			label5 = new Label();
			((Control)panel1).SuspendLayout();
			((Control)panel2).SuspendLayout();
			((Control)panel3).SuspendLayout();
			((Control)panel4).SuspendLayout();
			((Control)panel5).SuspendLayout();
			((Control)groupBox1).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)label1).set_BackColor(Color.Honeydew);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label1).set_ForeColor(Color.DarkBlue);
			((Control)label1).set_Location(new Point(1, 0));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(498, 97));
			((Control)label1).set_TabIndex(0);
			((Control)label1).set_Text(componentResourceManager.GetString("label1.Text"));
			label1.set_TextAlign(ContentAlignment.MiddleLeft);
			((Control)label2).set_BackColor(Color.LightCyan);
			((Control)label2).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label2).set_ForeColor(Color.Black);
			((Control)label2).set_Location(new Point(0, 0));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(498, 138));
			((Control)label2).set_TabIndex(1);
			((Control)label2).set_Text(componentResourceManager.GetString("label2.Text"));
			((Control)cmdNorth).set_BackColor(Color.Chartreuse);
			((Control)cmdNorth).set_ForeColor(Color.Black);
			((Control)cmdNorth).set_Location(new Point(4, 5));
			((Control)cmdNorth).set_Name("cmdNorth");
			((Control)cmdNorth).set_Size(new Size(84, 35));
			((Control)cmdNorth).set_TabIndex(2);
			((Control)cmdNorth).set_Text("Set the\r\nNorth chord");
			((ButtonBase)cmdNorth).set_UseVisualStyleBackColor(false);
			((Control)cmdNorth).add_Click((EventHandler)cmdNorth_Click);
			((Control)cmdSouth).set_BackColor(Color.Chartreuse);
			((Control)cmdSouth).set_ForeColor(Color.Black);
			((Control)cmdSouth).set_Location(new Point(4, 5));
			((Control)cmdSouth).set_Name("cmdSouth");
			((Control)cmdSouth).set_Size(new Size(84, 35));
			((Control)cmdSouth).set_TabIndex(3);
			((Control)cmdSouth).set_Text("Set the\r\nSouth chord");
			((ButtonBase)cmdSouth).set_UseVisualStyleBackColor(false);
			((Control)cmdSouth).add_Click((EventHandler)cmdSouth_Click);
			((Control)cmdSetOffsets).set_BackColor(Color.LightSkyBlue);
			((Control)cmdSetOffsets).set_Location(new Point(9, 5));
			((Control)cmdSetOffsets).set_Name("cmdSetOffsets");
			((Control)cmdSetOffsets).set_Size(new Size(97, 35));
			((Control)cmdSetOffsets).set_TabIndex(4);
			((Control)cmdSetOffsets).set_Text("Set the Offsets\r\nand CLOSE");
			((ButtonBase)cmdSetOffsets).set_UseVisualStyleBackColor(false);
			((Control)cmdSetOffsets).add_Click((EventHandler)cmdSetOffsets_Click);
			((Control)lblNX).set_AutoSize(true);
			((Control)lblNX).set_Location(new Point(4, 48));
			((Control)lblNX).set_Name("lblNX");
			((Control)lblNX).set_Size(new Size(61, 13));
			((Control)lblNX).set_TabIndex(6);
			((Control)lblNX).set_Text("North X = 0");
			((Control)lblSY).set_AutoSize(true);
			((Control)lblSY).set_Location(new Point(4, 61));
			((Control)lblSY).set_Name("lblSY");
			((Control)lblSY).set_Size(new Size(63, 13));
			((Control)lblSY).set_TabIndex(7);
			((Control)lblSY).set_Text("South Y = 0");
			((Control)lblSX).set_AutoSize(true);
			((Control)lblSX).set_Location(new Point(4, 48));
			((Control)lblSX).set_Name("lblSX");
			((Control)lblSX).set_Size(new Size(63, 13));
			((Control)lblSX).set_TabIndex(8);
			((Control)lblSX).set_Text("South X = 0");
			((Control)lblNY).set_AutoSize(true);
			((Control)lblNY).set_Location(new Point(4, 61));
			((Control)lblNY).set_Name("lblNY");
			((Control)lblNY).set_Size(new Size(61, 13));
			((Control)lblNY).set_TabIndex(9);
			((Control)lblNY).set_Text("North Y = 0");
			((Control)lblOffX).set_AutoSize(true);
			((Control)lblOffX).set_Location(new Point(9, 71));
			((Control)lblOffX).set_Name("lblOffX");
			((Control)lblOffX).set_Size(new Size(69, 13));
			((Control)lblOffX).set_TabIndex(11);
			((Control)lblOffX).set_Text("Offset X = ±0");
			((Control)lblOffY).set_AutoSize(true);
			((Control)lblOffY).set_Location(new Point(9, 84));
			((Control)lblOffY).set_Name("lblOffY");
			((Control)lblOffY).set_Size(new Size(69, 13));
			((Control)lblOffY).set_TabIndex(10);
			((Control)lblOffY).set_Text("Offset Y = ±0");
			((Control)lblCX).set_AutoSize(true);
			((Control)lblCX).set_Location(new Point(9, 45));
			((Control)lblCX).set_Name("lblCX");
			((Control)lblCX).set_Size(new Size(66, 13));
			((Control)lblCX).set_TabIndex(13);
			((Control)lblCX).set_Text("Centre X = 0");
			((Control)lblCY).set_AutoSize(true);
			((Control)lblCY).set_Location(new Point(9, 58));
			((Control)lblCY).set_Name("lblCY");
			((Control)lblCY).set_Size(new Size(66, 13));
			((Control)lblCY).set_TabIndex(12);
			((Control)lblCY).set_Text("Centre Y = 0");
			panel1.set_BorderStyle((BorderStyle)2);
			((Control)panel1).get_Controls().Add((Control)(object)label5);
			((Control)panel1).get_Controls().Add((Control)(object)label1);
			((Control)panel1).set_Location(new Point(5, 26));
			((Control)panel1).set_Name("panel1");
			((Control)panel1).set_Size(new Size(502, 165));
			((Control)panel1).set_TabIndex(15);
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label3).set_ForeColor(Color.Maroon);
			((Control)label3).set_Location(new Point(186, 7));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(142, 17));
			((Control)label3).set_TabIndex(16);
			((Control)label3).set_Text("P  U  R  P  O  S  E");
			panel2.set_BorderStyle((BorderStyle)2);
			((Control)panel2).get_Controls().Add((Control)(object)label2);
			((Control)panel2).set_Location(new Point(5, 219));
			((Control)panel2).set_Name("panel2");
			((Control)panel2).set_Size(new Size(501, 142));
			((Control)panel2).set_TabIndex(17);
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label4).set_ForeColor(Color.Maroon);
			((Control)label4).set_Location(new Point(147, 199));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(221, 20));
			((Control)label4).set_TabIndex(18);
			((Control)label4).set_Text("H  O  W      T  O      U  S  E");
			((Control)panel3).set_BackColor(Color.LemonChiffon);
			panel3.set_BorderStyle((BorderStyle)2);
			((Control)panel3).get_Controls().Add((Control)(object)lblNY);
			((Control)panel3).get_Controls().Add((Control)(object)lblNX);
			((Control)panel3).get_Controls().Add((Control)(object)cmdNorth);
			((Control)panel3).set_Location(new Point(38, 16));
			((Control)panel3).set_Name("panel3");
			((Control)panel3).set_Size(new Size(97, 83));
			((Control)panel3).set_TabIndex(19);
			((Control)panel4).set_BackColor(Color.LemonChiffon);
			panel4.set_BorderStyle((BorderStyle)2);
			((Control)panel4).get_Controls().Add((Control)(object)lblSX);
			((Control)panel4).get_Controls().Add((Control)(object)lblSY);
			((Control)panel4).get_Controls().Add((Control)(object)cmdSouth);
			((Control)panel4).set_Location(new Point(179, 16));
			((Control)panel4).set_Name("panel4");
			((Control)panel4).set_Size(new Size(97, 83));
			((Control)panel4).set_TabIndex(20);
			((Control)panel5).set_BackColor(Color.Honeydew);
			panel5.set_BorderStyle((BorderStyle)2);
			((Control)panel5).get_Controls().Add((Control)(object)lblCX);
			((Control)panel5).get_Controls().Add((Control)(object)lblCY);
			((Control)panel5).get_Controls().Add((Control)(object)lblOffX);
			((Control)panel5).get_Controls().Add((Control)(object)lblOffY);
			((Control)panel5).get_Controls().Add((Control)(object)cmdSetOffsets);
			((Control)panel5).set_Location(new Point(364, 369));
			((Control)panel5).set_Name("panel5");
			((Control)panel5).set_Size(new Size(120, 103));
			((Control)panel5).set_TabIndex(21);
			((Control)groupBox1).set_BackColor(Color.GhostWhite);
			((Control)groupBox1).get_Controls().Add((Control)(object)panel4);
			((Control)groupBox1).get_Controls().Add((Control)(object)panel3);
			((Control)groupBox1).set_ForeColor(Color.Maroon);
			((Control)groupBox1).set_Location(new Point(31, 370));
			((Control)groupBox1).set_Name("groupBox1");
			((Control)groupBox1).set_Size(new Size(315, 105));
			((Control)groupBox1).set_TabIndex(23);
			groupBox1.set_TabStop(false);
			((Control)groupBox1).set_Text("Set both of these before clicking 'Set the Offsets and CLOSE'. ");
			((Control)label5).set_BackColor(Color.Honeydew);
			((Control)label5).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label5).set_ForeColor(Color.Red);
			((Control)label5).set_Location(new Point(-1, 97));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(498, 64));
			((Control)label5).set_TabIndex(24);
			((Control)label5).set_Text(componentResourceManager.GetString("label5.Text"));
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(514, 472));
			((Control)this).get_Controls().Add((Control)(object)groupBox1);
			((Control)this).get_Controls().Add((Control)(object)panel5);
			((Control)this).get_Controls().Add((Control)(object)label4);
			((Control)this).get_Controls().Add((Control)(object)panel2);
			((Control)this).get_Controls().Add((Control)(object)label3);
			((Control)this).get_Controls().Add((Control)(object)panel1);
			((Form)this).set_FormBorderStyle((FormBorderStyle)2);
			((Form)this).set_MaximizeBox(false);
			((Form)this).set_MinimizeBox(false);
			((Control)this).set_Name("SetDoubleStarOffsets");
			((Control)this).set_Text("Set Double-Star Offsets");
			((Form)this).add_FormClosing(new FormClosingEventHandler(SetDoubleStarOffsets_FormClosing));
			((Control)panel1).ResumeLayout(false);
			((Control)panel2).ResumeLayout(false);
			((Control)panel3).ResumeLayout(false);
			((Control)panel3).PerformLayout();
			((Control)panel4).ResumeLayout(false);
			((Control)panel4).PerformLayout();
			((Control)panel5).ResumeLayout(false);
			((Control)panel5).PerformLayout();
			((Control)groupBox1).ResumeLayout(false);
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
