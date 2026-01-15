using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using Occult;
using Occult.Properties;

namespace AOTA
{
	public class VerifyTimes : Form
	{
		internal float FrameNoD;

		internal float FrameNoR;

		internal int ArrayIndexD;

		internal int ArrayIndexR;

		private double CorrnD = AOTAData.FrameTimeCorrnsD[AOTAData.EventIDnumber];

		private double CorrnR = AOTAData.FrameTimeCorrnsR[AOTAData.EventIDnumber];

		private IContainer components;

		private Label label1;

		private Label label2;

		internal Label lblInternalTimeD;

		internal Label lblInternalTimeR;

		internal NumericUpDown updnCorrnD;

		internal NumericUpDown updnCorrnR;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem helpToolStripMenuItem;

		private Label label3;

		private Label label4;

		private Label label5;

		private GroupBox grpR;

		internal GroupBox grpD;

		internal Label lblVTI_R;

		private Label label8;

		private Label label6;

		internal Label lblVTI_D;

		private Label label10;

		private Button cmdDisplayR;

		private Button cmdDisplayD;

		private Label label11;

		private Button cmdCancel;

		private Button cmdExit;

		internal Label lblFrameR;

		internal Label lblFrameD;

		public VerifyTimes()
		{
			InitializeComponent();
		}

		private void VerifyTimes_Load(object sender, EventArgs e)
		{
			((Form)AOTAData.PlotForm).set_WindowState((FormWindowState)1);
			((Form)this).set_TopMost(true);
		}

		private void cmdDisplayD_Click(object sender, EventArgs e)
		{
			Display((int)FrameNoD);
		}

		private void cmdDisplayR_Click(object sender, EventArgs e)
		{
			Display((int)FrameNoR);
		}

		private void Display(int Frame)
		{
			if (AOTA_ExternalAccess.AOTA_Client != null)
			{
				AOTA_ExternalAccess.FrameID_DisplayedInTangra = Frame;
				AOTA_ExternalAccess.AOTA_Client.PositionToFrame(Frame);
				AOTA_ExternalAccess.TangraFrameDisplayFromAOTA = true;
				AOTA_ExternalAccess.TangraFrameDisplayFromTangra = false;
			}
		}

		private void VerifyTimes_FormClosing(object sender, FormClosingEventArgs e)
		{
			((Form)AOTAData.PlotForm).set_WindowState((FormWindowState)0);
			((Form)this).set_TopMost(false);
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
		}

		private void updnCorrnD_ValueChanged(object sender, EventArgs e)
		{
			SetAdjustedDtime();
		}

		internal void SetAdjustedDtime()
		{
			AOTAData.FrameTimeCorrnsD[AOTAData.EventIDnumber] = (double)updnCorrnD.get_Value();
			((Control)lblVTI_D).set_Text(Utilities.DEGtoDMS((AOTAData.FrameTimeSecs[ArrayIndexD] + (double)updnCorrnD.get_Value()) / 3600.0, 2, 3, MinutesOnly: false));
		}

		private void updnCorrnR_ValueChanged(object sender, EventArgs e)
		{
			SetAdjustedRtime();
		}

		internal void SetAdjustedRtime()
		{
			AOTAData.FrameTimeCorrnsR[AOTAData.EventIDnumber] = (double)updnCorrnR.get_Value();
			((Control)lblVTI_R).set_Text(Utilities.DEGtoDMS((AOTAData.FrameTimeSecs[ArrayIndexR] + (double)updnCorrnR.get_Value()) / 3600.0, 2, 3, MinutesOnly: false));
		}

		private void cmdCancel_Click(object sender, EventArgs e)
		{
			AOTAData.FrameTimeCorrnsD[AOTAData.EventIDnumber] = CorrnD;
			AOTAData.FrameTimeCorrnsR[AOTAData.EventIDnumber] = CorrnR;
			((Form)this).Close();
		}

		private void cmdExit_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			AOTAData.PlotForm.SetDelayTimes();
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"AOTA Verify");
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
			//IL_0f2e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0f38: Expected O, but got Unknown
			label1 = new Label();
			label2 = new Label();
			lblInternalTimeD = new Label();
			lblInternalTimeR = new Label();
			updnCorrnD = new NumericUpDown();
			updnCorrnR = new NumericUpDown();
			menuStrip1 = new MenuStrip();
			helpToolStripMenuItem = new ToolStripMenuItem();
			label3 = new Label();
			label4 = new Label();
			label5 = new Label();
			grpR = new GroupBox();
			lblFrameR = new Label();
			cmdDisplayR = new Button();
			lblVTI_R = new Label();
			label8 = new Label();
			label6 = new Label();
			grpD = new GroupBox();
			lblFrameD = new Label();
			cmdDisplayD = new Button();
			lblVTI_D = new Label();
			label10 = new Label();
			label11 = new Label();
			cmdCancel = new Button();
			cmdExit = new Button();
			((ISupportInitialize)updnCorrnD).BeginInit();
			((ISupportInitialize)updnCorrnR).BeginInit();
			((Control)menuStrip1).SuspendLayout();
			((Control)grpR).SuspendLayout();
			((Control)grpD).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(8, 17));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(46, 13));
			((Control)label1).set_TabIndex(0);
			((Control)label1).set_Text("Frame #");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(8, 37));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(73, 13));
			((Control)label2).set_TabIndex(1);
			((Control)label2).set_Text("Time in AOTA");
			((Control)lblInternalTimeD).set_AutoSize(true);
			((Control)lblInternalTimeD).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblInternalTimeD).set_Location(new Point(96, 37));
			((Control)lblInternalTimeD).set_Name("lblInternalTimeD");
			((Control)lblInternalTimeD).set_Size(new Size(81, 13));
			((Control)lblInternalTimeD).set_TabIndex(2);
			((Control)lblInternalTimeD).set_Text("hh mm ss.sss");
			((Control)lblInternalTimeR).set_AutoSize(true);
			((Control)lblInternalTimeR).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblInternalTimeR).set_Location(new Point(96, 37));
			((Control)lblInternalTimeR).set_Name("lblInternalTimeR");
			((Control)lblInternalTimeR).set_Size(new Size(81, 13));
			((Control)lblInternalTimeR).set_TabIndex(3);
			((Control)lblInternalTimeR).set_Text("hh mm ss.sss");
			updnCorrnD.set_DecimalPlaces(3);
			updnCorrnD.set_Increment(new decimal(new int[4] { 1, 0, 0, 196608 }));
			((Control)updnCorrnD).set_Location(new Point(99, 53));
			updnCorrnD.set_Maximum(new decimal(new int[4] { 86400, 0, 0, 0 }));
			updnCorrnD.set_Minimum(new decimal(new int[4] { 86400, 0, 0, -2147483648 }));
			((Control)updnCorrnD).set_Name("updnCorrnD");
			((Control)updnCorrnD).set_Size(new Size(75, 20));
			((Control)updnCorrnD).set_TabIndex(4);
			updnCorrnD.add_ValueChanged((EventHandler)updnCorrnD_ValueChanged);
			updnCorrnR.set_DecimalPlaces(3);
			updnCorrnR.set_Increment(new decimal(new int[4] { 1, 0, 0, 196608 }));
			((Control)updnCorrnR).set_Location(new Point(99, 53));
			updnCorrnR.set_Maximum(new decimal(new int[4] { 86400, 0, 0, 0 }));
			updnCorrnR.set_Minimum(new decimal(new int[4] { 86400, 0, 0, -2147483648 }));
			((Control)updnCorrnR).set_Name("updnCorrnR");
			((Control)updnCorrnR).set_Size(new Size(75, 20));
			((Control)updnCorrnR).set_TabIndex(5);
			updnCorrnR.add_ValueChanged((EventHandler)updnCorrnR_ValueChanged);
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[1] { (ToolStripItem)helpToolStripMenuItem });
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(212, 24));
			((Control)menuStrip1).set_TabIndex(6);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(66, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("Help  ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Location(new Point(8, 57));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(86, 13));
			((Control)label3).set_TabIndex(7);
			((Control)label3).set_Text("Correction (secs)");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Location(new Point(8, 57));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(86, 13));
			((Control)label4).set_TabIndex(8);
			((Control)label4).set_Text("Correction (secs)");
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Location(new Point(8, 37));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(73, 13));
			((Control)label5).set_TabIndex(9);
			((Control)label5).set_Text("Time in AOTA");
			((Control)grpR).get_Controls().Add((Control)(object)updnCorrnR);
			((Control)grpR).get_Controls().Add((Control)(object)lblFrameR);
			((Control)grpR).get_Controls().Add((Control)(object)cmdDisplayR);
			((Control)grpR).get_Controls().Add((Control)(object)lblVTI_R);
			((Control)grpR).get_Controls().Add((Control)(object)label8);
			((Control)grpR).get_Controls().Add((Control)(object)label6);
			((Control)grpR).get_Controls().Add((Control)(object)label4);
			((Control)grpR).get_Controls().Add((Control)(object)lblInternalTimeR);
			((Control)grpR).get_Controls().Add((Control)(object)label2);
			((Control)grpR).set_Location(new Point(10, 168));
			((Control)grpR).set_Name("grpR");
			((Control)grpR).set_Size(new Size(189, 98));
			((Control)grpR).set_TabIndex(10);
			grpR.set_TabStop(false);
			((Control)grpR).set_Text("R event");
			((Control)lblFrameR).set_AutoSize(true);
			((Control)lblFrameR).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblFrameR).set_Location(new Point(54, 17));
			((Control)lblFrameR).set_Name("lblFrameR");
			((Control)lblFrameR).set_Size(new Size(16, 13));
			((Control)lblFrameR).set_TabIndex(14);
			((Control)lblFrameR).set_Text("R");
			((Control)cmdDisplayR).set_Location(new Point(104, 12));
			((Control)cmdDisplayR).set_Name("cmdDisplayR");
			((Control)cmdDisplayR).set_Size(new Size(81, 22));
			((Control)cmdDisplayR).set_TabIndex(13);
			((Control)cmdDisplayR).set_Text("Display frame");
			((ButtonBase)cmdDisplayR).set_UseVisualStyleBackColor(true);
			((Control)cmdDisplayR).add_Click((EventHandler)cmdDisplayR_Click);
			((Control)lblVTI_R).set_AutoSize(true);
			((Control)lblVTI_R).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblVTI_R).set_Location(new Point(96, 77));
			((Control)lblVTI_R).set_Name("lblVTI_R");
			((Control)lblVTI_R).set_Size(new Size(81, 13));
			((Control)lblVTI_R).set_TabIndex(11);
			((Control)lblVTI_R).set_Text("hh mm ss.sss");
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Location(new Point(8, 77));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(84, 13));
			((Control)label8).set_TabIndex(10);
			((Control)label8).set_Text("VTI Time stamp ");
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Location(new Point(8, 17));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(46, 13));
			((Control)label6).set_TabIndex(9);
			((Control)label6).set_Text("Frame #");
			((Control)grpD).get_Controls().Add((Control)(object)updnCorrnD);
			((Control)grpD).get_Controls().Add((Control)(object)lblFrameD);
			((Control)grpD).get_Controls().Add((Control)(object)cmdDisplayD);
			((Control)grpD).get_Controls().Add((Control)(object)lblVTI_D);
			((Control)grpD).get_Controls().Add((Control)(object)label10);
			((Control)grpD).get_Controls().Add((Control)(object)label5);
			((Control)grpD).get_Controls().Add((Control)(object)label3);
			((Control)grpD).get_Controls().Add((Control)(object)lblInternalTimeD);
			((Control)grpD).get_Controls().Add((Control)(object)label1);
			((Control)grpD).set_Location(new Point(10, 69));
			((Control)grpD).set_Name("grpD");
			((Control)grpD).set_Size(new Size(189, 98));
			((Control)grpD).set_TabIndex(11);
			grpD.set_TabStop(false);
			((Control)grpD).set_Text("D event");
			((Control)lblFrameD).set_AutoSize(true);
			((Control)lblFrameD).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblFrameD).set_Location(new Point(54, 17));
			((Control)lblFrameD).set_Name("lblFrameD");
			((Control)lblFrameD).set_Size(new Size(16, 13));
			((Control)lblFrameD).set_TabIndex(13);
			((Control)lblFrameD).set_Text("D");
			((Control)cmdDisplayD).set_Location(new Point(104, 12));
			((Control)cmdDisplayD).set_Name("cmdDisplayD");
			((Control)cmdDisplayD).set_Size(new Size(81, 22));
			((Control)cmdDisplayD).set_TabIndex(12);
			((Control)cmdDisplayD).set_Text("Display frame");
			((ButtonBase)cmdDisplayD).set_UseVisualStyleBackColor(true);
			((Control)cmdDisplayD).add_Click((EventHandler)cmdDisplayD_Click);
			((Control)lblVTI_D).set_AutoSize(true);
			((Control)lblVTI_D).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblVTI_D).set_Location(new Point(96, 77));
			((Control)lblVTI_D).set_Name("lblVTI_D");
			((Control)lblVTI_D).set_Size(new Size(81, 13));
			((Control)lblVTI_D).set_TabIndex(11);
			((Control)lblVTI_D).set_Text("hh mm ss.sss");
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Location(new Point(8, 77));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(84, 13));
			((Control)label10).set_TabIndex(10);
			((Control)label10).set_Text("VTI Time stamp ");
			((Control)label11).set_AutoSize(true);
			label11.set_BorderStyle((BorderStyle)1);
			((Control)label11).set_Location(new Point(10, 25));
			((Control)label11).set_Name("label11");
			((Control)label11).set_Size(new Size(191, 41));
			((Control)label11).set_TabIndex(12);
			((Control)label11).set_Text("Adjust the correction value so that the \r\nVTI time stamp corresponds to the \r\nrelevant time stamp on the VTI");
			((Control)cmdCancel).set_Location(new Point(10, 276));
			((Control)cmdCancel).set_Name("cmdCancel");
			((Control)cmdCancel).set_Size(new Size(56, 22));
			((Control)cmdCancel).set_TabIndex(13);
			((Control)cmdCancel).set_Text("Cancel");
			((ButtonBase)cmdCancel).set_UseVisualStyleBackColor(true);
			((Control)cmdCancel).add_Click((EventHandler)cmdCancel_Click);
			((Control)cmdExit).set_Location(new Point(87, 276));
			((Control)cmdExit).set_Name("cmdExit");
			((Control)cmdExit).set_Size(new Size(112, 22));
			((Control)cmdExit).set_TabIndex(14);
			((Control)cmdExit).set_Text("Exit with Corrections");
			((ButtonBase)cmdExit).set_UseVisualStyleBackColor(true);
			((Control)cmdExit).add_Click((EventHandler)cmdExit_Click);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(212, 303));
			((Form)this).set_ControlBox(false);
			((Control)this).get_Controls().Add((Control)(object)cmdExit);
			((Control)this).get_Controls().Add((Control)(object)cmdCancel);
			((Control)this).get_Controls().Add((Control)(object)label11);
			((Control)this).get_Controls().Add((Control)(object)grpD);
			((Control)this).get_Controls().Add((Control)(object)grpR);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_Name("VerifyTimes");
			((Control)this).set_Text("AOTA: Verify Times, Event #0");
			((Form)this).add_Load((EventHandler)VerifyTimes_Load);
			((Form)this).add_FormClosing(new FormClosingEventHandler(VerifyTimes_FormClosing));
			((ISupportInitialize)updnCorrnD).EndInit();
			((ISupportInitialize)updnCorrnR).EndInit();
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)grpR).ResumeLayout(false);
			((Control)grpR).PerformLayout();
			((Control)grpD).ResumeLayout(false);
			((Control)grpD).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
