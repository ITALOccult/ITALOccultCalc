using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult
{
	public class LunarGrazeSelection : Form
	{
		private bool Grazes;

		private bool MultiLocation;

		private bool FeetChanging;

		private bool MetersChanging;

		private IContainer components;

		private ListBox lstStarSelection;

		private Label label2;

		private Label label1;

		private NumericUpDown updnEndLong;

		private NumericUpDown updnStartLong;

		private Label label4;

		private Label label3;

		private NumericUpDown updnAlt;

		private NumericUpDown updnStep;

		private Button cmdSaveAndExit;

		private RadioButton optNth;

		private RadioButton optSth;

		private Label label5;

		private Label label6;

		private Button cmdReset;

		private Panel panelGraze;

		private Label labelSelection;

		private Button cmdHelp;

		private NumericUpDown updnAltFt;

		private Label label7;

		public LunarGrazeSelection(bool ShowGrazePanel, bool Multi)
		{
			InitializeComponent();
			MultiLocation = Multi;
			optSth.set_Checked(!optNth.get_Checked());
			((Control)panelGraze).set_Visible(ShowGrazePanel);
			if (!ShowGrazePanel)
			{
				if (MultiLocation)
				{
					((Control)labelSelection).set_Text("Select star\r\nfor\r\nMulti-sites");
				}
				else
				{
					((Control)labelSelection).set_Text("Select star\r\nfor\r\nWorld map");
				}
			}
			Grazes = ShowGrazePanel;
		}

		private void LunarGrazeSelection_Load(object sender, EventArgs e)
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
			lstStarSelection.get_Items().Clear();
			for (int i = 0; i < LunarOccultations.Elements.Count; i++)
			{
				lstStarSelection.get_Items().Add((object)(LunarOccultations.Elements[i].StarId + string.Format(" {0,4:F1}", LunarOccultations.Elements[i].Mv)));
			}
			if (lstStarSelection.get_Items().get_Count() == 1)
			{
				((ListControl)lstStarSelection).set_SelectedIndex(0);
			}
			updnAlt_ValueChanged(sender, e);
		}

		private void cmdSaveAndExit_Click(object sender, EventArgs e)
		{
			//IL_00ad: Unknown result type (might be due to invalid IL or missing references)
			if (Grazes)
			{
				LunarOccultations.GrazeStartLongitude = (double)updnStartLong.get_Value();
				LunarOccultations.GrazeEndLongitude = (double)updnEndLong.get_Value();
				LunarOccultations.GrazeLongitudeStep = (double)updnStep.get_Value();
				LunarOccultations.GrazeNominalAltitude = (double)updnAlt.get_Value();
				if (optNth.get_Checked())
				{
					LunarOccultations.GrazeLimit = 1;
				}
				else
				{
					LunarOccultations.GrazeLimit = -1;
				}
				LunarOccultations.GrazeSelectedStar = ((ListControl)lstStarSelection).get_SelectedIndex();
				if (LunarOccultations.GrazeEndLongitude - LunarOccultations.GrazeStartLongitude < LunarOccultations.GrazeLongitudeStep)
				{
					MessageBox.Show("The End longitude must be greater that the Start longitude\r\n\r\nPlease correct the entries.", "Incorrect data entry", (MessageBoxButtons)0, (MessageBoxIcon)48);
					return;
				}
			}
			else if (MultiLocation)
			{
				LunarOccultations.MultiLocationStar = ((ListControl)lstStarSelection).get_SelectedIndex();
			}
			else
			{
				LunarOccultations.MapSelectedStar = ((ListControl)lstStarSelection).get_SelectedIndex();
			}
			((Form)this).Close();
		}

		private void lstStarSelection_MouseDoubleClick(object sender, MouseEventArgs e)
		{
			cmdSaveAndExit_Click(sender, (EventArgs)(object)e);
		}

		private void cmdReset_Click(object sender, EventArgs e)
		{
			updnStartLong.set_Value(Settings.Default.Graze_DefaultStartLongitude);
			updnEndLong.set_Value(Settings.Default.Graze_DefaultEndLongitude);
			updnStep.set_Value(Settings.Default.Graze_DefaultStep);
			updnAlt.set_Value(Settings.Default.Graze_DefaultAltitude);
		}

		private void cmdHelp_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Lunar Occultations - Select star");
		}

		private void updnStartLong_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnStartLong).Select(0, 10);
		}

		private void updnEndLong_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnEndLong).Select(0, 10);
		}

		private void updnStep_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnStep).Select(0, 10);
		}

		private void updnAlt_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnAlt).Select(0, 10);
		}

		private void updnAlt_ValueChanged(object sender, EventArgs e)
		{
			if (!FeetChanging)
			{
				MetersChanging = true;
				try
				{
					updnAltFt.set_Value((decimal)(3.2808 * (double)updnAlt.get_Value()));
				}
				catch
				{
				}
				MetersChanging = false;
			}
		}

		private void updnAltFt_ValueChanged(object sender, EventArgs e)
		{
			if (!MetersChanging)
			{
				FeetChanging = true;
				try
				{
					updnAlt.set_Value((decimal)(0.3048 * (double)updnAltFt.get_Value()));
				}
				catch
				{
				}
				FeetChanging = false;
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
			//IL_01d0: Unknown result type (might be due to invalid IL or missing references)
			//IL_01da: Expected O, but got Unknown
			//IL_0428: Unknown result type (might be due to invalid IL or missing references)
			//IL_0432: Expected O, but got Unknown
			//IL_0533: Unknown result type (might be due to invalid IL or missing references)
			//IL_053d: Expected O, but got Unknown
			//IL_0643: Unknown result type (might be due to invalid IL or missing references)
			//IL_064d: Expected O, but got Unknown
			//IL_0747: Unknown result type (might be due to invalid IL or missing references)
			//IL_0751: Expected O, but got Unknown
			//IL_0824: Unknown result type (might be due to invalid IL or missing references)
			//IL_082e: Expected O, but got Unknown
			//IL_0e8c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0e96: Expected O, but got Unknown
			lstStarSelection = new ListBox();
			label2 = new Label();
			label1 = new Label();
			label4 = new Label();
			label3 = new Label();
			cmdSaveAndExit = new Button();
			optNth = new RadioButton();
			optSth = new RadioButton();
			updnAlt = new NumericUpDown();
			updnStep = new NumericUpDown();
			updnEndLong = new NumericUpDown();
			updnStartLong = new NumericUpDown();
			label5 = new Label();
			label6 = new Label();
			cmdReset = new Button();
			panelGraze = new Panel();
			updnAltFt = new NumericUpDown();
			label7 = new Label();
			labelSelection = new Label();
			cmdHelp = new Button();
			((ISupportInitialize)updnAlt).BeginInit();
			((ISupportInitialize)updnStep).BeginInit();
			((ISupportInitialize)updnEndLong).BeginInit();
			((ISupportInitialize)updnStartLong).BeginInit();
			((Control)panelGraze).SuspendLayout();
			((ISupportInitialize)updnAltFt).BeginInit();
			((Control)this).SuspendLayout();
			lstStarSelection.set_ColumnWidth(150);
			((Control)lstStarSelection).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstStarSelection).set_FormattingEnabled(true);
			lstStarSelection.set_ItemHeight(14);
			((Control)lstStarSelection).set_Location(new Point(169, 30));
			lstStarSelection.set_MultiColumn(true);
			((Control)lstStarSelection).set_Name("lstStarSelection");
			((Control)lstStarSelection).set_Size(new Size(774, 536));
			((Control)lstStarSelection).set_TabIndex(1);
			((Control)lstStarSelection).add_MouseDoubleClick(new MouseEventHandler(lstStarSelection_MouseDoubleClick));
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(19, 138));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(72, 13));
			((Control)label2).set_TabIndex(5);
			((Control)label2).set_Text("End longitude");
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(16, 110));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(75, 13));
			((Control)label1).set_TabIndex(3);
			((Control)label1).set_Text("Start longitude");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Location(new Point(12, 194));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(79, 13));
			((Control)label4).set_TabIndex(9);
			((Control)label4).set_Text("Nominal alt. (m)");
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Location(new Point(25, 166));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(66, 13));
			((Control)label3).set_TabIndex(7);
			((Control)label3).set_Text("Step interval");
			((Control)cmdSaveAndExit).set_Location(new Point(36, 322));
			((Control)cmdSaveAndExit).set_Name("cmdSaveAndExit");
			((Control)cmdSaveAndExit).set_Size(new Size(78, 31));
			((Control)cmdSaveAndExit).set_TabIndex(3);
			((Control)cmdSaveAndExit).set_Text("&Return");
			((ButtonBase)cmdSaveAndExit).set_UseVisualStyleBackColor(true);
			((Control)cmdSaveAndExit).add_Click((EventHandler)cmdSaveAndExit_Click);
			((Control)optNth).set_AutoSize(true);
			optNth.set_CheckAlign(ContentAlignment.MiddleRight);
			optNth.set_Checked(Settings.Default.GrazeSelection_North);
			((Control)optNth).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "GrazeSelection_North", true, (DataSourceUpdateMode)1));
			((Control)optNth).set_Location(new Point(22, 64));
			((Control)optNth).set_Name("optNth");
			((Control)optNth).set_Size(new Size(86, 17));
			((Control)optNth).set_TabIndex(1);
			optNth.set_TabStop(true);
			((Control)optNth).set_Text("Northern limit");
			((ButtonBase)optNth).set_UseVisualStyleBackColor(true);
			((Control)optSth).set_AutoSize(true);
			optSth.set_CheckAlign(ContentAlignment.MiddleRight);
			((Control)optSth).set_Location(new Point(20, 83));
			((Control)optSth).set_Name("optSth");
			((Control)optSth).set_Size(new Size(88, 17));
			((Control)optSth).set_TabIndex(2);
			((Control)optSth).set_Text("Southern limit");
			((ButtonBase)optSth).set_UseVisualStyleBackColor(true);
			((Control)updnAlt).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "GrazeNominalAlt", true, (DataSourceUpdateMode)1));
			updnAlt.set_Increment(new decimal(new int[4] { 50, 0, 0, 0 }));
			((Control)updnAlt).set_Location(new Point(93, 190));
			updnAlt.set_Maximum(new decimal(new int[4] { 10000, 0, 0, 0 }));
			updnAlt.set_Minimum(new decimal(new int[4] { 300, 0, 0, -2147483648 }));
			((Control)updnAlt).set_Name("updnAlt");
			((Control)updnAlt).set_Size(new Size(56, 20));
			((Control)updnAlt).set_TabIndex(10);
			updnAlt.set_Value(Settings.Default.GrazeNominalAlt);
			updnAlt.add_ValueChanged((EventHandler)updnAlt_ValueChanged);
			((Control)updnAlt).add_Enter((EventHandler)updnAlt_Enter);
			((Control)updnStep).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "GrazeStepInterval", true, (DataSourceUpdateMode)1));
			updnStep.set_DecimalPlaces(3);
			updnStep.set_Increment(new decimal(new int[4] { 2, 0, 0, 65536 }));
			((Control)updnStep).set_Location(new Point(93, 162));
			updnStep.set_Maximum(new decimal(new int[4] { 10, 0, 0, 0 }));
			updnStep.set_Minimum(new decimal(new int[4] { 1, 0, 0, 131072 }));
			((Control)updnStep).set_Name("updnStep");
			((Control)updnStep).set_Size(new Size(56, 20));
			((Control)updnStep).set_TabIndex(8);
			updnStep.set_Value(Settings.Default.GrazeStepInterval);
			((Control)updnStep).add_Enter((EventHandler)updnStep_Enter);
			((Control)updnEndLong).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "GrazeEndLongitude", true, (DataSourceUpdateMode)1));
			((Control)updnEndLong).set_Location(new Point(93, 134));
			updnEndLong.set_Maximum(new decimal(new int[4] { 360, 0, 0, 0 }));
			updnEndLong.set_Minimum(new decimal(new int[4] { 200, 0, 0, -2147483648 }));
			((Control)updnEndLong).set_Name("updnEndLong");
			((Control)updnEndLong).set_Size(new Size(60, 20));
			((Control)updnEndLong).set_TabIndex(6);
			updnEndLong.set_Value(Settings.Default.GrazeEndLongitude);
			((Control)updnEndLong).add_Enter((EventHandler)updnEndLong_Enter);
			((Control)updnStartLong).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "GrazeStartLongitude", true, (DataSourceUpdateMode)1));
			((Control)updnStartLong).set_Location(new Point(93, 106));
			updnStartLong.set_Maximum(new decimal(new int[4] { 200, 0, 0, 0 }));
			updnStartLong.set_Minimum(new decimal(new int[4] { 360, 0, 0, -2147483648 }));
			((Control)updnStartLong).set_Name("updnStartLong");
			((Control)updnStartLong).set_Size(new Size(60, 20));
			((Control)updnStartLong).set_TabIndex(4);
			updnStartLong.set_Value(Settings.Default.GrazeStartLongitude);
			((Control)updnStartLong).add_Enter((EventHandler)updnStartLong_Enter);
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Font(new Font("Microsoft Sans Serif", 16f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label5).set_Location(new Point(17, 0));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(129, 52));
			((Control)label5).set_TabIndex(0);
			((Control)label5).set_Text("Graze path\r\nsettings");
			label5.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Location(new Point(391, 15));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(180, 13));
			((Control)label6).set_TabIndex(2);
			((Control)label6).set_Text("Double-click to select star and return");
			((Control)cmdReset).set_Location(new Point(28, 250));
			((Control)cmdReset).set_Name("cmdReset");
			((Control)cmdReset).set_Size(new Size(102, 22));
			((Control)cmdReset).set_TabIndex(13);
			((Control)cmdReset).set_Text("Reset to defaults");
			((ButtonBase)cmdReset).set_UseVisualStyleBackColor(true);
			((Control)cmdReset).add_Click((EventHandler)cmdReset_Click);
			((Control)panelGraze).get_Controls().Add((Control)(object)updnAltFt);
			((Control)panelGraze).get_Controls().Add((Control)(object)label7);
			((Control)panelGraze).get_Controls().Add((Control)(object)cmdReset);
			((Control)panelGraze).get_Controls().Add((Control)(object)label5);
			((Control)panelGraze).get_Controls().Add((Control)(object)optSth);
			((Control)panelGraze).get_Controls().Add((Control)(object)optNth);
			((Control)panelGraze).get_Controls().Add((Control)(object)label4);
			((Control)panelGraze).get_Controls().Add((Control)(object)label3);
			((Control)panelGraze).get_Controls().Add((Control)(object)updnAlt);
			((Control)panelGraze).get_Controls().Add((Control)(object)updnStep);
			((Control)panelGraze).get_Controls().Add((Control)(object)label2);
			((Control)panelGraze).get_Controls().Add((Control)(object)label1);
			((Control)panelGraze).get_Controls().Add((Control)(object)updnEndLong);
			((Control)panelGraze).get_Controls().Add((Control)(object)updnStartLong);
			((Control)panelGraze).set_Location(new Point(6, 30));
			((Control)panelGraze).set_Name("panelGraze");
			((Control)panelGraze).set_Size(new Size(155, 286));
			((Control)panelGraze).set_TabIndex(0);
			((Control)updnAltFt).set_Location(new Point(93, 211));
			updnAltFt.set_Maximum(new decimal(new int[4] { 32000, 0, 0, 0 }));
			updnAltFt.set_Minimum(new decimal(new int[4] { 900, 0, 0, -2147483648 }));
			((Control)updnAltFt).set_Name("updnAltFt");
			((Control)updnAltFt).set_Size(new Size(56, 20));
			((Control)updnAltFt).set_TabIndex(12);
			updnAltFt.add_ValueChanged((EventHandler)updnAltFt_ValueChanged);
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Location(new Point(43, 212));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(48, 13));
			((Control)label7).set_TabIndex(11);
			((Control)label7).set_Text("... alt. (ft)");
			((Control)labelSelection).set_AutoSize(true);
			((Control)labelSelection).set_Font(new Font("Microsoft Sans Serif", 16f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)labelSelection).set_Location(new Point(21, 35));
			((Control)labelSelection).set_Name("labelSelection");
			((Control)labelSelection).set_Size(new Size(127, 78));
			((Control)labelSelection).set_TabIndex(5);
			((Control)labelSelection).set_Text("Select star\r\nfor\r\nWorld map");
			labelSelection.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)cmdHelp).set_Location(new Point(48, 396));
			((Control)cmdHelp).set_Name("cmdHelp");
			((Control)cmdHelp).set_Size(new Size(54, 26));
			((Control)cmdHelp).set_TabIndex(4);
			((Control)cmdHelp).set_Text("&Help");
			((ButtonBase)cmdHelp).set_UseVisualStyleBackColor(true);
			((Control)cmdHelp).add_Click((EventHandler)cmdHelp_Click);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(955, 577));
			((Form)this).set_ControlBox(false);
			((Control)this).get_Controls().Add((Control)(object)cmdHelp);
			((Control)this).get_Controls().Add((Control)(object)label6);
			((Control)this).get_Controls().Add((Control)(object)cmdSaveAndExit);
			((Control)this).get_Controls().Add((Control)(object)lstStarSelection);
			((Control)this).get_Controls().Add((Control)(object)panelGraze);
			((Control)this).get_Controls().Add((Control)(object)labelSelection);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationLunarGrazeSelection", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_FormBorderStyle((FormBorderStyle)2);
			((Form)this).set_Location(Settings.Default.LocationLunarGrazeSelection);
			((Control)this).set_Name("LunarGrazeSelection");
			((Control)this).set_Text("Lunar Graze Selection");
			((Form)this).add_Load((EventHandler)LunarGrazeSelection_Load);
			((ISupportInitialize)updnAlt).EndInit();
			((ISupportInitialize)updnStep).EndInit();
			((ISupportInitialize)updnEndLong).EndInit();
			((ISupportInitialize)updnStartLong).EndInit();
			((Control)panelGraze).ResumeLayout(false);
			((Control)panelGraze).PerformLayout();
			((ISupportInitialize)updnAltFt).EndInit();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
