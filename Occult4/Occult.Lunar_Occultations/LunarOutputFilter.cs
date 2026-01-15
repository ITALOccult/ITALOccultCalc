using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Lunar_Occultations
{
	public class LunarOutputFilter : Form
	{
		private IContainer components;

		private CheckBox chkExcludeBrightLimb;

		private CheckBox chkExcludeDayTime;

		private CheckBox chkStarName;

		private CheckBox chkVariableStar;

		private CheckBox chkDoubleStar;

		private CheckBox chkBrightLimb;

		private GroupBox grpMessages;

		private CheckBox chkTimeRange;

		private NumericUpDown numericUpDown1;

		private NumericUpDown numericUpDown2;

		private Label label1;

		private Label label2;

		private Label label3;

		private Label label4;

		private Label label5;

		private GroupBox grpEvents;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private CheckBox chkDurations;

		private Button cmdClear;

		private CheckBox chkGrazeMessage;

		private Button cmdExit;

		private CheckBox chkIncludeK2_only;

		private CheckBox chkExclude_LightCurves;

		private CheckBox chkExclude_Altitude;

		private Label label6;

		private NumericUpDown updnLunarAltitude;

		public LunarOutputFilter()
		{
			InitializeComponent();
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Lunar Occultations Filter");
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private void cmdClear_Click(object sender, EventArgs e)
		{
			CheckBox obj = chkExclude_LightCurves;
			CheckBox obj2 = chkIncludeK2_only;
			CheckBox obj3 = chkBrightLimb;
			CheckBox obj4 = chkDoubleStar;
			CheckBox obj5 = chkDurations;
			CheckBox obj6 = chkExcludeBrightLimb;
			CheckBox obj7 = chkExcludeDayTime;
			CheckBox obj8 = chkStarName;
			CheckBox obj9 = chkTimeRange;
			CheckBox obj10 = chkVariableStar;
			bool flag;
			chkGrazeMessage.set_Checked(flag = false);
			bool flag2;
			obj10.set_Checked(flag2 = flag);
			bool flag3;
			obj9.set_Checked(flag3 = flag2);
			bool flag4;
			obj8.set_Checked(flag4 = flag3);
			bool flag5;
			obj7.set_Checked(flag5 = flag4);
			bool flag6;
			obj6.set_Checked(flag6 = flag5);
			bool flag7;
			obj5.set_Checked(flag7 = flag6);
			bool flag8;
			obj4.set_Checked(flag8 = flag7);
			bool flag9;
			obj3.set_Checked(flag9 = flag8);
			bool @checked;
			obj2.set_Checked(@checked = flag9);
			obj.set_Checked(@checked);
		}

		private void cmdExit_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private void LunarOutputFilter_Load(object sender, EventArgs e)
		{
			((Control)this).set_Text(((Control)this).get_Text() + " : v." + Utilities.OccultVersion_Short);
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
			//IL_08d0: Unknown result type (might be due to invalid IL or missing references)
			//IL_08da: Expected O, but got Unknown
			//IL_097e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0988: Expected O, but got Unknown
			//IL_0a2c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a36: Expected O, but got Unknown
			//IL_0ab6: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ac0: Expected O, but got Unknown
			//IL_0b54: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b5e: Expected O, but got Unknown
			//IL_0c13: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c1d: Expected O, but got Unknown
			//IL_0cbd: Unknown result type (might be due to invalid IL or missing references)
			//IL_0cc7: Expected O, but got Unknown
			//IL_0d67: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d71: Expected O, but got Unknown
			//IL_0e11: Unknown result type (might be due to invalid IL or missing references)
			//IL_0e1b: Expected O, but got Unknown
			//IL_0ebe: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ec8: Expected O, but got Unknown
			//IL_0f68: Unknown result type (might be due to invalid IL or missing references)
			//IL_0f72: Expected O, but got Unknown
			//IL_1012: Unknown result type (might be due to invalid IL or missing references)
			//IL_101c: Expected O, but got Unknown
			//IL_10bc: Unknown result type (might be due to invalid IL or missing references)
			//IL_10c6: Expected O, but got Unknown
			//IL_1166: Unknown result type (might be due to invalid IL or missing references)
			//IL_1170: Expected O, but got Unknown
			//IL_11ef: Unknown result type (might be due to invalid IL or missing references)
			//IL_11f9: Expected O, but got Unknown
			grpMessages = new GroupBox();
			label1 = new Label();
			label2 = new Label();
			label3 = new Label();
			label4 = new Label();
			label5 = new Label();
			grpEvents = new GroupBox();
			menuStrip1 = new MenuStrip();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			cmdClear = new Button();
			cmdExit = new Button();
			chkExclude_Altitude = new CheckBox();
			chkExclude_LightCurves = new CheckBox();
			chkIncludeK2_only = new CheckBox();
			numericUpDown2 = new NumericUpDown();
			numericUpDown1 = new NumericUpDown();
			chkTimeRange = new CheckBox();
			chkExcludeDayTime = new CheckBox();
			chkExcludeBrightLimb = new CheckBox();
			chkGrazeMessage = new CheckBox();
			chkDurations = new CheckBox();
			chkBrightLimb = new CheckBox();
			chkDoubleStar = new CheckBox();
			chkVariableStar = new CheckBox();
			chkStarName = new CheckBox();
			updnLunarAltitude = new NumericUpDown();
			label6 = new Label();
			((Control)grpMessages).SuspendLayout();
			((Control)grpEvents).SuspendLayout();
			((Control)menuStrip1).SuspendLayout();
			((ISupportInitialize)numericUpDown2).BeginInit();
			((ISupportInitialize)numericUpDown1).BeginInit();
			((ISupportInitialize)updnLunarAltitude).BeginInit();
			((Control)this).SuspendLayout();
			((Control)grpMessages).get_Controls().Add((Control)(object)chkGrazeMessage);
			((Control)grpMessages).get_Controls().Add((Control)(object)chkDurations);
			((Control)grpMessages).get_Controls().Add((Control)(object)chkBrightLimb);
			((Control)grpMessages).get_Controls().Add((Control)(object)chkDoubleStar);
			((Control)grpMessages).get_Controls().Add((Control)(object)chkVariableStar);
			((Control)grpMessages).get_Controls().Add((Control)(object)chkStarName);
			((Control)grpMessages).set_Location(new Point(13, 259));
			((Control)grpMessages).set_Name("grpMessages");
			((Control)grpMessages).set_Size(new Size(221, 173));
			((Control)grpMessages).set_TabIndex(7);
			grpMessages.set_TabStop(false);
			((Control)grpMessages).set_Text("Messages to be excluded");
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(191, 25));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(25, 13));
			((Control)label1).set_TabIndex(11);
			((Control)label1).set_Text("and");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(256, 25));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(51, 13));
			((Control)label2).set_TabIndex(12);
			((Control)label2).set_Text("hours UT");
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label3).set_Location(new Point(150, 8));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(27, 13));
			((Control)label3).set_TabIndex(13);
			((Control)label3).set_Text("start");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label4).set_Location(new Point(216, 8));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(25, 13));
			((Control)label4).set_TabIndex(14);
			((Control)label4).set_Text("end");
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label5).set_Location(new Point(29, 43));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(306, 13));
			((Control)label5).set_TabIndex(15);
			((Control)label5).set_Text("For a range to include 0hrs UT,  Start  must be greater than  End");
			((Control)grpEvents).get_Controls().Add((Control)(object)label6);
			((Control)grpEvents).get_Controls().Add((Control)(object)updnLunarAltitude);
			((Control)grpEvents).get_Controls().Add((Control)(object)chkExclude_Altitude);
			((Control)grpEvents).get_Controls().Add((Control)(object)chkExclude_LightCurves);
			((Control)grpEvents).get_Controls().Add((Control)(object)chkIncludeK2_only);
			((Control)grpEvents).get_Controls().Add((Control)(object)label5);
			((Control)grpEvents).get_Controls().Add((Control)(object)label4);
			((Control)grpEvents).get_Controls().Add((Control)(object)label3);
			((Control)grpEvents).get_Controls().Add((Control)(object)label2);
			((Control)grpEvents).get_Controls().Add((Control)(object)label1);
			((Control)grpEvents).get_Controls().Add((Control)(object)numericUpDown2);
			((Control)grpEvents).get_Controls().Add((Control)(object)numericUpDown1);
			((Control)grpEvents).get_Controls().Add((Control)(object)chkTimeRange);
			((Control)grpEvents).get_Controls().Add((Control)(object)chkExcludeDayTime);
			((Control)grpEvents).get_Controls().Add((Control)(object)chkExcludeBrightLimb);
			((Control)grpEvents).set_Location(new Point(13, 40));
			((Control)grpEvents).set_Name("grpEvents");
			((Control)grpEvents).set_Size(new Size(354, 200));
			((Control)grpEvents).set_TabIndex(16);
			grpEvents.set_TabStop(false);
			((Control)grpEvents).set_Text("Events to be excluded");
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(375, 24));
			((Control)menuStrip1).set_TabIndex(17);
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
			((Control)cmdClear).set_Location(new Point(276, 284));
			((Control)cmdClear).set_Name("cmdClear");
			((Control)cmdClear).set_Size(new Size(58, 37));
			((Control)cmdClear).set_TabIndex(18);
			((Control)cmdClear).set_Text("Clear all\r\nchecks");
			((ButtonBase)cmdClear).set_UseVisualStyleBackColor(true);
			((Control)cmdClear).add_Click((EventHandler)cmdClear_Click);
			((Control)cmdExit).set_Location(new Point(276, 354));
			((Control)cmdExit).set_Name("cmdExit");
			((Control)cmdExit).set_Size(new Size(58, 25));
			((Control)cmdExit).set_TabIndex(19);
			((Control)cmdExit).set_Text("Exit");
			((ButtonBase)cmdExit).set_UseVisualStyleBackColor(true);
			((Control)cmdExit).add_Click((EventHandler)cmdExit_Click);
			((Control)chkExclude_Altitude).set_AutoSize(true);
			chkExclude_Altitude.set_Checked(Settings.Default.LunarFilter_ApplyAlt);
			((Control)chkExclude_Altitude).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "LunarFilter_ApplyAlt", true, (DataSourceUpdateMode)1));
			((Control)chkExclude_Altitude).set_Location(new Point(13, 158));
			((Control)chkExclude_Altitude).set_Name("chkExclude_Altitude");
			((Control)chkExclude_Altitude).set_Size(new Size(174, 17));
			((Control)chkExclude_Altitude).set_TabIndex(18);
			((Control)chkExclude_Altitude).set_Text("Exclude of altittude is less than ");
			((ButtonBase)chkExclude_Altitude).set_UseVisualStyleBackColor(true);
			((Control)chkExclude_LightCurves).set_AutoSize(true);
			chkExclude_LightCurves.set_Checked(Settings.Default.LunarFilter_NoLightCurve);
			((Control)chkExclude_LightCurves).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "LunarFilter_NoLightCurve", true, (DataSourceUpdateMode)1));
			((Control)chkExclude_LightCurves).set_Location(new Point(13, 135));
			((Control)chkExclude_LightCurves).set_Name("chkExclude_LightCurves");
			((Control)chkExclude_LightCurves).set_Size(new Size(184, 17));
			((Control)chkExclude_LightCurves).set_TabIndex(17);
			((Control)chkExclude_LightCurves).set_Text("Exclude if no light curve available");
			((ButtonBase)chkExclude_LightCurves).set_UseVisualStyleBackColor(true);
			((Control)chkIncludeK2_only).set_AutoSize(true);
			chkIncludeK2_only.set_Checked(Settings.Default.LunarFilter_ExcludeUnlessK2);
			((Control)chkIncludeK2_only).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "LunarFilter_ExcludeUnlessK2", true, (DataSourceUpdateMode)1));
			((Control)chkIncludeK2_only).set_Location(new Point(13, 112));
			((Control)chkIncludeK2_only).set_Name("chkIncludeK2_only");
			((Control)chkIncludeK2_only).set_Size(new Size(311, 17));
			((Control)chkIncludeK2_only).set_TabIndex(16);
			((Control)chkIncludeK2_only).set_Text("Exclude events that do not involve a double star or a K2 star");
			((ButtonBase)chkIncludeK2_only).set_UseVisualStyleBackColor(true);
			((Control)numericUpDown2).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "LunarFilter_Tend", true, (DataSourceUpdateMode)1));
			((Control)numericUpDown2).set_Location(new Point(216, 21));
			numericUpDown2.set_Maximum(new decimal(new int[4] { 24, 0, 0, 0 }));
			((Control)numericUpDown2).set_Name("numericUpDown2");
			((Control)numericUpDown2).set_Size(new Size(40, 20));
			((Control)numericUpDown2).set_TabIndex(10);
			numericUpDown2.set_Value(Settings.Default.LunarFilter_Tend);
			((Control)numericUpDown1).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "LunarFilter_Tstart", true, (DataSourceUpdateMode)1));
			((Control)numericUpDown1).set_Location(new Point(151, 21));
			numericUpDown1.set_Maximum(new decimal(new int[4] { 24, 0, 0, 0 }));
			((Control)numericUpDown1).set_Name("numericUpDown1");
			((Control)numericUpDown1).set_Size(new Size(40, 20));
			((Control)numericUpDown1).set_TabIndex(9);
			numericUpDown1.set_Value(Settings.Default.LunarFilter_Tstart);
			((Control)chkTimeRange).set_AutoSize(true);
			chkTimeRange.set_Checked(Settings.Default.LunarFilter_TimeRange);
			((Control)chkTimeRange).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "LunarFilter_TimeRange", true, (DataSourceUpdateMode)1));
			((Control)chkTimeRange).set_Location(new Point(13, 23));
			((Control)chkTimeRange).set_Name("chkTimeRange");
			((Control)chkTimeRange).set_Size(new Size(138, 17));
			((Control)chkTimeRange).set_TabIndex(8);
			((Control)chkTimeRange).set_Text("Limit events to between");
			((ButtonBase)chkTimeRange).set_UseVisualStyleBackColor(true);
			((Control)chkExcludeDayTime).set_AutoSize(true);
			chkExcludeDayTime.set_Checked(Settings.Default.LunarFilter_NoDaytime);
			((Control)chkExcludeDayTime).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "LunarFilter_NoDaytime", true, (DataSourceUpdateMode)1));
			((Control)chkExcludeDayTime).set_Location(new Point(13, 65));
			((Control)chkExcludeDayTime).set_Name("chkExcludeDayTime");
			((Control)chkExcludeDayTime).set_Size(new Size(141, 17));
			((Control)chkExcludeDayTime).set_TabIndex(1);
			((Control)chkExcludeDayTime).set_Text("Exclude day-time events");
			((ButtonBase)chkExcludeDayTime).set_UseVisualStyleBackColor(true);
			((Control)chkExcludeBrightLimb).set_AutoSize(true);
			chkExcludeBrightLimb.set_Checked(Settings.Default.LunarFilter_NoBrightLimb);
			((Control)chkExcludeBrightLimb).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "LunarFilter_NoBrightLimb", true, (DataSourceUpdateMode)1));
			((Control)chkExcludeBrightLimb).set_Location(new Point(13, 88));
			((Control)chkExcludeBrightLimb).set_Name("chkExcludeBrightLimb");
			((Control)chkExcludeBrightLimb).set_Size(new Size(149, 17));
			((Control)chkExcludeBrightLimb).set_TabIndex(0);
			((Control)chkExcludeBrightLimb).set_Text("Exclude bright limb events");
			((ButtonBase)chkExcludeBrightLimb).set_UseVisualStyleBackColor(true);
			((Control)chkGrazeMessage).set_AutoSize(true);
			chkGrazeMessage.set_Checked(Settings.Default.LunarFilter_ExcludeGrazeMessage);
			((Control)chkGrazeMessage).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "LunarFilter_ExcludeGrazeMessage", true, (DataSourceUpdateMode)1));
			((Control)chkGrazeMessage).set_Location(new Point(13, 146));
			((Control)chkGrazeMessage).set_Name("chkGrazeMessage");
			((Control)chkGrazeMessage).set_Size(new Size(184, 17));
			((Control)chkGrazeMessage).set_TabIndex(8);
			((Control)chkGrazeMessage).set_Text("Exclude 'Graze nearby' messages");
			((ButtonBase)chkGrazeMessage).set_UseVisualStyleBackColor(true);
			((Control)chkDurations).set_AutoSize(true);
			chkDurations.set_Checked(Settings.Default.LunarFilter_ExcludeDurations);
			((Control)chkDurations).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "LunarFilter_ExcludeDurations", true, (DataSourceUpdateMode)1));
			((Control)chkDurations).set_Location(new Point(13, 121));
			((Control)chkDurations).set_Name("chkDurations");
			((Control)chkDurations).set_Size(new Size(206, 17));
			((Control)chkDurations).set_TabIndex(7);
			((Control)chkDurations).set_Text("Exclude 'Planetary duration' messages");
			((ButtonBase)chkDurations).set_UseVisualStyleBackColor(true);
			((Control)chkBrightLimb).set_AutoSize(true);
			chkBrightLimb.set_Checked(Settings.Default.LunarFilter_BrightLimbMessage);
			((Control)chkBrightLimb).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "LunarFilter_BrightLimbMessage", true, (DataSourceUpdateMode)1));
			((Control)chkBrightLimb).set_Location(new Point(13, 21));
			((Control)chkBrightLimb).set_Name("chkBrightLimb");
			((Control)chkBrightLimb).set_Size(new Size(204, 17));
			((Control)chkBrightLimb).set_TabIndex(6);
			((Control)chkBrightLimb).set_Text("Exclude 'Bright limb nearby' messages");
			((ButtonBase)chkBrightLimb).set_UseVisualStyleBackColor(true);
			((Control)chkDoubleStar).set_AutoSize(true);
			chkDoubleStar.set_Checked(Settings.Default.LunarFilter_DoubleStarMessage);
			((Control)chkDoubleStar).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "LunarFilter_DoubleStarMessage", true, (DataSourceUpdateMode)1));
			((Control)chkDoubleStar).set_Location(new Point(13, 71));
			((Control)chkDoubleStar).set_Name("chkDoubleStar");
			((Control)chkDoubleStar).set_Size(new Size(175, 17));
			((Control)chkDoubleStar).set_TabIndex(5);
			((Control)chkDoubleStar).set_Text("Exclude 'Double star' messages");
			((ButtonBase)chkDoubleStar).set_UseVisualStyleBackColor(true);
			((Control)chkVariableStar).set_AutoSize(true);
			chkVariableStar.set_Checked(Settings.Default.LunarFilter_VariableStarMessage);
			((Control)chkVariableStar).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "LunarFilter_VariableStarMessage", true, (DataSourceUpdateMode)1));
			((Control)chkVariableStar).set_Location(new Point(13, 96));
			((Control)chkVariableStar).set_Name("chkVariableStar");
			((Control)chkVariableStar).set_Size(new Size(179, 17));
			((Control)chkVariableStar).set_TabIndex(4);
			((Control)chkVariableStar).set_Text("Exclude 'Variable star' messages");
			((ButtonBase)chkVariableStar).set_UseVisualStyleBackColor(true);
			((Control)chkStarName).set_AutoSize(true);
			chkStarName.set_Checked(Settings.Default.LunarFilter_StarNameMessage);
			((Control)chkStarName).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "LunarFilter_StarNameMessage", true, (DataSourceUpdateMode)1));
			((Control)chkStarName).set_Location(new Point(13, 46));
			((Control)chkStarName).set_Name("chkStarName");
			((Control)chkStarName).set_Size(new Size(169, 17));
			((Control)chkStarName).set_TabIndex(3);
			((Control)chkStarName).set_Text("Exclude 'Star name' messages");
			((ButtonBase)chkStarName).set_UseVisualStyleBackColor(true);
			((Control)updnLunarAltitude).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "LunarFilter_MinimumAltitude", true, (DataSourceUpdateMode)1));
			((Control)updnLunarAltitude).set_Location(new Point(187, 156));
			((Control)updnLunarAltitude).set_Name("updnLunarAltitude");
			((Control)updnLunarAltitude).set_Size(new Size(40, 20));
			((Control)updnLunarAltitude).set_TabIndex(19);
			((UpDownBase)updnLunarAltitude).set_TextAlign((HorizontalAlignment)2);
			updnLunarAltitude.set_Value(Settings.Default.LunarFilter_MinimumAltitude);
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Location(new Point(227, 160));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(45, 13));
			((Control)label6).set_TabIndex(20);
			((Control)label6).set_Text("degrees");
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(375, 452));
			((Control)this).get_Controls().Add((Control)(object)cmdExit);
			((Control)this).get_Controls().Add((Control)(object)cmdClear);
			((Control)this).get_Controls().Add((Control)(object)grpEvents);
			((Control)this).get_Controls().Add((Control)(object)grpMessages);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Form)this).set_FormBorderStyle((FormBorderStyle)2);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Form)this).set_MaximizeBox(false);
			((Form)this).set_MinimizeBox(false);
			((Control)this).set_Name("LunarOutputFilter");
			((Control)this).set_Text("Filter settings for prediction display");
			((Form)this).add_Load((EventHandler)LunarOutputFilter_Load);
			((Control)grpMessages).ResumeLayout(false);
			((Control)grpMessages).PerformLayout();
			((Control)grpEvents).ResumeLayout(false);
			((Control)grpEvents).PerformLayout();
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((ISupportInitialize)numericUpDown2).EndInit();
			((ISupportInitialize)numericUpDown1).EndInit();
			((ISupportInitialize)updnLunarAltitude).EndInit();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
