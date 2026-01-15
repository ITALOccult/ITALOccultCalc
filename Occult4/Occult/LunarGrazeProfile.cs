using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using Occult.Lunar_Occultations;
using Occult.Properties;

namespace Occult
{
	public class LunarGrazeProfile : Form
	{
		private const double Radian = 180.0 / Math.PI;

		private const double TwoPi = Math.PI * 2.0;

		private const double MoonRadius = 0.2725076;

		public static string AppPath;

		private static float OldX = 0f;

		private static float OldY = 0f;

		private static float OldArg0 = 0f;

		private static int CurrentTagRecord = -1;

		internal static List<LunarGrazeTags> Tags = new List<LunarGrazeTags>();

		private static bool FormCreated = false;

		private static bool GettingTag = false;

		private static bool ShowToolTip = true;

		private static bool GetToolTip = true;

		internal bool PlotIsBeingDrawn;

		private LunarListGrazesUsed GrazesUsed;

		private ListObservers CurrentObservers;

		private static double CurrentHeight;

		private IContainer components;

		private MenuStrip menuStrip1;

		public PictureBox picProfile;

		private ToolStripMenuItem observedGrazeoccultationDataToolStripMenuItem;

		private ToolStripMenuItem includeOrdinaryOccultationsToolStripMenuItem;

		private ToolStripMenuItem showEventsInColourToolStripMenuItem;

		private ToolStripMenuItem T0_ToolStripMenuItem;

		private ToolStripMenuItem T1_ToolStripMenuItem;

		private ToolStripMenuItem T2_ToolStripMenuItem;

		private ToolStripMenuItem T3_ToolStripMenuItem;

		private ToolStripMenuItem T4_ToolStripMenuItem;

		private ToolStripMenuItem T5_ToolStripMenuItem;

		private ToolStripMenuItem T6_ToolStripMenuItem;

		private ToolStripMenuItem T7_ToolStripMenuItem;

		private ToolStripMenuItem T8_ToolStripMenuItem;

		private ToolStripMenuItem T9_ToolStripMenuItem;

		private ToolStripMenuItem withProfileToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem printPreviewToolStripMenuItem;

		private ToolStripMenuItem printToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator1;

		private Label lblTag;

		private ToolStripMenuItem drawInColourToolStripMenuItem;

		private ToolStripMenuItem ListGrazesUsedMenuItem;

		private ToolTip toolTip;

		private ToolStripMenuItem showTimeAndHeightAtCursorToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator2;

		private ToolStripSeparator toolStripSeparator3;

		private ToolStripMenuItem exitToolStripMenuItem;

		private ContextMenuStrip contextHighlight;

		private ToolStripMenuItem highlightThisObserverToolStripMenuItem;

		private ToolStripMenuItem highlightThisStarToolStripMenuItem;

		private ToolStripMenuItem highlightThisDateToolStripMenuItem;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripMenuItem listObserversForPlottedEventsToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator4;

		private ToolStripSeparator toolStripSeparator5;

		private ToolStripMenuItem drawLineAtThisHeightToolStripMenuItem;

		private ToolStripMenuItem clearAllLinesToolStripMenuItem;

		private ToolStripMenuItem showEventsToolStripMenuItem;

		private ToolStripMenuItem showLOLAElevationDataPointsToolStripMenuItem;

		private ToolStripMenuItem showLOLAHighResolutionLimbToolStripMenuItem;

		private TrackBar tBar;

		private Panel panel1;

		private Label label1;

		private Label label2;

		private Label label3;

		private Label label4;

		private Label label5;

		private Label label6;

		private Label label7;

		private Label label10;

		private Label label8;

		private TrackBar tBarHorizontal;

		private Label label9;

		private ToolStripMenuItem showEventsByPathDistanceLowResolutionToolStripMenuItem;

		public LunarGrazeProfile()
		{
			InitializeComponent();
			FormCreated = false;
			AppPath = Utilities.AppPath;
		}

		private void LunarGrazeProfile_Load(object sender, EventArgs e)
		{
			((Control)this).set_Width(1000);
			((Control)this).set_Height(600);
			if (((Control)this).get_Top() < -30000)
			{
				((Control)this).set_Top(0);
			}
			if ((double)((Control)this).get_Left() + (double)((Control)this).get_Width() / 1.2 < (double)Screen.GetWorkingArea((Control)(object)this).Left)
			{
				((Control)this).set_Left(Screen.GetWorkingArea((Control)(object)this).Left);
			}
			SetResize();
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			FormCreated = false;
			includeOrdinaryOccultationsToolStripMenuItem.set_Checked(Settings.Default.GrazeProfile_LimitToGrazes);
			LunarProfile.LimitToGrazes = Settings.Default.GrazeProfile_LimitToGrazes;
			showEventsInColourToolStripMenuItem.set_Checked(Settings.Default.GrazeProfile_EventsInColour);
			LunarProfile.EventsInColour = Settings.Default.GrazeProfile_EventsInColour;
			drawInColourToolStripMenuItem.set_Checked(!Settings.Default.BWFlag);
			showTimeAndHeightAtCursorToolStripMenuItem.set_Checked(ShowToolTip);
			SetLibrationTags(Settings.Default.GrazeProfile_LibrationPlot);
			tBar.set_Value((int)(20f * (LunarProfile.HeightScale - 1f)));
			tBarHorizontal.set_Value((int)(20f * (LunarProfile.WidthScale - 1f)));
			bool grazeLOLAHires;
			showLOLAHighResolutionLimbToolStripMenuItem.set_Checked(grazeLOLAHires = Settings.Default.GrazeLOLAHires);
			LunarProfile.UseLOLAHires = grazeLOLAHires;
			showLOLAElevationDataPointsToolStripMenuItem.set_Checked(grazeLOLAHires = Settings.Default.GrazeLOLAPoints);
			LunarProfile.ShowLOLAPoints = grazeLOLAHires;
			showEventsToolStripMenuItem.set_Checked(grazeLOLAHires = Settings.Default.GrazeEventCount_HiRes);
			LunarProfile.generateHiresforEventCount = grazeLOLAHires;
			LunarProfile.DistanceLines.Clear();
			FormCreated = true;
		}

		private void SetResize()
		{
			((Control)panel1).set_Left(1);
			((Control)panel1).set_Top(25);
			((Control)panel1).set_Width(((Control)this).get_Width() - 17);
			((Control)panel1).set_Height(((Control)this).get_Height() - 62);
			((Control)picProfile).set_Left(1);
			((Control)picProfile).set_Top(0);
			((Control)picProfile).set_Width((int)(LunarProfile.WidthScale * (float)((Control)panel1).get_Width() - 17f));
			((Control)picProfile).set_Height((int)(LunarProfile.HeightScale * (float)((Control)panel1).get_Height() - 12f));
			if (FormCreated)
			{
				LunarProfile.PlotGrazeProfile(HighlightRePlot: false, "", "", "", ReReadHires: false);
			}
		}

		private void T0_ToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_000d: Unknown result type (might be due to invalid IL or missing references)
			MessageBox.Show("To set 'None' as the default setting,\r\ngo to the 'Maintenance/User Settings' form", "Set default to 'None'", (MessageBoxButtons)0, (MessageBoxIcon)64);
			SetLibrationTags(0);
		}

		private void T1_ToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SetLibrationTags(1);
		}

		private void T2_ToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SetLibrationTags(2);
		}

		private void T3_ToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SetLibrationTags(3);
		}

		private void T4_ToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SetLibrationTags(4);
		}

		private void T5_ToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SetLibrationTags(5);
		}

		private void T6_ToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SetLibrationTags(6);
		}

		private void T7_ToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SetLibrationTags(7);
		}

		private void T8_ToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SetLibrationTags(8);
		}

		private void T9_ToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SetLibrationTags(9);
		}

		private void SetLibrationTags(int f)
		{
			T0_ToolStripMenuItem.set_Checked(f == 0);
			T1_ToolStripMenuItem.set_Checked(f == 1);
			T2_ToolStripMenuItem.set_Checked(f == 2);
			T3_ToolStripMenuItem.set_Checked(f == 3);
			T4_ToolStripMenuItem.set_Checked(f == 4);
			T5_ToolStripMenuItem.set_Checked(f == 5);
			T6_ToolStripMenuItem.set_Checked(f == 6);
			T7_ToolStripMenuItem.set_Checked(f == 7);
			T8_ToolStripMenuItem.set_Checked(f == 8);
			T9_ToolStripMenuItem.set_Checked(f == 9);
			LunarProfile.LibrationSetting = f;
			if (FormCreated)
			{
				LunarProfile.PlotGrazeProfile(HighlightRePlot: false, "", "", "", ReReadHires: false);
			}
		}

		private void copyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetImage(picProfile.get_Image());
		}

		private void printPreviewToolStripMenuItem_Click(object sender, EventArgs e)
		{
			LunarProfile.PrintPreviewGrazeProfile();
		}

		private void printToolStripMenuItem_Click(object sender, EventArgs e)
		{
			LunarProfile.PrintGrazeProfile();
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings.Default.Save_LunarPredictions = Output.SaveGraphic(picProfile.get_Image(), "Graze Profile " + LunarOccultations.GrazePredictionLines[LunarProfile.ElementRecord].StarId.Trim().Replace("*", "") + " " + Utilities.Date_from_JD(LunarOccultations.GrazePredictionLines[LunarProfile.ElementRecord].JD, 0), Settings.Default.Save_LunarPredictions);
		}

		private void picProfile_MouseMove(object sender, MouseEventArgs e)
		{
			if (PlotIsBeingDrawn || GettingTag)
			{
				return;
			}
			float num = e.get_X();
			float num2 = e.get_Y();
			if (ShowToolTip)
			{
				GetToolTip = !GetToolTip;
				if (GetToolTip)
				{
					toolTip.SetToolTip((Control)(object)picProfile, LunarProfile.ToolTipText(num, num2));
				}
			}
			CurrentHeight = LunarProfile.CurrentCursorHeight(num2);
			if (Tags.Count < 1 || ((Math.Abs(num - OldX) < 2f) & (Math.Abs(num2 - OldY) < 2f)))
			{
				return;
			}
			GettingTag = true;
			float num3 = num - 2f - 1f;
			int num4 = 0;
			int num5 = Tags.Count - 1;
			int num6;
			do
			{
				num6 = (num5 + num4) / 2;
				if (num3 == Tags[num6].X)
				{
					break;
				}
				if (num3 < Tags[num6].X)
				{
					num5 = num6 - 1;
				}
				else
				{
					num4 = num6 + 1;
				}
			}
			while (num5 >= num4);
			int num7 = num6;
			do
			{
				if ((Math.Abs(num - Tags[num7].X) < 2f) & (Math.Abs(num2 - Tags[num7].Y) < 2f))
				{
					((Control)lblTag).set_Text(Tags[num7].Tag);
					OldX = num;
					OldY = num2;
					OldArg0 = num7;
					CurrentTagRecord = num7;
					Application.DoEvents();
					GettingTag = false;
					return;
				}
				if (Tags[num7].X - num > 3f)
				{
					((Control)lblTag).set_Text("");
					CurrentTagRecord = -1;
					Application.DoEvents();
					GettingTag = false;
					return;
				}
				num7++;
			}
			while (num7 < Tags.Count);
			((Control)lblTag).set_Text("");
			CurrentTagRecord = -1;
			GettingTag = false;
		}

		private void includeOrdinaryOccultationsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings.Default.GrazeProfile_LimitToGrazes = !Settings.Default.GrazeProfile_LimitToGrazes;
			includeOrdinaryOccultationsToolStripMenuItem.set_Checked(Settings.Default.GrazeProfile_LimitToGrazes);
			LunarProfile.LimitToGrazes = Settings.Default.GrazeProfile_LimitToGrazes;
			LunarProfile.PlotGrazeProfile(HighlightRePlot: false, "", "", "", ReReadHires: false);
		}

		private void showEventsInColourToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings.Default.GrazeProfile_EventsInColour = !Settings.Default.GrazeProfile_EventsInColour;
			showEventsInColourToolStripMenuItem.set_Checked(Settings.Default.GrazeProfile_EventsInColour);
			LunarProfile.EventsInColour = Settings.Default.GrazeProfile_EventsInColour;
			LunarProfile.PlotGrazeProfile(HighlightRePlot: false, "", "", "", ReReadHires: false);
		}

		private void drawInColourToolStripMenuItem_Click(object sender, EventArgs e)
		{
			drawInColourToolStripMenuItem.set_Checked(!drawInColourToolStripMenuItem.get_Checked());
			Settings.Default.BWFlag = !drawInColourToolStripMenuItem.get_Checked();
			LunarProfile.PlotGrazeProfile(HighlightRePlot: false, "", "", "", ReReadHires: false);
		}

		private void showTimeAndHeightAtCursorToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ShowToolTip = !ShowToolTip;
			showTimeAndHeightAtCursorToolStripMenuItem.set_Checked(ShowToolTip);
		}

		private void ListGrazesUsedMenuItem_Click(object sender, EventArgs e)
		{
			try
			{
				((Control)GrazesUsed).Show();
			}
			catch
			{
				GrazesUsed = new LunarListGrazesUsed();
				GrazesUsed.Predictions = true;
				((Control)GrazesUsed).Show();
			}
			GrazesUsed.Predictions = true;
			((Control)GrazesUsed).Focus();
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
		}

		private void highlightThisObserverToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (CurrentTagRecord != -1)
			{
				LunarProfile.PlotGrazeProfile(HighlightRePlot: true, "", Tags[CurrentTagRecord].ObserverID, "", ReReadHires: false);
			}
		}

		private void highlightThisStarToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (CurrentTagRecord != -1)
			{
				LunarProfile.PlotGrazeProfile(HighlightRePlot: true, Tags[CurrentTagRecord].StarID, "", "", ReReadHires: false);
			}
		}

		private void highlightThisDateToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (CurrentTagRecord != -1)
			{
				LunarProfile.PlotGrazeProfile(HighlightRePlot: true, "", "", Tags[CurrentTagRecord].Date, ReReadHires: false);
			}
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Lunar graze profile");
		}

		private void listObserversForPlottedEventsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			try
			{
				((Control)CurrentObservers).Show();
			}
			catch
			{
				CurrentObservers = new ListObservers();
				((Control)CurrentObservers).Show();
			}
			((Control)CurrentObservers).Focus();
		}

		private void tBar_MouseUp(object sender, MouseEventArgs e)
		{
			LunarProfile.HeightScale = 1f + (float)tBar.get_Value() / 20f;
			SetResize();
		}

		private void tBarHorizontal_MouseUp(object sender, MouseEventArgs e)
		{
			LunarProfile.WidthScale = 1f + (float)tBarHorizontal.get_Value() / 20f;
			SetResize();
		}

		private void showLOLAHighResolutionLimbToolStripMenuItem_Click(object sender, EventArgs e)
		{
			showLOLAHighResolutionLimbToolStripMenuItem.set_Checked(!showLOLAHighResolutionLimbToolStripMenuItem.get_Checked());
			bool useLOLAHires = (Settings.Default.GrazeLOLAHires = showLOLAHighResolutionLimbToolStripMenuItem.get_Checked());
			LunarProfile.UseLOLAHires = useLOLAHires;
			if (FormCreated)
			{
				LunarProfile.PlotGrazeProfile(HighlightRePlot: false, "", "", "", ReReadHires: true);
			}
		}

		private void LunarGrazeProfile_ResizeEnd(object sender, EventArgs e)
		{
			if (!((((Control)this).get_Width() < 300) | (((Control)this).get_Height() < 200)))
			{
				SetResize();
			}
		}

		private void showLOLAElevationDataPointsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			showLOLAElevationDataPointsToolStripMenuItem.set_Checked(!showLOLAElevationDataPointsToolStripMenuItem.get_Checked());
			bool showLOLAPoints = (Settings.Default.GrazeLOLAPoints = showLOLAElevationDataPointsToolStripMenuItem.get_Checked());
			LunarProfile.ShowLOLAPoints = showLOLAPoints;
			if (FormCreated)
			{
				LunarProfile.PlotGrazeProfile(HighlightRePlot: false, "", "", "", ReReadHires: true);
			}
		}

		private void drawLineAtThisHeightToolStripMenuItem_Click(object sender, EventArgs e)
		{
			PlotIsBeingDrawn = true;
			LunarProfile.DistanceLines.Add(CurrentHeight);
			LunarProfile.PlotGrazeProfile(HighlightRePlot: false, "", "", "", ReReadHires: false);
		}

		private void clearAllLinesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			PlotIsBeingDrawn = true;
			LunarProfile.DistanceLines.Clear();
			LunarProfile.PlotGrazeProfile(HighlightRePlot: false, "", "", "", ReReadHires: false);
		}

		private void showEventsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			showEventsToolStripMenuItem.set_Checked(!showEventsToolStripMenuItem.get_Checked());
			Settings.Default.GrazeEventCount_HiRes = showEventsToolStripMenuItem.get_Checked();
			if (Settings.Default.GrazeEventCount_HiRes)
			{
				Settings.Default.GrazeEventCount_LowRes = false;
				showEventsByPathDistanceLowResolutionToolStripMenuItem.set_Checked(false);
			}
			LunarProfile.generateHiresforEventCount = showEventsToolStripMenuItem.get_Checked();
			if (FormCreated)
			{
				LunarProfile.PlotGrazeProfile(HighlightRePlot: false, "", "", "", ReReadHires: false);
			}
		}

		private void showEventsByPathDistanceLowResolutionToolStripMenuItem_Click(object sender, EventArgs e)
		{
			showEventsByPathDistanceLowResolutionToolStripMenuItem.set_Checked(!showEventsByPathDistanceLowResolutionToolStripMenuItem.get_Checked());
			Settings.Default.GrazeEventCount_LowRes = showEventsByPathDistanceLowResolutionToolStripMenuItem.get_Checked();
			if (Settings.Default.GrazeEventCount_LowRes)
			{
				Settings.Default.GrazeEventCount_HiRes = false;
				showEventsToolStripMenuItem.set_Checked(false);
			}
			LunarProfile.generateHiresforEventCount = showEventsByPathDistanceLowResolutionToolStripMenuItem.get_Checked();
			if (FormCreated)
			{
				LunarProfile.PlotGrazeProfile(HighlightRePlot: false, "", "", "", ReReadHires: false);
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
			//IL_0188: Unknown result type (might be due to invalid IL or missing references)
			//IL_0192: Expected O, but got Unknown
			//IL_0199: Unknown result type (might be due to invalid IL or missing references)
			//IL_01a3: Expected O, but got Unknown
			//IL_01a4: Unknown result type (might be due to invalid IL or missing references)
			//IL_01ae: Expected O, but got Unknown
			//IL_01af: Unknown result type (might be due to invalid IL or missing references)
			//IL_01b9: Expected O, but got Unknown
			//IL_01ba: Unknown result type (might be due to invalid IL or missing references)
			//IL_01c4: Expected O, but got Unknown
			//IL_01c5: Unknown result type (might be due to invalid IL or missing references)
			//IL_01cf: Expected O, but got Unknown
			//IL_01d0: Unknown result type (might be due to invalid IL or missing references)
			//IL_01da: Expected O, but got Unknown
			//IL_01db: Unknown result type (might be due to invalid IL or missing references)
			//IL_01e5: Expected O, but got Unknown
			//IL_01e6: Unknown result type (might be due to invalid IL or missing references)
			//IL_01f0: Expected O, but got Unknown
			//IL_01f1: Unknown result type (might be due to invalid IL or missing references)
			//IL_01fb: Expected O, but got Unknown
			//IL_01fc: Unknown result type (might be due to invalid IL or missing references)
			//IL_0206: Expected O, but got Unknown
			//IL_0207: Unknown result type (might be due to invalid IL or missing references)
			//IL_0211: Expected O, but got Unknown
			//IL_0212: Unknown result type (might be due to invalid IL or missing references)
			//IL_021c: Expected O, but got Unknown
			//IL_021d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0227: Expected O, but got Unknown
			//IL_0228: Unknown result type (might be due to invalid IL or missing references)
			//IL_0232: Expected O, but got Unknown
			//IL_0233: Unknown result type (might be due to invalid IL or missing references)
			//IL_023d: Expected O, but got Unknown
			//IL_023e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0248: Expected O, but got Unknown
			//IL_0249: Unknown result type (might be due to invalid IL or missing references)
			//IL_0253: Expected O, but got Unknown
			//IL_0254: Unknown result type (might be due to invalid IL or missing references)
			//IL_025e: Expected O, but got Unknown
			//IL_025f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0269: Expected O, but got Unknown
			//IL_026a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0274: Expected O, but got Unknown
			//IL_0275: Unknown result type (might be due to invalid IL or missing references)
			//IL_027f: Expected O, but got Unknown
			//IL_127d: Unknown result type (might be due to invalid IL or missing references)
			//IL_1287: Expected O, but got Unknown
			//IL_12b0: Unknown result type (might be due to invalid IL or missing references)
			//IL_1313: Unknown result type (might be due to invalid IL or missing references)
			//IL_131d: Expected O, but got Unknown
			//IL_1847: Unknown result type (might be due to invalid IL or missing references)
			//IL_18ab: Unknown result type (might be due to invalid IL or missing references)
			//IL_18b5: Expected O, but got Unknown
			//IL_1a7d: Unknown result type (might be due to invalid IL or missing references)
			//IL_1a87: Expected O, but got Unknown
			components = new Container();
			menuStrip1 = new MenuStrip();
			withProfileToolStripMenuItem = new ToolStripMenuItem();
			showLOLAHighResolutionLimbToolStripMenuItem = new ToolStripMenuItem();
			showLOLAElevationDataPointsToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator4 = new ToolStripSeparator();
			showTimeAndHeightAtCursorToolStripMenuItem = new ToolStripMenuItem();
			showEventsByPathDistanceLowResolutionToolStripMenuItem = new ToolStripMenuItem();
			showEventsToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator2 = new ToolStripSeparator();
			copyToolStripMenuItem = new ToolStripMenuItem();
			printPreviewToolStripMenuItem = new ToolStripMenuItem();
			printToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			observedGrazeoccultationDataToolStripMenuItem = new ToolStripMenuItem();
			T0_ToolStripMenuItem = new ToolStripMenuItem();
			T1_ToolStripMenuItem = new ToolStripMenuItem();
			T2_ToolStripMenuItem = new ToolStripMenuItem();
			T3_ToolStripMenuItem = new ToolStripMenuItem();
			T4_ToolStripMenuItem = new ToolStripMenuItem();
			T5_ToolStripMenuItem = new ToolStripMenuItem();
			T6_ToolStripMenuItem = new ToolStripMenuItem();
			T7_ToolStripMenuItem = new ToolStripMenuItem();
			T8_ToolStripMenuItem = new ToolStripMenuItem();
			T9_ToolStripMenuItem = new ToolStripMenuItem();
			includeOrdinaryOccultationsToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator3 = new ToolStripSeparator();
			ListGrazesUsedMenuItem = new ToolStripMenuItem();
			listObserversForPlottedEventsToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator1 = new ToolStripSeparator();
			drawInColourToolStripMenuItem = new ToolStripMenuItem();
			showEventsInColourToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			lblTag = new Label();
			toolTip = new ToolTip(components);
			contextHighlight = new ContextMenuStrip(components);
			highlightThisObserverToolStripMenuItem = new ToolStripMenuItem();
			highlightThisStarToolStripMenuItem = new ToolStripMenuItem();
			highlightThisDateToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator5 = new ToolStripSeparator();
			drawLineAtThisHeightToolStripMenuItem = new ToolStripMenuItem();
			clearAllLinesToolStripMenuItem = new ToolStripMenuItem();
			picProfile = new PictureBox();
			tBar = new TrackBar();
			panel1 = new Panel();
			label1 = new Label();
			label2 = new Label();
			label3 = new Label();
			label4 = new Label();
			label5 = new Label();
			label6 = new Label();
			label7 = new Label();
			label10 = new Label();
			label8 = new Label();
			tBarHorizontal = new TrackBar();
			label9 = new Label();
			((Control)menuStrip1).SuspendLayout();
			((Control)contextHighlight).SuspendLayout();
			((ISupportInitialize)picProfile).BeginInit();
			((ISupportInitialize)tBar).BeginInit();
			((Control)panel1).SuspendLayout();
			((ISupportInitialize)tBarHorizontal).BeginInit();
			((Control)this).SuspendLayout();
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[4]
			{
				(ToolStripItem)withProfileToolStripMenuItem,
				(ToolStripItem)observedGrazeoccultationDataToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(820, 24));
			((Control)menuStrip1).set_TabIndex(0);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withProfileToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[11]
			{
				(ToolStripItem)showLOLAHighResolutionLimbToolStripMenuItem,
				(ToolStripItem)showLOLAElevationDataPointsToolStripMenuItem,
				(ToolStripItem)toolStripSeparator4,
				(ToolStripItem)showTimeAndHeightAtCursorToolStripMenuItem,
				(ToolStripItem)showEventsByPathDistanceLowResolutionToolStripMenuItem,
				(ToolStripItem)showEventsToolStripMenuItem,
				(ToolStripItem)toolStripSeparator2,
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)printPreviewToolStripMenuItem,
				(ToolStripItem)printToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem
			});
			((ToolStripItem)withProfileToolStripMenuItem).set_Name("withProfileToolStripMenuItem");
			((ToolStripItem)withProfileToolStripMenuItem).set_Size(new Size(103, 20));
			((ToolStripItem)withProfileToolStripMenuItem).set_Text("with Profile ...    ");
			((ToolStripItem)showLOLAHighResolutionLimbToolStripMenuItem).set_Name("showLOLAHighResolutionLimbToolStripMenuItem");
			((ToolStripItem)showLOLAHighResolutionLimbToolStripMenuItem).set_Size(new Size(334, 22));
			((ToolStripItem)showLOLAHighResolutionLimbToolStripMenuItem).set_Text("showLOLA high resolution limb");
			((ToolStripItem)showLOLAHighResolutionLimbToolStripMenuItem).add_Click((EventHandler)showLOLAHighResolutionLimbToolStripMenuItem_Click);
			((ToolStripItem)showLOLAElevationDataPointsToolStripMenuItem).set_Name("showLOLAElevationDataPointsToolStripMenuItem");
			((ToolStripItem)showLOLAElevationDataPointsToolStripMenuItem).set_Size(new Size(334, 22));
			((ToolStripItem)showLOLAElevationDataPointsToolStripMenuItem).set_Text("show LOLA elevation data points");
			((ToolStripItem)showLOLAElevationDataPointsToolStripMenuItem).add_Click((EventHandler)showLOLAElevationDataPointsToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator4).set_Name("toolStripSeparator4");
			((ToolStripItem)toolStripSeparator4).set_Size(new Size(331, 6));
			((ToolStripItem)showTimeAndHeightAtCursorToolStripMenuItem).set_Name("showTimeAndHeightAtCursorToolStripMenuItem");
			showTimeAndHeightAtCursorToolStripMenuItem.set_ShortcutKeys((Keys)131156);
			((ToolStripItem)showTimeAndHeightAtCursorToolStripMenuItem).set_Size(new Size(334, 22));
			((ToolStripItem)showTimeAndHeightAtCursorToolStripMenuItem).set_Text("Show time and height at cursor");
			((ToolStripItem)showTimeAndHeightAtCursorToolStripMenuItem).add_Click((EventHandler)showTimeAndHeightAtCursorToolStripMenuItem_Click);
			((ToolStripItem)showEventsByPathDistanceLowResolutionToolStripMenuItem).set_Name("showEventsByPathDistanceLowResolutionToolStripMenuItem");
			((ToolStripItem)showEventsByPathDistanceLowResolutionToolStripMenuItem).set_Size(new Size(334, 22));
			((ToolStripItem)showEventsByPathDistanceLowResolutionToolStripMenuItem).set_Text("Event #'s by path distance - low resolution");
			((ToolStripItem)showEventsByPathDistanceLowResolutionToolStripMenuItem).add_Click((EventHandler)showEventsByPathDistanceLowResolutionToolStripMenuItem_Click);
			((ToolStripItem)showEventsToolStripMenuItem).set_Name("showEventsToolStripMenuItem");
			((ToolStripItem)showEventsToolStripMenuItem).set_Size(new Size(334, 22));
			((ToolStripItem)showEventsToolStripMenuItem).set_Text("Event #'s by path distance - high resolutn (Slow! )");
			((ToolStripItem)showEventsToolStripMenuItem).add_Click((EventHandler)showEventsToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator2).set_Name("toolStripSeparator2");
			((ToolStripItem)toolStripSeparator2).set_Size(new Size(331, 6));
			((ToolStripItem)copyToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyToolStripMenuItem).set_Name("copyToolStripMenuItem");
			copyToolStripMenuItem.set_ShortcutKeys((Keys)131139);
			((ToolStripItem)copyToolStripMenuItem).set_Size(new Size(334, 22));
			((ToolStripItem)copyToolStripMenuItem).set_Text("&Copy");
			((ToolStripItem)copyToolStripMenuItem).add_Click((EventHandler)copyToolStripMenuItem_Click);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Image((Image)Resources.PrintPreview);
			((ToolStripItem)printPreviewToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Name("printPreviewToolStripMenuItem");
			((ToolStripItem)printPreviewToolStripMenuItem).set_Size(new Size(334, 22));
			((ToolStripItem)printPreviewToolStripMenuItem).set_Text("&Print preview");
			((ToolStripItem)printPreviewToolStripMenuItem).add_Click((EventHandler)printPreviewToolStripMenuItem_Click);
			((ToolStripItem)printToolStripMenuItem).set_Image((Image)Resources.Print);
			((ToolStripItem)printToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printToolStripMenuItem).set_Name("printToolStripMenuItem");
			printToolStripMenuItem.set_ShortcutKeys((Keys)131152);
			((ToolStripItem)printToolStripMenuItem).set_Size(new Size(334, 22));
			((ToolStripItem)printToolStripMenuItem).set_Text("&Print");
			((ToolStripItem)printToolStripMenuItem).add_Click((EventHandler)printToolStripMenuItem_Click);
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			saveToolStripMenuItem.set_ShortcutKeys((Keys)131155);
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(334, 22));
			((ToolStripItem)saveToolStripMenuItem).set_Text("&Save");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
			((ToolStripDropDownItem)observedGrazeoccultationDataToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[17]
			{
				(ToolStripItem)T0_ToolStripMenuItem,
				(ToolStripItem)T1_ToolStripMenuItem,
				(ToolStripItem)T2_ToolStripMenuItem,
				(ToolStripItem)T3_ToolStripMenuItem,
				(ToolStripItem)T4_ToolStripMenuItem,
				(ToolStripItem)T5_ToolStripMenuItem,
				(ToolStripItem)T6_ToolStripMenuItem,
				(ToolStripItem)T7_ToolStripMenuItem,
				(ToolStripItem)T8_ToolStripMenuItem,
				(ToolStripItem)T9_ToolStripMenuItem,
				(ToolStripItem)includeOrdinaryOccultationsToolStripMenuItem,
				(ToolStripItem)toolStripSeparator3,
				(ToolStripItem)ListGrazesUsedMenuItem,
				(ToolStripItem)listObserversForPlottedEventsToolStripMenuItem,
				(ToolStripItem)toolStripSeparator1,
				(ToolStripItem)drawInColourToolStripMenuItem,
				(ToolStripItem)showEventsInColourToolStripMenuItem
			});
			((ToolStripItem)observedGrazeoccultationDataToolStripMenuItem).set_Image((Image)Resources.DataSet_TableView);
			((ToolStripItem)observedGrazeoccultationDataToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)observedGrazeoccultationDataToolStripMenuItem).set_Name("observedGrazeoccultationDataToolStripMenuItem");
			((ToolStripItem)observedGrazeoccultationDataToolStripMenuItem).set_Size(new Size(129, 20));
			((ToolStripItem)observedGrazeoccultationDataToolStripMenuItem).set_Text("Observed data      ");
			((ToolStripItem)T0_ToolStripMenuItem).set_Name("T0_ToolStripMenuItem");
			T0_ToolStripMenuItem.set_ShortcutKeys((Keys)131120);
			((ToolStripItem)T0_ToolStripMenuItem).set_Size(new Size(307, 22));
			((ToolStripItem)T0_ToolStripMenuItem).set_Text("0  None");
			((ToolStripItem)T0_ToolStripMenuItem).add_Click((EventHandler)T0_ToolStripMenuItem_Click);
			((ToolStripItem)T1_ToolStripMenuItem).set_Name("T1_ToolStripMenuItem");
			T1_ToolStripMenuItem.set_ShortcutKeys((Keys)131121);
			((ToolStripItem)T1_ToolStripMenuItem).set_Size(new Size(307, 22));
			((ToolStripItem)T1_ToolStripMenuItem).set_Text("1  P,  D[+/-0.3 deg]");
			((ToolStripItem)T1_ToolStripMenuItem).add_Click((EventHandler)T1_ToolStripMenuItem_Click);
			((ToolStripItem)T2_ToolStripMenuItem).set_Name("T2_ToolStripMenuItem");
			T2_ToolStripMenuItem.set_ShortcutKeys((Keys)131122);
			((ToolStripItem)T2_ToolStripMenuItem).set_Size(new Size(307, 22));
			((ToolStripItem)T2_ToolStripMenuItem).set_Text("2  P,  D[+/-0.5 deg]");
			((ToolStripItem)T2_ToolStripMenuItem).add_Click((EventHandler)T2_ToolStripMenuItem_Click);
			((ToolStripItem)T3_ToolStripMenuItem).set_Name("T3_ToolStripMenuItem");
			T3_ToolStripMenuItem.set_ShortcutKeys((Keys)131123);
			((ToolStripItem)T3_ToolStripMenuItem).set_Size(new Size(307, 22));
			((ToolStripItem)T3_ToolStripMenuItem).set_Text("3  P,  D[+/- 1.0 deg]");
			((ToolStripItem)T3_ToolStripMenuItem).add_Click((EventHandler)T3_ToolStripMenuItem_Click);
			((ToolStripItem)T4_ToolStripMenuItem).set_Name("T4_ToolStripMenuItem");
			T4_ToolStripMenuItem.set_ShortcutKeys((Keys)131124);
			((ToolStripItem)T4_ToolStripMenuItem).set_Size(new Size(307, 22));
			((ToolStripItem)T4_ToolStripMenuItem).set_Text("4  AA,  L[+/- 1 deg],  B[+/- 0.5 deg]");
			((ToolStripItem)T4_ToolStripMenuItem).add_Click((EventHandler)T4_ToolStripMenuItem_Click);
			((ToolStripItem)T5_ToolStripMenuItem).set_Name("T5_ToolStripMenuItem");
			T5_ToolStripMenuItem.set_ShortcutKeys((Keys)131125);
			((ToolStripItem)T5_ToolStripMenuItem).set_Size(new Size(307, 22));
			((ToolStripItem)T5_ToolStripMenuItem).set_Text("5  AA,  L[+/- 2 deg],  B[+/- 0.5 deg]");
			((ToolStripItem)T5_ToolStripMenuItem).add_Click((EventHandler)T5_ToolStripMenuItem_Click);
			((ToolStripItem)T6_ToolStripMenuItem).set_Name("T6_ToolStripMenuItem");
			T6_ToolStripMenuItem.set_ShortcutKeys((Keys)131126);
			((ToolStripItem)T6_ToolStripMenuItem).set_Size(new Size(307, 22));
			((ToolStripItem)T6_ToolStripMenuItem).set_Text("6  AA,  L[+/- 1 deg],  B[+/- 1 deg]");
			((ToolStripItem)T6_ToolStripMenuItem).add_Click((EventHandler)T6_ToolStripMenuItem_Click);
			((ToolStripItem)T7_ToolStripMenuItem).set_Name("T7_ToolStripMenuItem");
			T7_ToolStripMenuItem.set_ShortcutKeys((Keys)131127);
			((ToolStripItem)T7_ToolStripMenuItem).set_Size(new Size(307, 22));
			((ToolStripItem)T7_ToolStripMenuItem).set_Text("7  AA,  L[+/- 2 deg],  B[+/- 1 deg]");
			((ToolStripItem)T7_ToolStripMenuItem).add_Click((EventHandler)T7_ToolStripMenuItem_Click);
			((ToolStripItem)T8_ToolStripMenuItem).set_Name("T8_ToolStripMenuItem");
			T8_ToolStripMenuItem.set_ShortcutKeys((Keys)131128);
			((ToolStripItem)T8_ToolStripMenuItem).set_Size(new Size(307, 22));
			((ToolStripItem)T8_ToolStripMenuItem).set_Text("8  AA,  L[+/- 4 deg],  B[+/- 1 deg]");
			((ToolStripItem)T8_ToolStripMenuItem).add_Click((EventHandler)T8_ToolStripMenuItem_Click);
			((ToolStripItem)T9_ToolStripMenuItem).set_Name("T9_ToolStripMenuItem");
			T9_ToolStripMenuItem.set_ShortcutKeys((Keys)131129);
			((ToolStripItem)T9_ToolStripMenuItem).set_Size(new Size(307, 22));
			((ToolStripItem)T9_ToolStripMenuItem).set_Text("9  AA,  L[+/- 4 deg],  B[+/- 2 deg]");
			((ToolStripItem)T9_ToolStripMenuItem).add_Click((EventHandler)T9_ToolStripMenuItem_Click);
			((ToolStripItem)includeOrdinaryOccultationsToolStripMenuItem).set_Name("includeOrdinaryOccultationsToolStripMenuItem");
			includeOrdinaryOccultationsToolStripMenuItem.set_ShortcutKeys((Keys)131151);
			((ToolStripItem)includeOrdinaryOccultationsToolStripMenuItem).set_Size(new Size(307, 22));
			((ToolStripItem)includeOrdinaryOccultationsToolStripMenuItem).set_Text("Exclude ordinary Visual occultations");
			((ToolStripItem)includeOrdinaryOccultationsToolStripMenuItem).add_Click((EventHandler)includeOrdinaryOccultationsToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator3).set_Name("toolStripSeparator3");
			((ToolStripItem)toolStripSeparator3).set_Size(new Size(304, 6));
			((ToolStripItem)ListGrazesUsedMenuItem).set_Name("ListGrazesUsedMenuItem");
			ListGrazesUsedMenuItem.set_ShortcutKeys((Keys)131148);
			((ToolStripItem)ListGrazesUsedMenuItem).set_Size(new Size(307, 22));
			((ToolStripItem)ListGrazesUsedMenuItem).set_Text("List grazes used");
			((ToolStripItem)ListGrazesUsedMenuItem).add_Click((EventHandler)ListGrazesUsedMenuItem_Click);
			((ToolStripItem)listObserversForPlottedEventsToolStripMenuItem).set_Name("listObserversForPlottedEventsToolStripMenuItem");
			((ToolStripItem)listObserversForPlottedEventsToolStripMenuItem).set_Size(new Size(307, 22));
			((ToolStripItem)listObserversForPlottedEventsToolStripMenuItem).set_Text("List observers for plotted events");
			((ToolStripItem)listObserversForPlottedEventsToolStripMenuItem).add_Click((EventHandler)listObserversForPlottedEventsToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator1).set_Name("toolStripSeparator1");
			((ToolStripItem)toolStripSeparator1).set_Size(new Size(304, 6));
			((ToolStripItem)drawInColourToolStripMenuItem).set_Image((Image)Resources.ColorHS);
			((ToolStripItem)drawInColourToolStripMenuItem).set_Name("drawInColourToolStripMenuItem");
			((ToolStripItem)drawInColourToolStripMenuItem).set_Size(new Size(307, 22));
			((ToolStripItem)drawInColourToolStripMenuItem).set_Text("Draw in colour");
			((ToolStripItem)drawInColourToolStripMenuItem).add_Click((EventHandler)drawInColourToolStripMenuItem_Click);
			((ToolStripItem)showEventsInColourToolStripMenuItem).set_Image((Image)Resources.ColorHS);
			((ToolStripItem)showEventsInColourToolStripMenuItem).set_Name("showEventsInColourToolStripMenuItem");
			((ToolStripItem)showEventsInColourToolStripMenuItem).set_Size(new Size(307, 22));
			((ToolStripItem)showEventsInColourToolStripMenuItem).set_Text("Show events in colour");
			((ToolStripItem)showEventsInColourToolStripMenuItem).add_Click((EventHandler)showEventsInColourToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(72, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("&Help    ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(54, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("E&xit");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((Control)lblTag).set_AutoSize(true);
			((Control)lblTag).set_Location(new Point(799, 9));
			((Control)lblTag).set_Name("lblTag");
			((Control)lblTag).set_Size(new Size(13, 13));
			((Control)lblTag).set_TabIndex(2);
			((Control)lblTag).set_Text("[]");
			toolTip.set_AutoPopDelay(5000);
			toolTip.set_InitialDelay(100);
			toolTip.set_ReshowDelay(100);
			((ToolStrip)contextHighlight).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[6]
			{
				(ToolStripItem)highlightThisObserverToolStripMenuItem,
				(ToolStripItem)highlightThisStarToolStripMenuItem,
				(ToolStripItem)highlightThisDateToolStripMenuItem,
				(ToolStripItem)toolStripSeparator5,
				(ToolStripItem)drawLineAtThisHeightToolStripMenuItem,
				(ToolStripItem)clearAllLinesToolStripMenuItem
			});
			((Control)contextHighlight).set_Name("contextHighlight");
			((Control)contextHighlight).set_Size(new Size(196, 120));
			((ToolStripItem)highlightThisObserverToolStripMenuItem).set_Name("highlightThisObserverToolStripMenuItem");
			((ToolStripItem)highlightThisObserverToolStripMenuItem).set_Size(new Size(195, 22));
			((ToolStripItem)highlightThisObserverToolStripMenuItem).set_Text("highlight this Observer");
			((ToolStripItem)highlightThisObserverToolStripMenuItem).add_Click((EventHandler)highlightThisObserverToolStripMenuItem_Click);
			((ToolStripItem)highlightThisStarToolStripMenuItem).set_Name("highlightThisStarToolStripMenuItem");
			((ToolStripItem)highlightThisStarToolStripMenuItem).set_Size(new Size(195, 22));
			((ToolStripItem)highlightThisStarToolStripMenuItem).set_Text("highlight this Star");
			((ToolStripItem)highlightThisStarToolStripMenuItem).add_Click((EventHandler)highlightThisStarToolStripMenuItem_Click);
			((ToolStripItem)highlightThisDateToolStripMenuItem).set_Name("highlightThisDateToolStripMenuItem");
			((ToolStripItem)highlightThisDateToolStripMenuItem).set_Size(new Size(195, 22));
			((ToolStripItem)highlightThisDateToolStripMenuItem).set_Text("highlight this Date");
			((ToolStripItem)highlightThisDateToolStripMenuItem).add_Click((EventHandler)highlightThisDateToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator5).set_Name("toolStripSeparator5");
			((ToolStripItem)toolStripSeparator5).set_Size(new Size(192, 6));
			((ToolStripItem)drawLineAtThisHeightToolStripMenuItem).set_Name("drawLineAtThisHeightToolStripMenuItem");
			((ToolStripItem)drawLineAtThisHeightToolStripMenuItem).set_Size(new Size(195, 22));
			((ToolStripItem)drawLineAtThisHeightToolStripMenuItem).set_Text("Draw line at this height");
			((ToolStripItem)drawLineAtThisHeightToolStripMenuItem).add_Click((EventHandler)drawLineAtThisHeightToolStripMenuItem_Click);
			((ToolStripItem)clearAllLinesToolStripMenuItem).set_Name("clearAllLinesToolStripMenuItem");
			((ToolStripItem)clearAllLinesToolStripMenuItem).set_Size(new Size(195, 22));
			((ToolStripItem)clearAllLinesToolStripMenuItem).set_Text("Clear all lines");
			((ToolStripItem)clearAllLinesToolStripMenuItem).add_Click((EventHandler)clearAllLinesToolStripMenuItem_Click);
			picProfile.set_BorderStyle((BorderStyle)2);
			((Control)picProfile).set_ContextMenuStrip(contextHighlight);
			((Control)picProfile).set_Location(new Point(0, 0));
			((Control)picProfile).set_Name("picProfile");
			((Control)picProfile).set_Size(new Size(776, 528));
			picProfile.set_TabIndex(1);
			picProfile.set_TabStop(false);
			((Control)picProfile).add_MouseMove(new MouseEventHandler(picProfile_MouseMove));
			((Control)tBar).set_AutoSize(false);
			((Control)tBar).set_Location(new Point(410, 1));
			((Control)tBar).set_Margin(new Padding(1));
			tBar.set_Maximum(60);
			((Control)tBar).set_Name("tBar");
			((Control)tBar).set_Size(new Size(131, 16));
			((Control)tBar).set_TabIndex(3);
			tBar.set_TickFrequency(5);
			((Control)tBar).add_MouseUp(new MouseEventHandler(tBar_MouseUp));
			((ScrollableControl)panel1).set_AutoScroll(true);
			((ScrollableControl)panel1).set_AutoScrollMargin(new Size(0, 10));
			((Control)panel1).get_Controls().Add((Control)(object)picProfile);
			((Control)panel1).set_Location(new Point(3, 25));
			((Control)panel1).set_Name("panel1");
			((Control)panel1).set_Size(new Size(809, 545));
			((Control)panel1).set_TabIndex(4);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(373, 0));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(39, 24));
			((Control)label1).set_TabIndex(5);
			((Control)label1).set_Text("Vertical \r\nMagnify");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Font(new Font("Microsoft Sans Serif", 6f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label2).set_Location(new Point(413, 16));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(9, 9));
			((Control)label2).set_TabIndex(6);
			((Control)label2).set_Text("1");
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Font(new Font("Microsoft Sans Serif", 6f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label3).set_Location(new Point(530, 16));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(9, 9));
			((Control)label3).set_TabIndex(7);
			((Control)label3).set_Text("4");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Font(new Font("Microsoft Sans Serif", 6f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label4).set_Location(new Point(491, 16));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(9, 9));
			((Control)label4).set_TabIndex(8);
			((Control)label4).set_Text("3");
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Font(new Font("Microsoft Sans Serif", 6f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label5).set_Location(new Point(451, 16));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(9, 9));
			((Control)label5).set_TabIndex(9);
			((Control)label5).set_Text("2");
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Font(new Font("Microsoft Sans Serif", 6f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label6).set_Location(new Point(637, 16));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(9, 9));
			((Control)label6).set_TabIndex(21);
			((Control)label6).set_Text("2");
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Font(new Font("Microsoft Sans Serif", 6f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label7).set_Location(new Point(677, 16));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(9, 9));
			((Control)label7).set_TabIndex(20);
			((Control)label7).set_Text("3");
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label10).set_Location(new Point(552, 0));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(49, 24));
			((Control)label10).set_TabIndex(17);
			((Control)label10).set_Text("Horizontal \r\nMagnify");
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Font(new Font("Microsoft Sans Serif", 6f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label8).set_Location(new Point(716, 16));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(9, 9));
			((Control)label8).set_TabIndex(19);
			((Control)label8).set_Text("4");
			((Control)tBarHorizontal).set_AutoSize(false);
			((Control)tBarHorizontal).set_Location(new Point(596, 1));
			((Control)tBarHorizontal).set_Margin(new Padding(1));
			tBarHorizontal.set_Maximum(60);
			((Control)tBarHorizontal).set_Name("tBarHorizontal");
			((Control)tBarHorizontal).set_Size(new Size(131, 16));
			((Control)tBarHorizontal).set_TabIndex(16);
			tBarHorizontal.set_TickFrequency(5);
			((Control)tBarHorizontal).add_MouseUp(new MouseEventHandler(tBarHorizontal_MouseUp));
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Font(new Font("Microsoft Sans Serif", 6f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label9).set_Location(new Point(599, 16));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(9, 9));
			((Control)label9).set_TabIndex(18);
			((Control)label9).set_Text("1");
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(820, 572));
			((Control)this).get_Controls().Add((Control)(object)label6);
			((Control)this).get_Controls().Add((Control)(object)label7);
			((Control)this).get_Controls().Add((Control)(object)label10);
			((Control)this).get_Controls().Add((Control)(object)label8);
			((Control)this).get_Controls().Add((Control)(object)tBarHorizontal);
			((Control)this).get_Controls().Add((Control)(object)label9);
			((Control)this).get_Controls().Add((Control)(object)label5);
			((Control)this).get_Controls().Add((Control)(object)label4);
			((Control)this).get_Controls().Add((Control)(object)label3);
			((Control)this).get_Controls().Add((Control)(object)label2);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)panel1);
			((Control)this).get_Controls().Add((Control)(object)tBar);
			((Control)this).get_Controls().Add((Control)(object)lblTag);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationLunarGrazeProfile", true, (DataSourceUpdateMode)1));
			((Control)this).set_DoubleBuffered(true);
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_Location(Settings.Default.LocationLunarGrazeProfile);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_Name("LunarGrazeProfile");
			((Control)this).set_Text("Lunar graze - prediction profile");
			((Form)this).add_Load((EventHandler)LunarGrazeProfile_Load);
			((Form)this).add_ResizeEnd((EventHandler)LunarGrazeProfile_ResizeEnd);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)contextHighlight).ResumeLayout(false);
			((ISupportInitialize)picProfile).EndInit();
			((ISupportInitialize)tBar).EndInit();
			((Control)panel1).ResumeLayout(false);
			((ISupportInitialize)tBarHorizontal).EndInit();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
