using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using Occult.Mapping;
using Occult.Properties;

namespace Occult.Lunar_Observations
{
	public class ReductionPlot : Form
	{
		internal static bool Updating = false;

		internal static bool UpdatingPlot = false;

		internal static List<LunarGrazeTags> Tags = new List<LunarGrazeTags>();

		private static float OldX = 0f;

		private static float OldY = 0f;

		private static float OldArg0 = 0f;

		private static int XatStart;

		private static int YatStart;

		private static int XatEnd;

		private static int YatEnd;

		private static int CurrentTagRecord = -1;

		private LunarListGrazesUsed GrazesUsed;

		private ListCurrentObserversInProfile CurrentObservers;

		private ListPlottedObservers HistoricalObservers;

		private string ToolText = "";

		private bool SepnSmallScroll;

		private bool PAsmallScroll;

		private bool GetToolTip = true;

		private static bool FormCreated = false;

		private static bool GettingTag = false;

		private static bool ShowToolTip = true;

		private static bool MouseMoving = false;

		private static bool ChecksChanging = false;

		private IContainer components;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem withPlotToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem printPreviewToolStripMenuItem;

		private ToolStripMenuItem printToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private Button cmdDrawProfile;

		internal PictureBox picProfile;

		internal NumericUpDown updnTop;

		internal NumericUpDown updnRight;

		internal NumericUpDown updnLeft;

		internal NumericUpDown updnBottom;

		private ToolTip toolTip;

		private Label lblTag;

		private ContextMenuStrip contextMenuStrip1;

		private ToolStripMenuItem highlightThisObserverToolStripMenuItem;

		private ToolStripMenuItem highlightThisStarToolStripMenuItem;

		private ToolStripMenuItem highlightThisDateToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private Label label1;

		private Label label2;

		private Label label3;

		private Label label4;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripMenuItem observedGrazeoccultationDataToolStripMenuItem;

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

		private ToolStripMenuItem includeOrdinaryOccultationsToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator3;

		private ToolStripMenuItem ListGrazesUsedMenuItem;

		private ToolStripSeparator toolStripSeparator1;

		private Label label5;

		private Label label6;

		internal NumericUpDown updnL;

		internal NumericUpDown updnB;

		private Label label7;

		internal NumericUpDown updnD;

		private ToolStripMenuItem listCurrentObserversToolStripMenuItem;

		private ToolStripMenuItem listHistoricalObserversThatArePlottedToolStripMenuItem;

		private Label label8;

		internal NumericUpDown updnScale;

		private Label label9;

		private Label lblSlope;

		internal GroupBox grpDouble;

		internal NumericUpDown updn_Sep;

		internal CheckBox chkB;

		internal CheckBox chkA;

		internal NumericUpDown updn_PA;

		private CheckBox chkLOLApoints;

		internal Label lblDblPA;

		internal Label lblDblSep;

		private ToolStripMenuItem copyDoubleStarSolutionToolStripMenuItem;

		private CheckBox chkLOLAHires;

		private ToolStripSeparator toolStripSeparator2;

		private ToolStripMenuItem plotLimbRegionForThisAxisAngleToolStripMenuItem;

		internal CheckBox chkExcludeLargeResiduals;

		private ToolStripMenuItem drawInColorToolStripMenuItem;

		private ToolStripMenuItem eventsInColorToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator4;

		public ReductionPlot()
		{
			InitializeComponent();
		}

		private void ReductionPlot_Load(object sender, EventArgs e)
		{
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			FormCreated = false;
			includeOrdinaryOccultationsToolStripMenuItem.set_Checked(Settings.Default.ReductionProfile_LimitToGrazes);
			ReductionProfile.LimitToGrazes = Settings.Default.ReductionProfile_LimitToGrazes;
			eventsInColorToolStripMenuItem.set_Checked(Settings.Default.ReductionProfile_EventsInColour);
			ReductionProfile.EventsInColour = Settings.Default.ReductionProfile_EventsInColour;
			drawInColorToolStripMenuItem.set_Checked(!Settings.Default.BWFlag);
			SetLibrationTags(Settings.Default.GrazeProfile_LibrationPlot_Reduction);
			((Control)this).set_Width(1200);
			if (((Control)this).get_Top() < -30000)
			{
				((Control)this).set_Top(0);
			}
			if ((double)((Control)this).get_Left() + (double)((Control)this).get_Width() / 1.2 < (double)Screen.GetWorkingArea((Control)(object)this).Left)
			{
				((Control)this).set_Left(Screen.GetWorkingArea((Control)(object)this).Left);
			}
			CheckBox obj = chkLOLApoints;
			bool lOLAFileExists;
			((Control)chkLOLAHires).set_Enabled(lOLAFileExists = Utilities.LOLAFileExists);
			((Control)obj).set_Enabled(lOLAFileExists);
			SetProfiles(ReReadHiRes: true);
			FormCreated = true;
		}

		private void T0_ToolStripMenuItem_Click(object sender, EventArgs e)
		{
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
			int num3 = (ReductionProfile.LibrationSetting = (Settings.Default.GrazeProfile_LibrationPlot_Reduction = f));
			if (FormCreated)
			{
				DrawProfile(ReReadHiresData: false);
			}
		}

		private void includeOrdinaryOccultationsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings.Default.ReductionProfile_LimitToGrazes = !Settings.Default.ReductionProfile_LimitToGrazes;
			includeOrdinaryOccultationsToolStripMenuItem.set_Checked(Settings.Default.ReductionProfile_LimitToGrazes);
			ReductionProfile.LimitToGrazes = Settings.Default.ReductionProfile_LimitToGrazes;
			if (FormCreated)
			{
				DrawProfile(ReReadHiresData: false);
			}
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
				((Control)GrazesUsed).Show();
			}
			GrazesUsed.Predictions = false;
			((Control)GrazesUsed).Focus();
		}

		private void listCurrentObserversToolStripMenuItem_Click(object sender, EventArgs e)
		{
			try
			{
				((Control)CurrentObservers).Show();
			}
			catch
			{
				CurrentObservers = new ListCurrentObserversInProfile();
				((Control)CurrentObservers).Show();
			}
			((Control)CurrentObservers).Focus();
		}

		private void listHistoricalObserversThatArePlottedToolStripMenuItem_Click(object sender, EventArgs e)
		{
			try
			{
				((Control)HistoricalObservers).Show();
			}
			catch
			{
				HistoricalObservers = new ListPlottedObservers();
				((Control)HistoricalObservers).Show();
			}
			((Control)HistoricalObservers).Focus();
		}

		private void cmdDrawProfile_Click(object sender, EventArgs e)
		{
			ReductionProfile.IsDrawing = false;
			if (FormCreated)
			{
				DrawProfile(ReReadHiresData: true);
			}
		}

		internal void DrawProfile(bool ReReadHiresData)
		{
			if (FormCreated)
			{
				ReductionProfile.PlotReductionProfile(HighlightRePlot: false, "", "", "", chkExcludeLargeResiduals.get_Checked(), IncludeStarPath: false, IncludeAllPaths: false, ReReadHiresData);
			}
		}

		private void updnTop_ValueChanged(object sender, EventArgs e)
		{
			ReductionProfile.HeightMax = (double)updnTop.get_Value();
			if (updnTop.get_Value() - updnBottom.get_Value() < 0.5m)
			{
				updnBottom.set_Value(updnTop.get_Value() - 0.5m);
				ReductionProfile.HeightMin = (double)updnBottom.get_Value();
			}
		}

		private void updnBottom_ValueChanged(object sender, EventArgs e)
		{
			ReductionProfile.HeightMin = (double)updnBottom.get_Value();
			if (updnTop.get_Value() - updnBottom.get_Value() < 0.5m)
			{
				updnTop.set_Value(updnBottom.get_Value() + 0.5m);
				ReductionProfile.HeightMax = (double)updnTop.get_Value();
			}
		}

		private void updnLeft_ValueChanged(object sender, EventArgs e)
		{
			ReductionProfile.AA_Min = (ReductionProfile.PMin = (double)updnLeft.get_Value());
			Utilities.Librations_P_D(ReductionProfile.l, ReductionProfile.b, (ReductionProfile.AA_Max + ReductionProfile.AA_Min) / 2.0, out var _, out ReductionProfile.D);
		}

		private void updnRight_ValueChanged(object sender, EventArgs e)
		{
			ReductionProfile.AA_Max = (ReductionProfile.PMax = (double)updnRight.get_Value());
			Utilities.Librations_P_D(ReductionProfile.l, ReductionProfile.b, (ReductionProfile.AA_Max + ReductionProfile.AA_Min) / 2.0, out var _, out ReductionProfile.D);
			updnD.set_Value((decimal)ReductionProfile.D);
		}

		private void updnL_ValueChanged(object sender, EventArgs e)
		{
			ReductionProfile.l = (double)updnL.get_Value();
			Utilities.Librations_P_D(ReductionProfile.l, ReductionProfile.b, (ReductionProfile.AA_Max + ReductionProfile.AA_Min) / 2.0, out var _, out ReductionProfile.D);
			updnD.set_Value((decimal)ReductionProfile.D);
		}

		private void updnB_ValueChanged(object sender, EventArgs e)
		{
			ReductionProfile.b = (double)updnB.get_Value();
			Utilities.Librations_P_D(ReductionProfile.l, ReductionProfile.b, (ReductionProfile.AA_Max + ReductionProfile.AA_Min) / 2.0, out var _, out ReductionProfile.D);
			updnD.set_Value((decimal)ReductionProfile.D);
		}

		private void updnD_ValueChanged(object sender, EventArgs e)
		{
			ReductionProfile.D = (double)updnD.get_Value();
		}

		private void updnScale_ValueChanged(object sender, EventArgs e)
		{
			ReductionProfile.MoonScale = (double)updnScale.get_Value();
		}

		private void picProfile_MouseMove(object sender, MouseEventArgs e)
		{
			//IL_00b0: Unknown result type (might be due to invalid IL or missing references)
			//IL_00ba: Invalid comparison between Unknown and I4
			if (GettingTag)
			{
				return;
			}
			float num = e.get_X();
			float num2 = e.get_Y();
			if (((Control)picProfile).get_BackColor() == Color.White)
			{
				_ = Color.Black;
			}
			else
			{
				_ = Color.White;
			}
			if (((Control)picProfile).get_BackColor() == Color.White)
			{
				new Pen(Brushes.Red, 2f);
			}
			else
			{
				new Pen(Brushes.White, 2f);
			}
			if (((Control)picProfile).get_BackColor() == Color.White)
			{
				_ = Color.Black;
			}
			_ = (((Control)picProfile).get_Bottom() - ((Control)picProfile).get_Top()) / 2;
			if ((int)e.get_Button() == 1048576)
			{
				MouseMoving = true;
				if (XatStart < 0)
				{
					XatStart = (int)num;
					YatStart = (int)num2;
					XatEnd = (int)num;
					YatEnd = (int)num2;
				}
				else
				{
					XatEnd = (int)num;
					YatEnd = (int)num2;
					ToolText = ReductionProfile.ToolTipTextProfileSlope(XatEnd - XatStart, YatEnd - YatStart);
					((Control)lblSlope).set_Visible(true);
					GetToolTip = !GetToolTip;
					if (GetToolTip)
					{
						toolTip.SetToolTip((Control)(object)picProfile, ToolText);
					}
					Application.DoEvents();
				}
				MouseMoving = false;
				return;
			}
			if (XatStart >= 0)
			{
				ControlPaint.DrawReversibleLine(((Control)picProfile).PointToScreen(new Point(XatStart, YatStart)), ((Control)picProfile).PointToScreen(new Point(XatEnd, YatEnd)), ((Control)picProfile).get_BackColor());
				XatStart = -1;
			}
			if (ShowToolTip)
			{
				GetToolTip = !GetToolTip;
				if (GetToolTip)
				{
					toolTip.SetToolTip((Control)(object)picProfile, ReductionProfile.ToolTipText(num, num2));
				}
			}
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

		private void highlightThisObserverToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (CurrentTagRecord != -1)
			{
				ReductionProfile.PlotReductionProfile(HighlightRePlot: true, "", Tags[CurrentTagRecord].ObserverID, "", chkExcludeLargeResiduals.get_Checked(), IncludeStarPath: false, IncludeAllPaths: false, ReReadHiresData: false);
			}
		}

		private void highlightThisStarToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (CurrentTagRecord != -1)
			{
				ReductionProfile.PlotReductionProfile(HighlightRePlot: true, Tags[CurrentTagRecord].StarID, "", "", chkExcludeLargeResiduals.get_Checked(), IncludeStarPath: false, IncludeAllPaths: false, ReReadHiresData: false);
			}
		}

		private void highlightThisDateToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (CurrentTagRecord != -1)
			{
				ReductionProfile.PlotReductionProfile(HighlightRePlot: true, "", "", Tags[CurrentTagRecord].Date, chkExcludeLargeResiduals.get_Checked(), IncludeStarPath: false, IncludeAllPaths: false, ReReadHiresData: false);
			}
		}

		private void copyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetImage(picProfile.get_Image());
		}

		private void printPreviewToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ReductionProfile.PrintPreviewReductionProfile();
		}

		private void printToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ReductionProfile.PrintReductionProfile();
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings.Default.Save_LunarObservations = Output.SaveGraphic(picProfile.get_Image(), "Graze Profile ", Settings.Default.Save_LunarObservations);
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private void ReductionPlot_Resize(object sender, EventArgs e)
		{
			((Control)picProfile).set_Width(((Control)this).get_Width() - 35);
			((Control)picProfile).set_Height(((Control)this).get_Height() - 140);
		}

		private void ReductionPlot_ResizeBegin(object sender, EventArgs e)
		{
		}

		private void ReductionPlot_ResizeEnd(object sender, EventArgs e)
		{
			DrawProfile(ReReadHiresData: false);
		}

		private void ResizePlot()
		{
			if (FormCreated)
			{
				DrawProfile(ReReadHiresData: false);
			}
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Lunar Occultations - Observed profile");
		}

		private void ReductionPlot_FormClosed(object sender, FormClosedEventArgs e)
		{
			((Component)this).Dispose();
		}

		private void ReductionPlot_FormClosing(object sender, FormClosingEventArgs e)
		{
			try
			{
				((Form)GrazesUsed).Close();
				((Component)(object)GrazesUsed).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)CurrentObservers).Close();
				((Component)(object)CurrentObservers).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)HistoricalObservers).Close();
				((Component)(object)HistoricalObservers).Dispose();
			}
			catch
			{
			}
		}

		private void chkLOLApoints_Click(object sender, EventArgs e)
		{
			if (!ChecksChanging)
			{
				ChecksChanging = true;
				chkLOLApoints.set_Checked(!chkLOLApoints.get_Checked());
				SetProfiles(ReReadHiRes: false);
			}
		}

		private void drawInColorToolStripMenuItem_Click(object sender, EventArgs e)
		{
			drawInColorToolStripMenuItem.set_Checked(!drawInColorToolStripMenuItem.get_Checked());
			Settings.Default.BWFlag = !drawInColorToolStripMenuItem.get_Checked();
			if (FormCreated)
			{
				DrawProfile(ReReadHiresData: false);
			}
		}

		private void eventsInColorToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings.Default.ReductionProfile_EventsInColour = !Settings.Default.ReductionProfile_EventsInColour;
			eventsInColorToolStripMenuItem.set_Checked(Settings.Default.ReductionProfile_EventsInColour);
			ReductionProfile.EventsInColour = Settings.Default.ReductionProfile_EventsInColour;
			if (FormCreated)
			{
				DrawProfile(ReReadHiresData: false);
			}
		}

		private void chkLOLAHires_Click(object sender, EventArgs e)
		{
			if (!ChecksChanging)
			{
				ChecksChanging = true;
				chkLOLAHires.set_Checked(!chkLOLAHires.get_Checked());
				SetProfiles(ReReadHiRes: true);
			}
		}

		private void SetProfiles(bool ReReadHiRes)
		{
			ReductionProfile.Use_LOLA_HiRes = chkLOLAHires.get_Checked();
			ReductionProfile.ShowLOLAPoints = chkLOLApoints.get_Checked();
			if (FormCreated)
			{
				DrawProfile(ReReadHiRes);
			}
			ChecksChanging = false;
		}

		private void picProfile_MouseUp(object sender, MouseEventArgs e)
		{
			((Control)lblSlope).set_Text("Last " + ToolText);
		}

		private void plotLimbRegionForThisAxisAngleToolStripMenuItem_Click(object sender, EventArgs e)
		{
			double SlopeBefore_Deg = 0.0;
			double SlopeAfter_Deg = 0.0;
			LOLAHiRes.LimbHeight_Slope(ReductionProfile.CurrentAAofMouse, (double)updnL.get_Value(), (double)updnB.get_Value(), (double)updnScale.get_Value(), IncludeSlope: true, WideSlope: true, out SlopeBefore_Deg, out SlopeAfter_Deg, out var P_Limb_deg, out var D_Limb_deg);
			LOLAHiRes.MapLimbRegion((double)updnL.get_Value(), (double)updnB.get_Value(), ReductionProfile.CurrentAAofMouse, ShowLimbPoint: true, P_Limb_deg, D_Limb_deg);
		}

		private void chkA_CheckedChanged(object sender, EventArgs e)
		{
			if (FormCreated)
			{
				DrawProfile(ReReadHiresData: false);
			}
		}

		private void chkB_CheckedChanged(object sender, EventArgs e)
		{
			if (FormCreated)
			{
				DrawProfile(ReReadHiresData: false);
			}
		}

		private void updn_dRA_ValueChanged(object sender, EventArgs e)
		{
			if (FormCreated)
			{
				DrawProfile(ReReadHiresData: false);
			}
		}

		private void updn_dDec_ValueChanged(object sender, EventArgs e)
		{
			if (FormCreated)
			{
				DrawProfile(ReReadHiresData: false);
			}
		}

		private void updn_Sep_DoubleClick(object sender, EventArgs e)
		{
			SepnSmallScroll = !SepnSmallScroll;
			if (SepnSmallScroll)
			{
				updn_Sep.set_Increment(0.01m);
				((Control)updn_Sep).set_BackColor(Color.LightGreen);
			}
			else
			{
				updn_Sep.set_Increment(0.1m);
				((Control)updn_Sep).set_BackColor(Color.FromName("Window"));
			}
		}

		private void updn_PA_DoubleClick(object sender, EventArgs e)
		{
			PAsmallScroll = !PAsmallScroll;
			if (PAsmallScroll)
			{
				updn_PA.set_Increment(0.01m);
				((Control)updn_PA).set_BackColor(Color.LightGreen);
			}
			else
			{
				updn_PA.set_Increment(0.2m);
				((Control)updn_PA).set_BackColor(Color.FromName("Window"));
			}
		}

		private void copyDoubleStarSolutionToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(LunarObservations.Residuals[0].StarCat_Number + "\r\nDouble star solution from grazing occultation\r\n" + string.Format("Sep = {0,1:F2}\"  [predicted {1,1:f2}\"]", updn_Sep.get_Value(), ReductionProfile.StarSepForPlot) + "\r\n" + string.Format("PA = {0,1:F2}°  [predicted {1,1:f2}°]", updn_PA.get_Value(), ReductionProfile.StarPAforPlot) + "\r\n" + string.Format("Date of graze: {0,1:f2}", Utilities.BesselianYear(LunarObservations.Residuals[0].EventJDTime)) + "\r\nNumber of events in the graze: " + LunarObservations.Residuals.Count + "\r\n");
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
			//IL_0130: Unknown result type (might be due to invalid IL or missing references)
			//IL_013a: Expected O, but got Unknown
			//IL_013b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0145: Expected O, but got Unknown
			//IL_0146: Unknown result type (might be due to invalid IL or missing references)
			//IL_0150: Expected O, but got Unknown
			//IL_0151: Unknown result type (might be due to invalid IL or missing references)
			//IL_015b: Expected O, but got Unknown
			//IL_015c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0166: Expected O, but got Unknown
			//IL_0167: Unknown result type (might be due to invalid IL or missing references)
			//IL_0171: Expected O, but got Unknown
			//IL_0172: Unknown result type (might be due to invalid IL or missing references)
			//IL_017c: Expected O, but got Unknown
			//IL_017d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0187: Expected O, but got Unknown
			//IL_0188: Unknown result type (might be due to invalid IL or missing references)
			//IL_0192: Expected O, but got Unknown
			//IL_0193: Unknown result type (might be due to invalid IL or missing references)
			//IL_019d: Expected O, but got Unknown
			//IL_019e: Unknown result type (might be due to invalid IL or missing references)
			//IL_01a8: Expected O, but got Unknown
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
			//IL_0280: Unknown result type (might be due to invalid IL or missing references)
			//IL_028a: Expected O, but got Unknown
			//IL_028b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0295: Expected O, but got Unknown
			//IL_0296: Unknown result type (might be due to invalid IL or missing references)
			//IL_02a0: Expected O, but got Unknown
			//IL_02a1: Unknown result type (might be due to invalid IL or missing references)
			//IL_02ab: Expected O, but got Unknown
			//IL_02ac: Unknown result type (might be due to invalid IL or missing references)
			//IL_02b6: Expected O, but got Unknown
			//IL_02b7: Unknown result type (might be due to invalid IL or missing references)
			//IL_02c1: Expected O, but got Unknown
			//IL_02c2: Unknown result type (might be due to invalid IL or missing references)
			//IL_02cc: Expected O, but got Unknown
			//IL_02cd: Unknown result type (might be due to invalid IL or missing references)
			//IL_02d7: Expected O, but got Unknown
			//IL_02d8: Unknown result type (might be due to invalid IL or missing references)
			//IL_02e2: Expected O, but got Unknown
			//IL_02e3: Unknown result type (might be due to invalid IL or missing references)
			//IL_02ed: Expected O, but got Unknown
			//IL_02ee: Unknown result type (might be due to invalid IL or missing references)
			//IL_02f8: Expected O, but got Unknown
			//IL_21a7: Unknown result type (might be due to invalid IL or missing references)
			//IL_21b1: Expected O, but got Unknown
			//IL_227d: Unknown result type (might be due to invalid IL or missing references)
			//IL_2287: Expected O, but got Unknown
			//IL_238e: Unknown result type (might be due to invalid IL or missing references)
			//IL_2398: Expected O, but got Unknown
			//IL_23a5: Unknown result type (might be due to invalid IL or missing references)
			//IL_23af: Expected O, but got Unknown
			//IL_2741: Unknown result type (might be due to invalid IL or missing references)
			//IL_274b: Expected O, but got Unknown
			//IL_27b2: Unknown result type (might be due to invalid IL or missing references)
			//IL_27bc: Expected O, but got Unknown
			//IL_27c4: Unknown result type (might be due to invalid IL or missing references)
			//IL_27ce: Expected O, but got Unknown
			components = new Container();
			menuStrip1 = new MenuStrip();
			withPlotToolStripMenuItem = new ToolStripMenuItem();
			copyToolStripMenuItem = new ToolStripMenuItem();
			copyDoubleStarSolutionToolStripMenuItem = new ToolStripMenuItem();
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
			listCurrentObserversToolStripMenuItem = new ToolStripMenuItem();
			listHistoricalObserversThatArePlottedToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator1 = new ToolStripSeparator();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			contextMenuStrip1 = new ContextMenuStrip(components);
			plotLimbRegionForThisAxisAngleToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator2 = new ToolStripSeparator();
			highlightThisObserverToolStripMenuItem = new ToolStripMenuItem();
			highlightThisStarToolStripMenuItem = new ToolStripMenuItem();
			highlightThisDateToolStripMenuItem = new ToolStripMenuItem();
			updnTop = new NumericUpDown();
			updnRight = new NumericUpDown();
			updnLeft = new NumericUpDown();
			updnBottom = new NumericUpDown();
			cmdDrawProfile = new Button();
			toolTip = new ToolTip(components);
			lblTag = new Label();
			label1 = new Label();
			label2 = new Label();
			label3 = new Label();
			label4 = new Label();
			updnL = new NumericUpDown();
			updnB = new NumericUpDown();
			label5 = new Label();
			label6 = new Label();
			label7 = new Label();
			updnD = new NumericUpDown();
			label8 = new Label();
			updnScale = new NumericUpDown();
			label9 = new Label();
			lblSlope = new Label();
			grpDouble = new GroupBox();
			lblDblPA = new Label();
			lblDblSep = new Label();
			updn_PA = new NumericUpDown();
			updn_Sep = new NumericUpDown();
			chkB = new CheckBox();
			chkA = new CheckBox();
			chkLOLApoints = new CheckBox();
			chkLOLAHires = new CheckBox();
			picProfile = new PictureBox();
			chkExcludeLargeResiduals = new CheckBox();
			drawInColorToolStripMenuItem = new ToolStripMenuItem();
			eventsInColorToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator4 = new ToolStripSeparator();
			((Control)menuStrip1).SuspendLayout();
			((Control)contextMenuStrip1).SuspendLayout();
			((ISupportInitialize)updnTop).BeginInit();
			((ISupportInitialize)updnRight).BeginInit();
			((ISupportInitialize)updnLeft).BeginInit();
			((ISupportInitialize)updnBottom).BeginInit();
			((ISupportInitialize)updnL).BeginInit();
			((ISupportInitialize)updnB).BeginInit();
			((ISupportInitialize)updnD).BeginInit();
			((ISupportInitialize)updnScale).BeginInit();
			((Control)grpDouble).SuspendLayout();
			((ISupportInitialize)updn_PA).BeginInit();
			((ISupportInitialize)updn_Sep).BeginInit();
			((ISupportInitialize)picProfile).BeginInit();
			((Control)this).SuspendLayout();
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[4]
			{
				(ToolStripItem)withPlotToolStripMenuItem,
				(ToolStripItem)observedGrazeoccultationDataToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(1000, 24));
			((Control)menuStrip1).set_TabIndex(27);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withPlotToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[8]
			{
				(ToolStripItem)drawInColorToolStripMenuItem,
				(ToolStripItem)eventsInColorToolStripMenuItem,
				(ToolStripItem)toolStripSeparator4,
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)copyDoubleStarSolutionToolStripMenuItem,
				(ToolStripItem)printPreviewToolStripMenuItem,
				(ToolStripItem)printToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem
			});
			((ToolStripItem)withPlotToolStripMenuItem).set_Name("withPlotToolStripMenuItem");
			((ToolStripItem)withPlotToolStripMenuItem).set_Size(new Size(90, 20));
			((ToolStripItem)withPlotToolStripMenuItem).set_Text("with Plot....    ");
			((ToolStripItem)copyToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyToolStripMenuItem).set_Name("copyToolStripMenuItem");
			copyToolStripMenuItem.set_ShortcutKeys((Keys)131139);
			((ToolStripItem)copyToolStripMenuItem).set_Size(new Size(211, 22));
			((ToolStripItem)copyToolStripMenuItem).set_Text("&Copy");
			((ToolStripItem)copyToolStripMenuItem).add_Click((EventHandler)copyToolStripMenuItem_Click);
			((ToolStripItem)copyDoubleStarSolutionToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyDoubleStarSolutionToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyDoubleStarSolutionToolStripMenuItem).set_Name("copyDoubleStarSolutionToolStripMenuItem");
			((ToolStripItem)copyDoubleStarSolutionToolStripMenuItem).set_Size(new Size(211, 22));
			((ToolStripItem)copyDoubleStarSolutionToolStripMenuItem).set_Text("Copy Double star solution");
			((ToolStripItem)copyDoubleStarSolutionToolStripMenuItem).add_Click((EventHandler)copyDoubleStarSolutionToolStripMenuItem_Click);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Image((Image)Resources.PrintPreview);
			((ToolStripItem)printPreviewToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Name("printPreviewToolStripMenuItem");
			((ToolStripItem)printPreviewToolStripMenuItem).set_Size(new Size(211, 22));
			((ToolStripItem)printPreviewToolStripMenuItem).set_Text("Print preview");
			((ToolStripItem)printPreviewToolStripMenuItem).add_Click((EventHandler)printPreviewToolStripMenuItem_Click);
			((ToolStripItem)printToolStripMenuItem).set_Image((Image)Resources.Print);
			((ToolStripItem)printToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printToolStripMenuItem).set_Name("printToolStripMenuItem");
			printToolStripMenuItem.set_ShortcutKeys((Keys)131152);
			((ToolStripItem)printToolStripMenuItem).set_Size(new Size(211, 22));
			((ToolStripItem)printToolStripMenuItem).set_Text("&Print");
			((ToolStripItem)printToolStripMenuItem).add_Click((EventHandler)printToolStripMenuItem_Click);
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			saveToolStripMenuItem.set_ShortcutKeys((Keys)131155);
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(211, 22));
			((ToolStripItem)saveToolStripMenuItem).set_Text("&Save");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
			((ToolStripDropDownItem)observedGrazeoccultationDataToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[16]
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
				(ToolStripItem)listCurrentObserversToolStripMenuItem,
				(ToolStripItem)listHistoricalObserversThatArePlottedToolStripMenuItem,
				(ToolStripItem)toolStripSeparator1
			});
			((ToolStripItem)observedGrazeoccultationDataToolStripMenuItem).set_Image((Image)Resources.DataSet_TableView);
			((ToolStripItem)observedGrazeoccultationDataToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)observedGrazeoccultationDataToolStripMenuItem).set_Name("observedGrazeoccultationDataToolStripMenuItem");
			((ToolStripItem)observedGrazeoccultationDataToolStripMenuItem).set_Size(new Size(236, 20));
			((ToolStripItem)observedGrazeoccultationDataToolStripMenuItem).set_Text("Observed graze && occultation data      ");
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
			((ToolStripItem)toolStripSeparator3).set_Visible(false);
			((ToolStripItem)ListGrazesUsedMenuItem).set_Name("ListGrazesUsedMenuItem");
			ListGrazesUsedMenuItem.set_ShortcutKeys((Keys)131148);
			((ToolStripItem)ListGrazesUsedMenuItem).set_Size(new Size(307, 22));
			((ToolStripItem)ListGrazesUsedMenuItem).set_Text("List grazes used");
			((ToolStripItem)ListGrazesUsedMenuItem).add_Click((EventHandler)ListGrazesUsedMenuItem_Click);
			((ToolStripItem)listCurrentObserversToolStripMenuItem).set_Name("listCurrentObserversToolStripMenuItem");
			((ToolStripItem)listCurrentObserversToolStripMenuItem).set_Size(new Size(307, 22));
			((ToolStripItem)listCurrentObserversToolStripMenuItem).set_Text("List current observers");
			((ToolStripItem)listCurrentObserversToolStripMenuItem).add_Click((EventHandler)listCurrentObserversToolStripMenuItem_Click);
			((ToolStripItem)listHistoricalObserversThatArePlottedToolStripMenuItem).set_Name("listHistoricalObserversThatArePlottedToolStripMenuItem");
			((ToolStripItem)listHistoricalObserversThatArePlottedToolStripMenuItem).set_Size(new Size(307, 22));
			((ToolStripItem)listHistoricalObserversThatArePlottedToolStripMenuItem).set_Text("List historical observers");
			((ToolStripItem)listHistoricalObserversThatArePlottedToolStripMenuItem).add_Click((EventHandler)listHistoricalObserversThatArePlottedToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator1).set_Name("toolStripSeparator1");
			((ToolStripItem)toolStripSeparator1).set_Size(new Size(304, 6));
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
			((ToolStrip)contextMenuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[5]
			{
				(ToolStripItem)plotLimbRegionForThisAxisAngleToolStripMenuItem,
				(ToolStripItem)toolStripSeparator2,
				(ToolStripItem)highlightThisObserverToolStripMenuItem,
				(ToolStripItem)highlightThisStarToolStripMenuItem,
				(ToolStripItem)highlightThisDateToolStripMenuItem
			});
			((Control)contextMenuStrip1).set_Name("contextHighlight");
			((Control)contextMenuStrip1).set_Size(new Size(259, 98));
			((ToolStripItem)plotLimbRegionForThisAxisAngleToolStripMenuItem).set_Name("plotLimbRegionForThisAxisAngleToolStripMenuItem");
			((ToolStripItem)plotLimbRegionForThisAxisAngleToolStripMenuItem).set_Size(new Size(258, 22));
			((ToolStripItem)plotLimbRegionForThisAxisAngleToolStripMenuItem).set_Text("Plot limb region for this Axis Angle");
			((ToolStripItem)plotLimbRegionForThisAxisAngleToolStripMenuItem).add_Click((EventHandler)plotLimbRegionForThisAxisAngleToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator2).set_Name("toolStripSeparator2");
			((ToolStripItem)toolStripSeparator2).set_Size(new Size(255, 6));
			((ToolStripItem)highlightThisObserverToolStripMenuItem).set_Name("highlightThisObserverToolStripMenuItem");
			((ToolStripItem)highlightThisObserverToolStripMenuItem).set_Size(new Size(258, 22));
			((ToolStripItem)highlightThisObserverToolStripMenuItem).set_Text("highlight this Observer");
			((ToolStripItem)highlightThisObserverToolStripMenuItem).add_Click((EventHandler)highlightThisObserverToolStripMenuItem_Click);
			((ToolStripItem)highlightThisStarToolStripMenuItem).set_Name("highlightThisStarToolStripMenuItem");
			((ToolStripItem)highlightThisStarToolStripMenuItem).set_Size(new Size(258, 22));
			((ToolStripItem)highlightThisStarToolStripMenuItem).set_Text("highlight this Star");
			((ToolStripItem)highlightThisStarToolStripMenuItem).add_Click((EventHandler)highlightThisStarToolStripMenuItem_Click);
			((ToolStripItem)highlightThisDateToolStripMenuItem).set_Name("highlightThisDateToolStripMenuItem");
			((ToolStripItem)highlightThisDateToolStripMenuItem).set_Size(new Size(258, 22));
			((ToolStripItem)highlightThisDateToolStripMenuItem).set_Text("highlight this Date");
			((ToolStripItem)highlightThisDateToolStripMenuItem).add_Click((EventHandler)highlightThisDateToolStripMenuItem_Click);
			((Control)updnTop).set_Anchor((AnchorStyles)1);
			updnTop.set_DecimalPlaces(1);
			updnTop.set_Increment(new decimal(new int[4] { 5, 0, 0, 65536 }));
			((Control)updnTop).set_Location(new Point(290, 32));
			updnTop.set_Maximum(new decimal(new int[4] { 35, 0, 0, 65536 }));
			updnTop.set_Minimum(new decimal(new int[4] { 30, 0, 0, -2147418112 }));
			((Control)updnTop).set_Name("updnTop");
			((Control)updnTop).set_Size(new Size(40, 20));
			((Control)updnTop).set_TabIndex(1);
			updnTop.set_Value(new decimal(new int[4] { 3, 0, 0, 0 }));
			updnTop.add_ValueChanged((EventHandler)updnTop_ValueChanged);
			((Control)updnRight).set_Anchor((AnchorStyles)1);
			updnRight.set_DecimalPlaces(1);
			((Control)updnRight).set_Location(new Point(449, 32));
			updnRight.set_Maximum(new decimal(new int[4] { 410, 0, 0, 0 }));
			updnRight.set_Minimum(new decimal(new int[4] { 360, 0, 0, -2147483648 }));
			((Control)updnRight).set_Name("updnRight");
			((Control)updnRight).set_Size(new Size(59, 20));
			((Control)updnRight).set_TabIndex(5);
			updnRight.set_Value(new decimal(new int[4] { 5, 0, 0, 0 }));
			updnRight.add_ValueChanged((EventHandler)updnRight_ValueChanged);
			((Control)updnLeft).set_Anchor((AnchorStyles)1);
			updnLeft.set_DecimalPlaces(1);
			((Control)updnLeft).set_Location(new Point(449, 58));
			updnLeft.set_Maximum(new decimal(new int[4] { 360, 0, 0, 0 }));
			updnLeft.set_Minimum(new decimal(new int[4] { 360, 0, 0, -2147483648 }));
			((Control)updnLeft).set_Name("updnLeft");
			((Control)updnLeft).set_Size(new Size(59, 20));
			((Control)updnLeft).set_TabIndex(7);
			updnLeft.set_Value(new decimal(new int[4] { 355, 0, 0, 0 }));
			updnLeft.add_ValueChanged((EventHandler)updnLeft_ValueChanged);
			((Control)updnBottom).set_Anchor((AnchorStyles)1);
			updnBottom.set_DecimalPlaces(1);
			updnBottom.set_Increment(new decimal(new int[4] { 5, 0, 0, 65536 }));
			((Control)updnBottom).set_Location(new Point(290, 58));
			updnBottom.set_Maximum(new decimal(new int[4] { 30, 0, 0, 65536 }));
			updnBottom.set_Minimum(new decimal(new int[4] { 35, 0, 0, -2147418112 }));
			((Control)updnBottom).set_Name("updnBottom");
			((Control)updnBottom).set_Size(new Size(40, 20));
			((Control)updnBottom).set_TabIndex(3);
			updnBottom.set_Value(new decimal(new int[4] { 3, 0, 0, -2147483648 }));
			updnBottom.add_ValueChanged((EventHandler)updnBottom_ValueChanged);
			((Control)cmdDrawProfile).set_Anchor((AnchorStyles)1);
			((Control)cmdDrawProfile).set_Location(new Point(897, 37));
			((Control)cmdDrawProfile).set_Name("cmdDrawProfile");
			((Control)cmdDrawProfile).set_Size(new Size(71, 43));
			((Control)cmdDrawProfile).set_TabIndex(17);
			((Control)cmdDrawProfile).set_Text("Re-draw");
			((ButtonBase)cmdDrawProfile).set_UseVisualStyleBackColor(true);
			((Control)cmdDrawProfile).add_Click((EventHandler)cmdDrawProfile_Click);
			toolTip.set_AutoPopDelay(2000);
			toolTip.set_InitialDelay(100);
			toolTip.set_ReshowDelay(100);
			((Control)lblTag).set_Anchor((AnchorStyles)1);
			((Control)lblTag).set_AutoSize(true);
			((Control)lblTag).set_Location(new Point(385, 86));
			((Control)lblTag).set_Name("lblTag");
			((Control)lblTag).set_Size(new Size(13, 13));
			((Control)lblTag).set_TabIndex(18);
			((Control)lblTag).set_Text("[]");
			((Control)label1).set_Anchor((AnchorStyles)1);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(189, 61));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(99, 13));
			((Control)label1).set_TabIndex(2);
			((Control)label1).set_Text("Height at BOTTOM");
			((Control)label2).set_Anchor((AnchorStyles)1);
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(342, 61));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(105, 13));
			((Control)label2).set_TabIndex(6);
			((Control)label2).set_Text("Axis Angle at RIGHT");
			((Control)label3).set_Anchor((AnchorStyles)1);
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Location(new Point(350, 35));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(97, 13));
			((Control)label3).set_TabIndex(4);
			((Control)label3).set_Text("Axis Angle at LEFT");
			((Control)label4).set_Anchor((AnchorStyles)1);
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Location(new Point(213, 35));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(75, 13));
			((Control)label4).set_TabIndex(0);
			((Control)label4).set_Text("Height at TOP");
			((Control)updnL).set_Anchor((AnchorStyles)1);
			updnL.set_DecimalPlaces(2);
			((Control)updnL).set_Enabled(false);
			updnL.set_Increment(new decimal(new int[4] { 1, 0, 0, 65536 }));
			((Control)updnL).set_Location(new Point(564, 32));
			updnL.set_Maximum(new decimal(new int[4] { 95, 0, 0, 65536 }));
			updnL.set_Minimum(new decimal(new int[4] { 95, 0, 0, -2147418112 }));
			((Control)updnL).set_Name("updnL");
			((Control)updnL).set_Size(new Size(49, 20));
			((Control)updnL).set_TabIndex(9);
			updnL.add_ValueChanged((EventHandler)updnL_ValueChanged);
			((Control)updnB).set_Anchor((AnchorStyles)1);
			updnB.set_DecimalPlaces(2);
			((Control)updnB).set_Enabled(false);
			updnB.set_Increment(new decimal(new int[4] { 1, 0, 0, 65536 }));
			((Control)updnB).set_Location(new Point(564, 58));
			updnB.set_Maximum(new decimal(new int[4] { 8, 0, 0, 0 }));
			updnB.set_Minimum(new decimal(new int[4] { 8, 0, 0, -2147483648 }));
			((Control)updnB).set_Name("updnB");
			((Control)updnB).set_Size(new Size(49, 20));
			((Control)updnB).set_TabIndex(11);
			updnB.add_ValueChanged((EventHandler)updnB_ValueChanged);
			((Control)label5).set_Anchor((AnchorStyles)1);
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Location(new Point(518, 61));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(40, 13));
			((Control)label5).set_TabIndex(10);
			((Control)label5).set_Text("Libn  B");
			((Control)label6).set_Anchor((AnchorStyles)1);
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Location(new Point(519, 35));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(39, 13));
			((Control)label6).set_TabIndex(8);
			((Control)label6).set_Text("Libn  L");
			((Control)label7).set_Anchor((AnchorStyles)1);
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Location(new Point(622, 35));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(41, 13));
			((Control)label7).set_TabIndex(12);
			((Control)label7).set_Text("Libn  D");
			((Control)updnD).set_Anchor((AnchorStyles)1);
			updnD.set_DecimalPlaces(2);
			((Control)updnD).set_Enabled(false);
			updnD.set_Increment(new decimal(new int[4] { 1, 0, 0, 65536 }));
			((Control)updnD).set_Location(new Point(669, 32));
			updnD.set_Maximum(new decimal(new int[4] { 11, 0, 0, 0 }));
			updnD.set_Minimum(new decimal(new int[4] { 11, 0, 0, -2147483648 }));
			((Control)updnD).set_Name("updnD");
			((Control)updnD).set_Size(new Size(49, 20));
			((Control)updnD).set_TabIndex(13);
			updnD.add_ValueChanged((EventHandler)updnD_ValueChanged);
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label8).set_Location(new Point(1, 71));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(151, 13));
			((Control)label8).set_TabIndex(25);
			((Control)label8).set_Text("Left-click to measure limb slope");
			((Control)updnScale).set_Anchor((AnchorStyles)1);
			updnScale.set_DecimalPlaces(3);
			updnScale.set_Increment(new decimal(new int[4] { 1, 0, 0, 131072 }));
			((Control)updnScale).set_Location(new Point(669, 58));
			updnScale.set_Maximum(new decimal(new int[4] { 115, 0, 0, 131072 }));
			updnScale.set_Minimum(new decimal(new int[4] { 85, 0, 0, 131072 }));
			((Control)updnScale).set_Name("updnScale");
			((Control)updnScale).set_Size(new Size(49, 20));
			((Control)updnScale).set_TabIndex(15);
			updnScale.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			updnScale.add_ValueChanged((EventHandler)updnScale_ValueChanged);
			((Control)label9).set_Anchor((AnchorStyles)1);
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Location(new Point(629, 61));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(34, 13));
			((Control)label9).set_TabIndex(14);
			((Control)label9).set_Text("Scale");
			((Control)lblSlope).set_Anchor((AnchorStyles)1);
			((Control)lblSlope).set_AutoSize(true);
			((Control)lblSlope).set_Location(new Point(743, 87));
			((Control)lblSlope).set_Name("lblSlope");
			((Control)lblSlope).set_Size(new Size(64, 13));
			((Control)lblSlope).set_TabIndex(26);
			((Control)lblSlope).set_Text("Last slope : ");
			((Control)lblSlope).set_Visible(false);
			((Control)grpDouble).set_Anchor((AnchorStyles)1);
			((Control)grpDouble).get_Controls().Add((Control)(object)lblDblPA);
			((Control)grpDouble).get_Controls().Add((Control)(object)lblDblSep);
			((Control)grpDouble).get_Controls().Add((Control)(object)updn_PA);
			((Control)grpDouble).get_Controls().Add((Control)(object)updn_Sep);
			((Control)grpDouble).get_Controls().Add((Control)(object)chkB);
			((Control)grpDouble).get_Controls().Add((Control)(object)chkA);
			((Control)grpDouble).set_Enabled(false);
			((Control)grpDouble).set_Location(new Point(724, 26));
			((Control)grpDouble).set_Name("grpDouble");
			((Control)grpDouble).set_Size(new Size(163, 57));
			((Control)grpDouble).set_TabIndex(16);
			grpDouble.set_TabStop(false);
			((Control)grpDouble).set_Text("Double star");
			((Control)lblDblPA).set_AutoSize(true);
			((Control)lblDblPA).set_Location(new Point(123, 35));
			((Control)lblDblPA).set_Name("lblDblPA");
			((Control)lblDblPA).set_Size(new Size(21, 13));
			((Control)lblDblPA).set_TabIndex(5);
			((Control)lblDblPA).set_Text("PA");
			((Control)lblDblSep).set_AutoSize(true);
			((Control)lblDblSep).set_Location(new Point(123, 16));
			((Control)lblDblSep).set_Name("lblDblSep");
			((Control)lblDblSep).set_Size(new Size(32, 13));
			((Control)lblDblSep).set_TabIndex(4);
			((Control)lblDblSep).set_Text("Sepn");
			updn_PA.set_DecimalPlaces(2);
			updn_PA.set_Increment(new decimal(new int[4] { 1, 0, 0, 65536 }));
			((Control)updn_PA).set_Location(new Point(69, 31));
			updn_PA.set_Maximum(new decimal(new int[4] { 360, 0, 0, 0 }));
			((Control)updn_PA).set_Name("updn_PA");
			((Control)updn_PA).set_Size(new Size(53, 20));
			((Control)updn_PA).set_TabIndex(3);
			updn_PA.add_ValueChanged((EventHandler)updn_dDec_ValueChanged);
			((Control)updn_PA).add_DoubleClick((EventHandler)updn_PA_DoubleClick);
			updn_Sep.set_DecimalPlaces(2);
			updn_Sep.set_Increment(new decimal(new int[4] { 1, 0, 0, 65536 }));
			((Control)updn_Sep).set_Location(new Point(69, 11));
			updn_Sep.set_Maximum(new decimal(new int[4] { 20, 0, 0, 0 }));
			updn_Sep.set_Minimum(new decimal(new int[4] { 2, 0, 0, -2147483648 }));
			((Control)updn_Sep).set_Name("updn_Sep");
			((Control)updn_Sep).set_Size(new Size(53, 20));
			((Control)updn_Sep).set_TabIndex(2);
			updn_Sep.add_ValueChanged((EventHandler)updn_dRA_ValueChanged);
			((Control)updn_Sep).add_DoubleClick((EventHandler)updn_Sep_DoubleClick);
			((Control)chkB).set_AutoSize(true);
			chkB.set_Checked(true);
			chkB.set_CheckState((CheckState)1);
			((Control)chkB).set_Location(new Point(9, 34));
			((Control)chkB).set_Name("chkB");
			((Control)chkB).set_Size(new Size(61, 17));
			((Control)chkB).set_TabIndex(1);
			((Control)chkB).set_Text("show B");
			((ButtonBase)chkB).set_UseVisualStyleBackColor(true);
			chkB.add_CheckedChanged((EventHandler)chkB_CheckedChanged);
			((Control)chkA).set_AutoSize(true);
			chkA.set_Checked(true);
			chkA.set_CheckState((CheckState)1);
			((Control)chkA).set_Location(new Point(9, 17));
			((Control)chkA).set_Name("chkA");
			((Control)chkA).set_Size(new Size(61, 17));
			((Control)chkA).set_TabIndex(0);
			((Control)chkA).set_Text("show A");
			((ButtonBase)chkA).set_UseVisualStyleBackColor(true);
			chkA.add_CheckedChanged((EventHandler)chkA_CheckedChanged);
			((Control)chkLOLApoints).set_Anchor((AnchorStyles)1);
			chkLOLApoints.set_AutoCheck(false);
			((Control)chkLOLApoints).set_AutoSize(true);
			((Control)chkLOLApoints).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "RednLOLAPoints", true, (DataSourceUpdateMode)1));
			((Control)chkLOLApoints).set_Location(new Point(6, 33));
			((Control)chkLOLApoints).set_Name("chkLOLApoints");
			((Control)chkLOLApoints).set_Size(new Size(84, 17));
			((Control)chkLOLApoints).set_TabIndex(19);
			((Control)chkLOLApoints).set_Text("LOLA points");
			((ButtonBase)chkLOLApoints).set_UseVisualStyleBackColor(true);
			((Control)chkLOLApoints).add_Click((EventHandler)chkLOLApoints_Click);
			((Control)chkLOLAHires).set_Anchor((AnchorStyles)1);
			chkLOLAHires.set_AutoCheck(false);
			((Control)chkLOLAHires).set_AutoSize(true);
			chkLOLAHires.set_Checked(Settings.Default.RednLOLAHires);
			((Control)chkLOLAHires).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "RednLOLAHires", true, (DataSourceUpdateMode)1));
			((Control)chkLOLAHires).set_Location(new Point(6, 53));
			((Control)chkLOLAHires).set_Name("chkLOLAHires");
			((Control)chkLOLAHires).set_Size(new Size(85, 17));
			((Control)chkLOLAHires).set_TabIndex(20);
			((Control)chkLOLAHires).set_Text("LOLA HiRes");
			((ButtonBase)chkLOLAHires).set_UseVisualStyleBackColor(true);
			((Control)chkLOLAHires).add_Click((EventHandler)chkLOLAHires_Click);
			((Control)picProfile).set_BackColor(Color.White);
			picProfile.set_BorderStyle((BorderStyle)1);
			((Control)picProfile).set_ContextMenuStrip(contextMenuStrip1);
			((Control)picProfile).set_Location(new Point(10, 86));
			((Control)picProfile).set_Name("picProfile");
			((Control)picProfile).set_Size(new Size(981, 392));
			picProfile.set_TabIndex(1);
			picProfile.set_TabStop(false);
			((Control)picProfile).add_MouseMove(new MouseEventHandler(picProfile_MouseMove));
			((Control)picProfile).add_MouseUp(new MouseEventHandler(picProfile_MouseUp));
			((Control)chkExcludeLargeResiduals).set_Anchor((AnchorStyles)1);
			((Control)chkExcludeLargeResiduals).set_AutoSize(true);
			chkExcludeLargeResiduals.set_Checked(true);
			chkExcludeLargeResiduals.set_CheckState((CheckState)1);
			((Control)chkExcludeLargeResiduals).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkExcludeLargeResiduals).set_Location(new Point(102, 26));
			((Control)chkExcludeLargeResiduals).set_Name("chkExcludeLargeResiduals");
			((Control)chkExcludeLargeResiduals).set_Size(new Size(96, 30));
			((Control)chkExcludeLargeResiduals).set_TabIndex(28);
			((Control)chkExcludeLargeResiduals).set_Text("Exclude\r\nresiduals >0.2\"");
			((ButtonBase)chkExcludeLargeResiduals).set_UseVisualStyleBackColor(true);
			((ToolStripItem)drawInColorToolStripMenuItem).set_Image((Image)Resources.ColorHS);
			((ToolStripItem)drawInColorToolStripMenuItem).set_Name("drawInColorToolStripMenuItem");
			((ToolStripItem)drawInColorToolStripMenuItem).set_Size(new Size(211, 22));
			((ToolStripItem)drawInColorToolStripMenuItem).set_Text("Draw in color");
			((ToolStripItem)drawInColorToolStripMenuItem).add_Click((EventHandler)drawInColorToolStripMenuItem_Click);
			((ToolStripItem)eventsInColorToolStripMenuItem).set_Image((Image)Resources.ColorHS);
			((ToolStripItem)eventsInColorToolStripMenuItem).set_Name("eventsInColorToolStripMenuItem");
			((ToolStripItem)eventsInColorToolStripMenuItem).set_Size(new Size(211, 22));
			((ToolStripItem)eventsInColorToolStripMenuItem).set_Text("Show events in color");
			((ToolStripItem)eventsInColorToolStripMenuItem).add_Click((EventHandler)eventsInColorToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator4).set_Name("toolStripSeparator4");
			((ToolStripItem)toolStripSeparator4).set_Size(new Size(208, 6));
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(1000, 488));
			((Control)this).get_Controls().Add((Control)(object)chkExcludeLargeResiduals);
			((Control)this).get_Controls().Add((Control)(object)chkLOLAHires);
			((Control)this).get_Controls().Add((Control)(object)chkLOLApoints);
			((Control)this).get_Controls().Add((Control)(object)grpDouble);
			((Control)this).get_Controls().Add((Control)(object)lblSlope);
			((Control)this).get_Controls().Add((Control)(object)label9);
			((Control)this).get_Controls().Add((Control)(object)updnScale);
			((Control)this).get_Controls().Add((Control)(object)label8);
			((Control)this).get_Controls().Add((Control)(object)label7);
			((Control)this).get_Controls().Add((Control)(object)updnD);
			((Control)this).get_Controls().Add((Control)(object)label6);
			((Control)this).get_Controls().Add((Control)(object)label5);
			((Control)this).get_Controls().Add((Control)(object)updnB);
			((Control)this).get_Controls().Add((Control)(object)updnL);
			((Control)this).get_Controls().Add((Control)(object)label4);
			((Control)this).get_Controls().Add((Control)(object)label3);
			((Control)this).get_Controls().Add((Control)(object)label2);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)lblTag);
			((Control)this).get_Controls().Add((Control)(object)cmdDrawProfile);
			((Control)this).get_Controls().Add((Control)(object)picProfile);
			((Control)this).get_Controls().Add((Control)(object)updnLeft);
			((Control)this).get_Controls().Add((Control)(object)updnBottom);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_Controls().Add((Control)(object)updnRight);
			((Control)this).get_Controls().Add((Control)(object)updnTop);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationReductionProfile", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_Location(Settings.Default.LocationReductionProfile);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_MinimumSize(new Size(778, 495));
			((Control)this).set_Name("ReductionPlot");
			((Control)this).set_Text("Reduction profile");
			((Form)this).add_FormClosing(new FormClosingEventHandler(ReductionPlot_FormClosing));
			((Form)this).add_FormClosed(new FormClosedEventHandler(ReductionPlot_FormClosed));
			((Form)this).add_Load((EventHandler)ReductionPlot_Load);
			((Form)this).add_ResizeBegin((EventHandler)ReductionPlot_ResizeBegin);
			((Form)this).add_ResizeEnd((EventHandler)ReductionPlot_ResizeEnd);
			((Control)this).add_Resize((EventHandler)ReductionPlot_Resize);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)contextMenuStrip1).ResumeLayout(false);
			((ISupportInitialize)updnTop).EndInit();
			((ISupportInitialize)updnRight).EndInit();
			((ISupportInitialize)updnLeft).EndInit();
			((ISupportInitialize)updnBottom).EndInit();
			((ISupportInitialize)updnL).EndInit();
			((ISupportInitialize)updnB).EndInit();
			((ISupportInitialize)updnD).EndInit();
			((ISupportInitialize)updnScale).EndInit();
			((Control)grpDouble).ResumeLayout(false);
			((Control)grpDouble).PerformLayout();
			((ISupportInitialize)updn_PA).EndInit();
			((ISupportInitialize)updn_Sep).EndInit();
			((ISupportInitialize)picProfile).EndInit();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
