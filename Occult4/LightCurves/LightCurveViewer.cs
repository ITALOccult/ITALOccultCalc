using System;
using System.Collections;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Text;
using System.Windows.Forms;
using AOTA;
using Occult;
using Occult.Properties;

namespace LightCurves
{
	public class LightCurveViewer : Form
	{
		private bool IsPlotting;

		private bool PlotMain;

		private int NumPoints;

		private int SelectedCatalogue;

		internal static LightCurve_MultiPlot MultiPlot;

		internal static LightCurve_Edit LCE;

		internal static int LightCurveIndex_Current = 0;

		private ArrayList Indices = new ArrayList();

		private ArrayList Dates = new ArrayList();

		private bool External;

		internal static float VerticalPlotHeight = 1f;

		internal static float VerticalPlotScale = 1f;

		internal static float PicPlotInitialHeight = -1f;

		private bool LoadMyReportedLightCurves;

		internal bool LastPlotWasMain = true;

		internal bool LastPlotWasSubmitted;

		internal bool LastPlotWasSubmittedAll;

		internal bool LastPlotWasAsteroidNumber;

		internal bool LastPlotWasAsteroidName;

		internal bool PlotAllAsteroidByNumber;

		internal bool PlotAllAsteroidByName;

		internal static DisplayData NewInOld_Events;

		internal double OldScaleValue = 1.0;

		private IContainer components;

		internal PictureBox picPlot;

		internal Panel panelPlot;

		private MenuStrip menuStrip1;

		private Label label1;

		private Label label2;

		private Button cmd_x15;

		private Button cmd_x10;

		private Button cmd_x5;

		private Button cmd_x1;

		internal NumericUpDown updnScale;

		private Label label3;

		private Panel panel1;

		internal TextBox txtStar;

		internal TextBox txtCirc;

		internal TextBox txtObs;

		private ListBox lstDates;

		private Panel pnlStarByDate;

		private Label lblStarCat;

		private ToolStripMenuItem exitToolStripMenuItem;

		private Label lblIndexName;

		private ToolStripMenuItem helpToolStripMenuItem;

		private Label lblLightCurves;

		private Label lblObserverCount;

		internal ComboBox cmbStar;

		internal RadioButton optXZ;

		internal RadioButton optSAO;

		internal RadioButton optHip;

		internal RadioButton optTycho2;

		internal RadioButton optZC;

		internal RadioButton optU4;

		private Panel panel3;

		private Label label6;

		internal Panel pnlSelectStar;

		internal ToolStripMenuItem adminFunctionsToolStripMenuItem;

		private ToolStripMenuItem editALightCurveMainFileToolStripMenuItem;

		private ToolStripMenuItem editALightCurveSubmittedFileToolStripMenuItem;

		private ToolStripMenuItem mergeSubmittedLightCurveIntoTheMainFileToolStripMenuItem;

		private ToolTip toolTip1;

		private CheckBox chkTimeHeight;

		internal ToolStripMenuItem viewLightCurvesMarkedForDeletiondelinitToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator3;

		private ToolStripMenuItem manageEmbargoesToolStripMenuItem;

		private ToolStripMenuItem view0LightCurvesMarkedForReviewreviewToolStripMenuItem;

		private ToolStripMenuItem rereadLightCurvesToolStripMenuItem;

		private ToolStripMenuItem editToolStripMenuItem;

		private ToolStripMenuItem withDisplayedLightCurveToolStripMenuItem;

		private ToolStripMenuItem viewToolStripMenuItem1;

		private ToolStripMenuItem my0UnsubmittedLightCurvesToolStripMenuItem;

		private ToolStripMenuItem myReportedLightCurvesToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem1;

		private ToolStripMenuItem displayInAOTAToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator5;

		private ToolStripMenuItem rereadSubmittedLightCurvesToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator7;

		private ToolStripMenuItem listSubmittedLightCurvesAlreadyInTheMainFileToolStripMenuItem;

		private ToolStripMenuItem listAndDeletesubmittedCurvesAlreadyInMainFileToolStripMenuItem;

		private TextBox txtRecord;

		internal Label lblRecords;

		internal ComboBox cmbAsteroidNumber;

		private Label label7;

		private Panel panel6;

		private Panel panel5;

		private Panel panel4;

		internal ComboBox cmbObserver;

		private Label label4;

		private Panel panel7;

		internal Label label8;

		private ListBox lstSubmitted;

		private Label lblSubmitted;

		private Label lblAsteroidCount;

		internal ComboBox cmbByDate;

		private Label label5;

		private Label label9;

		private Panel panel2;

		private Button cmdUp;

		private Button cmdDown;

		private Panel panel8;

		private Label lblAstNamesCount;

		private Label label11;

		internal ComboBox cmbAsteroidName;

		private Button cmdCopy;

		private ToolStripMenuItem rightClickToSetMeanLightToolStripMenuItem;

		internal TextBox txtAsteroidNumber;

		internal TextBox txtObserver;

		internal TextBox txtAsteroidName;

		private ToolStripMenuItem lightCurveStatsforVizieRToolStripMenuItem;

		private ToolStripMenuItem saveImageToolStripMenuItem;

		public TrackBar SliderHeight;

		private Label lblPlotHeight;

		private ToolStripTextBox HeaderText;

		internal ToolStripMenuItem addTextToolStripMenuItem;

		internal ToolStripMenuItem addMarkerForThisUTTime1;

		internal ToolStripTextBox toolStripTextBox_UT1;

		private Panel panel9;

		private Label lblFormOpacity;

		public TrackBar SliderOpacity;

		private Button cmdAstNameDec;

		private Button cmdAstNameInc;

		private Button cmdAstDateDec;

		private Button cmdAstDateInc;

		private Button cmdAstNumDec;

		private Button cmdAstNumInc;

		private Label label13;

		private Button cmdPlotAll_Number;

		private Button cmdPlotAll_Name;

		private Button cmbPlotAll_Star;

		private Button cmdPlotAll_Observer;

		private Button cmdPlotAll_Submitted;

		private Panel panel10;

		private CheckBox chkFineScroll;

		private ToolStripMenuItem addMarkerForThisUTTime2;

		private ToolStripTextBox toolStripTextBox_UT2;

		private ToolStripMenuItem writecsvFileForThisLightCurveToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator9;

		private ToolStripSeparator toolStripSeparator2;

		private ToolStripMenuItem manageMyUnsubmittedLightCurvesToolStripMenuItem;

		private ToolStripMenuItem removeReviewStatusToolStripMenuItem;

		private ToolStripMenuItem deleteDelFilesToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator1;

		private ToolStripMenuItem submittedLightCurvesToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator6;

		private ToolStripMenuItem mergeSubmittedWithMainToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator4;

		internal ToolStripMenuItem drawWithThickLinesToolStripMenuItem;

		internal ToolStripMenuItem indicateMissedImagesToolStripMenuItem;

		private Label lblVerticalScale;

		public TrackBar SliderVerticalScale;

		public LightCurveViewer()
		{
			InitializeComponent();
		}

		private void LightCurveViewer_Load(object sender, EventArgs e)
		{
			((ToolStripItem)adminFunctionsToolStripMenuItem).set_Visible(Settings.Default.Administrator);
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			if (!Directory.Exists(Utilities.AppPath + "\\LightCurves"))
			{
				Directory.CreateDirectory(Utilities.AppPath + "\\LightCurves");
				Directory.CreateDirectory(Utilities.AppPath + "\\Observations\\LightCurves\\Reported");
			}
			if (((Control)this).get_Top() < -30000)
			{
				((Control)this).set_Top(0);
			}
			if ((double)((Control)this).get_Left() + (double)((Control)this).get_Width() / 1.2 < (double)Screen.GetWorkingArea((Control)(object)this).Left)
			{
				((Control)this).set_Left(Screen.GetWorkingArea((Control)(object)this).Left);
			}
			if (PicPlotInitialHeight < 0f)
			{
				PicPlotInitialHeight = ((Control)this).get_Height() - 276;
			}
			ReadLightCurves();
			if (Settings.Default.Administrator)
			{
				ReadLightCurvesReceived(0, ShowReadErrorMessages: true);
			}
			SetNumberOfLightCurves();
			((Control)txtObserver).set_Text(LightData.MainLightCurves[0].Observer.Trim());
			int num = ((Control)txtObserver).get_Text().LastIndexOf(" ");
			if (num > 0)
			{
				((Control)txtObserver).set_Text(((Control)txtObserver).get_Text().Substring(num + 1));
			}
			LightData.CreateObserverNameIndex(((Control)txtObserver).get_Text());
			LightData.CreateAsteroidNameIndex(((Control)txtAsteroidName).get_Text());
			if (cmbStar.get_Items().get_Count() > 0)
			{
				((ListControl)cmbStar).set_SelectedIndex(0);
			}
			((Control)pnlStarByDate).set_Location(((Control)pnlSelectStar).get_Location());
			toolTip1.set_Active(chkTimeHeight.get_Checked());
			OldScaleValue = (double)updnScale.get_Value();
			SetSizes(ResizeEvent: true);
			((Control)txtRecord).set_Text((LightData.MainLightCurves.Count - 100).ToString());
		}

		internal void ReadLightCurves()
		{
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			Application.DoEvents();
			cmbAsteroidNumber.get_Items().Clear();
			cmbAsteroidName.get_Items().Clear();
			cmbStar.get_Items().Clear();
			LightData.ReadMainFile(Index: true, ReRead: true, ShowReadErrorMessages: true);
			((Control)lblRecords).set_Text(string.Format("Record number ( 1 to {0,1})", LightData.MainLightCurves.Count));
			LightCurveStarIndex.SortByDate = true;
			LightCurveStarIndex.SortByName = false;
			LightData.AsteroidIndex.Sort();
			for (int i = 0; i < LightData.AsteroidIndex.Count; i++)
			{
				cmbByDate.get_Items().Add((object)LightData.AsteroidIndex[i].ToString());
			}
			LightCurveStarIndex.SortByDate = false;
			LightCurveStarIndex.SortByName = false;
			LightData.AsteroidIndex.Sort();
			for (int j = 0; j < LightData.AsteroidIndex.Count; j++)
			{
				cmbAsteroidNumber.get_Items().Add((object)LightData.AsteroidIndex[j].ToString());
			}
			PopulateNameSelection();
			ReadMyPendingReports();
			if (LoadMyReportedLightCurves)
			{
				ReadMyReports();
			}
			ReadLightCurvesReceived(0, ShowReadErrorMessages: true);
			Read_ToDelete_LightCurves(ShowMultiPlot: false);
			Read_ToReview_LightCurves(ShowMultiPlot: false);
			LoadMyReportedLightCurves = true;
			((Control)this).set_Cursor(Cursors.get_Default());
		}

		private void SetNumberOfLightCurves()
		{
			((Control)lblLightCurves).set_Text("# light curves: " + LightData.MainLightCurves.Count + "  ");
		}

		private void LightCurveViewer_Resize(object sender, EventArgs e)
		{
			SetSizes(ResizeEvent: true);
		}

		internal void SetSizes(bool ResizeEvent)
		{
			int num = (int)((double)((float)((ScrollProperties)((ScrollableControl)panelPlot).get_HorizontalScroll()).get_Value() + (float)((Control)panelPlot).get_Width() / 2f) / OldScaleValue);
			int num2 = ((Control)panel3).get_Width() + ((Control)txtStar).get_Width() + ((Control)txtObs).get_Width() + ((Control)txtCirc).get_Width() + ((Control)lstSubmitted).get_Width() + 39;
			if (((Control)this).get_Width() < num2)
			{
				((Control)this).set_Width(num2);
			}
			((Control)pnlSelectStar).set_Left(((Control)this).get_Width() - 240);
			((Control)label9).set_Left(((Control)this).get_Width() - 130 - ((Control)label9).get_Width() / 2);
			((Control)panelPlot).set_Width(((Control)this).get_Width() - 255);
			((Control)panelPlot).set_Height(((Control)this).get_Height() - 250);
			((Control)picPlot).set_Height(((Control)panelPlot).get_Height() - 26);
			((Control)picPlot).set_Width(((Control)panelPlot).get_Width() - 12);
			((Control)panel1).set_Top(((Control)this).get_Height() - 229);
			if (PlotMain)
			{
				PlotLightCurveFromMainFile(LightCurveIndex_Current, indicateMissedImagesToolStripMenuItem.get_Checked());
			}
			else
			{
				PlotLightCurveFromSubmittedFile(LightCurveIndex_Current, indicateMissedImagesToolStripMenuItem.get_Checked());
			}
			((Control)panel1).set_Width(((Control)this).get_Width() - 28);
			int num3 = (((Control)panel1).get_Width() - ((Control)panel3).get_Width() - ((Control)txtStar).get_Width() - ((Control)txtObs).get_Width() - ((Control)txtCirc).get_Width() - ((Control)lstSubmitted).get_Width() - 7) / 4;
			TextBox obj = txtStar;
			int left;
			((Control)label1).set_Left(left = ((Control)panel3).get_Right() + num3);
			((Control)obj).set_Left(left);
			TextBox obj2 = txtObs;
			((Control)label3).set_Left(left = ((Control)txtStar).get_Right() + num3);
			((Control)obj2).set_Left(left);
			TextBox obj3 = txtCirc;
			((Control)label2).set_Left(left = ((Control)txtObs).get_Right() + num3);
			((Control)obj3).set_Left(left);
			ListBox obj4 = lstSubmitted;
			((Control)lblSubmitted).set_Left(left = ((Control)txtCirc).get_Right() + num3);
			((Control)obj4).set_Left(left);
			((Control)cmdPlotAll_Submitted).set_Left(((Control)lstSubmitted).get_Right() - ((Control)cmdPlotAll_Submitted).get_Width());
			((Control)panel9).set_Left(0);
			((Control)panel9).set_Top(((Control)picPlot).get_Bottom());
			((Control)panel9).set_Width(((Control)panelPlot).get_Width() - 4);
			((Control)panel9).set_Height(15);
			TrackBar sliderHeight = SliderHeight;
			TrackBar sliderVerticalScale = SliderVerticalScale;
			int num4;
			((Control)SliderOpacity).set_Width(num4 = ((Control)txtStar).get_Left() - 5);
			((Control)sliderVerticalScale).set_Width(left = num4);
			((Control)sliderHeight).set_Width(left);
			((Control)lblPlotHeight).set_Text(string.Format("Plot Height  {0,1:f0}%", 100 * SliderHeight.get_Value() / SliderHeight.get_Maximum()));
			((Control)lblPlotHeight).set_Left(((Control)SliderHeight).get_Left() + (((Control)SliderHeight).get_Width() - ((Control)lblPlotHeight).get_Width()) / 2);
			((Control)lblVerticalScale).set_Left(((Control)SliderVerticalScale).get_Left() + (((Control)SliderVerticalScale).get_Width() - ((Control)lblVerticalScale).get_Width()) / 2);
			((Control)lblFormOpacity).set_Left(((Control)SliderOpacity).get_Left() + (((Control)SliderOpacity).get_Width() - ((Control)lblFormOpacity).get_Width()) / 2);
			OldScaleValue = (double)updnScale.get_Value();
			int num5 = (int)((double)num * OldScaleValue - (double)((float)((Control)panelPlot).get_Width() / 2f));
			if (num5 < 1)
			{
				num5 = 1;
			}
			if (num5 > 32000)
			{
				num5 = 31000;
			}
			if (num5 > ((ScrollProperties)((ScrollableControl)panelPlot).get_HorizontalScroll()).get_Maximum())
			{
				num5 = ((ScrollProperties)((ScrollableControl)panelPlot).get_HorizontalScroll()).get_Maximum();
			}
			try
			{
				((ScrollProperties)((ScrollableControl)panelPlot).get_HorizontalScroll()).set_Value(num5);
				((ScrollProperties)((ScrollableControl)panelPlot).get_HorizontalScroll()).set_Value(num5);
				Application.DoEvents();
			}
			catch
			{
			}
		}

		private void cmd_x1_Click(object sender, EventArgs e)
		{
			updnScale.set_Value(1m);
		}

		private void cmd_x5_Click(object sender, EventArgs e)
		{
			updnScale.set_Value(5m);
		}

		private void cmd_x10_Click(object sender, EventArgs e)
		{
			updnScale.set_Value(10m);
		}

		private void cmd_x15_Click(object sender, EventArgs e)
		{
			updnScale.set_Value(15m);
		}

		private void updnScale_ValueChanged(object sender, EventArgs e)
		{
			int num = (int)((double)((float)((ScrollProperties)((ScrollableControl)panelPlot).get_HorizontalScroll()).get_Value() + (float)((Control)this).get_Width() / 2f) / OldScaleValue);
			Application.DoEvents();
			if (PlotMain)
			{
				PlotLightCurveFromMainFile(LightCurveIndex_Current, indicateMissedImagesToolStripMenuItem.get_Checked());
			}
			else
			{
				PlotLightCurveFromSubmittedFile(LightCurveIndex_Current, indicateMissedImagesToolStripMenuItem.get_Checked());
			}
			OldScaleValue = (double)updnScale.get_Value();
			int num2 = (int)((double)num * OldScaleValue - (double)((float)((Control)this).get_Width() / 2f) + OldScaleValue / 2.0);
			if (num2 < 1)
			{
				num2 = 1;
			}
			if (num2 > 32000)
			{
				num2 = 31000;
			}
			if (num2 > ((ScrollProperties)((ScrollableControl)panelPlot).get_HorizontalScroll()).get_Maximum())
			{
				num2 = ((ScrollProperties)((ScrollableControl)panelPlot).get_HorizontalScroll()).get_Maximum();
			}
			try
			{
				((ScrollProperties)((ScrollableControl)panelPlot).get_HorizontalScroll()).set_Value(num2);
				((ScrollProperties)((ScrollableControl)panelPlot).get_HorizontalScroll()).set_Value(num2);
			}
			catch
			{
			}
		}

		private void PlotLightCurve(int Main_index, int Submitted_index)
		{
			if (LightData.IndexesCreated)
			{
				if (Main_index >= 0)
				{
					NumPoints = LightData.MainLightCurves[Main_index].NumPoints;
				}
				else
				{
					NumPoints = LightData.LightCurvesSubmitted[Submitted_index].NumPoints;
				}
				decimal num = (decimal)(int)((double)(100 * (((Control)panelPlot).get_Width() - 14)) / (double)NumPoints) / 100m;
				if (num < 0.2m)
				{
					num = 0.2m;
				}
				if (num > 30m)
				{
					num = 30m;
				}
				updnScale.set_Value(num);
				IsPlotting = false;
				if (Main_index >= 0)
				{
					PlotLightCurveFromMainFile(Main_index, indicateMissedImagesToolStripMenuItem.get_Checked());
				}
				else if (Submitted_index >= 0)
				{
					PlotLightCurveFromSubmittedFile(Submitted_index, indicateMissedImagesToolStripMenuItem.get_Checked());
				}
			}
		}

		internal void PlotLightCurveFromMainFile(int RecordNumber, bool ShowSkippedFrames)
		{
			if (RecordNumber >= 0 && LightData.IndexesCreated && !IsPlotting && NumPoints >= 3 && !External)
			{
				IsPlotting = true;
				((Control)updnScale).set_Enabled(false);
				((Control)picPlot).set_Width((int)((decimal)NumPoints * updnScale.get_Value()));
				picPlot.set_Image((Image)LightData.LightCurveImage(RecordNumber, ((Control)picPlot).get_Width(), (int)((float)((Control)picPlot).get_Height() * VerticalPlotHeight), (int)((float)((Control)picPlot).get_Height() * (1f - VerticalPlotHeight) / 2f), (float)updnScale.get_Value(), VerticalPlotScale, Main: true, Submitted: false, Mine: false, Pending: false, ToDelete: false, ToReview: false, ShowSkippedFrames, IsMultiPlot: false, drawWithThickLinesToolStripMenuItem.get_Checked()));
				try
				{
					Application.DoEvents();
				}
				catch
				{
				}
				((Control)updnScale).set_Enabled(true);
				IsPlotting = false;
			}
		}

		internal void PlotLightCurveFromSubmittedFile(int RecordNumber, bool ShowSkippedFrames)
		{
			if (RecordNumber >= 0 && LightData.IndexesCreated && !IsPlotting && NumPoints >= 3 && !External)
			{
				IsPlotting = true;
				((Control)updnScale).set_Enabled(false);
				((Control)picPlot).set_Width((int)((decimal)NumPoints * updnScale.get_Value()));
				picPlot.set_Image((Image)LightData.LightCurveImage(RecordNumber, ((Control)picPlot).get_Width(), (int)((float)((Control)picPlot).get_Height() * VerticalPlotHeight), (int)((float)((Control)picPlot).get_Height() * (1f - VerticalPlotHeight) / 2f), (float)updnScale.get_Value(), VerticalPlotScale, Main: false, Submitted: true, Mine: false, Pending: false, ToDelete: false, ToReview: false, ShowSkippedFrames, IsMultiPlot: false, drawWithThickLinesToolStripMenuItem.get_Checked()));
				try
				{
					Application.DoEvents();
				}
				catch
				{
				}
				((Control)updnScale).set_Enabled(true);
				IsPlotting = false;
			}
		}

		internal void MultiCurvePlot(ref List<int> RecordNumbersForMultiCurve, bool Main, bool Submitted, bool Mine, bool Pending, bool ToDelete, bool ToReview, bool ShowSkippedFrames)
		{
			//IL_003c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0042: Invalid comparison between Unknown and I4
			if (RecordNumbersForMultiCurve == null || RecordNumbersForMultiCurve.Count < 1)
			{
				return;
			}
			List<Bitmap> list = new List<Bitmap>();
			Bitmap bitmap = null;
			int num = 0;
			int num2 = RecordNumbersForMultiCurve.Count - 1;
			if (num2 > 599)
			{
				LightCurvesSelectRange lightCurvesSelectRange = new LightCurvesSelectRange(RecordNumbersForMultiCurve.Count);
				if ((int)((Form)lightCurvesSelectRange).ShowDialog() == 2)
				{
					return;
				}
				num = lightCurvesSelectRange.FirstRecord;
				num2 = lightCurvesSelectRange.LastRecord;
			}
			try
			{
				((Control)MultiPlot).Show();
			}
			catch
			{
				MultiPlot = new LightCurve_MultiPlot();
				((Control)MultiPlot).Show();
			}
			((Form)MultiPlot).set_Location(Settings.Default.LocationMultiLightCurves);
			if ((((Control)MultiPlot).get_Left() < -5000) | (((Control)MultiPlot).get_Top() < -5000))
			{
				LightCurve_MultiPlot multiPlot = MultiPlot;
				int left;
				((Control)MultiPlot).set_Top(left = 0);
				((Control)multiPlot).set_Left(left);
			}
			if (Mine)
			{
				((Control)MultiPlot).set_Text("My reported light curves");
			}
			else if (Submitted)
			{
				((Control)MultiPlot).set_Text("Light curves reported to me");
			}
			else if (Pending)
			{
				((Control)MultiPlot).set_Text("My light curves that are waiting to be reported");
			}
			else if (ToDelete)
			{
				((Control)MultiPlot).set_Text("Light curves marked as 'To Delete' or 'pre-Edit'");
			}
			else if (ToReview)
			{
				((Control)MultiPlot).set_Text("Light curves marked as 'To Review'");
			}
			else
			{
				((Control)MultiPlot).set_Text("Light curves from Occult light curves file");
			}
			int num3 = num2 - num + 1;
			int num4 = (((Control)MultiPlot.picPlotAll).get_Width() - 10) / 2;
			int num5 = num4 / 2;
			int num6 = 1 + (num3 - 1) / 2;
			int height = num5 * num6 + 14;
			((Control)MultiPlot.picPlotAll).set_Height(height);
			if (num6 < 5)
			{
				((Control)MultiPlot.panelAll).set_Height(height);
			}
			else
			{
				((Control)MultiPlot.panelAll).set_Height(num5 * 4);
			}
			((Control)MultiPlot).set_Height(((Control)MultiPlot.panelAll).get_Height() + 90);
			double num7 = 0.0;
			for (int i = num; i <= num2; i++)
			{
				if (Main)
				{
					num7 = LightData.MainLightCurves[RecordNumbersForMultiCurve[i]].NumPoints;
				}
				else if (Mine)
				{
					num7 = LightData.MyReportedLightCurves[RecordNumbersForMultiCurve[i]].NumPoints;
				}
				else if (Submitted)
				{
					num7 = LightData.LightCurvesSubmitted[RecordNumbersForMultiCurve[i]].NumPoints;
				}
				else if (Pending)
				{
					num7 = LightData.MyPendingLightCurves[RecordNumbersForMultiCurve[i]].NumPoints;
				}
				else if (ToDelete)
				{
					num7 = LightData.ToDeleteLightCurves[RecordNumbersForMultiCurve[i]].NumPoints;
				}
				else if (ToReview)
				{
					num7 = LightData.ToReviewLightCurves[RecordNumbersForMultiCurve[i]].NumPoints;
				}
				float plotScale = (float)((double)(num4 - 10) / num7);
				if (Main)
				{
					list.Add(LightData.LightCurveImage(RecordNumbersForMultiCurve[i], num4 - 10, num5 - 10, 0, plotScale, 1f, Main: true, Submitted: false, Mine: false, Pending: false, ToDelete: false, ToReview: false, ShowSkippedFrames, IsMultiPlot: true, ThickLines: false));
				}
				else if (Mine)
				{
					list.Add(LightData.LightCurveImage(RecordNumbersForMultiCurve[i], num4 - 10, num5 - 10, 0, plotScale, 1f, Main: false, Submitted: false, Mine: true, Pending: false, ToDelete: false, ToReview: false, ShowSkippedFrames, IsMultiPlot: true, ThickLines: false));
				}
				else if (Submitted)
				{
					list.Add(LightData.LightCurveImage(RecordNumbersForMultiCurve[i], num4 - 10, num5 - 10, 0, plotScale, 1f, Main: false, Submitted: true, Mine: false, Pending: false, ToDelete: false, ToReview: false, ShowSkippedFrames, IsMultiPlot: true, ThickLines: false));
				}
				else if (Pending)
				{
					list.Add(LightData.LightCurveImage(RecordNumbersForMultiCurve[i], num4 - 10, num5 - 10, 0, plotScale, 1f, Main: false, Submitted: false, Mine: false, Pending: true, ToDelete: false, ToReview: false, ShowSkippedFrames, IsMultiPlot: true, ThickLines: false));
				}
				else if (ToDelete)
				{
					list.Add(LightData.LightCurveImage(RecordNumbersForMultiCurve[i], num4 - 10, num5 - 10, 0, plotScale, 1f, Main: false, Submitted: false, Mine: false, Pending: false, ToDelete: true, ToReview: false, ShowSkippedFrames, IsMultiPlot: true, ThickLines: false));
				}
				else if (ToReview)
				{
					list.Add(LightData.LightCurveImage(RecordNumbersForMultiCurve[i], num4 - 10, num5 - 10, 0, plotScale, 1f, Main: false, Submitted: false, Mine: false, Pending: false, ToDelete: false, ToReview: true, ShowSkippedFrames, IsMultiPlot: true, ThickLines: false));
				}
			}
			bitmap = new Bitmap(((Control)MultiPlot.picPlotAll).get_Width(), ((Control)MultiPlot.picPlotAll).get_Height());
			using Graphics graphics = Graphics.FromImage(bitmap);
			graphics.Clear(Color.White);
			int num8 = 0;
			int num9 = 0;
			int num10 = 0;
			foreach (Bitmap item in list)
			{
				num9 = num8 % 2 * num4;
				num10 = num8 / 2 * num5;
				graphics.DrawImage(item, new Rectangle(5 + num9, 10 + num10, item.Width, item.Height));
				num8++;
			}
			MultiPlot.picPlotAll.set_Image((Image)bitmap);
		}

		internal void MultiCurvePlot_Main_Submitted(ref List<int> MainRecordNumbers, ref List<int> SubmittedRecordNumbers, bool ShowSkippedFrames)
		{
			MultiCurvePlot_Main_Submitted(ref MainRecordNumbers, ref SubmittedRecordNumbers, ShowSkippedFrames, SaveImages: false);
		}

		internal void MultiCurvePlot_Main_Submitted(ref List<int> MainRecordNumbers, ref List<int> SubmittedRecordNumbers, bool ShowSkippedFrames, bool SaveImages)
		{
			//IL_0055: Unknown result type (might be due to invalid IL or missing references)
			//IL_005b: Invalid comparison between Unknown and I4
			string text = Utilities.AppPath + "\\LightCurvePlots";
			if (!Directory.Exists(text))
			{
				Directory.CreateDirectory(text);
			}
			List<Bitmap> list = new List<Bitmap>();
			Bitmap bitmap = null;
			int num = 0;
			int num2 = MainRecordNumbers.Count + SubmittedRecordNumbers.Count - 1;
			if (num2 > 599)
			{
				LightCurvesSelectRange lightCurvesSelectRange = new LightCurvesSelectRange(MainRecordNumbers.Count);
				if ((int)((Form)lightCurvesSelectRange).ShowDialog() == 2)
				{
					return;
				}
				num = lightCurvesSelectRange.FirstRecord;
				num2 = lightCurvesSelectRange.LastRecord;
			}
			try
			{
				((Control)MultiPlot).Show();
			}
			catch
			{
				MultiPlot = new LightCurve_MultiPlot();
				((Control)MultiPlot).Show();
			}
			((Form)MultiPlot).set_Location(Settings.Default.LocationMultiLightCurves);
			if ((((Control)MultiPlot).get_Left() < -5000) | (((Control)MultiPlot).get_Top() < -5000))
			{
				LightCurve_MultiPlot multiPlot = MultiPlot;
				int left;
				((Control)MultiPlot).set_Top(left = 0);
				((Control)multiPlot).set_Left(left);
			}
			((Control)MultiPlot).set_Text("Light curves - from Main file and Submitted files");
			int num3 = num2 - num + 1;
			int num4 = (((Control)MultiPlot.picPlotAll).get_Width() - 10) / 2;
			int num5 = num4 / 2;
			int num6 = 1 + (num3 - 1) / 2;
			int height = num5 * num6 + 14;
			((Control)MultiPlot.picPlotAll).set_Height(height);
			if (num6 < 5)
			{
				((Control)MultiPlot.panelAll).set_Height(height);
			}
			else
			{
				((Control)MultiPlot.panelAll).set_Height(num5 * 4);
			}
			((Control)MultiPlot).set_Height(((Control)MultiPlot.panelAll).get_Height() + 90);
			double num7 = 0.0;
			for (int i = num; i <= num2; i++)
			{
				num7 = ((i >= MainRecordNumbers.Count) ? ((double)LightData.LightCurvesSubmitted[SubmittedRecordNumbers[i - MainRecordNumbers.Count]].NumPoints) : ((double)LightData.MainLightCurves[MainRecordNumbers[i]].NumPoints));
				float plotScale = (float)((double)(num4 - 10) / num7);
				string Header;
				Bitmap bitmap2 = ((i >= MainRecordNumbers.Count) ? LightData.LightCurveImage(SubmittedRecordNumbers[i - MainRecordNumbers.Count], num4 - 10, num5 - 10, 0, plotScale, 1f, Main: false, Submitted: true, Mine: false, Pending: false, ToDelete: false, ToReview: false, ShowSkippedFrames, IsMultiPlot: true, ThickLines: false, out Header) : LightData.LightCurveImage(MainRecordNumbers[i], num4 - 10, num5 - 10, 0, plotScale, 1f, Main: true, Submitted: false, Mine: false, Pending: false, ToDelete: false, ToReview: false, ShowSkippedFrames, IsMultiPlot: true, ThickLines: false, out Header));
				list.Add(bitmap2);
				if (SaveImages)
				{
					string fileRootName = Header.Replace(",", "");
					Output.SaveGraphic(bitmap2, fileRootName, text);
				}
			}
			bitmap = new Bitmap(((Control)MultiPlot.picPlotAll).get_Width(), ((Control)MultiPlot.picPlotAll).get_Height());
			using Graphics graphics = Graphics.FromImage(bitmap);
			graphics.Clear(Color.White);
			int num8 = 0;
			int num9 = 0;
			int num10 = 0;
			foreach (Bitmap item in list)
			{
				num9 = num8 % 2 * num4;
				num10 = num8 / 2 * num5;
				graphics.DrawImage(item, new Rectangle(5 + num9, 10 + num10, item.Width, item.Height));
				num8++;
			}
			MultiPlot.picPlotAll.set_Image((Image)bitmap);
		}

		private void optHip_CheckedChanged(object sender, EventArgs e)
		{
			if (optHip.get_Checked())
			{
				cmbStar.get_Items().Clear();
				for (int i = 0; i < LightData.HipIndex.Count; i++)
				{
					cmbStar.get_Items().Add((object)LightData.HipIndex[i].ToString());
				}
				SelectedCatalogue = 0;
			}
		}

		private void optSAO_CheckedChanged(object sender, EventArgs e)
		{
			if (optSAO.get_Checked())
			{
				cmbStar.get_Items().Clear();
				for (int i = 0; i < LightData.SAOIndex.Count; i++)
				{
					cmbStar.get_Items().Add((object)LightData.SAOIndex[i].ToString());
				}
				SelectedCatalogue = 1;
			}
		}

		private void optXZ_CheckedChanged(object sender, EventArgs e)
		{
			if (optXZ.get_Checked())
			{
				cmbStar.get_Items().Clear();
				for (int i = 0; i < LightData.XZ80Index.Count; i++)
				{
					cmbStar.get_Items().Add((object)LightData.XZ80Index[i].ToString());
				}
				SelectedCatalogue = 2;
			}
		}

		private void optZC_CheckedChanged(object sender, EventArgs e)
		{
			if (optZC.get_Checked())
			{
				cmbStar.get_Items().Clear();
				for (int i = 0; i < LightData.ZCIndex.Count; i++)
				{
					cmbStar.get_Items().Add((object)LightData.ZCIndex[i].ToString());
				}
				SelectedCatalogue = 3;
			}
		}

		private void optTycho2_CheckedChanged(object sender, EventArgs e)
		{
			if (optTycho2.get_Checked())
			{
				cmbStar.get_Items().Clear();
				for (int i = 0; i < LightData.Tyc2Index.Count; i++)
				{
					cmbStar.get_Items().Add((object)LightData.Tyc2Index[i].ToString());
				}
				SelectedCatalogue = 4;
			}
		}

		private void optU4_CheckedChanged(object sender, EventArgs e)
		{
			if (optU4.get_Checked())
			{
				cmbStar.get_Items().Clear();
				for (int i = 0; i < LightData.U4Index.Count; i++)
				{
					cmbStar.get_Items().Add((object)LightData.U4Index[i].ToString());
				}
				SelectedCatalogue = 5;
			}
		}

		internal void ReadXZIndexForDates(int StarNumber)
		{
			((Control)lblStarCat).set_Text("XZ " + StarNumber);
			ReadIndexForDates("XZIndex.txt", StarNumber);
		}

		private void PopulateNameSelection()
		{
			if (LightData.ObserverNameIndex.Count > 0)
			{
				cmbObserver.get_Items().Clear();
				for (int i = 0; i < LightData.ObserverNameIndex.Count; i++)
				{
					cmbObserver.get_Items().Add((object)LightData.ObserverNameIndex[i].ToString());
				}
				((ListControl)cmbObserver).set_SelectedIndex(0);
			}
			((Control)lblObserverCount).set_Text(LightData.ObserverNameIndex.Count.ToString());
			((Control)lblObserverCount).set_Left(((Control)txtObserver).get_Left() - ((Control)lblObserverCount).get_Width());
			SelectedCatalogue = 7;
		}

		private void txtObserver_Leave(object sender, EventArgs e)
		{
			if (((Control)txtObserver).get_Text().Trim().Length >= 3)
			{
				LightData.CreateObserverNameIndex(((Control)txtObserver).get_Text().Trim());
				PopulateNameSelection();
			}
		}

		private void txtObserver_KeyDown(object sender, KeyEventArgs e)
		{
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_0008: Invalid comparison between Unknown and I4
			if ((int)e.get_KeyCode() == 13)
			{
				((Control)cmbObserver).Focus();
			}
		}

		private void txtObserver_TextChanged(object sender, EventArgs e)
		{
			if (((Control)txtObserver).get_Text().Trim().Length >= 3)
			{
				LightData.CreateObserverNameIndex(((Control)txtObserver).get_Text().Trim());
				PopulateNameSelection();
				LightData.HeaderExtraText = "";
			}
		}

		private void cmdCopy_Click(object sender, EventArgs e)
		{
			LightData.CreateObserverEventIndex(((Control)txtObserver).get_Text().Trim());
		}

		internal void ReadIndexForDates(string IndexFileName, int StarNumber)
		{
			((Control)pnlSelectStar).set_Visible(false);
			((Control)pnlStarByDate).set_Visible(true);
			string path = Utilities.AppPath + "\\LightCurves\\" + IndexFileName;
			lstDates.get_Items().Clear();
			Indices.Clear();
			Dates.Clear();
			((ListControl)lstDates).set_SelectedIndex(-1);
			if (!File.Exists(path))
			{
				return;
			}
			using (StreamReader streamReader = new StreamReader(path))
			{
				do
				{
					string[] array = streamReader.ReadLine()!.Split(new char[1] { ',' });
					if (int.Parse(array[0].Split(new char[1] { '-' })[0]) == StarNumber)
					{
						lstDates.get_Items().Add((object)array[2]);
						Indices.Add(array[1]);
					}
				}
				while (!streamReader.EndOfStream);
			}
			if (lstDates.get_Items().get_Count() > 0)
			{
				((ListControl)lstDates).set_SelectedIndex(0);
			}
		}

		private void lstDates_SelectedIndexChanged(object sender, EventArgs e)
		{
		}

		internal void CloseThis()
		{
			((Form)this).Close();
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"LightCurveViewer");
		}

		private void LightCurveViewer_FormClosing(object sender, FormClosingEventArgs e)
		{
			Kepler2.XZinK2.Clear();
		}

		private void panelPlot_Scroll(object sender, ScrollEventArgs e)
		{
			try
			{
				Application.DoEvents();
			}
			catch
			{
			}
		}

		private void ReadLightCurvesReceived(int AsteroidNumber, bool ShowReadErrorMessages)
		{
			LightData.LoadLightCurvesReceived_Admin(IncludeEmbargoed: true, includeForReview: true, AsteroidNumber, ShowReadErrorMessages);
			((ToolStripItem)editALightCurveSubmittedFileToolStripMenuItem).set_Text(".... " + LightData.LightCurvesSubmitted.Count + " submitted light curves");
			lstSubmitted.get_Items().Clear();
			for (int i = 0; i < LightData.LightCurvesSubmitted.Count; i++)
			{
				StringBuilder stringBuilder = new StringBuilder();
				stringBuilder.AppendFormat("#{0,3} ", i + 1);
				if (LightData.LightCurvesSubmitted[i].AsteroidNumber > 0)
				{
					stringBuilder.Append(LightData.LightCurvesSubmitted[i].AsteroidNumber.ToString().PadLeft(7) + " " + LightData.LightCurvesSubmitted[i].AsteroidName.PadRight(14) + " ");
				}
				else
				{
					string text = " XZ" + LightData.LightCurvesSubmitted[i].XZ.ToString().PadLeft(6);
					if (LightData.LightCurvesSubmitted[i].ZC > 0)
					{
						text = text + " = ZC" + LightData.LightCurvesSubmitted[i].ZC;
					}
					else if (LightData.LightCurvesSubmitted[i].SAO > 0)
					{
						text = text + " = SAO" + LightData.LightCurvesSubmitted[i].SAO;
					}
					stringBuilder.Append(text.PadRight(23));
				}
				stringBuilder.Append(LightData.LightCurvesSubmitted[i].EventDate + "  ");
				stringBuilder.Append(LightData.LightCurvesSubmitted[i].Observer);
				lstSubmitted.get_Items().Add((object)stringBuilder.ToString());
			}
			((Control)lblSubmitted).set_Text(string.Format("{0,1} submitted light curves", lstSubmitted.get_Items().get_Count()));
		}

		private void ReadMyPendingReports()
		{
			LightData.LoadMyPendingReports();
			((ToolStripItem)my0UnsubmittedLightCurvesToolStripMenuItem).set_Text("My " + LightData.MyPendingLightCurves.Count + " unsubmitted light curves");
		}

		private void ReadMyReports()
		{
			LightData.LoadMyReports();
			((ToolStripItem)myReportedLightCurvesToolStripMenuItem).set_Text("My " + LightData.MyReportedLightCurves.Count + " reported light curves");
		}

		private void editALightCurveMainFileToolStripMenuItem_Click(object sender, EventArgs e)
		{
			try
			{
				((Form)LCE).Close();
			}
			catch
			{
			}
			LCE = new LightCurve_Edit(SubmittedFile: false);
			((Form)LCE).Show((IWin32Window)(object)this);
		}

		private void editALightCurveSubmittedFileToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (LightData.LightCurvesSubmitted == null)
			{
				LightData.LoadLightCurvesReceived_Admin(IncludeEmbargoed: true, includeForReview: true, 0, ShowReadErrorMessages: true);
			}
			if (LightData.LightCurvesSubmitted.Count < 1)
			{
				LightData.LoadLightCurvesReceived_Admin(IncludeEmbargoed: true, includeForReview: true, 0, ShowReadErrorMessages: true);
			}
			try
			{
				((Form)LCE).Close();
			}
			catch
			{
			}
			LCE = new LightCurve_Edit(SubmittedFile: true);
			((Form)LCE).Show((IWin32Window)(object)this);
		}

		private void mergeSubmittedLightCurveIntoTheMainFileToolStripMenuItem_Click(object sender, EventArgs e)
		{
			LightData.MergeMainAndSubmitted();
		}

		private void picPlot_MouseMove(object sender, MouseEventArgs e)
		{
			float num = e.get_X();
			float num2 = e.get_Y();
			double num3 = (double)(num / (float)((Control)picPlot).get_Width()) * LightData.CurrentPlotDuration;
			double degree = (LightData.CurrentPlotStartTime_Secs + num3) / 3600.0;
			string text = "UT: " + Utilities.DEGtoDMS(degree, 1, 2, MinutesOnly: false).Trim();
			string text2 = "";
			float num4 = LightData.YatBottomOfPlot + LightData.HeightRatioForSelectedLightLevel * (LightData.YatTopOfPlot - LightData.YatBottomOfPlot) * VerticalPlotScale;
			float num5 = LightData.YatBottomOfPlot - LightData.ZeroHeight;
			float num6 = (num5 - num2) / (num5 - num4);
			text2 = ((!(LightData.HeightRatioForSelectedLightLevel > 0f)) ? string.Format(", Light level: {0,1:f3}", (num2 - LightData.YatBottomOfPlot + LightData.ZeroHeight) / (LightData.YatTopOfPlot - LightData.YatBottomOfPlot + LightData.ZeroHeight)) : string.Format(", Light drop: {0,1:f0}%", 100f * (1f - num6)));
			string text3 = "File data error";
			try
			{
				text3 = string.Format(", Frame {0,1:f0}", Convert.ToInt32(num3 / (double)LightData.Interval));
			}
			catch
			{
			}
			toolTip1.SetToolTip((Control)(object)picPlot, text + text3 + text2);
		}

		private void chkTimeHeight_Click(object sender, EventArgs e)
		{
			chkTimeHeight.set_Checked(!chkTimeHeight.get_Checked());
			toolTip1.set_Active(chkTimeHeight.get_Checked());
		}

		private void picPlot_MouseEnter(object sender, EventArgs e)
		{
			if (chkTimeHeight.get_Checked())
			{
				((Control)this).set_Cursor(Cursors.get_HSplit());
			}
		}

		private void picPlot_MouseLeave(object sender, EventArgs e)
		{
			((Control)this).set_Cursor(Cursors.get_Default());
		}

		private void viewLightCurvesMarkedForDeletiondelinitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Read_ToDelete_LightCurves(ShowMultiPlot: true);
		}

		internal void Read_ToDelete_LightCurves(bool ShowMultiPlot)
		{
			LightData.LoadToDeleteLightCurves();
			if (ShowMultiPlot & (LightData.ToDeleteLightCurves.Count > 0))
			{
				MultiCurvePlot(ref LightData.ToDeleteRecordNumbers, Main: false, Submitted: false, Mine: false, Pending: false, ToDelete: true, ToReview: false, indicateMissedImagesToolStripMenuItem.get_Checked());
			}
			((ToolStripItem)viewLightCurvesMarkedForDeletiondelinitToolStripMenuItem).set_Text(".... View " + LightData.ToDeleteLightCurves.Count + " files marked for deletion  ( .del  && .init)");
			((ToolStripItem)viewLightCurvesMarkedForDeletiondelinitToolStripMenuItem).set_Enabled(LightData.ToDeleteLightCurves.Count > 0);
		}

		private void view0LightCurvesMarkedForReviewToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Read_ToReview_LightCurves(ShowMultiPlot: true);
		}

		internal void Read_ToReview_LightCurves(bool ShowMultiPlot)
		{
			LightData.LoadToReviewLightCurves();
			if (ShowMultiPlot & (LightData.ToReviewLightCurves.Count > 0))
			{
				MultiCurvePlot(ref LightData.ToReviewRecordNumbers, Main: false, Submitted: false, Mine: false, Pending: false, ToDelete: false, ToReview: true, indicateMissedImagesToolStripMenuItem.get_Checked());
			}
			((ToolStripItem)view0LightCurvesMarkedForReviewreviewToolStripMenuItem).set_Text(".... View " + LightData.ToReviewLightCurves.Count + " files marked for review ( .review )");
			((ToolStripItem)view0LightCurvesMarkedForReviewreviewToolStripMenuItem).set_Enabled(LightData.ToReviewLightCurves.Count > 0);
		}

		internal void Display_PendingLightCurves(int AsteroidNumber, bool ShowMessage, bool ShowReadErrorMessages)
		{
			ReadLightCurvesReceived(AsteroidNumber, ShowReadErrorMessages);
			if (LightData.LightCurvesSubmitted.Count > 0)
			{
				MultiCurvePlot(ref LightData.SubmittedRecordNumbers, Main: false, Submitted: true, Mine: false, Pending: false, ToDelete: false, ToReview: false, indicateMissedImagesToolStripMenuItem.get_Checked());
			}
		}

		private void manageEmbargoesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			try
			{
				((Control)LightData.EmbargoForm).Show();
			}
			catch
			{
				LightData.EmbargoForm = new Embargo();
				((Control)LightData.EmbargoForm).Show();
			}
		}

		private void rereadLightCurvesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ReadLightCurves();
		}

		private void rereadSubmittedLightCurvesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ReReadSubmitted();
		}

		internal void ReReadSubmitted()
		{
			LightData.LightCurvesSubmitted.Clear();
			ReadLightCurvesReceived(0, ShowReadErrorMessages: true);
			Display_PendingLightCurves(0, ShowMessage: true, ShowReadErrorMessages: true);
		}

		private void copyToolStripMenuItem1_Click(object sender, EventArgs e)
		{
			//IL_0018: Unknown result type (might be due to invalid IL or missing references)
			try
			{
				Clipboard.SetImage(picPlot.get_Image());
			}
			catch
			{
				MessageBox.Show("Error - Image not copied");
			}
		}

		private void saveImageToolStripMenuItem_Click(object sender, EventArgs e)
		{
			string text = "";
			if (PlotMain)
			{
				LightCurveData lightCurveData = LightData.MainLightCurves[LightCurveIndex_Current];
				text = "(" + lightCurveData.AsteroidNumber + ") " + lightCurveData.AsteroidName + "_" + lightCurveData.EventDate + "_" + lightCurveData.Observer.ToString();
			}
			else
			{
				LightCurveData lightCurveData2 = LightData.LightCurvesSubmitted[LightCurveIndex_Current];
				text = "(" + lightCurveData2.AsteroidNumber + ") " + lightCurveData2.AsteroidName + "_" + lightCurveData2.EventDate + "_" + lightCurveData2.Observer.ToString();
			}
			Output.SaveGraphic(picPlot.get_Image(), text, Utilities.AppPath + "\\Observations");
		}

		private void writecsvFileForThisLightCurveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			WriteCSV_forLightCurve(out var _, Write_csv_file: true);
		}

		private void displayInAOTAToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (WriteCSV_forLightCurve(out var Out, Write_csv_file: false))
			{
				try
				{
					((Control)AOTAData.PlotForm).Show();
				}
				catch
				{
					AOTAData.PlotForm = new PlotForm();
					((Form)this).set_StartPosition((FormStartPosition)4);
					((Control)AOTAData.PlotForm).Show();
				}
				AOTAData.Read_CSV_Data(Out.ToString(), FromLightCurve: false, FrameInsertions_Allow: false);
			}
		}

		private bool WriteCSV_forLightCurve(out StringBuilder Out, bool Write_csv_file)
		{
			Out = new StringBuilder();
			Out.AppendLine("Tangra       ");
			string text = "";
			if (PlotMain)
			{
				if (LightCurveIndex_Current < 0)
				{
					return false;
				}
				((Control)txtStar).set_Text(LightData.MainLightCurves[LightCurveIndex_Current].StarDetails());
				((Control)txtObs).set_Text(LightData.MainLightCurves[LightCurveIndex_Current].DateDetails() + "\r\n" + LightData.MainLightCurves[LightCurveIndex_Current].ObserverDetails());
				((Control)txtCirc).set_Text(LightData.MainLightCurves[LightCurveIndex_Current].Circumstances());
				text = "(" + LightData.MainLightCurves[LightCurveIndex_Current].AsteroidNumber + ") " + LightData.MainLightCurves[LightCurveIndex_Current].Year + Utilities.ShortMonths[LightData.MainLightCurves[LightCurveIndex_Current].Month] + LightData.MainLightCurves[LightCurveIndex_Current].Day.ToString().PadLeft(2, '0') + " " + LightData.MainLightCurves[LightCurveIndex_Current].Hr.ToString().PadLeft(2, '0') + "h" + LightData.MainLightCurves[LightCurveIndex_Current].Min.ToString().PadLeft(2, '0') + "m" + LightData.MainLightCurves[LightCurveIndex_Current].Sec.ToString().PadLeft(2, '0') + "s " + LightData.MainLightCurves[LightCurveIndex_Current].Observer.Replace(".", "_") + ".csv";
				Out.AppendLine("Date " + LightData.MainLightCurves[LightCurveIndex_Current].Year + " " + Utilities.ShortMonths[LightData.MainLightCurves[LightCurveIndex_Current].Month] + " " + LightData.MainLightCurves[LightCurveIndex_Current].Day);
				if (LightData.MainLightCurves[LightCurveIndex_Current].Hipparcos > 0)
				{
					Out.AppendLine("Star = Hip2 " + LightData.MainLightCurves[LightCurveIndex_Current].Hipparcos);
				}
				else if (LightData.MainLightCurves[LightCurveIndex_Current].Tyc2A > 0)
				{
					Out.AppendLine("Star = TYC2 " + LightData.MainLightCurves[LightCurveIndex_Current].Tyc2A + "-" + LightData.MainLightCurves[LightCurveIndex_Current].Tyc2B + "-" + LightData.MainLightCurves[LightCurveIndex_Current].Tyc2C);
				}
				else if (LightData.MainLightCurves[LightCurveIndex_Current].U4Zone > 0)
				{
					Out.AppendLine("Star = UCAC4 " + LightData.MainLightCurves[LightCurveIndex_Current].U4Zone + "-" + LightData.MainLightCurves[LightCurveIndex_Current].U4Number);
				}
				else if (LightData.MainLightCurves[LightCurveIndex_Current].ZC > 0)
				{
					Out.AppendLine("Star = ZC " + LightData.MainLightCurves[LightCurveIndex_Current].ZC);
				}
				else if (LightData.MainLightCurves[LightCurveIndex_Current].SAO > 0)
				{
					Out.AppendLine("Star = SAO " + LightData.MainLightCurves[LightCurveIndex_Current].SAO);
				}
				else if (LightData.MainLightCurves[LightCurveIndex_Current].XZ > 0)
				{
					Out.AppendLine("Star = XZ " + LightData.MainLightCurves[LightCurveIndex_Current].XZ);
				}
				if ((LightData.MainLightCurves[LightCurveIndex_Current].AsteroidNumber > 0) | (LightData.MainLightCurves[LightCurveIndex_Current].AsteroidName.Trim().Length > 0))
				{
					Out.AppendLine("Asteroid = (" + LightData.MainLightCurves[LightCurveIndex_Current].AsteroidNumber + ") " + LightData.MainLightCurves[LightCurveIndex_Current].AsteroidName);
				}
				Out.AppendLine("Observer = " + LightData.MainLightCurves[LightCurveIndex_Current].Observer);
				AOTAData.CurrentFileName = Out.ToString().Replace("\r\n", "  ").Replace("Tangra", "")
					.Trim();
				Out.AppendLine("FrameNo,Time (UT),Signal (1), Background (1)");
				double num = (double)LightData.MainLightCurves[LightCurveIndex_Current].Hr * 3600.0 + (double)LightData.MainLightCurves[LightCurveIndex_Current].Min * 60.0 + LightData.MainLightCurves[LightCurveIndex_Current].Sec;
				double num2 = LightData.MainLightCurves[LightCurveIndex_Current].Duration;
				double num3 = LightData.MainLightCurves[LightCurveIndex_Current].NumPoints;
				double num4 = num2 / (num3 - 1.0);
				for (int i = 0; i < LightData.MainLightCurves[LightCurveIndex_Current].LightValues.Count; i++)
				{
					double num5 = num + (double)i * num4;
					Out.AppendLine(i + ",[" + Utilities.DEGtoDMS(num5 / 3600.0, 2, 3, MinutesOnly: false, IncludeLeadingZeros: true).Trim().Replace(" ", ":") + "]," + LightData.MainLightCurves[LightCurveIndex_Current].LightValues[i] + ",0");
				}
			}
			else
			{
				if (LightCurveIndex_Current < 0)
				{
					return false;
				}
				((Control)txtStar).set_Text(LightData.LightCurvesSubmitted[LightCurveIndex_Current].StarDetails());
				((Control)txtObs).set_Text(LightData.LightCurvesSubmitted[LightCurveIndex_Current].DateDetails() + "\r\n" + LightData.LightCurvesSubmitted[LightCurveIndex_Current].ObserverDetails());
				((Control)txtCirc).set_Text(LightData.LightCurvesSubmitted[LightCurveIndex_Current].Circumstances());
				text = "(" + LightData.LightCurvesSubmitted[LightCurveIndex_Current].AsteroidNumber + ") " + LightData.LightCurvesSubmitted[LightCurveIndex_Current].Year + Utilities.ShortMonths[LightData.LightCurvesSubmitted[LightCurveIndex_Current].Month] + LightData.LightCurvesSubmitted[LightCurveIndex_Current].Day.ToString().PadLeft(2, '0') + " " + LightData.LightCurvesSubmitted[LightCurveIndex_Current].Hr.ToString().PadLeft(2, '0') + "h" + LightData.LightCurvesSubmitted[LightCurveIndex_Current].Min.ToString().PadLeft(2, '0') + "m" + LightData.LightCurvesSubmitted[LightCurveIndex_Current].Sec.ToString().PadLeft(2, '0') + "s " + LightData.LightCurvesSubmitted[LightCurveIndex_Current].Observer.Replace(".", "_") + ".csv";
				Out.AppendLine("Date " + LightData.LightCurvesSubmitted[LightCurveIndex_Current].Year + " " + Utilities.ShortMonths[LightData.LightCurvesSubmitted[LightCurveIndex_Current].Month] + " " + LightData.LightCurvesSubmitted[LightCurveIndex_Current].Day);
				if (LightData.LightCurvesSubmitted[LightCurveIndex_Current].Hipparcos > 0)
				{
					Out.AppendLine("Star = Hip2 " + LightData.LightCurvesSubmitted[LightCurveIndex_Current].Hipparcos);
				}
				else if (LightData.LightCurvesSubmitted[LightCurveIndex_Current].Tyc2A > 0)
				{
					Out.AppendLine("Star = TYC2 " + LightData.LightCurvesSubmitted[LightCurveIndex_Current].Tyc2A + "-" + LightData.LightCurvesSubmitted[LightCurveIndex_Current].Tyc2B + "-" + LightData.LightCurvesSubmitted[LightCurveIndex_Current].Tyc2C);
				}
				else if (LightData.LightCurvesSubmitted[LightCurveIndex_Current].U4Zone > 0)
				{
					Out.AppendLine("Star = UCAC4 " + LightData.LightCurvesSubmitted[LightCurveIndex_Current].U4Zone + "-" + LightData.LightCurvesSubmitted[LightCurveIndex_Current].U4Number);
				}
				else if (LightData.LightCurvesSubmitted[LightCurveIndex_Current].ZC > 0)
				{
					Out.AppendLine("Star = ZC " + LightData.LightCurvesSubmitted[LightCurveIndex_Current].ZC);
				}
				else if (LightData.LightCurvesSubmitted[LightCurveIndex_Current].SAO > 0)
				{
					Out.AppendLine("Star = SAO " + LightData.LightCurvesSubmitted[LightCurveIndex_Current].SAO);
				}
				else if (LightData.LightCurvesSubmitted[LightCurveIndex_Current].XZ > 0)
				{
					Out.AppendLine("Star = XZ " + LightData.LightCurvesSubmitted[LightCurveIndex_Current].XZ);
				}
				if ((LightData.LightCurvesSubmitted[LightCurveIndex_Current].AsteroidNumber > 0) | (LightData.LightCurvesSubmitted[LightCurveIndex_Current].AsteroidName.Trim().Length > 0))
				{
					Out.AppendLine("Asteroid = (" + LightData.LightCurvesSubmitted[LightCurveIndex_Current].AsteroidNumber + ") " + LightData.LightCurvesSubmitted[LightCurveIndex_Current].AsteroidName);
				}
				Out.AppendLine("Observer = " + LightData.LightCurvesSubmitted[LightCurveIndex_Current].Observer);
				AOTAData.CurrentFileName = Out.ToString().Replace("\r\n", "  ").Replace("Tangra", "")
					.Trim();
				Out.AppendLine("FrameNo,Time (UT),Signal (1), Background (1)");
				double num6 = (double)LightData.LightCurvesSubmitted[LightCurveIndex_Current].Hr * 3600.0 + (double)LightData.LightCurvesSubmitted[LightCurveIndex_Current].Min * 60.0 + LightData.LightCurvesSubmitted[LightCurveIndex_Current].Sec;
				double num7 = LightData.LightCurvesSubmitted[LightCurveIndex_Current].Duration;
				double num8 = LightData.LightCurvesSubmitted[LightCurveIndex_Current].NumPoints;
				double num9 = num7 / (num8 - 1.0);
				for (int j = 0; j < LightData.LightCurvesSubmitted[LightCurveIndex_Current].LightValues.Count; j++)
				{
					double num10 = num6 + (double)j * num9;
					Out.AppendLine(j + ",[" + Utilities.DEGtoDMS(num10 / 3600.0, 2, 3, MinutesOnly: false, IncludeLeadingZeros: true).Trim().Replace(" ", ":") + "]," + LightData.LightCurvesSubmitted[LightCurveIndex_Current].LightValues[j] + ",0");
				}
			}
			if (Write_csv_file)
			{
				using StreamWriter streamWriter = new StreamWriter(Utilities.AppPath + "\\Generated Files\\" + text);
				streamWriter.Write(Out.ToString());
			}
			return true;
		}

		private void my0UnsubmittedLightCurvesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (LightData.MyPendingLightCurves.Count < 1)
			{
				ReadMyPendingReports();
			}
			if (LightData.MyPendingLightCurves.Count > 0)
			{
				MultiCurvePlot(ref LightData.PendingRecordNumbers, Main: false, Submitted: false, Mine: false, Pending: true, ToDelete: false, ToReview: false, indicateMissedImagesToolStripMenuItem.get_Checked());
			}
		}

		private void manageMyUnsubmittedLightCurvesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Utilities.UploadLightCurvesIsDue(AutoShow: false);
		}

		private void myReportedLightCurvesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (LightData.MyReportedLightCurves.Count < 1)
			{
				ReadMyReports();
			}
			if (LightData.MyReportedLightCurves.Count > 0)
			{
				MultiCurvePlot(ref LightData.MyRecordNumbers, Main: false, Submitted: false, Mine: true, Pending: false, ToDelete: false, ToReview: false, indicateMissedImagesToolStripMenuItem.get_Checked());
			}
		}

		private void listSubmittedLightCurvesAlreadyInTheMainFileToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SubmittedInMain(MarkAs_Del: false);
		}

		private void listAndDeletesubmittedCurvesAlreadyInMainFileToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0017: Unknown result type (might be due to invalid IL or missing references)
			//IL_001d: Invalid comparison between Unknown and I4
			if ((int)MessageBox.Show("Where a submitted light curve file (or an equivalent) is \r\nfound in the main file of light curves, this \r\nwill change the extension to 'del'\r\n\r\nDo you want to proceed?", "Change extension of submitted files", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 6)
			{
				SubmittedInMain(MarkAs_Del: true);
			}
		}

		private void SubmittedInMain(bool MarkAs_Del)
		{
			string text = "";
			int num = 0;
			for (int i = 0; i < LightData.LightCurvesSubmitted.Count - 1; i++)
			{
				if (LightData.LightCurvesSubmitted[i].AsteroidNumber < 1 || (LightData.LightCurvesSubmitted[i].FileName.Contains(".embargoed") | LightData.LightCurvesSubmitted[i].FileName.Contains(".review") | LightData.LightCurvesSubmitted[i].FileName.Contains(".del") | LightData.LightCurvesSubmitted[i].FileName.Contains(".init")))
				{
					continue;
				}
				Application.DoEvents();
				for (int j = 0; j < LightData.AsteroidIndex.Count; j++)
				{
					if (LightData.AsteroidIndex[j].StarNum1_or_AsteroidNo < LightData.LightCurvesSubmitted[i].AsteroidNumber)
					{
						continue;
					}
					if (LightData.AsteroidIndex[j].StarNum1_or_AsteroidNo > LightData.LightCurvesSubmitted[i].AsteroidNumber)
					{
						break;
					}
					num = LightData.AsteroidIndex[j].LightCurveIndex;
					if (LightData.LightCurvesSubmitted[i].Year != LightData.MainLightCurves[num].Year || LightData.LightCurvesSubmitted[i].Month != LightData.MainLightCurves[num].Month || LightData.LightCurvesSubmitted[i].Day != LightData.MainLightCurves[num].Day)
					{
						continue;
					}
					double num2 = (double)LightData.LightCurvesSubmitted[i].LongD + (double)LightData.LightCurvesSubmitted[i].LongM / 60.0 + LightData.LightCurvesSubmitted[i].LongS / 3600.0;
					double num3 = (double)LightData.MainLightCurves[num].LongD + (double)LightData.MainLightCurves[num].LongM / 60.0 + LightData.MainLightCurves[num].LongS / 3600.0;
					if (Math.Abs(num2 - num3) > 0.001)
					{
						continue;
					}
					double num4 = (double)LightData.LightCurvesSubmitted[i].LatD + (double)LightData.LightCurvesSubmitted[i].LatM / 60.0 + LightData.LightCurvesSubmitted[i].LatS / 3600.0;
					double num5 = (double)LightData.MainLightCurves[num].LatD + (double)LightData.MainLightCurves[num].LatM / 60.0 + LightData.MainLightCurves[num].LatS / 3600.0;
					if (Math.Abs(num4 - num5) > 0.001)
					{
						continue;
					}
					string text2 = LightData.LightCurvesSubmitted[i].Observer.Trim().ToLower();
					int num6 = text2.LastIndexOf(" ");
					if (num6 > 0)
					{
						text2 = text2.Substring(num6);
					}
					string text3 = LightData.MainLightCurves[num].Observer.Trim().ToLower();
					num6 = text3.LastIndexOf(" ");
					if (num6 > 0)
					{
						text3 = text3.Substring(num6);
					}
					if (text3 != text2 || LightData.LightCurvesSubmitted[i].MoonSize > 2.0 != LightData.MainLightCurves[num].MoonSize > 2.0)
					{
						continue;
					}
					double num7 = (double)LightData.LightCurvesSubmitted[i].Hr + (double)LightData.LightCurvesSubmitted[i].Min / 60.0 + LightData.LightCurvesSubmitted[i].Sec / 3600.0;
					double num8 = (double)LightData.MainLightCurves[num].Hr + (double)LightData.MainLightCurves[num].Min / 60.0 + LightData.MainLightCurves[num].Sec / 3600.0;
					if (LightData.LightCurvesSubmitted[i].MoonSize > 2.0)
					{
						if (LightData.LightCurvesSubmitted[i].AsteroidNumber != LightData.MainLightCurves[num].AsteroidNumber || ((Math.Abs(num8 - num7) > 0.018) & (Math.Abs(num8 - num7) < 0.9982)) || Math.Abs(num8 - num7) > 1.0018)
						{
							continue;
						}
					}
					else
					{
						if (LightData.LightCurvesSubmitted[i].XZ != LightData.MainLightCurves[num].XZ || Math.Sign(LightData.LightCurvesSubmitted[i].CA) != Math.Abs(LightData.MainLightCurves[num].CA))
						{
							continue;
						}
						if (Math.Abs(LightData.LightCurvesSubmitted[i].CA) < 22)
						{
							if (Math.Abs(LightData.LightCurvesSubmitted[i].PosAngle - LightData.MainLightCurves[num].PosAngle) < 12.0)
							{
								continue;
							}
						}
						else if (Math.Abs(num8 - num7) > 0.0015)
						{
							continue;
						}
					}
					text += string.Format("Submitted: #{0,3} ↔ Main: #{1,5}   ({2,1}) {3,1}\r\n", i + 1, num + 1, LightData.LightCurvesSubmitted[i].AsteroidNumber, LightData.LightCurvesSubmitted[i].AsteroidName);
					if (MarkAs_Del)
					{
						string text4 = Utilities.AppPath + "\\LightCurveReports\\";
						string fileName = Path.GetFileName(LightData.LightCurvesSubmitted[i].FileName);
						string text5 = fileName.Replace(".dat", ".del");
						if (!File.Exists(text4 + text5))
						{
							File.Move(text4 + fileName, text4 + text5);
						}
						if (File.Exists(text4 + fileName))
						{
							File.Delete(text4 + fileName);
						}
					}
				}
			}
			if (text.Length == 0)
			{
				text = "No duplicates found";
			}
			try
			{
				((Control)NewInOld_Events).Show();
			}
			catch
			{
				NewInOld_Events = new DisplayData();
				((Control)NewInOld_Events).Show();
			}
			string text6 = "Record numbers of Submitted light curves that are\r\nalready in the Main File.\r\n";
			if (MarkAs_Del)
			{
				text6 += "\r\nThe file extension for these files is now '.del'\r\n";
			}
			((Control)NewInOld_Events.txtBox).set_Text(text6 + "\r\n" + text);
			if (MarkAs_Del)
			{
				ReReadSubmitted();
			}
		}

		private void PlotMainFromRecordNumber(int LightCurveIndex_Current)
		{
			//IL_0026: Unknown result type (might be due to invalid IL or missing references)
			LightCurveIndex_Current--;
			if (LightCurveIndex_Current > LightData.MainLightCurves.Count || LightCurveIndex_Current < 0)
			{
				MessageBox.Show("Specified value is outside range", "Oustside Range", (MessageBoxButtons)0, (MessageBoxIcon)16);
				return;
			}
			((Control)txtStar).set_Text(LightData.MainLightCurves[LightCurveIndex_Current].StarDetails());
			((Control)txtObs).set_Text(LightData.MainLightCurves[LightCurveIndex_Current].DateDetails() + "\r\n" + LightData.MainLightCurves[LightCurveIndex_Current].ObserverDetails());
			((Control)txtCirc).set_Text(LightData.MainLightCurves[LightCurveIndex_Current].Circumstances());
			PlotMain = true;
			PlotLightCurve(LightCurveIndex_Current, 0);
		}

		private void txtRecord_KeyPress(object sender, KeyPressEventArgs e)
		{
			e.set_Handled(!(char.IsDigit(e.get_KeyChar()) | char.IsControl(e.get_KeyChar())));
		}

		private void txtRecord_KeyDown(object sender, KeyEventArgs e)
		{
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_0008: Invalid comparison between Unknown and I4
			if ((int)e.get_KeyCode() == 13)
			{
				int.TryParse(((Control)txtRecord).get_Text(), out var result);
				PlotMainFromRecordNumber(result);
			}
		}

		private void cmbAsteroidNumber_SelectedIndexChanged(object sender, EventArgs e)
		{
			LightData.HeightRatioForSelectedLightLevel = 0f;
			SliderVerticalScale.set_Value(150);
			VerticalPlotScale = 1f;
			bool addExtraHeader;
			addTextToolStripMenuItem.set_Checked(addExtraHeader = false);
			LightData.AddExtraHeader = addExtraHeader;
			LightData.HeaderExtraText = "";
			LightData.HeaderExtraTime_UT1 = (LightData.HeaderExtraTime_UT2 = 0.0);
			addMarkerForThisUTTime1.set_Checked(addExtraHeader = false);
			LightData.AddTimeMarker1 = addExtraHeader;
			addMarkerForThisUTTime2.set_Checked(addExtraHeader = false);
			LightData.AddTimeMarker2 = addExtraHeader;
			PlotSelectedAsteroid(ByAsteroidNumber: true, ByAsteroidName: false);
		}

		internal void PlotSelectedAsteroid(bool ByAsteroidNumber, bool ByAsteroidName)
		{
			if ((ByAsteroidNumber & (((ListControl)cmbAsteroidNumber).get_SelectedIndex() < 0)) || (ByAsteroidName & (((ListControl)cmbAsteroidName).get_SelectedIndex() < 0)) || ((!ByAsteroidNumber && !ByAsteroidName) & (((ListControl)cmbByDate).get_SelectedIndex() < 0)))
			{
				return;
			}
			List<int> RecordNumbersForMultiCurve = new List<int>();
			int num = 1;
			string text = "";
			PlotMain = true;
			if (ByAsteroidNumber)
			{
				LightCurveIndex_Current = LightData.AsteroidNumberIndex[((ListControl)cmbAsteroidNumber).get_SelectedIndex()].LightCurveIndex;
			}
			else if (ByAsteroidName)
			{
				LightCurveIndex_Current = LightData.AsteroidNameIndex[((ListControl)cmbAsteroidName).get_SelectedIndex()].LightCurveIndex;
			}
			else
			{
				LightCurveStarIndex.SortByDate = true;
				LightCurveStarIndex.SortByName = false;
				LightData.AsteroidIndex.Sort();
				LightCurveIndex_Current = LightData.AsteroidIndex[((ListControl)cmbByDate).get_SelectedIndex()].LightCurveIndex;
			}
			if (PlotAllAsteroidByNumber)
			{
				if (cmbAsteroidNumber.get_Items().get_Count() <= 0)
				{
					return;
				}
				if (((ListControl)cmbAsteroidNumber).get_SelectedIndex() >= 0)
				{
					num = LightData.AsteroidNumberIndex[((ListControl)cmbAsteroidNumber).get_SelectedIndex()].StarNum1_or_AsteroidNo;
					for (int i = 0; i < LightData.AsteroidNumberIndex.Count; i++)
					{
						if (LightData.AsteroidNumberIndex[i].StarNum1_or_AsteroidNo == num)
						{
							RecordNumbersForMultiCurve.Add(LightData.AsteroidNumberIndex[i].LightCurveIndex);
						}
					}
				}
				MultiCurvePlot(ref RecordNumbersForMultiCurve, Main: true, Submitted: false, Mine: false, Pending: false, ToDelete: false, ToReview: false, indicateMissedImagesToolStripMenuItem.get_Checked());
			}
			else if (PlotAllAsteroidByName)
			{
				if (cmbAsteroidName.get_Items().get_Count() <= 0)
				{
					return;
				}
				if (((ListControl)cmbAsteroidName).get_SelectedIndex() >= 0)
				{
					text = LightData.AsteroidNameIndex[((ListControl)cmbAsteroidName).get_SelectedIndex()].ObjectName;
					for (int j = 0; j < LightData.AsteroidNameIndex.Count; j++)
					{
						if (LightData.AsteroidNameIndex[j].ObjectName == text)
						{
							RecordNumbersForMultiCurve.Add(LightData.AsteroidNameIndex[j].LightCurveIndex);
						}
					}
				}
				else
				{
					for (int k = 0; k < cmbAsteroidName.get_Items().get_Count(); k++)
					{
						RecordNumbersForMultiCurve.Add(LightData.AsteroidNameIndex[k].LightCurveIndex);
					}
				}
				MultiCurvePlot(ref RecordNumbersForMultiCurve, Main: true, Submitted: false, Mine: false, Pending: false, ToDelete: false, ToReview: false, indicateMissedImagesToolStripMenuItem.get_Checked());
			}
			((Control)txtStar).set_Text(LightData.MainLightCurves[LightCurveIndex_Current].StarDetails());
			((Control)txtObs).set_Text(LightData.MainLightCurves[LightCurveIndex_Current].DateDetails() + "\r\n" + LightData.MainLightCurves[LightCurveIndex_Current].ObserverDetails());
			((Control)txtCirc).set_Text(LightData.MainLightCurves[LightCurveIndex_Current].Circumstances());
			PlotLightCurve(LightCurveIndex_Current, 0);
			PlotAllAsteroidByNumber = (PlotAllAsteroidByName = false);
			if (ByAsteroidName)
			{
				LastPlotWasAsteroidName = true;
			}
			else
			{
				LastPlotWasAsteroidNumber = true;
			}
			LastPlotWasMain = (LastPlotWasSubmitted = (LastPlotWasSubmittedAll = false));
		}

		private void cmdPlotAll_Number_Click(object sender, EventArgs e)
		{
			LightData.HeaderExtraText = "";
			PlotAllAsteroidByNumber = true;
			PlotSelectedAsteroid(ByAsteroidNumber: true, ByAsteroidName: false);
		}

		private void cmbObserver_SelectedIndexChanged(object sender, EventArgs e)
		{
			LightData.HeightRatioForSelectedLightLevel = 0f;
			SliderVerticalScale.set_Value(150);
			VerticalPlotScale = 1f;
			bool addExtraHeader;
			addTextToolStripMenuItem.set_Checked(addExtraHeader = false);
			LightData.AddExtraHeader = addExtraHeader;
			LightData.HeaderExtraText = "";
			LightData.HeaderExtraTime_UT1 = (LightData.HeaderExtraTime_UT2 = 0.0);
			addMarkerForThisUTTime1.set_Checked(addExtraHeader = false);
			LightData.AddTimeMarker1 = addExtraHeader;
			addMarkerForThisUTTime2.set_Checked(addExtraHeader = false);
			LightData.AddTimeMarker2 = addExtraHeader;
			LightCurveIndex_Current = LightData.ObserverNameIndex[((ListControl)cmbObserver).get_SelectedIndex()].LightCurveIndex;
			((Control)txtStar).set_Text(LightData.MainLightCurves[LightCurveIndex_Current].StarDetails());
			((Control)txtObs).set_Text(LightData.MainLightCurves[LightCurveIndex_Current].DateDetails() + "\r\n" + LightData.MainLightCurves[LightCurveIndex_Current].ObserverDetails());
			((Control)txtCirc).set_Text(LightData.MainLightCurves[LightCurveIndex_Current].Circumstances());
			PlotMain = true;
			PlotLightCurve(LightCurveIndex_Current, 0);
		}

		private void cmbPlotAll_Observer_Click(object sender, EventArgs e)
		{
			if (((ListControl)cmbObserver).get_SelectedIndex() < 0)
			{
				return;
			}
			List<int> RecordNumbersForMultiCurve = new List<int>();
			if ((((ListControl)cmbObserver).get_SelectedIndex() < 0) & (cmbObserver.get_Items().get_Count() > 0))
			{
				((ListControl)cmbObserver).set_SelectedIndex(0);
			}
			string observer = LightData.ObserverNameIndex[((ListControl)cmbObserver).get_SelectedIndex()].Observer;
			for (int i = 0; i < LightData.ObserverNameIndex.Count; i++)
			{
				if (((ListControl)cmbObserver).get_SelectedIndex() == 0)
				{
					RecordNumbersForMultiCurve.Add(LightData.ObserverNameIndex[i].LightCurveIndex);
				}
				else if (LightData.ObserverNameIndex[i].Observer == observer)
				{
					RecordNumbersForMultiCurve.Add(LightData.ObserverNameIndex[i].LightCurveIndex);
				}
			}
			MultiCurvePlot(ref RecordNumbersForMultiCurve, Main: true, Submitted: false, Mine: false, Pending: false, ToDelete: false, ToReview: false, indicateMissedImagesToolStripMenuItem.get_Checked());
		}

		private void txtRecord_TextChanged(object sender, EventArgs e)
		{
			LightData.HeightRatioForSelectedLightLevel = 0f;
			SliderVerticalScale.set_Value(150);
			VerticalPlotScale = 1f;
			bool addExtraHeader;
			addTextToolStripMenuItem.set_Checked(addExtraHeader = false);
			LightData.AddExtraHeader = addExtraHeader;
			LightData.HeaderExtraText = "";
			LightData.HeaderExtraTime_UT1 = (LightData.HeaderExtraTime_UT2 = 0.0);
			addMarkerForThisUTTime1.set_Checked(addExtraHeader = false);
			LightData.AddTimeMarker1 = addExtraHeader;
			addMarkerForThisUTTime2.set_Checked(addExtraHeader = false);
			LightData.AddTimeMarker2 = addExtraHeader;
			PlotRecordFromMain();
		}

		private void PlotRecordFromMain()
		{
			int.TryParse(((Control)txtRecord).get_Text(), out var result);
			LightCurveIndex_Current = result - 1;
			PlotMainFromRecordNumber(result);
			LastPlotWasMain = true;
			LastPlotWasSubmitted = (LastPlotWasSubmittedAll = (LastPlotWasAsteroidNumber = (LastPlotWasAsteroidName = false)));
		}

		private void cmbPlotAll_Star_Click(object sender, EventArgs e)
		{
			List<int> RecordNumbersForMultiCurve = new List<int>();
			((ListControl)cmbStar).get_SelectedIndex();
			LightData.HeaderExtraText = "";
			switch (SelectedCatalogue)
			{
			case 0:
			{
				int starNum1_or_AsteroidNo = LightData.HipIndex[((ListControl)cmbStar).get_SelectedIndex()].StarNum1_or_AsteroidNo;
				for (int n = 0; n < LightData.HipIndex.Count; n++)
				{
					if (LightData.HipIndex[n].StarNum1_or_AsteroidNo == starNum1_or_AsteroidNo)
					{
						RecordNumbersForMultiCurve.Add(LightData.HipIndex[n].LightCurveIndex);
					}
				}
				break;
			}
			case 1:
			{
				int starNum1_or_AsteroidNo = LightData.SAOIndex[((ListControl)cmbStar).get_SelectedIndex()].StarNum1_or_AsteroidNo;
				for (int num2 = 0; num2 < LightData.SAOIndex.Count; num2++)
				{
					if (LightData.SAOIndex[num2].StarNum1_or_AsteroidNo == starNum1_or_AsteroidNo)
					{
						RecordNumbersForMultiCurve.Add(LightData.SAOIndex[num2].LightCurveIndex);
					}
				}
				break;
			}
			case 2:
			{
				int starNum1_or_AsteroidNo = LightData.XZ80Index[((ListControl)cmbStar).get_SelectedIndex()].StarNum1_or_AsteroidNo;
				for (int j = 0; j < LightData.XZ80Index.Count; j++)
				{
					if (LightData.XZ80Index[j].StarNum1_or_AsteroidNo == starNum1_or_AsteroidNo)
					{
						RecordNumbersForMultiCurve.Add(LightData.XZ80Index[j].LightCurveIndex);
					}
				}
				break;
			}
			case 3:
			{
				if (LightData.ZCIndex[((ListControl)cmbStar).get_SelectedIndex()].StarNum1_or_AsteroidNo < 1)
				{
					break;
				}
				int starNum1_or_AsteroidNo = LightData.ZCIndex[((ListControl)cmbStar).get_SelectedIndex()].StarNum1_or_AsteroidNo;
				for (int l = 0; l < LightData.ZCIndex.Count; l++)
				{
					if (LightData.ZCIndex[l].StarNum1_or_AsteroidNo == starNum1_or_AsteroidNo)
					{
						RecordNumbersForMultiCurve.Add(LightData.ZCIndex[l].LightCurveIndex);
					}
				}
				break;
			}
			case 4:
			{
				int starNum1_or_AsteroidNo = LightData.Tyc2Index[((ListControl)cmbStar).get_SelectedIndex()].StarNum1_or_AsteroidNo;
				int starNum = LightData.Tyc2Index[((ListControl)cmbStar).get_SelectedIndex()].StarNum2;
				for (int k = 0; k < LightData.Tyc2Index.Count; k++)
				{
					if ((LightData.Tyc2Index[k].StarNum1_or_AsteroidNo == starNum1_or_AsteroidNo) & (LightData.Tyc2Index[k].StarNum2 == starNum))
					{
						RecordNumbersForMultiCurve.Add(LightData.Tyc2Index[k].LightCurveIndex);
					}
				}
				break;
			}
			case 5:
			{
				int starNum1_or_AsteroidNo = LightData.U4Index[((ListControl)cmbStar).get_SelectedIndex()].StarNum1_or_AsteroidNo;
				int starNum = LightData.U4Index[((ListControl)cmbStar).get_SelectedIndex()].StarNum2;
				for (int num = 0; num < LightData.U4Index.Count; num++)
				{
					if ((LightData.U4Index[num].StarNum1_or_AsteroidNo == starNum1_or_AsteroidNo) & (LightData.U4Index[num].StarNum2 == starNum))
					{
						RecordNumbersForMultiCurve.Add(LightData.U4Index[num].LightCurveIndex);
					}
				}
				break;
			}
			case 6:
			{
				int starNum1_or_AsteroidNo = LightData.AsteroidIndex[((ListControl)cmbStar).get_SelectedIndex()].StarNum1_or_AsteroidNo;
				for (int m = 0; m < LightData.AsteroidIndex.Count; m++)
				{
					if (LightData.AsteroidIndex[m].StarNum1_or_AsteroidNo == starNum1_or_AsteroidNo)
					{
						RecordNumbersForMultiCurve.Add(LightData.AsteroidIndex[m].LightCurveIndex);
					}
				}
				break;
			}
			case 7:
			{
				if ((((ListControl)cmbStar).get_SelectedIndex() < 0) & (cmbStar.get_Items().get_Count() > 0))
				{
					((ListControl)cmbStar).set_SelectedIndex(0);
				}
				string observer = LightData.ObserverNameIndex[((ListControl)cmbStar).get_SelectedIndex()].Observer;
				for (int i = 0; i < LightData.ObserverNameIndex.Count; i++)
				{
					if (LightData.ObserverNameIndex[i].Observer == observer)
					{
						RecordNumbersForMultiCurve.Add(LightData.ObserverNameIndex[i].LightCurveIndex);
					}
				}
				break;
			}
			}
			MultiCurvePlot(ref RecordNumbersForMultiCurve, Main: true, Submitted: false, Mine: false, Pending: false, ToDelete: false, ToReview: false, indicateMissedImagesToolStripMenuItem.get_Checked());
		}

		private void cmbStar_SelectedIndexChanged(object sender, EventArgs e)
		{
			IsPlotting = false;
			if (!LightData.IndexesCreated)
			{
				return;
			}
			LightCurveIndex_Current = 0;
			if (((ListControl)cmbStar).get_SelectedIndex() >= 0)
			{
				LightData.HeightRatioForSelectedLightLevel = 0f;
				SliderVerticalScale.set_Value(150);
				VerticalPlotScale = 1f;
				bool addExtraHeader;
				addTextToolStripMenuItem.set_Checked(addExtraHeader = false);
				LightData.AddExtraHeader = addExtraHeader;
				LightData.HeaderExtraText = "";
				LightData.HeaderExtraTime_UT1 = (LightData.HeaderExtraTime_UT2 = 0.0);
				addMarkerForThisUTTime1.set_Checked(addExtraHeader = false);
				LightData.AddTimeMarker1 = addExtraHeader;
				addMarkerForThisUTTime2.set_Checked(addExtraHeader = false);
				LightData.AddTimeMarker2 = addExtraHeader;
				switch (SelectedCatalogue)
				{
				case 0:
					LightCurveIndex_Current = LightData.HipIndex[((ListControl)cmbStar).get_SelectedIndex()].LightCurveIndex;
					break;
				case 1:
					LightCurveIndex_Current = LightData.SAOIndex[((ListControl)cmbStar).get_SelectedIndex()].LightCurveIndex;
					break;
				case 2:
					LightCurveIndex_Current = LightData.XZ80Index[((ListControl)cmbStar).get_SelectedIndex()].LightCurveIndex;
					break;
				case 3:
					LightCurveIndex_Current = LightData.ZCIndex[((ListControl)cmbStar).get_SelectedIndex()].LightCurveIndex;
					break;
				case 4:
					LightCurveIndex_Current = LightData.Tyc2Index[((ListControl)cmbStar).get_SelectedIndex()].LightCurveIndex;
					break;
				case 5:
					LightCurveIndex_Current = LightData.U4Index[((ListControl)cmbStar).get_SelectedIndex()].LightCurveIndex;
					break;
				}
				((Control)txtStar).set_Text(LightData.MainLightCurves[LightCurveIndex_Current].StarDetails());
				((Control)txtObs).set_Text(LightData.MainLightCurves[LightCurveIndex_Current].DateDetails() + "\r\n" + LightData.MainLightCurves[LightCurveIndex_Current].ObserverDetails());
				((Control)txtCirc).set_Text(LightData.MainLightCurves[LightCurveIndex_Current].Circumstances());
				PlotMain = true;
				PlotLightCurve(LightCurveIndex_Current, 0);
			}
		}

		private void lstSubmitted_SelectedIndexChanged(object sender, EventArgs e)
		{
			LightData.HeightRatioForSelectedLightLevel = 0f;
			SliderVerticalScale.set_Value(150);
			VerticalPlotScale = 1f;
			bool addExtraHeader;
			addTextToolStripMenuItem.set_Checked(addExtraHeader = false);
			LightData.AddExtraHeader = addExtraHeader;
			LightData.HeaderExtraText = "";
			LightData.HeaderExtraTime_UT1 = (LightData.HeaderExtraTime_UT2 = 0.0);
			addMarkerForThisUTTime1.set_Checked(addExtraHeader = false);
			LightData.AddTimeMarker1 = addExtraHeader;
			addMarkerForThisUTTime2.set_Checked(addExtraHeader = false);
			LightData.AddTimeMarker2 = addExtraHeader;
			PlotRecordFromSubmitted();
			((ScrollProperties)((ScrollableControl)panelPlot).get_HorizontalScroll()).set_Value(0);
			Application.DoEvents();
		}

		private void PlotRecordFromSubmitted()
		{
			int selectedIndex = ((ListControl)lstSubmitted).get_SelectedIndex();
			if (selectedIndex >= 0)
			{
				PlotMain = false;
				LightCurveIndex_Current = selectedIndex;
				((Control)txtStar).set_Text(LightData.LightCurvesSubmitted[LightCurveIndex_Current].StarDetails());
				((Control)txtObs).set_Text(LightData.LightCurvesSubmitted[LightCurveIndex_Current].DateDetails() + "\r\n" + LightData.LightCurvesSubmitted[LightCurveIndex_Current].ObserverDetails());
				((Control)txtCirc).set_Text(LightData.LightCurvesSubmitted[LightCurveIndex_Current].Circumstances());
				PlotLightCurve(-1, selectedIndex);
				LastPlotWasSubmitted = true;
				LastPlotWasMain = (LastPlotWasSubmittedAll = (LastPlotWasAsteroidNumber = (LastPlotWasAsteroidNumber = (LastPlotWasAsteroidName = false))));
			}
		}

		private void txtAsteroidNumber_TextChanged(object sender, EventArgs e)
		{
			LightData.HeightRatioForSelectedLightLevel = 0f;
			SliderVerticalScale.set_Value(150);
			VerticalPlotScale = 1f;
			bool addExtraHeader;
			addTextToolStripMenuItem.set_Checked(addExtraHeader = false);
			LightData.AddExtraHeader = addExtraHeader;
			LightData.HeaderExtraText = "";
			LightData.HeaderExtraTime_UT1 = (LightData.HeaderExtraTime_UT2 = 0.0);
			addMarkerForThisUTTime1.set_Checked(addExtraHeader = false);
			LightData.AddTimeMarker1 = addExtraHeader;
			addMarkerForThisUTTime2.set_Checked(addExtraHeader = false);
			LightData.AddTimeMarker2 = addExtraHeader;
			int.TryParse(((Control)txtAsteroidNumber).get_Text(), out var result);
			LightData.CreateAsteroidNumberIndex(result);
			if (result > 0)
			{
				SetSelectedAsteroidNumber(result);
			}
		}

		private void SetSelectedAsteroidNumber(int AsteroidNumber)
		{
			cmbAsteroidNumber.get_Items().Clear();
			for (int i = 0; i < LightData.AsteroidNumberIndex.Count; i++)
			{
				cmbAsteroidNumber.get_Items().Add((object)(("(" + LightData.AsteroidNumberIndex[i].StarNum1_or_AsteroidNo + ")").PadRight(10) + LightData.AsteroidNumberIndex[i].Date));
			}
			((Control)lblAsteroidCount).set_Text(LightData.AsteroidNumberIndex.Count.ToString());
			((Control)lblAsteroidCount).set_Left(((Control)txtAsteroidNumber).get_Left() - ((Control)lblAsteroidCount).get_Width());
			try
			{
				((ListControl)cmbAsteroidNumber).set_SelectedIndex(0);
			}
			catch
			{
			}
		}

		private void txtAsteroidNumber_KeyDown(object sender, KeyEventArgs e)
		{
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_0008: Invalid comparison between Unknown and I4
			if ((int)e.get_KeyCode() == 13)
			{
				int.TryParse(((Control)txtAsteroidNumber).get_Text(), out var result);
				LightData.CreateAsteroidNumberIndex(result);
				if (result > 0)
				{
					SetSelectedAsteroidNumber(result);
				}
			}
		}

		private void txtAsteroidNumber_KeyPress(object sender, KeyPressEventArgs e)
		{
			e.set_Handled(!(char.IsDigit(e.get_KeyChar()) | char.IsControl(e.get_KeyChar())));
		}

		private void SetSelectedAsteroid(int AsteroidNumber)
		{
			if (LightCurveStarIndex.SortByDate)
			{
				LightCurveStarIndex.SortByDate = false;
				LightCurveStarIndex.SortByName = false;
				LightData.AsteroidIndex.Sort();
			}
			int num = LightData.AsteroidIndex.Count - 1;
			int num2 = 0;
			int num3 = 0;
			bool flag = false;
			int num4;
			do
			{
				num4 = (num + num2) / 2;
				if (LightData.AsteroidIndex[num4].StarNum1_or_AsteroidNo == AsteroidNumber)
				{
					flag = true;
					break;
				}
				if (LightData.AsteroidIndex[num4].StarNum1_or_AsteroidNo < AsteroidNumber)
				{
					num2 = num4 + 1;
				}
				else
				{
					num = num4 - 1;
				}
			}
			while (num2 <= num);
			if (flag)
			{
				while (LightData.AsteroidIndex[num4].StarNum1_or_AsteroidNo == AsteroidNumber)
				{
					num4--;
					if (num4 < 0)
					{
						break;
					}
				}
				num3 = num4 + 1;
				while (LightData.AsteroidIndex[num3].StarNum1_or_AsteroidNo == AsteroidNumber)
				{
					num3++;
					if (num3 >= LightData.AsteroidIndex.Count)
					{
						break;
					}
				}
				((ListControl)cmbAsteroidNumber).set_SelectedIndex(num4 + 1);
				((Control)lblAsteroidCount).set_Text((num3 - num4 - 1).ToString());
				((Control)lblAsteroidCount).set_Left(((Control)txtAsteroidNumber).get_Left() - ((Control)lblAsteroidCount).get_Width());
				PlotSelectedAsteroid(ByAsteroidNumber: true, ByAsteroidName: false);
			}
			else
			{
				((Control)lblAsteroidCount).set_Text("0");
			}
		}

		private void txtAsteroidName_TextChanged(object sender, EventArgs e)
		{
			if (((Control)txtAsteroidName).get_Text().Trim().Length < 3)
			{
				((Control)lblAstNamesCount).set_Text("-");
				return;
			}
			LightData.HeightRatioForSelectedLightLevel = 0f;
			SliderVerticalScale.set_Value(150);
			VerticalPlotScale = 1f;
			bool addExtraHeader;
			addTextToolStripMenuItem.set_Checked(addExtraHeader = false);
			LightData.AddExtraHeader = addExtraHeader;
			LightData.HeaderExtraText = "";
			LightData.HeaderExtraTime_UT1 = (LightData.HeaderExtraTime_UT2 = 0.0);
			addMarkerForThisUTTime1.set_Checked(addExtraHeader = false);
			LightData.AddTimeMarker1 = addExtraHeader;
			addMarkerForThisUTTime2.set_Checked(addExtraHeader = false);
			LightData.AddTimeMarker2 = addExtraHeader;
			LightData.CreateAsteroidNameIndex(((Control)txtAsteroidName).get_Text());
			SetSelectedAsteroidName(((Control)txtAsteroidName).get_Text().Trim());
		}

		private void txtAsteroidName_Leave(object sender, EventArgs e)
		{
			if (((Control)txtAsteroidName).get_Text().Trim().Length < 3)
			{
				((Control)lblAstNamesCount).set_Text("-");
				return;
			}
			LightData.HeightRatioForSelectedLightLevel = 0f;
			bool addExtraHeader;
			addTextToolStripMenuItem.set_Checked(addExtraHeader = false);
			LightData.AddExtraHeader = addExtraHeader;
			LightData.HeaderExtraText = "";
			LightData.HeaderExtraTime_UT1 = (LightData.HeaderExtraTime_UT2 = 0.0);
			addMarkerForThisUTTime1.set_Checked(addExtraHeader = false);
			LightData.AddTimeMarker1 = addExtraHeader;
			addMarkerForThisUTTime2.set_Checked(addExtraHeader = false);
			LightData.AddTimeMarker2 = addExtraHeader;
			LightData.CreateAsteroidNameIndex(((Control)txtAsteroidName).get_Text());
			SetSelectedAsteroidName(((Control)txtAsteroidName).get_Text().Trim());
		}

		private void txtAsteroidName_KeyDown(object sender, KeyEventArgs e)
		{
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_0008: Invalid comparison between Unknown and I4
			if ((int)e.get_KeyCode() == 13)
			{
				if (((Control)txtAsteroidName).get_Text().Trim().Length < 3)
				{
					((Control)lblAstNamesCount).set_Text("-");
					return;
				}
				LightData.CreateAsteroidNameIndex(((Control)txtAsteroidName).get_Text());
				SetSelectedAsteroidName(((Control)txtAsteroidName).get_Text().Trim());
			}
		}

		private void SetSelectedAsteroidName(string AsteroidName)
		{
			cmbAsteroidName.get_Items().Clear();
			for (int i = 0; i < LightData.AsteroidNameIndex.Count; i++)
			{
				cmbAsteroidName.get_Items().Add((object)(LightData.AsteroidNameIndex[i].ObjectName.PadRight(14) + LightData.AsteroidNameIndex[i].Date));
			}
			((Control)lblAstNamesCount).set_Text(LightData.AsteroidNameIndex.Count.ToString());
			((Control)lblAstNamesCount).set_Left(((Control)txtAsteroidName).get_Left() - ((Control)lblAstNamesCount).get_Width());
			try
			{
				((ListControl)cmbAsteroidName).set_SelectedIndex(0);
			}
			catch
			{
			}
		}

		private void cmbAsteroidName_SelectedIndexChanged(object sender, EventArgs e)
		{
			LightData.HeightRatioForSelectedLightLevel = 0f;
			SliderVerticalScale.set_Value(150);
			VerticalPlotScale = 1f;
			bool addExtraHeader;
			addTextToolStripMenuItem.set_Checked(addExtraHeader = false);
			LightData.AddExtraHeader = addExtraHeader;
			LightData.HeaderExtraText = "";
			LightData.HeaderExtraTime_UT1 = (LightData.HeaderExtraTime_UT2 = 0.0);
			addMarkerForThisUTTime1.set_Checked(addExtraHeader = false);
			LightData.AddTimeMarker1 = addExtraHeader;
			addMarkerForThisUTTime2.set_Checked(addExtraHeader = false);
			LightData.AddTimeMarker2 = addExtraHeader;
			PlotSelectedAsteroid(ByAsteroidNumber: false, ByAsteroidName: true);
		}

		private void cmbPlotAll_Name_Click(object sender, EventArgs e)
		{
			LightData.HeightRatioForSelectedLightLevel = 0f;
			bool addExtraHeader;
			addTextToolStripMenuItem.set_Checked(addExtraHeader = false);
			LightData.AddExtraHeader = addExtraHeader;
			LightData.HeaderExtraText = "";
			LightData.HeaderExtraTime_UT1 = (LightData.HeaderExtraTime_UT2 = 0.0);
			addMarkerForThisUTTime1.set_Checked(addExtraHeader = false);
			LightData.AddTimeMarker1 = addExtraHeader;
			addMarkerForThisUTTime2.set_Checked(addExtraHeader = false);
			LightData.AddTimeMarker2 = addExtraHeader;
			PlotAllAsteroidByName = true;
			PlotSelectedAsteroid(ByAsteroidNumber: false, ByAsteroidName: true);
		}

		private void cmbByDate_SelectedIndexChanged(object sender, EventArgs e)
		{
			LightData.HeightRatioForSelectedLightLevel = 0f;
			SliderVerticalScale.set_Value(150);
			VerticalPlotScale = 1f;
			bool addExtraHeader;
			addTextToolStripMenuItem.set_Checked(addExtraHeader = false);
			LightData.AddExtraHeader = addExtraHeader;
			LightData.HeaderExtraText = "";
			LightData.HeaderExtraTime_UT1 = (LightData.HeaderExtraTime_UT2 = 0.0);
			addMarkerForThisUTTime1.set_Checked(addExtraHeader = false);
			LightData.AddTimeMarker1 = addExtraHeader;
			addMarkerForThisUTTime2.set_Checked(addExtraHeader = false);
			LightData.AddTimeMarker2 = addExtraHeader;
			PlotSelectedAsteroid(ByAsteroidNumber: false, ByAsteroidName: false);
		}

		private void cmdUp_Click(object sender, EventArgs e)
		{
			if (int.TryParse(((Control)txtRecord).get_Text(), out var result))
			{
				LightData.HeightRatioForSelectedLightLevel = 0f;
				SliderVerticalScale.set_Value(150);
				VerticalPlotScale = 1f;
				bool addExtraHeader;
				addTextToolStripMenuItem.set_Checked(addExtraHeader = false);
				LightData.AddExtraHeader = addExtraHeader;
				LightData.HeaderExtraText = "";
				LightData.HeaderExtraTime_UT1 = (LightData.HeaderExtraTime_UT2 = 0.0);
				addMarkerForThisUTTime1.set_Checked(addExtraHeader = false);
				LightData.AddTimeMarker1 = addExtraHeader;
				addMarkerForThisUTTime2.set_Checked(addExtraHeader = false);
				LightData.AddTimeMarker2 = addExtraHeader;
				if (result < LightData.MainLightCurves.Count)
				{
					((Control)txtRecord).set_Text((result + 1).ToString());
				}
			}
		}

		private void picPlot_MouseClick(object sender, MouseEventArgs e)
		{
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_000b: Invalid comparison between Unknown and I4
			if ((int)e.get_Button() == 2097152)
			{
				float num = e.get_Y();
				if (num < LightData.YatTopOfPlot + 10f)
				{
					LightData.HeightRatioForSelectedLightLevel = 0f;
				}
				else
				{
					LightData.HeightRatioForSelectedLightLevel = (num - LightData.YatBottomOfPlot) / (LightData.YatTopOfPlot - LightData.YatBottomOfPlot) / VerticalPlotScale;
				}
			}
			SetSizes(ResizeEvent: true);
		}

		private void lightCurveStatsforVizieRToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Cursor.set_Current(Cursors.get_WaitCursor());
			LightData.MainLightCurves.Clear();
			LightData.ReadMainFile(Index: true, ReRead: true, ShowReadErrorMessages: true);
			ReadLightCurves();
			int num = 100;
			int num2 = 0;
			int num3 = 0;
			int num4 = 0;
			int num5 = 0;
			double num6 = 100.0;
			double num7 = 0.0;
			double num8 = 0.0;
			double num9 = 0.0;
			double num10 = 0.0;
			double num11 = 0.0;
			double num12 = 0.0;
			double num13 = 0.0;
			double num14 = 0.0;
			double num15 = 0.0;
			int num16 = 100;
			int num17 = 0;
			int num18 = 0;
			int num19 = 0;
			int num20 = 0;
			double num21 = 100.0;
			double num22 = 0.0;
			double num23 = 100.0;
			double num24 = 0.0;
			int num25 = 0;
			string text = "";
			string text2 = "";
			string text3 = "";
			for (int i = 0; i < LightData.MainLightCurves.Count; i++)
			{
				LightCurveData lightCurveData = LightData.MainLightCurves[i];
				if (lightCurveData.IsMoon)
				{
					num3++;
					if (num4 == 0)
					{
						num4 = i + 1;
					}
					else
					{
						num5 = i + 1;
					}
					if (lightCurveData.Duration < 0.1)
					{
						text = text + (i + 1) + " : ";
					}
					else
					{
						if (num6 > lightCurveData.Duration)
						{
							num6 = lightCurveData.Duration;
						}
						if (num7 < lightCurveData.Duration)
						{
							num7 = lightCurveData.Duration;
						}
						if (lightCurveData.Duration > 120.0)
						{
							text2 = text2 + (i + 1) + " : ";
						}
					}
					if (num > lightCurveData.NumPoints)
					{
						num = lightCurveData.NumPoints;
					}
					if (num2 < lightCurveData.NumPoints)
					{
						num2 = lightCurveData.NumPoints;
					}
					if (num8 > lightCurveData.Lib_L)
					{
						num8 = lightCurveData.Lib_L;
					}
					if (num9 < lightCurveData.Lib_L)
					{
						num9 = lightCurveData.Lib_L;
					}
					if (num10 > lightCurveData.Lib_B)
					{
						num10 = lightCurveData.Lib_B;
					}
					if (num11 < lightCurveData.Lib_B)
					{
						num11 = lightCurveData.Lib_B;
					}
					if (num12 > lightCurveData.LimbSlope)
					{
						num12 = lightCurveData.LimbSlope;
					}
					if (num13 < lightCurveData.LimbSlope)
					{
						num13 = lightCurveData.LimbSlope;
					}
					if (num14 > lightCurveData.ContactAngle)
					{
						num14 = lightCurveData.ContactAngle;
					}
					if (num15 < lightCurveData.ContactAngle)
					{
						num15 = lightCurveData.ContactAngle;
					}
				}
				else if (lightCurveData.IsAsteroid)
				{
					num18++;
					if (num19 == 0)
					{
						num19 = i + 1;
					}
					else
					{
						num20 = i + 1;
					}
					if (lightCurveData.Duration <= 0.1)
					{
						text = text + (i + 1) + " : ";
					}
					else
					{
						if (num21 > lightCurveData.Duration)
						{
							num21 = lightCurveData.Duration;
						}
						if (num22 < lightCurveData.Duration)
						{
							num22 = lightCurveData.Duration;
						}
						if (lightCurveData.Duration > 360.0)
						{
							text3 = text3 + (i + 1) + " : ";
						}
					}
					if (num16 > lightCurveData.NumPoints)
					{
						num16 = lightCurveData.NumPoints;
					}
					if (num17 < lightCurveData.NumPoints)
					{
						num17 = lightCurveData.NumPoints;
					}
					if (num23 > (double)lightCurveData.AltM)
					{
						num23 = lightCurveData.AltM;
					}
					if (num24 < (double)lightCurveData.AltM)
					{
						num24 = lightCurveData.AltM;
					}
				}
				else
				{
					num25++;
				}
			}
			string text4 = "Moon\r\n";
			text4 += string.Format("Number of records {0,1}\r\n", num3);
			text4 += string.Format("[{0,1:f2}/{1,1:f2}] Duration of recording\r\n", num6, num7);
			text4 += string.Format("[{0,1}/{1,1}] Number of points\r\n", num, num2);
			text4 += string.Format("[{0,1:f2}/{1,1:f2}] Libration L\r\n", num8, num9);
			text4 += string.Format("[{0,1:f2}/{1,1:f2}] Libration B\r\n", num10, num11);
			text4 += string.Format("[{0,1:f2}/{1,1:f2}] Limb slope\r\n", num12, num13);
			text4 += string.Format("[{0,1:f2}/{1,1:f2}] Contact angle\r\n", num14, num15);
			text4 += string.Format("[{0,1}/{1,1}] Sequential Number\r\n", num4, num5);
			text4 += "\r\nAsteroid\r\n";
			text4 += string.Format("Number of records {0,1}\r\n", num18);
			text4 += string.Format("[{0,1:f2}/{1,1:f2}] Duration of recording\r\n", num21, num22);
			text4 += string.Format("[{0,1}/{1,1}] Number of points\r\n", num16, num17);
			text4 += string.Format("[{0,1}/{1,1}] Sequential Number\r\n", num19, num20);
			text4 = text4 + "\r\nRecords with negative or zero durations\r\n" + text;
			if (text2.Length > 2)
			{
				text4 = text4 + "\r\n\r\nRecords with moon durations > 2 mins\r\n" + text2;
			}
			if (text3.Length > 2)
			{
				text4 = text4 + "\r\n\r\nRecords with asteroid durations > 6 min\r\n" + text3;
			}
			DisplayData displayData = new DisplayData();
			((Control)displayData).set_Text("Statistics for VizieR ReadMe file");
			((Control)displayData.txtBox).set_Text(text4.ToString());
			((Control)displayData).set_Height(460);
			((Control)displayData).Show();
			Cursor.set_Current(((Control)this).get_DefaultCursor());
		}

		private void SliderHeight_ValueChanged(object sender, EventArgs e)
		{
			VerticalPlotHeight = (float)SliderHeight.get_Value() / (float)SliderHeight.get_Maximum();
			((Control)lblFormOpacity).set_Left(((Control)SliderOpacity).get_Left() + (((Control)SliderOpacity).get_Width() - ((Control)lblFormOpacity).get_Width()) / 2);
			((Control)lblPlotHeight).set_Text(string.Format("Plot height {0,1:f0} %", 100f * VerticalPlotHeight));
			((Control)lblPlotHeight).set_Left(((Control)SliderHeight).get_Left() + (((Control)SliderHeight).get_Width() - ((Control)lblPlotHeight).get_Width()) / 2);
			if (PlotMain)
			{
				PlotLightCurveFromMainFile(LightCurveIndex_Current, indicateMissedImagesToolStripMenuItem.get_Checked());
			}
			else
			{
				PlotLightCurveFromSubmittedFile(LightCurveIndex_Current, indicateMissedImagesToolStripMenuItem.get_Checked());
			}
		}

		private void sliderOpacity_ValueChanged(object sender, EventArgs e)
		{
			((Form)this).set_Opacity((double)((float)SliderOpacity.get_Value() / (float)SliderOpacity.get_Maximum()));
			if (SliderOpacity.get_Value() <= 170)
			{
				TrackBar sliderHeight = SliderHeight;
				TrackBar sliderVerticalScale = SliderVerticalScale;
				Color maroon;
				((Control)SliderOpacity).set_BackColor(maroon = Color.Maroon);
				Color backColor;
				((Control)sliderVerticalScale).set_BackColor(backColor = maroon);
				((Control)sliderHeight).set_BackColor(backColor);
			}
			else if (SliderOpacity.get_Value() <= 195)
			{
				TrackBar sliderHeight2 = SliderHeight;
				TrackBar sliderVerticalScale2 = SliderVerticalScale;
				Color maroon;
				((Control)SliderOpacity).set_BackColor(maroon = Color.Firebrick);
				Color backColor;
				((Control)sliderVerticalScale2).set_BackColor(backColor = maroon);
				((Control)sliderHeight2).set_BackColor(backColor);
			}
			else
			{
				TrackBar sliderHeight3 = SliderHeight;
				TrackBar sliderVerticalScale3 = SliderVerticalScale;
				Color maroon;
				((Control)SliderOpacity).set_BackColor(maroon = Color.LemonChiffon);
				Color backColor;
				((Control)sliderVerticalScale3).set_BackColor(backColor = maroon);
				((Control)sliderHeight3).set_BackColor(backColor);
			}
		}

		private void SliderVerticalScale_ValueChanged(object sender, EventArgs e)
		{
			VerticalPlotScale = (float)SliderVerticalScale.get_Value() / (float)(SliderVerticalScale.get_Maximum() - 50);
			((Control)lblVerticalScale).set_Text(string.Format("Vertical scale  {0,1:f0}%", 100f * VerticalPlotScale));
			((Control)lblVerticalScale).set_Left(((Control)SliderVerticalScale).get_Left() + (((Control)SliderVerticalScale).get_Width() - ((Control)lblVerticalScale).get_Width()) / 2);
			if (PlotMain)
			{
				PlotLightCurveFromMainFile(LightCurveIndex_Current, indicateMissedImagesToolStripMenuItem.get_Checked());
			}
			else
			{
				PlotLightCurveFromSubmittedFile(LightCurveIndex_Current, indicateMissedImagesToolStripMenuItem.get_Checked());
			}
		}

		private void addTextToolStripMenuItem_Click(object sender, EventArgs e)
		{
			bool addExtraHeader;
			addTextToolStripMenuItem.set_Checked(addExtraHeader = !addTextToolStripMenuItem.get_Checked());
			LightData.AddExtraHeader = addExtraHeader;
			if (LightData.AddExtraHeader)
			{
				LightData.HeaderExtraText = ((ToolStripItem)HeaderText).get_Text();
			}
			if (PlotMain)
			{
				PlotLightCurveFromMainFile(LightCurveIndex_Current, indicateMissedImagesToolStripMenuItem.get_Checked());
			}
			else
			{
				PlotLightCurveFromSubmittedFile(LightCurveIndex_Current, indicateMissedImagesToolStripMenuItem.get_Checked());
			}
		}

		private void addMarkerForThisUTTime1_Click(object sender, EventArgs e)
		{
			bool addTimeMarker;
			addMarkerForThisUTTime1.set_Checked(addTimeMarker = !addMarkerForThisUTTime1.get_Checked());
			LightData.AddTimeMarker1 = addTimeMarker;
			if (LightData.AddTimeMarker1)
			{
				ExtraTime_GetUT(1);
			}
			if (PlotMain)
			{
				PlotLightCurveFromMainFile(LightCurveIndex_Current, indicateMissedImagesToolStripMenuItem.get_Checked());
			}
			else
			{
				PlotLightCurveFromSubmittedFile(LightCurveIndex_Current, indicateMissedImagesToolStripMenuItem.get_Checked());
			}
		}

		private void addMarkerForThisUTTime2_Click(object sender, EventArgs e)
		{
			bool addTimeMarker;
			addMarkerForThisUTTime2.set_Checked(addTimeMarker = !addMarkerForThisUTTime2.get_Checked());
			LightData.AddTimeMarker2 = addTimeMarker;
			if (LightData.AddTimeMarker2)
			{
				ExtraTime_GetUT(2);
			}
			if (PlotMain)
			{
				PlotLightCurveFromMainFile(LightCurveIndex_Current, indicateMissedImagesToolStripMenuItem.get_Checked());
			}
			else
			{
				PlotLightCurveFromSubmittedFile(LightCurveIndex_Current, indicateMissedImagesToolStripMenuItem.get_Checked());
			}
		}

		private void ExtraTime_GetUT(int num)
		{
			string[] array = ((num != 1) ? ((ToolStripItem)toolStripTextBox_UT2).get_Text().Split(new char[1] { ':' }) : ((ToolStripItem)toolStripTextBox_UT1).get_Text().Split(new char[1] { ':' }));
			if (array.Length == 3)
			{
				bool flag = true;
				if (!int.TryParse(array[0], out var result))
				{
					flag = false;
				}
				if (!int.TryParse(array[1], out var result2))
				{
					flag = false;
				}
				if (!double.TryParse(array[2], out var result3))
				{
					flag = false;
				}
				if (!flag)
				{
					ToolStripMenuItem obj = addMarkerForThisUTTime1;
					Color mistyRose;
					((ToolStripItem)toolStripTextBox_UT1).set_BackColor(mistyRose = Color.MistyRose);
					((ToolStripItem)obj).set_BackColor(mistyRose);
				}
				else
				{
					ToolStripMenuItem obj2 = addMarkerForThisUTTime1;
					Color mistyRose;
					((ToolStripItem)toolStripTextBox_UT1).set_BackColor(mistyRose = Color.FromArgb(230, 248, 230));
					((ToolStripItem)obj2).set_BackColor(mistyRose);
				}
				if (num == 1)
				{
					LightData.HeaderExtraTime_UT1 = (double)(result * 3600 + result2 * 60) + result3;
				}
				else
				{
					LightData.HeaderExtraTime_UT2 = (double)(result * 3600 + result2 * 60) + result3;
				}
			}
			else
			{
				ToolStripMenuItem obj3 = addMarkerForThisUTTime1;
				Color mistyRose;
				((ToolStripItem)toolStripTextBox_UT1).set_BackColor(mistyRose = Color.MistyRose);
				((ToolStripItem)obj3).set_BackColor(mistyRose);
			}
		}

		private void toolStripTextBox_UT_TextChanged(object sender, EventArgs e)
		{
			ExtraTime_GetUT(1);
		}

		private void toolStripTextBox_UT1_KeyPress(object sender, KeyPressEventArgs e)
		{
			if (!char.IsControl(e.get_KeyChar()) && (!char.IsDigit(e.get_KeyChar()) & (e.get_KeyChar() != ':') & (e.get_KeyChar() != '.')))
			{
				e.set_Handled(true);
			}
		}

		private void toolStripTextBox_UT1_Leave(object sender, EventArgs e)
		{
			if (PlotMain)
			{
				PlotLightCurveFromMainFile(LightCurveIndex_Current, indicateMissedImagesToolStripMenuItem.get_Checked());
			}
			else
			{
				PlotLightCurveFromSubmittedFile(LightCurveIndex_Current, indicateMissedImagesToolStripMenuItem.get_Checked());
			}
			if (LightData.AddTimeMarker1)
			{
				if (PlotMain)
				{
					PlotLightCurveFromMainFile(LightCurveIndex_Current, indicateMissedImagesToolStripMenuItem.get_Checked());
				}
				else
				{
					PlotLightCurveFromSubmittedFile(LightCurveIndex_Current, indicateMissedImagesToolStripMenuItem.get_Checked());
				}
			}
		}

		private void toolStripTextBox_UT1_KeyDown(object sender, KeyEventArgs e)
		{
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_0008: Invalid comparison between Unknown and I4
			if ((int)e.get_KeyCode() == 13 && LightData.AddTimeMarker1)
			{
				if (PlotMain)
				{
					PlotLightCurveFromMainFile(LightCurveIndex_Current, indicateMissedImagesToolStripMenuItem.get_Checked());
				}
				else
				{
					PlotLightCurveFromSubmittedFile(LightCurveIndex_Current, indicateMissedImagesToolStripMenuItem.get_Checked());
				}
			}
		}

		private void toolStripTextBox_UT2_KeyDown(object sender, KeyEventArgs e)
		{
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_0008: Invalid comparison between Unknown and I4
			if ((int)e.get_KeyCode() == 13 && LightData.AddTimeMarker2)
			{
				if (PlotMain)
				{
					PlotLightCurveFromMainFile(LightCurveIndex_Current, indicateMissedImagesToolStripMenuItem.get_Checked());
				}
				else
				{
					PlotLightCurveFromSubmittedFile(LightCurveIndex_Current, indicateMissedImagesToolStripMenuItem.get_Checked());
				}
			}
		}

		private void toolStripTextBox_UT2_KeyPress(object sender, KeyPressEventArgs e)
		{
			if (!char.IsControl(e.get_KeyChar()) && (!char.IsDigit(e.get_KeyChar()) & (e.get_KeyChar() != ':') & (e.get_KeyChar() != '.')))
			{
				e.set_Handled(true);
			}
		}

		private void toolStripTextBox_UT2_Leave(object sender, EventArgs e)
		{
			if (PlotMain)
			{
				PlotLightCurveFromMainFile(LightCurveIndex_Current, indicateMissedImagesToolStripMenuItem.get_Checked());
			}
			else
			{
				PlotLightCurveFromSubmittedFile(LightCurveIndex_Current, indicateMissedImagesToolStripMenuItem.get_Checked());
			}
			if (LightData.AddTimeMarker2)
			{
				if (PlotMain)
				{
					PlotLightCurveFromMainFile(LightCurveIndex_Current, indicateMissedImagesToolStripMenuItem.get_Checked());
				}
				else
				{
					PlotLightCurveFromSubmittedFile(LightCurveIndex_Current, indicateMissedImagesToolStripMenuItem.get_Checked());
				}
			}
		}

		private void cmdAstNumInc_Click(object sender, EventArgs e)
		{
			if (((ListControl)cmbAsteroidNumber).get_SelectedIndex() < cmbAsteroidNumber.get_Items().get_Count() - 1)
			{
				ComboBox obj = cmbAsteroidNumber;
				int selectedIndex = ((ListControl)obj).get_SelectedIndex();
				((ListControl)obj).set_SelectedIndex(selectedIndex + 1);
			}
		}

		private void cmdAstNumDec_Click(object sender, EventArgs e)
		{
			if (((ListControl)cmbAsteroidNumber).get_SelectedIndex() > 0)
			{
				ComboBox obj = cmbAsteroidNumber;
				int selectedIndex = ((ListControl)obj).get_SelectedIndex();
				((ListControl)obj).set_SelectedIndex(selectedIndex - 1);
			}
		}

		private void cmdAstNameInc_Click(object sender, EventArgs e)
		{
			if (((ListControl)cmbAsteroidName).get_SelectedIndex() < cmbAsteroidName.get_Items().get_Count() - 1)
			{
				ComboBox obj = cmbAsteroidName;
				int selectedIndex = ((ListControl)obj).get_SelectedIndex();
				((ListControl)obj).set_SelectedIndex(selectedIndex + 1);
			}
		}

		private void cmdAstNameDec_Click(object sender, EventArgs e)
		{
			if (((ListControl)cmbAsteroidName).get_SelectedIndex() > 0)
			{
				ComboBox obj = cmbAsteroidName;
				int selectedIndex = ((ListControl)obj).get_SelectedIndex();
				((ListControl)obj).set_SelectedIndex(selectedIndex - 1);
			}
		}

		private void chkThickLines_CheckedChanged(object sender, EventArgs e)
		{
		}

		private void chkFineScroll_CheckedChanged(object sender, EventArgs e)
		{
			if (chkFineScroll.get_Checked())
			{
				updnScale.set_Increment(0.01m);
			}
			else
			{
				updnScale.set_Increment(0.2m);
			}
		}

		private void deleteDelFilesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			LightData.ShowDeleteDelInit();
		}

		private void removeReviewStatusToolStripMenuItem_Click(object sender, EventArgs e)
		{
			LightData.ShowRemoveReview();
		}

		private void drawWithThickLinesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			drawWithThickLinesToolStripMenuItem.set_Checked(!drawWithThickLinesToolStripMenuItem.get_Checked());
			if (LastPlotWasMain)
			{
				PlotRecordFromMain();
			}
			if (LastPlotWasSubmitted)
			{
				PlotRecordFromSubmitted();
			}
			if (LastPlotWasSubmittedAll)
			{
				PlotAllSubmitted();
			}
			if (LastPlotWasAsteroidNumber)
			{
				PlotSelectedAsteroid(ByAsteroidNumber: true, ByAsteroidName: false);
			}
			if (LastPlotWasAsteroidName)
			{
				PlotSelectedAsteroid(ByAsteroidNumber: false, ByAsteroidName: true);
			}
		}

		private void indicateMissedImagesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			indicateMissedImagesToolStripMenuItem.set_Checked(!indicateMissedImagesToolStripMenuItem.get_Checked());
			if (LastPlotWasMain)
			{
				PlotRecordFromMain();
			}
			if (LastPlotWasSubmitted)
			{
				PlotRecordFromSubmitted();
			}
			if (LastPlotWasSubmittedAll)
			{
				PlotAllSubmitted();
			}
			if (LastPlotWasAsteroidNumber)
			{
				PlotSelectedAsteroid(ByAsteroidNumber: true, ByAsteroidName: false);
			}
			if (LastPlotWasAsteroidName)
			{
				PlotSelectedAsteroid(ByAsteroidNumber: false, ByAsteroidName: true);
			}
		}

		private void cmdAstDateInc_Click(object sender, EventArgs e)
		{
			if (((ListControl)cmbByDate).get_SelectedIndex() < cmbByDate.get_Items().get_Count() - 1)
			{
				ComboBox obj = cmbByDate;
				int selectedIndex = ((ListControl)obj).get_SelectedIndex();
				((ListControl)obj).set_SelectedIndex(selectedIndex + 1);
			}
		}

		private void cmdAstDateDec_Click(object sender, EventArgs e)
		{
			if (((ListControl)cmbByDate).get_SelectedIndex() > 0)
			{
				ComboBox obj = cmbByDate;
				int selectedIndex = ((ListControl)obj).get_SelectedIndex();
				((ListControl)obj).set_SelectedIndex(selectedIndex - 1);
			}
		}

		private void cmdDown_Click(object sender, EventArgs e)
		{
			if (int.TryParse(((Control)txtRecord).get_Text(), out var result))
			{
				LightData.HeightRatioForSelectedLightLevel = 0f;
				SliderVerticalScale.set_Value(150);
				VerticalPlotScale = 1f;
				bool addExtraHeader;
				addTextToolStripMenuItem.set_Checked(addExtraHeader = false);
				LightData.AddExtraHeader = addExtraHeader;
				LightData.HeaderExtraText = "";
				if (result > 1)
				{
					((Control)txtRecord).set_Text((result - 1).ToString());
				}
			}
		}

		private void cmdPlotAll_Submitted_Click(object sender, EventArgs e)
		{
			PlotAllSubmitted();
		}

		private void PlotAllSubmitted()
		{
			LightData.HeightRatioForSelectedLightLevel = 0f;
			Display_PendingLightCurves(0, ShowMessage: true, ShowReadErrorMessages: true);
			LastPlotWasSubmittedAll = true;
			LastPlotWasMain = (LastPlotWasSubmitted = false);
		}

		private void chkShowSkippedFrames_CheckedChanged(object sender, EventArgs e)
		{
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
			//IL_022c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0236: Expected O, but got Unknown
			//IL_0237: Unknown result type (might be due to invalid IL or missing references)
			//IL_0241: Expected O, but got Unknown
			//IL_0242: Unknown result type (might be due to invalid IL or missing references)
			//IL_024c: Expected O, but got Unknown
			//IL_024d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0257: Expected O, but got Unknown
			//IL_0258: Unknown result type (might be due to invalid IL or missing references)
			//IL_0262: Expected O, but got Unknown
			//IL_0263: Unknown result type (might be due to invalid IL or missing references)
			//IL_026d: Expected O, but got Unknown
			//IL_026e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0278: Expected O, but got Unknown
			//IL_0279: Unknown result type (might be due to invalid IL or missing references)
			//IL_0283: Expected O, but got Unknown
			//IL_0284: Unknown result type (might be due to invalid IL or missing references)
			//IL_028e: Expected O, but got Unknown
			//IL_028f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0299: Expected O, but got Unknown
			//IL_029a: Unknown result type (might be due to invalid IL or missing references)
			//IL_02a4: Expected O, but got Unknown
			//IL_02a5: Unknown result type (might be due to invalid IL or missing references)
			//IL_02af: Expected O, but got Unknown
			//IL_02b0: Unknown result type (might be due to invalid IL or missing references)
			//IL_02ba: Expected O, but got Unknown
			//IL_02bb: Unknown result type (might be due to invalid IL or missing references)
			//IL_02c5: Expected O, but got Unknown
			//IL_02c6: Unknown result type (might be due to invalid IL or missing references)
			//IL_02d0: Expected O, but got Unknown
			//IL_02d1: Unknown result type (might be due to invalid IL or missing references)
			//IL_02db: Expected O, but got Unknown
			//IL_02dc: Unknown result type (might be due to invalid IL or missing references)
			//IL_02e6: Expected O, but got Unknown
			//IL_02e7: Unknown result type (might be due to invalid IL or missing references)
			//IL_02f1: Expected O, but got Unknown
			//IL_02f2: Unknown result type (might be due to invalid IL or missing references)
			//IL_02fc: Expected O, but got Unknown
			//IL_02fd: Unknown result type (might be due to invalid IL or missing references)
			//IL_0307: Expected O, but got Unknown
			//IL_0308: Unknown result type (might be due to invalid IL or missing references)
			//IL_0312: Expected O, but got Unknown
			//IL_0313: Unknown result type (might be due to invalid IL or missing references)
			//IL_031d: Expected O, but got Unknown
			//IL_031e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0328: Expected O, but got Unknown
			//IL_0329: Unknown result type (might be due to invalid IL or missing references)
			//IL_0333: Expected O, but got Unknown
			//IL_0334: Unknown result type (might be due to invalid IL or missing references)
			//IL_033e: Expected O, but got Unknown
			//IL_033f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0349: Expected O, but got Unknown
			//IL_034a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0354: Expected O, but got Unknown
			//IL_0355: Unknown result type (might be due to invalid IL or missing references)
			//IL_035f: Expected O, but got Unknown
			//IL_0360: Unknown result type (might be due to invalid IL or missing references)
			//IL_036a: Expected O, but got Unknown
			//IL_036b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0375: Expected O, but got Unknown
			//IL_0376: Unknown result type (might be due to invalid IL or missing references)
			//IL_0380: Expected O, but got Unknown
			//IL_0381: Unknown result type (might be due to invalid IL or missing references)
			//IL_038b: Expected O, but got Unknown
			//IL_038c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0396: Expected O, but got Unknown
			//IL_0397: Unknown result type (might be due to invalid IL or missing references)
			//IL_03a1: Expected O, but got Unknown
			//IL_03a2: Unknown result type (might be due to invalid IL or missing references)
			//IL_03ac: Expected O, but got Unknown
			//IL_03ad: Unknown result type (might be due to invalid IL or missing references)
			//IL_03b7: Expected O, but got Unknown
			//IL_03b8: Unknown result type (might be due to invalid IL or missing references)
			//IL_03c2: Expected O, but got Unknown
			//IL_03c3: Unknown result type (might be due to invalid IL or missing references)
			//IL_03cd: Expected O, but got Unknown
			//IL_03ce: Unknown result type (might be due to invalid IL or missing references)
			//IL_03d8: Expected O, but got Unknown
			//IL_03d9: Unknown result type (might be due to invalid IL or missing references)
			//IL_03e3: Expected O, but got Unknown
			//IL_03e4: Unknown result type (might be due to invalid IL or missing references)
			//IL_03ee: Expected O, but got Unknown
			//IL_03ef: Unknown result type (might be due to invalid IL or missing references)
			//IL_03f9: Expected O, but got Unknown
			//IL_03fa: Unknown result type (might be due to invalid IL or missing references)
			//IL_0404: Expected O, but got Unknown
			//IL_0405: Unknown result type (might be due to invalid IL or missing references)
			//IL_040f: Expected O, but got Unknown
			//IL_0410: Unknown result type (might be due to invalid IL or missing references)
			//IL_041a: Expected O, but got Unknown
			//IL_041b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0425: Expected O, but got Unknown
			//IL_0426: Unknown result type (might be due to invalid IL or missing references)
			//IL_0430: Expected O, but got Unknown
			//IL_0431: Unknown result type (might be due to invalid IL or missing references)
			//IL_043b: Expected O, but got Unknown
			//IL_043c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0446: Expected O, but got Unknown
			//IL_0447: Unknown result type (might be due to invalid IL or missing references)
			//IL_0451: Expected O, but got Unknown
			//IL_0452: Unknown result type (might be due to invalid IL or missing references)
			//IL_045c: Expected O, but got Unknown
			//IL_045d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0467: Expected O, but got Unknown
			//IL_0468: Unknown result type (might be due to invalid IL or missing references)
			//IL_0472: Expected O, but got Unknown
			//IL_0473: Unknown result type (might be due to invalid IL or missing references)
			//IL_047d: Expected O, but got Unknown
			//IL_047e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0488: Expected O, but got Unknown
			//IL_0489: Unknown result type (might be due to invalid IL or missing references)
			//IL_0493: Expected O, but got Unknown
			//IL_0494: Unknown result type (might be due to invalid IL or missing references)
			//IL_049e: Expected O, but got Unknown
			//IL_049f: Unknown result type (might be due to invalid IL or missing references)
			//IL_04a9: Expected O, but got Unknown
			//IL_04aa: Unknown result type (might be due to invalid IL or missing references)
			//IL_04b4: Expected O, but got Unknown
			//IL_04b5: Unknown result type (might be due to invalid IL or missing references)
			//IL_04bf: Expected O, but got Unknown
			//IL_04c0: Unknown result type (might be due to invalid IL or missing references)
			//IL_04ca: Expected O, but got Unknown
			//IL_04cb: Unknown result type (might be due to invalid IL or missing references)
			//IL_04d5: Expected O, but got Unknown
			//IL_04d6: Unknown result type (might be due to invalid IL or missing references)
			//IL_04e0: Expected O, but got Unknown
			//IL_04e1: Unknown result type (might be due to invalid IL or missing references)
			//IL_04eb: Expected O, but got Unknown
			//IL_04ec: Unknown result type (might be due to invalid IL or missing references)
			//IL_04f6: Expected O, but got Unknown
			//IL_04f7: Unknown result type (might be due to invalid IL or missing references)
			//IL_0501: Expected O, but got Unknown
			//IL_0502: Unknown result type (might be due to invalid IL or missing references)
			//IL_050c: Expected O, but got Unknown
			//IL_050d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0517: Expected O, but got Unknown
			//IL_0518: Unknown result type (might be due to invalid IL or missing references)
			//IL_0522: Expected O, but got Unknown
			//IL_0523: Unknown result type (might be due to invalid IL or missing references)
			//IL_052d: Expected O, but got Unknown
			//IL_052e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0538: Expected O, but got Unknown
			//IL_0539: Unknown result type (might be due to invalid IL or missing references)
			//IL_0543: Expected O, but got Unknown
			//IL_0544: Unknown result type (might be due to invalid IL or missing references)
			//IL_054e: Expected O, but got Unknown
			//IL_054f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0559: Expected O, but got Unknown
			//IL_055a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0564: Expected O, but got Unknown
			//IL_0565: Unknown result type (might be due to invalid IL or missing references)
			//IL_056f: Expected O, but got Unknown
			//IL_0576: Unknown result type (might be due to invalid IL or missing references)
			//IL_0580: Expected O, but got Unknown
			//IL_0581: Unknown result type (might be due to invalid IL or missing references)
			//IL_058b: Expected O, but got Unknown
			//IL_058c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0596: Expected O, but got Unknown
			//IL_070c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0716: Expected O, but got Unknown
			//IL_07f0: Unknown result type (might be due to invalid IL or missing references)
			//IL_07fa: Expected O, but got Unknown
			//IL_0835: Unknown result type (might be due to invalid IL or missing references)
			//IL_083f: Expected O, but got Unknown
			//IL_0dd4: Unknown result type (might be due to invalid IL or missing references)
			//IL_0dde: Expected O, but got Unknown
			//IL_0deb: Unknown result type (might be due to invalid IL or missing references)
			//IL_0df5: Expected O, but got Unknown
			//IL_0f47: Unknown result type (might be due to invalid IL or missing references)
			//IL_0f51: Expected O, but got Unknown
			//IL_0f5e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0f68: Expected O, but got Unknown
			//IL_3515: Unknown result type (might be due to invalid IL or missing references)
			//IL_351f: Expected O, but got Unknown
			//IL_43d9: Unknown result type (might be due to invalid IL or missing references)
			//IL_43e3: Expected O, but got Unknown
			//IL_4778: Unknown result type (might be due to invalid IL or missing references)
			//IL_4782: Expected O, but got Unknown
			//IL_478f: Unknown result type (might be due to invalid IL or missing references)
			//IL_4799: Expected O, but got Unknown
			//IL_4b42: Unknown result type (might be due to invalid IL or missing references)
			//IL_4b4c: Expected O, but got Unknown
			//IL_4b59: Unknown result type (might be due to invalid IL or missing references)
			//IL_4b63: Expected O, but got Unknown
			//IL_4be1: Unknown result type (might be due to invalid IL or missing references)
			//IL_4beb: Expected O, but got Unknown
			//IL_4db3: Unknown result type (might be due to invalid IL or missing references)
			//IL_4dbd: Expected O, but got Unknown
			//IL_4e22: Unknown result type (might be due to invalid IL or missing references)
			//IL_4e2c: Expected O, but got Unknown
			components = new Container();
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(LightCurveViewer));
			panelPlot = new Panel();
			panel9 = new Panel();
			picPlot = new PictureBox();
			menuStrip1 = new MenuStrip();
			viewToolStripMenuItem1 = new ToolStripMenuItem();
			my0UnsubmittedLightCurvesToolStripMenuItem = new ToolStripMenuItem();
			manageMyUnsubmittedLightCurvesToolStripMenuItem = new ToolStripMenuItem();
			myReportedLightCurvesToolStripMenuItem = new ToolStripMenuItem();
			withDisplayedLightCurveToolStripMenuItem = new ToolStripMenuItem();
			addTextToolStripMenuItem = new ToolStripMenuItem();
			HeaderText = new ToolStripTextBox();
			addMarkerForThisUTTime1 = new ToolStripMenuItem();
			toolStripTextBox_UT1 = new ToolStripTextBox();
			addMarkerForThisUTTime2 = new ToolStripMenuItem();
			toolStripTextBox_UT2 = new ToolStripTextBox();
			toolStripSeparator9 = new ToolStripSeparator();
			drawWithThickLinesToolStripMenuItem = new ToolStripMenuItem();
			indicateMissedImagesToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator4 = new ToolStripSeparator();
			writecsvFileForThisLightCurveToolStripMenuItem = new ToolStripMenuItem();
			displayInAOTAToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator2 = new ToolStripSeparator();
			copyToolStripMenuItem1 = new ToolStripMenuItem();
			saveImageToolStripMenuItem = new ToolStripMenuItem();
			adminFunctionsToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator5 = new ToolStripSeparator();
			rereadLightCurvesToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator1 = new ToolStripSeparator();
			submittedLightCurvesToolStripMenuItem = new ToolStripMenuItem();
			rereadSubmittedLightCurvesToolStripMenuItem = new ToolStripMenuItem();
			viewLightCurvesMarkedForDeletiondelinitToolStripMenuItem = new ToolStripMenuItem();
			deleteDelFilesToolStripMenuItem = new ToolStripMenuItem();
			view0LightCurvesMarkedForReviewreviewToolStripMenuItem = new ToolStripMenuItem();
			removeReviewStatusToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator6 = new ToolStripSeparator();
			editToolStripMenuItem = new ToolStripMenuItem();
			editALightCurveMainFileToolStripMenuItem = new ToolStripMenuItem();
			editALightCurveSubmittedFileToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator7 = new ToolStripSeparator();
			mergeSubmittedWithMainToolStripMenuItem = new ToolStripMenuItem();
			listSubmittedLightCurvesAlreadyInTheMainFileToolStripMenuItem = new ToolStripMenuItem();
			listAndDeletesubmittedCurvesAlreadyInMainFileToolStripMenuItem = new ToolStripMenuItem();
			mergeSubmittedLightCurveIntoTheMainFileToolStripMenuItem = new ToolStripMenuItem();
			lightCurveStatsforVizieRToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator3 = new ToolStripSeparator();
			manageEmbargoesToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			rightClickToSetMeanLightToolStripMenuItem = new ToolStripMenuItem();
			txtStar = new TextBox();
			txtCirc = new TextBox();
			label1 = new Label();
			label2 = new Label();
			cmd_x15 = new Button();
			cmd_x10 = new Button();
			cmd_x5 = new Button();
			cmd_x1 = new Button();
			updnScale = new NumericUpDown();
			txtObs = new TextBox();
			label3 = new Label();
			panel1 = new Panel();
			lblVerticalScale = new Label();
			SliderVerticalScale = new TrackBar();
			cmdPlotAll_Submitted = new Button();
			lblFormOpacity = new Label();
			SliderOpacity = new TrackBar();
			lblPlotHeight = new Label();
			lblSubmitted = new Label();
			lstSubmitted = new ListBox();
			panel3 = new Panel();
			panel10 = new Panel();
			label13 = new Label();
			chkFineScroll = new CheckBox();
			label6 = new Label();
			pnlStarByDate = new Panel();
			lblStarCat = new Label();
			lstDates = new ListBox();
			lblLightCurves = new Label();
			SliderHeight = new TrackBar();
			pnlSelectStar = new Panel();
			panel8 = new Panel();
			cmdPlotAll_Name = new Button();
			cmdAstNameDec = new Button();
			cmdAstNameInc = new Button();
			lblAstNamesCount = new Label();
			txtAsteroidName = new TextBox();
			label11 = new Label();
			cmbAsteroidName = new ComboBox();
			panel2 = new Panel();
			cmdAstDateDec = new Button();
			cmdAstDateInc = new Button();
			label5 = new Label();
			cmbByDate = new ComboBox();
			panel7 = new Panel();
			cmbPlotAll_Star = new Button();
			label8 = new Label();
			optU4 = new RadioButton();
			optZC = new RadioButton();
			optTycho2 = new RadioButton();
			optXZ = new RadioButton();
			optSAO = new RadioButton();
			optHip = new RadioButton();
			cmbStar = new ComboBox();
			panel6 = new Panel();
			cmdPlotAll_Observer = new Button();
			cmdCopy = new Button();
			cmbObserver = new ComboBox();
			lblObserverCount = new Label();
			label4 = new Label();
			lblIndexName = new Label();
			txtObserver = new TextBox();
			panel5 = new Panel();
			cmdPlotAll_Number = new Button();
			cmdAstNumDec = new Button();
			cmdAstNumInc = new Button();
			lblAsteroidCount = new Label();
			txtAsteroidNumber = new TextBox();
			label7 = new Label();
			cmbAsteroidNumber = new ComboBox();
			panel4 = new Panel();
			cmdDown = new Button();
			cmdUp = new Button();
			lblRecords = new Label();
			txtRecord = new TextBox();
			toolTip1 = new ToolTip(components);
			chkTimeHeight = new CheckBox();
			label9 = new Label();
			((Control)panelPlot).SuspendLayout();
			((ISupportInitialize)picPlot).BeginInit();
			((Control)menuStrip1).SuspendLayout();
			((ISupportInitialize)updnScale).BeginInit();
			((Control)panel1).SuspendLayout();
			((ISupportInitialize)SliderVerticalScale).BeginInit();
			((ISupportInitialize)SliderOpacity).BeginInit();
			((Control)panel3).SuspendLayout();
			((Control)panel10).SuspendLayout();
			((Control)pnlStarByDate).SuspendLayout();
			((ISupportInitialize)SliderHeight).BeginInit();
			((Control)pnlSelectStar).SuspendLayout();
			((Control)panel8).SuspendLayout();
			((Control)panel2).SuspendLayout();
			((Control)panel7).SuspendLayout();
			((Control)panel6).SuspendLayout();
			((Control)panel5).SuspendLayout();
			((Control)panel4).SuspendLayout();
			((Control)this).SuspendLayout();
			((ScrollableControl)panelPlot).set_AutoScroll(true);
			((Control)panelPlot).set_BackColor(Color.Beige);
			panelPlot.set_BorderStyle((BorderStyle)2);
			((Control)panelPlot).get_Controls().Add((Control)(object)panel9);
			((Control)panelPlot).get_Controls().Add((Control)(object)picPlot);
			((Control)panelPlot).set_Location(new Point(4, 25));
			((Control)panelPlot).set_Name("panelPlot");
			((Control)panelPlot).set_Size(new Size(910, 407));
			((Control)panelPlot).set_TabIndex(2);
			((ScrollableControl)panelPlot).add_Scroll(new ScrollEventHandler(panelPlot_Scroll));
			((Control)panel9).set_BackColor(SystemColors.ScrollBar);
			((Control)panel9).set_ForeColor(SystemColors.ControlLight);
			((Control)panel9).set_Location(new Point(5, 390));
			((Control)panel9).set_Name("panel9");
			((Control)panel9).set_Size(new Size(888, 9));
			((Control)panel9).set_TabIndex(1);
			((Control)picPlot).set_BackColor(Color.White);
			((Control)picPlot).set_Location(new Point(4, 5));
			((Control)picPlot).set_Name("picPlot");
			((Control)picPlot).set_Size(new Size(898, 378));
			picPlot.set_TabIndex(0);
			picPlot.set_TabStop(false);
			((Control)picPlot).add_MouseClick(new MouseEventHandler(picPlot_MouseClick));
			((Control)picPlot).add_MouseEnter((EventHandler)picPlot_MouseEnter);
			((Control)picPlot).add_MouseLeave((EventHandler)picPlot_MouseLeave);
			((Control)picPlot).add_MouseMove(new MouseEventHandler(picPlot_MouseMove));
			((ToolStrip)menuStrip1).set_BackColor(Color.Ivory);
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[6]
			{
				(ToolStripItem)viewToolStripMenuItem1,
				(ToolStripItem)withDisplayedLightCurveToolStripMenuItem,
				(ToolStripItem)adminFunctionsToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem,
				(ToolStripItem)rightClickToSetMeanLightToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(1141, 24));
			((Control)menuStrip1).set_TabIndex(3);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)viewToolStripMenuItem1).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)my0UnsubmittedLightCurvesToolStripMenuItem,
				(ToolStripItem)manageMyUnsubmittedLightCurvesToolStripMenuItem,
				(ToolStripItem)myReportedLightCurvesToolStripMenuItem
			});
			((ToolStripItem)viewToolStripMenuItem1).set_Font(new Font("Segoe UI", 9f, FontStyle.Bold));
			((ToolStripItem)viewToolStripMenuItem1).set_ForeColor(Color.DarkBlue);
			((ToolStripItem)viewToolStripMenuItem1).set_Name("viewToolStripMenuItem1");
			((ToolStripItem)viewToolStripMenuItem1).set_Size(new Size(156, 20));
			((ToolStripItem)viewToolStripMenuItem1).set_Text("View my light curves...    ");
			((ToolStripItem)my0UnsubmittedLightCurvesToolStripMenuItem).set_Name("my0UnsubmittedLightCurvesToolStripMenuItem");
			((ToolStripItem)my0UnsubmittedLightCurvesToolStripMenuItem).set_Size(new Size(310, 22));
			((ToolStripItem)my0UnsubmittedLightCurvesToolStripMenuItem).set_Text("My 0 unsubmitted light curves (as a group)");
			((ToolStripItem)my0UnsubmittedLightCurvesToolStripMenuItem).add_Click((EventHandler)my0UnsubmittedLightCurvesToolStripMenuItem_Click);
			((ToolStripItem)manageMyUnsubmittedLightCurvesToolStripMenuItem).set_Name("manageMyUnsubmittedLightCurvesToolStripMenuItem");
			((ToolStripItem)manageMyUnsubmittedLightCurvesToolStripMenuItem).set_Size(new Size(310, 22));
			((ToolStripItem)manageMyUnsubmittedLightCurvesToolStripMenuItem).set_Text("Manage my unsubmitted light curves");
			((ToolStripItem)manageMyUnsubmittedLightCurvesToolStripMenuItem).add_Click((EventHandler)manageMyUnsubmittedLightCurvesToolStripMenuItem_Click);
			((ToolStripItem)myReportedLightCurvesToolStripMenuItem).set_Name("myReportedLightCurvesToolStripMenuItem");
			((ToolStripItem)myReportedLightCurvesToolStripMenuItem).set_Size(new Size(310, 22));
			((ToolStripItem)myReportedLightCurvesToolStripMenuItem).set_Text("My reported light curves (as a group)");
			((ToolStripItem)myReportedLightCurvesToolStripMenuItem).add_Click((EventHandler)myReportedLightCurvesToolStripMenuItem_Click);
			((ToolStripDropDownItem)withDisplayedLightCurveToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[12]
			{
				(ToolStripItem)addTextToolStripMenuItem,
				(ToolStripItem)addMarkerForThisUTTime1,
				(ToolStripItem)addMarkerForThisUTTime2,
				(ToolStripItem)toolStripSeparator9,
				(ToolStripItem)drawWithThickLinesToolStripMenuItem,
				(ToolStripItem)indicateMissedImagesToolStripMenuItem,
				(ToolStripItem)toolStripSeparator4,
				(ToolStripItem)writecsvFileForThisLightCurveToolStripMenuItem,
				(ToolStripItem)displayInAOTAToolStripMenuItem,
				(ToolStripItem)toolStripSeparator2,
				(ToolStripItem)copyToolStripMenuItem1,
				(ToolStripItem)saveImageToolStripMenuItem
			});
			((ToolStripItem)withDisplayedLightCurveToolStripMenuItem).set_Font(new Font("Segoe UI", 9f));
			((ToolStripItem)withDisplayedLightCurveToolStripMenuItem).set_ForeColor(Color.Blue);
			((ToolStripItem)withDisplayedLightCurveToolStripMenuItem).set_Name("withDisplayedLightCurveToolStripMenuItem");
			((ToolStripItem)withDisplayedLightCurveToolStripMenuItem).set_Size(new Size(171, 20));
			((ToolStripItem)withDisplayedLightCurveToolStripMenuItem).set_Text("With displayed light curve...  ");
			((ToolStripItem)addTextToolStripMenuItem).set_BackColor(Color.Lavender);
			((ToolStripDropDownItem)addTextToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[1] { (ToolStripItem)HeaderText });
			((ToolStripItem)addTextToolStripMenuItem).set_Font(new Font("Segoe UI", 9f, FontStyle.Bold));
			((ToolStripItem)addTextToolStripMenuItem).set_ForeColor(Color.DarkGreen);
			((ToolStripItem)addTextToolStripMenuItem).set_Name("addTextToolStripMenuItem");
			((ToolStripItem)addTextToolStripMenuItem).set_Size(new Size(243, 22));
			((ToolStripItem)addTextToolStripMenuItem).set_Text("Add this text to plot Header");
			((ToolStripItem)addTextToolStripMenuItem).add_Click((EventHandler)addTextToolStripMenuItem_Click);
			((ToolStripItem)HeaderText).set_BackColor(Color.Lavender);
			((ToolStripItem)HeaderText).set_Font(new Font("Segoe UI", 9f, FontStyle.Bold));
			((ToolStripItem)HeaderText).set_ForeColor(Color.DarkGreen);
			((ToolStripItem)HeaderText).set_Name("HeaderText");
			((ToolStripItem)HeaderText).set_Size(new Size(100, 23));
			((ToolStripItem)HeaderText).set_Text("#1");
			((ToolStripItem)addMarkerForThisUTTime1).set_BackColor(Color.FromArgb(230, 248, 230));
			((ToolStripDropDownItem)addMarkerForThisUTTime1).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[1] { (ToolStripItem)toolStripTextBox_UT1 });
			((ToolStripItem)addMarkerForThisUTTime1).set_Font(new Font("Segoe UI", 9f, FontStyle.Bold));
			((ToolStripItem)addMarkerForThisUTTime1).set_ForeColor(Color.Red);
			((ToolStripItem)addMarkerForThisUTTime1).set_Name("addMarkerForThisUTTime1");
			((ToolStripItem)addMarkerForThisUTTime1).set_Size(new Size(243, 22));
			((ToolStripItem)addMarkerForThisUTTime1).set_Text("Add marker for this UT time");
			((ToolStripItem)addMarkerForThisUTTime1).add_Click((EventHandler)addMarkerForThisUTTime1_Click);
			((ToolStripItem)toolStripTextBox_UT1).set_BackColor(Color.FromArgb(230, 248, 230));
			((ToolStripItem)toolStripTextBox_UT1).set_Font(new Font("Segoe UI", 9f, FontStyle.Bold));
			((ToolStripItem)toolStripTextBox_UT1).set_ForeColor(Color.Red);
			((ToolStripItem)toolStripTextBox_UT1).set_Name("toolStripTextBox_UT1");
			((ToolStripItem)toolStripTextBox_UT1).set_Size(new Size(70, 23));
			((ToolStripItem)toolStripTextBox_UT1).set_Text("00:00:00.00");
			((ToolStripItem)toolStripTextBox_UT1).set_ToolTipText("Enter as hh:mm:ss.ss");
			((ToolStripControlHost)toolStripTextBox_UT1).add_Leave((EventHandler)toolStripTextBox_UT1_Leave);
			((ToolStripControlHost)toolStripTextBox_UT1).add_KeyDown(new KeyEventHandler(toolStripTextBox_UT1_KeyDown));
			((ToolStripControlHost)toolStripTextBox_UT1).add_KeyPress(new KeyPressEventHandler(toolStripTextBox_UT1_KeyPress));
			((ToolStripItem)toolStripTextBox_UT1).add_TextChanged((EventHandler)toolStripTextBox_UT_TextChanged);
			((ToolStripItem)addMarkerForThisUTTime2).set_BackColor(Color.AntiqueWhite);
			((ToolStripDropDownItem)addMarkerForThisUTTime2).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[1] { (ToolStripItem)toolStripTextBox_UT2 });
			((ToolStripItem)addMarkerForThisUTTime2).set_Font(new Font("Segoe UI", 9f, FontStyle.Bold));
			((ToolStripItem)addMarkerForThisUTTime2).set_ForeColor(Color.DarkViolet);
			((ToolStripItem)addMarkerForThisUTTime2).set_Name("addMarkerForThisUTTime2");
			((ToolStripItem)addMarkerForThisUTTime2).set_Size(new Size(243, 22));
			((ToolStripItem)addMarkerForThisUTTime2).set_Text("Add marker for this UT time");
			((ToolStripItem)addMarkerForThisUTTime2).add_Click((EventHandler)addMarkerForThisUTTime2_Click);
			((ToolStripItem)toolStripTextBox_UT2).set_BackColor(Color.AntiqueWhite);
			((ToolStripItem)toolStripTextBox_UT2).set_Font(new Font("Segoe UI", 9f, FontStyle.Bold));
			((ToolStripItem)toolStripTextBox_UT2).set_ForeColor(Color.DarkViolet);
			((ToolStripItem)toolStripTextBox_UT2).set_Name("toolStripTextBox_UT2");
			((ToolStripItem)toolStripTextBox_UT2).set_Size(new Size(100, 23));
			((ToolStripItem)toolStripTextBox_UT2).set_Text("00:00:00.00");
			((ToolStripControlHost)toolStripTextBox_UT2).add_Leave((EventHandler)toolStripTextBox_UT2_Leave);
			((ToolStripControlHost)toolStripTextBox_UT2).add_KeyDown(new KeyEventHandler(toolStripTextBox_UT2_KeyDown));
			((ToolStripControlHost)toolStripTextBox_UT2).add_KeyPress(new KeyPressEventHandler(toolStripTextBox_UT2_KeyPress));
			((ToolStripItem)toolStripSeparator9).set_Name("toolStripSeparator9");
			((ToolStripItem)toolStripSeparator9).set_Size(new Size(240, 6));
			((ToolStripItem)drawWithThickLinesToolStripMenuItem).set_Font(new Font("Segoe UI", 9f, FontStyle.Bold));
			((ToolStripItem)drawWithThickLinesToolStripMenuItem).set_ForeColor(Color.Blue);
			((ToolStripItem)drawWithThickLinesToolStripMenuItem).set_Name("drawWithThickLinesToolStripMenuItem");
			((ToolStripItem)drawWithThickLinesToolStripMenuItem).set_Size(new Size(243, 22));
			((ToolStripItem)drawWithThickLinesToolStripMenuItem).set_Text("Draw curve with Thick lines");
			((ToolStripItem)drawWithThickLinesToolStripMenuItem).add_Click((EventHandler)drawWithThickLinesToolStripMenuItem_Click);
			((ToolStripItem)indicateMissedImagesToolStripMenuItem).set_Font(new Font("Segoe UI", 9f, FontStyle.Bold));
			((ToolStripItem)indicateMissedImagesToolStripMenuItem).set_ForeColor(Color.Magenta);
			((ToolStripItem)indicateMissedImagesToolStripMenuItem).set_Name("indicateMissedImagesToolStripMenuItem");
			((ToolStripItem)indicateMissedImagesToolStripMenuItem).set_Size(new Size(243, 22));
			((ToolStripItem)indicateMissedImagesToolStripMenuItem).set_Text("Indicate missed images");
			((ToolStripItem)indicateMissedImagesToolStripMenuItem).add_Click((EventHandler)indicateMissedImagesToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator4).set_Name("toolStripSeparator4");
			((ToolStripItem)toolStripSeparator4).set_Size(new Size(240, 6));
			((ToolStripItem)writecsvFileForThisLightCurveToolStripMenuItem).set_Name("writecsvFileForThisLightCurveToolStripMenuItem");
			((ToolStripItem)writecsvFileForThisLightCurveToolStripMenuItem).set_Size(new Size(243, 22));
			((ToolStripItem)writecsvFileForThisLightCurveToolStripMenuItem).set_Text("Write .csv file for this light curve");
			((ToolStripItem)writecsvFileForThisLightCurveToolStripMenuItem).add_Click((EventHandler)writecsvFileForThisLightCurveToolStripMenuItem_Click);
			((ToolStripItem)displayInAOTAToolStripMenuItem).set_Name("displayInAOTAToolStripMenuItem");
			((ToolStripItem)displayInAOTAToolStripMenuItem).set_Size(new Size(243, 22));
			((ToolStripItem)displayInAOTAToolStripMenuItem).set_Text("Display light curve in AOTA");
			((ToolStripItem)displayInAOTAToolStripMenuItem).add_Click((EventHandler)displayInAOTAToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator2).set_Name("toolStripSeparator2");
			((ToolStripItem)toolStripSeparator2).set_Size(new Size(240, 6));
			((ToolStripItem)copyToolStripMenuItem1).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem1).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyToolStripMenuItem1).set_Name("copyToolStripMenuItem1");
			((ToolStripItem)copyToolStripMenuItem1).set_Size(new Size(243, 22));
			((ToolStripItem)copyToolStripMenuItem1).set_Text("Copy image");
			((ToolStripItem)copyToolStripMenuItem1).add_Click((EventHandler)copyToolStripMenuItem1_Click);
			((ToolStripItem)saveImageToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveImageToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveImageToolStripMenuItem).set_Name("saveImageToolStripMenuItem");
			((ToolStripItem)saveImageToolStripMenuItem).set_Size(new Size(243, 22));
			((ToolStripItem)saveImageToolStripMenuItem).set_Text("Save image");
			((ToolStripItem)saveImageToolStripMenuItem).add_Click((EventHandler)saveImageToolStripMenuItem_Click);
			((ToolStripDropDownItem)adminFunctionsToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[21]
			{
				(ToolStripItem)toolStripSeparator5,
				(ToolStripItem)rereadLightCurvesToolStripMenuItem,
				(ToolStripItem)toolStripSeparator1,
				(ToolStripItem)submittedLightCurvesToolStripMenuItem,
				(ToolStripItem)rereadSubmittedLightCurvesToolStripMenuItem,
				(ToolStripItem)viewLightCurvesMarkedForDeletiondelinitToolStripMenuItem,
				(ToolStripItem)deleteDelFilesToolStripMenuItem,
				(ToolStripItem)view0LightCurvesMarkedForReviewreviewToolStripMenuItem,
				(ToolStripItem)removeReviewStatusToolStripMenuItem,
				(ToolStripItem)toolStripSeparator6,
				(ToolStripItem)editToolStripMenuItem,
				(ToolStripItem)editALightCurveMainFileToolStripMenuItem,
				(ToolStripItem)editALightCurveSubmittedFileToolStripMenuItem,
				(ToolStripItem)toolStripSeparator7,
				(ToolStripItem)mergeSubmittedWithMainToolStripMenuItem,
				(ToolStripItem)listSubmittedLightCurvesAlreadyInTheMainFileToolStripMenuItem,
				(ToolStripItem)listAndDeletesubmittedCurvesAlreadyInMainFileToolStripMenuItem,
				(ToolStripItem)mergeSubmittedLightCurveIntoTheMainFileToolStripMenuItem,
				(ToolStripItem)lightCurveStatsforVizieRToolStripMenuItem,
				(ToolStripItem)toolStripSeparator3,
				(ToolStripItem)manageEmbargoesToolStripMenuItem
			});
			((ToolStripItem)adminFunctionsToolStripMenuItem).set_Font(new Font("Segoe UI", 9f, FontStyle.Bold));
			((ToolStripItem)adminFunctionsToolStripMenuItem).set_ForeColor(Color.DarkRed);
			((ToolStripItem)adminFunctionsToolStripMenuItem).set_Image((Image)Resources.AdministerTestControllers_8573);
			((ToolStripItem)adminFunctionsToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)adminFunctionsToolStripMenuItem).set_Name("adminFunctionsToolStripMenuItem");
			((ToolStripItem)adminFunctionsToolStripMenuItem).set_Size(new Size(126, 20));
			((ToolStripItem)adminFunctionsToolStripMenuItem).set_Text("Admin functions");
			((ToolStripItem)toolStripSeparator5).set_Name("toolStripSeparator5");
			((ToolStripItem)toolStripSeparator5).set_Size(new Size(353, 6));
			((ToolStripItem)rereadLightCurvesToolStripMenuItem).set_BackColor(Color.LightCyan);
			((ToolStripItem)rereadLightCurvesToolStripMenuItem).set_Font(new Font("Segoe UI", 10f, FontStyle.Bold));
			((ToolStripItem)rereadLightCurvesToolStripMenuItem).set_ForeColor(Color.Blue);
			((ToolStripItem)rereadLightCurvesToolStripMenuItem).set_Name("rereadLightCurvesToolStripMenuItem");
			((ToolStripItem)rereadLightCurvesToolStripMenuItem).set_Size(new Size(356, 24));
			((ToolStripItem)rereadLightCurvesToolStripMenuItem).set_Text("Re-read all light curves");
			((ToolStripItem)rereadLightCurvesToolStripMenuItem).add_Click((EventHandler)rereadLightCurvesToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator1).set_Name("toolStripSeparator1");
			((ToolStripItem)toolStripSeparator1).set_Size(new Size(353, 6));
			((ToolStripItem)submittedLightCurvesToolStripMenuItem).set_BackColor(Color.Wheat);
			((ToolStripItem)submittedLightCurvesToolStripMenuItem).set_Enabled(false);
			((ToolStripItem)submittedLightCurvesToolStripMenuItem).set_Font(new Font("Segoe UI", 10f, FontStyle.Bold));
			((ToolStripItem)submittedLightCurvesToolStripMenuItem).set_ForeColor(Color.DarkViolet);
			((ToolStripItem)submittedLightCurvesToolStripMenuItem).set_Name("submittedLightCurvesToolStripMenuItem");
			((ToolStripItem)submittedLightCurvesToolStripMenuItem).set_Size(new Size(356, 24));
			((ToolStripItem)submittedLightCurvesToolStripMenuItem).set_Text("Submitted light curves");
			((ToolStripItem)rereadSubmittedLightCurvesToolStripMenuItem).set_BackColor(Color.PapayaWhip);
			((ToolStripItem)rereadSubmittedLightCurvesToolStripMenuItem).set_ForeColor(Color.DarkViolet);
			((ToolStripItem)rereadSubmittedLightCurvesToolStripMenuItem).set_Name("rereadSubmittedLightCurvesToolStripMenuItem");
			((ToolStripItem)rereadSubmittedLightCurvesToolStripMenuItem).set_Size(new Size(356, 24));
			((ToolStripItem)rereadSubmittedLightCurvesToolStripMenuItem).set_Text(".... Re-read Submitted light curves");
			((ToolStripItem)rereadSubmittedLightCurvesToolStripMenuItem).add_Click((EventHandler)rereadSubmittedLightCurvesToolStripMenuItem_Click);
			((ToolStripItem)viewLightCurvesMarkedForDeletiondelinitToolStripMenuItem).set_BackColor(Color.PapayaWhip);
			((ToolStripItem)viewLightCurvesMarkedForDeletiondelinitToolStripMenuItem).set_Font(new Font("Segoe UI", 9f, FontStyle.Bold));
			((ToolStripItem)viewLightCurvesMarkedForDeletiondelinitToolStripMenuItem).set_ForeColor(Color.DarkBlue);
			((ToolStripItem)viewLightCurvesMarkedForDeletiondelinitToolStripMenuItem).set_Name("viewLightCurvesMarkedForDeletiondelinitToolStripMenuItem");
			((ToolStripItem)viewLightCurvesMarkedForDeletiondelinitToolStripMenuItem).set_Size(new Size(356, 24));
			((ToolStripItem)viewLightCurvesMarkedForDeletiondelinitToolStripMenuItem).set_Text(".... View 0 files marked for deletion  ( .del  &&  .init )");
			((ToolStripItem)viewLightCurvesMarkedForDeletiondelinitToolStripMenuItem).add_Click((EventHandler)viewLightCurvesMarkedForDeletiondelinitToolStripMenuItem_Click);
			((ToolStripItem)deleteDelFilesToolStripMenuItem).set_BackColor(Color.PapayaWhip);
			((ToolStripItem)deleteDelFilesToolStripMenuItem).set_ForeColor(Color.DarkBlue);
			((ToolStripItem)deleteDelFilesToolStripMenuItem).set_Name("deleteDelFilesToolStripMenuItem");
			((ToolStripItem)deleteDelFilesToolStripMenuItem).set_Size(new Size(356, 24));
			((ToolStripItem)deleteDelFilesToolStripMenuItem).set_Text("....... Delete '.del' && '.init' files");
			((ToolStripItem)deleteDelFilesToolStripMenuItem).add_Click((EventHandler)deleteDelFilesToolStripMenuItem_Click);
			((ToolStripItem)view0LightCurvesMarkedForReviewreviewToolStripMenuItem).set_BackColor(Color.PapayaWhip);
			((ToolStripItem)view0LightCurvesMarkedForReviewreviewToolStripMenuItem).set_Font(new Font("Segoe UI", 9f, FontStyle.Bold));
			((ToolStripItem)view0LightCurvesMarkedForReviewreviewToolStripMenuItem).set_ForeColor(Color.Blue);
			((ToolStripItem)view0LightCurvesMarkedForReviewreviewToolStripMenuItem).set_Name("view0LightCurvesMarkedForReviewreviewToolStripMenuItem");
			((ToolStripItem)view0LightCurvesMarkedForReviewreviewToolStripMenuItem).set_Size(new Size(356, 24));
			((ToolStripItem)view0LightCurvesMarkedForReviewreviewToolStripMenuItem).set_Text(".... View 0 files marked for review ( .review )");
			((ToolStripItem)view0LightCurvesMarkedForReviewreviewToolStripMenuItem).add_Click((EventHandler)view0LightCurvesMarkedForReviewToolStripMenuItem_Click);
			((ToolStripItem)removeReviewStatusToolStripMenuItem).set_BackColor(Color.PapayaWhip);
			((ToolStripItem)removeReviewStatusToolStripMenuItem).set_ForeColor(Color.Blue);
			((ToolStripItem)removeReviewStatusToolStripMenuItem).set_Name("removeReviewStatusToolStripMenuItem");
			((ToolStripItem)removeReviewStatusToolStripMenuItem).set_Size(new Size(356, 24));
			((ToolStripItem)removeReviewStatusToolStripMenuItem).set_Text("....... Remove '.review' status of files");
			((ToolStripItem)removeReviewStatusToolStripMenuItem).add_Click((EventHandler)removeReviewStatusToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator6).set_BackColor(SystemColors.Control);
			((ToolStripItem)toolStripSeparator6).set_Name("toolStripSeparator6");
			((ToolStripItem)toolStripSeparator6).set_Size(new Size(353, 6));
			((ToolStripItem)editToolStripMenuItem).set_BackColor(Color.PaleGoldenrod);
			((ToolStripItem)editToolStripMenuItem).set_Enabled(false);
			((ToolStripItem)editToolStripMenuItem).set_Font(new Font("Segoe UI", 10f, FontStyle.Bold));
			((ToolStripItem)editToolStripMenuItem).set_ForeColor(Color.DarkRed);
			((ToolStripItem)editToolStripMenuItem).set_Name("editToolStripMenuItem");
			((ToolStripItem)editToolStripMenuItem).set_Size(new Size(356, 24));
			((ToolStripItem)editToolStripMenuItem).set_Text("Edit");
			((ToolStripItem)editALightCurveMainFileToolStripMenuItem).set_BackColor(Color.LightGoldenrodYellow);
			((ToolStripItem)editALightCurveMainFileToolStripMenuItem).set_ForeColor(Color.DarkRed);
			((ToolStripItem)editALightCurveMainFileToolStripMenuItem).set_Name("editALightCurveMainFileToolStripMenuItem");
			((ToolStripItem)editALightCurveMainFileToolStripMenuItem).set_Size(new Size(356, 24));
			((ToolStripItem)editALightCurveMainFileToolStripMenuItem).set_Text(".... a light curve - main file");
			((ToolStripItem)editALightCurveMainFileToolStripMenuItem).add_Click((EventHandler)editALightCurveMainFileToolStripMenuItem_Click);
			((ToolStripItem)editALightCurveSubmittedFileToolStripMenuItem).set_BackColor(Color.LightGoldenrodYellow);
			((ToolStripItem)editALightCurveSubmittedFileToolStripMenuItem).set_ForeColor(Color.DarkRed);
			((ToolStripItem)editALightCurveSubmittedFileToolStripMenuItem).set_Name("editALightCurveSubmittedFileToolStripMenuItem");
			((ToolStripItem)editALightCurveSubmittedFileToolStripMenuItem).set_Size(new Size(356, 24));
			((ToolStripItem)editALightCurveSubmittedFileToolStripMenuItem).set_Text(".... 0 submitted light curves");
			((ToolStripItem)editALightCurveSubmittedFileToolStripMenuItem).add_Click((EventHandler)editALightCurveSubmittedFileToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator7).set_Name("toolStripSeparator7");
			((ToolStripItem)toolStripSeparator7).set_Size(new Size(353, 6));
			((ToolStripItem)mergeSubmittedWithMainToolStripMenuItem).set_BackColor(Color.LightGreen);
			((ToolStripItem)mergeSubmittedWithMainToolStripMenuItem).set_Enabled(false);
			((ToolStripItem)mergeSubmittedWithMainToolStripMenuItem).set_Font(new Font("Segoe UI", 10f, FontStyle.Bold));
			((ToolStripItem)mergeSubmittedWithMainToolStripMenuItem).set_ForeColor(Color.DarkGreen);
			((ToolStripItem)mergeSubmittedWithMainToolStripMenuItem).set_Name("mergeSubmittedWithMainToolStripMenuItem");
			((ToolStripItem)mergeSubmittedWithMainToolStripMenuItem).set_Size(new Size(356, 24));
			((ToolStripItem)mergeSubmittedWithMainToolStripMenuItem).set_Text("Merge submitted files with Main file");
			((ToolStripItem)listSubmittedLightCurvesAlreadyInTheMainFileToolStripMenuItem).set_BackColor(Color.Honeydew);
			((ToolStripItem)listSubmittedLightCurvesAlreadyInTheMainFileToolStripMenuItem).set_Font(new Font("Segoe UI", 9f, FontStyle.Bold));
			((ToolStripItem)listSubmittedLightCurvesAlreadyInTheMainFileToolStripMenuItem).set_ForeColor(Color.Red);
			((ToolStripItem)listSubmittedLightCurvesAlreadyInTheMainFileToolStripMenuItem).set_Image((Image)Resources.Find_5650);
			((ToolStripItem)listSubmittedLightCurvesAlreadyInTheMainFileToolStripMenuItem).set_Name("listSubmittedLightCurvesAlreadyInTheMainFileToolStripMenuItem");
			((ToolStripItem)listSubmittedLightCurvesAlreadyInTheMainFileToolStripMenuItem).set_Size(new Size(356, 24));
			((ToolStripItem)listSubmittedLightCurvesAlreadyInTheMainFileToolStripMenuItem).set_Text(".... List submitted curves already in the Main file");
			((ToolStripItem)listSubmittedLightCurvesAlreadyInTheMainFileToolStripMenuItem).add_Click((EventHandler)listSubmittedLightCurvesAlreadyInTheMainFileToolStripMenuItem_Click);
			((ToolStripItem)listAndDeletesubmittedCurvesAlreadyInMainFileToolStripMenuItem).set_BackColor(Color.Honeydew);
			((ToolStripItem)listAndDeletesubmittedCurvesAlreadyInMainFileToolStripMenuItem).set_Font(new Font("Segoe UI", 9f, FontStyle.Bold));
			((ToolStripItem)listAndDeletesubmittedCurvesAlreadyInMainFileToolStripMenuItem).set_ForeColor(Color.Red);
			((ToolStripItem)listAndDeletesubmittedCurvesAlreadyInMainFileToolStripMenuItem).set_Image((Image)Resources.Critical);
			((ToolStripItem)listAndDeletesubmittedCurvesAlreadyInMainFileToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)listAndDeletesubmittedCurvesAlreadyInMainFileToolStripMenuItem).set_Name("listAndDeletesubmittedCurvesAlreadyInMainFileToolStripMenuItem");
			((ToolStripItem)listAndDeletesubmittedCurvesAlreadyInMainFileToolStripMenuItem).set_Size(new Size(356, 24));
			((ToolStripItem)listAndDeletesubmittedCurvesAlreadyInMainFileToolStripMenuItem).set_Text(".... Delete submitted curves already in the Main file");
			((ToolStripItem)listAndDeletesubmittedCurvesAlreadyInMainFileToolStripMenuItem).add_Click((EventHandler)listAndDeletesubmittedCurvesAlreadyInMainFileToolStripMenuItem_Click);
			((ToolStripItem)mergeSubmittedLightCurveIntoTheMainFileToolStripMenuItem).set_BackColor(Color.Honeydew);
			((ToolStripItem)mergeSubmittedLightCurveIntoTheMainFileToolStripMenuItem).set_Font(new Font("Segoe UI", 10f, FontStyle.Bold));
			((ToolStripItem)mergeSubmittedLightCurveIntoTheMainFileToolStripMenuItem).set_ForeColor(Color.DarkGreen);
			((ToolStripItem)mergeSubmittedLightCurveIntoTheMainFileToolStripMenuItem).set_Image((Image)Resources.Merge_13256);
			((ToolStripItem)mergeSubmittedLightCurveIntoTheMainFileToolStripMenuItem).set_Name("mergeSubmittedLightCurveIntoTheMainFileToolStripMenuItem");
			((ToolStripItem)mergeSubmittedLightCurveIntoTheMainFileToolStripMenuItem).set_Size(new Size(356, 24));
			((ToolStripItem)mergeSubmittedLightCurveIntoTheMainFileToolStripMenuItem).set_Text(".... Merge ");
			((ToolStripItem)mergeSubmittedLightCurveIntoTheMainFileToolStripMenuItem).add_Click((EventHandler)mergeSubmittedLightCurveIntoTheMainFileToolStripMenuItem_Click);
			((ToolStripItem)lightCurveStatsforVizieRToolStripMenuItem).set_BackColor(Color.Honeydew);
			((ToolStripItem)lightCurveStatsforVizieRToolStripMenuItem).set_Font(new Font("Segoe UI", 9f, FontStyle.Bold));
			((ToolStripItem)lightCurveStatsforVizieRToolStripMenuItem).set_ForeColor(Color.SaddleBrown);
			((ToolStripItem)lightCurveStatsforVizieRToolStripMenuItem).set_Name("lightCurveStatsforVizieRToolStripMenuItem");
			((ToolStripItem)lightCurveStatsforVizieRToolStripMenuItem).set_Size(new Size(356, 24));
			((ToolStripItem)lightCurveStatsforVizieRToolStripMenuItem).set_Text(".... Light curve statistics (for VizieR)");
			((ToolStripItem)lightCurveStatsforVizieRToolStripMenuItem).add_Click((EventHandler)lightCurveStatsforVizieRToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator3).set_Name("toolStripSeparator3");
			((ToolStripItem)toolStripSeparator3).set_Size(new Size(353, 6));
			((ToolStripItem)manageEmbargoesToolStripMenuItem).set_BackColor(Color.LavenderBlush);
			((ToolStripItem)manageEmbargoesToolStripMenuItem).set_Font(new Font("Segoe UI Semibold", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((ToolStripItem)manageEmbargoesToolStripMenuItem).set_ForeColor(Color.DarkMagenta);
			((ToolStripItem)manageEmbargoesToolStripMenuItem).set_Name("manageEmbargoesToolStripMenuItem");
			((ToolStripItem)manageEmbargoesToolStripMenuItem).set_Size(new Size(356, 24));
			((ToolStripItem)manageEmbargoesToolStripMenuItem).set_Text("Manage embargoes");
			((ToolStripItem)manageEmbargoesToolStripMenuItem).add_Click((EventHandler)manageEmbargoesToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(69, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("Help   ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_ImageAlign(ContentAlignment.MiddleLeft);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(54, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("Exit");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((ToolStripItem)rightClickToSetMeanLightToolStripMenuItem).set_ForeColor(Color.Red);
			((ToolStripItem)rightClickToSetMeanLightToolStripMenuItem).set_Name("rightClickToSetMeanLightToolStripMenuItem");
			((ToolStripItem)rightClickToSetMeanLightToolStripMenuItem).set_Size(new Size(193, 20));
			((ToolStripItem)rightClickToSetMeanLightToolStripMenuItem).set_Text("Right click to set mean light level");
			((ToolStripItem)rightClickToSetMeanLightToolStripMenuItem).set_ToolTipText("Right click near top of plot to clear the mean light level line");
			((Control)txtStar).set_BackColor(Color.MintCream);
			((Control)txtStar).set_Font(new Font("Consolas", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtStar).set_Location(new Point(150, 17));
			((TextBoxBase)txtStar).set_Multiline(true);
			((Control)txtStar).set_Name("txtStar");
			((TextBoxBase)txtStar).set_ReadOnly(true);
			((Control)txtStar).set_Size(new Size(114, 159));
			((Control)txtStar).set_TabIndex(4);
			((Control)txtCirc).set_BackColor(Color.MintCream);
			((Control)txtCirc).set_Font(new Font("Consolas", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtCirc).set_Location(new Point(523, 17));
			((TextBoxBase)txtCirc).set_Multiline(true);
			((Control)txtCirc).set_Name("txtCirc");
			((TextBoxBase)txtCirc).set_ReadOnly(true);
			((Control)txtCirc).set_Size(new Size(227, 159));
			((Control)txtCirc).set_TabIndex(5);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(150, 1));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(30, 13));
			((Control)label1).set_TabIndex(6);
			((Control)label1).set_Text("Star");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label2).set_Location(new Point(523, 1));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(89, 13));
			((Control)label2).set_TabIndex(7);
			((Control)label2).set_Text("Circumstances");
			((Control)cmd_x15).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmd_x15).set_Location(new Point(104, 26));
			((Control)cmd_x15).set_Name("cmd_x15");
			((Control)cmd_x15).set_Size(new Size(27, 20));
			((Control)cmd_x15).set_TabIndex(5);
			((Control)cmd_x15).set_Text("15");
			((ButtonBase)cmd_x15).set_UseVisualStyleBackColor(true);
			((Control)cmd_x15).add_Click((EventHandler)cmd_x15_Click);
			((Control)cmd_x10).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmd_x10).set_Location(new Point(71, 26));
			((Control)cmd_x10).set_Name("cmd_x10");
			((Control)cmd_x10).set_Size(new Size(27, 20));
			((Control)cmd_x10).set_TabIndex(4);
			((Control)cmd_x10).set_Text("10");
			((ButtonBase)cmd_x10).set_UseVisualStyleBackColor(true);
			((Control)cmd_x10).add_Click((EventHandler)cmd_x10_Click);
			((Control)cmd_x5).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmd_x5).set_Location(new Point(38, 26));
			((Control)cmd_x5).set_Name("cmd_x5");
			((Control)cmd_x5).set_Size(new Size(27, 20));
			((Control)cmd_x5).set_TabIndex(3);
			((Control)cmd_x5).set_Text("5");
			((ButtonBase)cmd_x5).set_UseVisualStyleBackColor(true);
			((Control)cmd_x5).add_Click((EventHandler)cmd_x5_Click);
			((Control)cmd_x1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmd_x1).set_Location(new Point(5, 26));
			((Control)cmd_x1).set_Name("cmd_x1");
			((Control)cmd_x1).set_Size(new Size(27, 20));
			((Control)cmd_x1).set_TabIndex(2);
			((Control)cmd_x1).set_Text("1");
			((ButtonBase)cmd_x1).set_UseVisualStyleBackColor(true);
			((Control)cmd_x1).add_Click((EventHandler)cmd_x1_Click);
			updnScale.set_DecimalPlaces(2);
			((Control)updnScale).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnScale.set_Increment(new decimal(new int[4] { 2, 0, 0, 65536 }));
			((Control)updnScale).set_Location(new Point(33, 3));
			updnScale.set_Minimum(new decimal(new int[4] { 2, 0, 0, 65536 }));
			((Control)updnScale).set_Name("updnScale");
			((Control)updnScale).set_Size(new Size(52, 20));
			((Control)updnScale).set_TabIndex(1);
			((UpDownBase)updnScale).set_TextAlign((HorizontalAlignment)2);
			updnScale.set_Value(new decimal(new int[4] { 5, 0, 0, 0 }));
			updnScale.add_ValueChanged((EventHandler)updnScale_ValueChanged);
			((Control)txtObs).set_BackColor(Color.MintCream);
			((Control)txtObs).set_Font(new Font("Consolas", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtObs).set_Location(new Point(279, 17));
			((TextBoxBase)txtObs).set_Multiline(true);
			((Control)txtObs).set_Name("txtObs");
			((TextBoxBase)txtObs).set_ReadOnly(true);
			((Control)txtObs).set_Size(new Size(229, 159));
			((Control)txtObs).set_TabIndex(9);
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label3).set_Location(new Point(279, 1));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(114, 13));
			((Control)label3).set_TabIndex(10);
			((Control)label3).set_Text("Date and Observer");
			((Control)panel1).set_BackColor(Color.PaleTurquoise);
			panel1.set_BorderStyle((BorderStyle)2);
			((Control)panel1).get_Controls().Add((Control)(object)lblVerticalScale);
			((Control)panel1).get_Controls().Add((Control)(object)SliderVerticalScale);
			((Control)panel1).get_Controls().Add((Control)(object)cmdPlotAll_Submitted);
			((Control)panel1).get_Controls().Add((Control)(object)lblFormOpacity);
			((Control)panel1).get_Controls().Add((Control)(object)SliderOpacity);
			((Control)panel1).get_Controls().Add((Control)(object)lblPlotHeight);
			((Control)panel1).get_Controls().Add((Control)(object)lblSubmitted);
			((Control)panel1).get_Controls().Add((Control)(object)lstSubmitted);
			((Control)panel1).get_Controls().Add((Control)(object)panel3);
			((Control)panel1).get_Controls().Add((Control)(object)label6);
			((Control)panel1).get_Controls().Add((Control)(object)pnlStarByDate);
			((Control)panel1).get_Controls().Add((Control)(object)lblLightCurves);
			((Control)panel1).get_Controls().Add((Control)(object)label3);
			((Control)panel1).get_Controls().Add((Control)(object)txtObs);
			((Control)panel1).get_Controls().Add((Control)(object)label2);
			((Control)panel1).get_Controls().Add((Control)(object)label1);
			((Control)panel1).get_Controls().Add((Control)(object)txtCirc);
			((Control)panel1).get_Controls().Add((Control)(object)txtStar);
			((Control)panel1).get_Controls().Add((Control)(object)SliderHeight);
			((Control)panel1).set_Location(new Point(4, 439));
			((Control)panel1).set_Name("panel1");
			((Control)panel1).set_Size(new Size(1129, 184));
			((Control)panel1).set_TabIndex(11);
			((Control)lblVerticalScale).set_AutoSize(true);
			((Control)lblVerticalScale).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblVerticalScale).set_Location(new Point(8, 82));
			((Control)lblVerticalScale).set_Name("lblVerticalScale");
			((Control)lblVerticalScale).set_Size(new Size(121, 13));
			((Control)lblVerticalScale).set_TabIndex(49);
			((Control)lblVerticalScale).set_Text("Vertical scale  100%");
			((Control)SliderVerticalScale).set_AutoSize(false);
			((Control)SliderVerticalScale).set_BackColor(Color.Firebrick);
			SliderVerticalScale.set_LargeChange(1);
			((Control)SliderVerticalScale).set_Location(new Point(1, 96));
			SliderVerticalScale.set_Maximum(200);
			SliderVerticalScale.set_Minimum(40);
			((Control)SliderVerticalScale).set_Name("SliderVerticalScale");
			((Control)SliderVerticalScale).set_Size(new Size(135, 17));
			((Control)SliderVerticalScale).set_TabIndex(48);
			SliderVerticalScale.set_TickFrequency(10);
			SliderVerticalScale.set_Value(150);
			SliderVerticalScale.add_ValueChanged((EventHandler)SliderVerticalScale_ValueChanged);
			((Control)cmdPlotAll_Submitted).set_BackColor(Color.Chartreuse);
			((Control)cmdPlotAll_Submitted).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdPlotAll_Submitted).set_Location(new Point(1009, 1));
			((Control)cmdPlotAll_Submitted).set_Name("cmdPlotAll_Submitted");
			((Control)cmdPlotAll_Submitted).set_Size(new Size(116, 22));
			((Control)cmdPlotAll_Submitted).set_TabIndex(46);
			((Control)cmdPlotAll_Submitted).set_Text("Plot All Submitted");
			((ButtonBase)cmdPlotAll_Submitted).set_UseVisualStyleBackColor(false);
			((Control)cmdPlotAll_Submitted).add_Click((EventHandler)cmdPlotAll_Submitted_Click);
			((Control)lblFormOpacity).set_AutoSize(true);
			((Control)lblFormOpacity).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblFormOpacity).set_Location(new Point(28, 148));
			((Control)lblFormOpacity).set_Name("lblFormOpacity");
			((Control)lblFormOpacity).set_Size(new Size(78, 13));
			((Control)lblFormOpacity).set_TabIndex(45);
			((Control)lblFormOpacity).set_Text("Form opacity");
			((Control)SliderOpacity).set_AutoSize(false);
			((Control)SliderOpacity).set_BackColor(Color.Firebrick);
			((Control)SliderOpacity).set_Location(new Point(1, 162));
			SliderOpacity.set_Maximum(200);
			SliderOpacity.set_Minimum(80);
			((Control)SliderOpacity).set_Name("SliderOpacity");
			((Control)SliderOpacity).set_Size(new Size(135, 17));
			SliderOpacity.set_SmallChange(5);
			((Control)SliderOpacity).set_TabIndex(44);
			SliderOpacity.set_TickFrequency(10);
			SliderOpacity.set_Value(200);
			SliderOpacity.add_ValueChanged((EventHandler)sliderOpacity_ValueChanged);
			((Control)lblPlotHeight).set_AutoSize(true);
			((Control)lblPlotHeight).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblPlotHeight).set_Location(new Point(8, 115));
			((Control)lblPlotHeight).set_Name("lblPlotHeight");
			((Control)lblPlotHeight).set_Size(new Size(107, 13));
			((Control)lblPlotHeight).set_TabIndex(43);
			((Control)lblPlotHeight).set_Text("Plot height  100%");
			((Control)lblSubmitted).set_AutoSize(true);
			((Control)lblSubmitted).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblSubmitted).set_Location(new Point(765, 6));
			((Control)lblSubmitted).set_Name("lblSubmitted");
			((Control)lblSubmitted).set_Size(new Size(142, 13));
			((Control)lblSubmitted).set_TabIndex(41);
			((Control)lblSubmitted).set_Text("0 submitted light curves");
			((Control)lstSubmitted).set_BackColor(Color.OldLace);
			((Control)lstSubmitted).set_Font(new Font("Consolas", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstSubmitted).set_FormattingEnabled(true);
			((Control)lstSubmitted).set_Location(new Point(765, 25));
			((Control)lstSubmitted).set_Name("lstSubmitted");
			((Control)lstSubmitted).set_Size(new Size(360, 147));
			((Control)lstSubmitted).set_TabIndex(40);
			lstSubmitted.add_SelectedIndexChanged((EventHandler)lstSubmitted_SelectedIndexChanged);
			((Control)panel3).set_BackColor(Color.PeachPuff);
			panel3.set_BorderStyle((BorderStyle)1);
			((Control)panel3).get_Controls().Add((Control)(object)panel10);
			((Control)panel3).get_Controls().Add((Control)(object)cmd_x15);
			((Control)panel3).get_Controls().Add((Control)(object)cmd_x10);
			((Control)panel3).get_Controls().Add((Control)(object)cmd_x5);
			((Control)panel3).get_Controls().Add((Control)(object)cmd_x1);
			((Control)panel3).set_Location(new Point(0, 30));
			((Control)panel3).set_Name("panel3");
			((Control)panel3).set_Size(new Size(135, 51));
			((Control)panel3).set_TabIndex(21);
			((Control)panel10).set_BackColor(Color.AntiqueWhite);
			((Control)panel10).get_Controls().Add((Control)(object)updnScale);
			((Control)panel10).get_Controls().Add((Control)(object)label13);
			((Control)panel10).get_Controls().Add((Control)(object)chkFineScroll);
			((Control)panel10).set_Location(new Point(-1, -1));
			((Control)panel10).set_Name("panel10");
			((Control)panel10).set_Size(new Size(135, 26));
			((Control)panel10).set_TabIndex(8);
			((Control)label13).set_AutoSize(true);
			((Control)label13).set_Font(new Font("Microsoft Sans Serif", 6.2f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label13).set_Location(new Point(0, 1));
			((Control)label13).set_Name("label13");
			((Control)label13).set_Size(new Size(32, 24));
			((Control)label13).set_TabIndex(6);
			((Control)label13).set_Text(" 0.2 to \r\n100");
			label13.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)chkFineScroll).set_AutoSize(true);
			((Control)chkFineScroll).set_Location(new Point(92, 5));
			((Control)chkFineScroll).set_Name("chkFineScroll");
			((Control)chkFineScroll).set_Size(new Size(46, 17));
			((Control)chkFineScroll).set_TabIndex(7);
			((Control)chkFineScroll).set_Text("Fine");
			((ButtonBase)chkFineScroll).set_UseVisualStyleBackColor(true);
			chkFineScroll.add_CheckedChanged((EventHandler)chkFineScroll_CheckedChanged);
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label6).set_Location(new Point(18, 15));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(98, 13));
			((Control)label6).set_TabIndex(20);
			((Control)label6).set_Text("Horizontal scale");
			((Control)pnlStarByDate).get_Controls().Add((Control)(object)lblStarCat);
			((Control)pnlStarByDate).get_Controls().Add((Control)(object)lstDates);
			((Control)pnlStarByDate).set_Location(new Point(428, 48));
			((Control)pnlStarByDate).set_Name("pnlStarByDate");
			((Control)pnlStarByDate).set_Size(new Size(126, 171));
			((Control)pnlStarByDate).set_TabIndex(18);
			((Control)pnlStarByDate).set_Visible(false);
			((Control)lblStarCat).set_AutoSize(true);
			((Control)lblStarCat).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblStarCat).set_Location(new Point(6, 7));
			((Control)lblStarCat).set_Name("lblStarCat");
			((Control)lblStarCat).set_Size(new Size(30, 13));
			((Control)lblStarCat).set_TabIndex(19);
			((Control)lblStarCat).set_Text("Star");
			((Control)lstDates).set_Font(new Font("Consolas", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstDates).set_FormattingEnabled(true);
			((Control)lstDates).set_Location(new Point(7, 23));
			((Control)lstDates).set_Name("lstDates");
			((Control)lstDates).set_Size(new Size(111, 147));
			((Control)lstDates).set_TabIndex(17);
			lstDates.add_SelectedIndexChanged((EventHandler)lstDates_SelectedIndexChanged);
			((Control)lblLightCurves).set_AutoSize(true);
			((Control)lblLightCurves).set_BackColor(Color.Cornsilk);
			((Control)lblLightCurves).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblLightCurves).set_Location(new Point(0, 0));
			((Control)lblLightCurves).set_Name("lblLightCurves");
			((Control)lblLightCurves).set_Size(new Size(100, 13));
			((Control)lblLightCurves).set_TabIndex(19);
			((Control)lblLightCurves).set_Text("# light curves: 0");
			((Control)SliderHeight).set_AutoSize(false);
			((Control)SliderHeight).set_BackColor(Color.Firebrick);
			SliderHeight.set_LargeChange(1);
			((Control)SliderHeight).set_Location(new Point(1, 129));
			SliderHeight.set_Maximum(200);
			SliderHeight.set_Minimum(40);
			((Control)SliderHeight).set_Name("SliderHeight");
			((Control)SliderHeight).set_Size(new Size(135, 17));
			((Control)SliderHeight).set_TabIndex(28);
			SliderHeight.set_TickFrequency(10);
			SliderHeight.set_Value(200);
			SliderHeight.add_ValueChanged((EventHandler)SliderHeight_ValueChanged);
			pnlSelectStar.set_BorderStyle((BorderStyle)1);
			((Control)pnlSelectStar).get_Controls().Add((Control)(object)panel8);
			((Control)pnlSelectStar).get_Controls().Add((Control)(object)panel2);
			((Control)pnlSelectStar).get_Controls().Add((Control)(object)panel7);
			((Control)pnlSelectStar).get_Controls().Add((Control)(object)panel6);
			((Control)pnlSelectStar).get_Controls().Add((Control)(object)panel5);
			((Control)pnlSelectStar).get_Controls().Add((Control)(object)panel4);
			((Control)pnlSelectStar).set_Location(new Point(916, 25));
			((Control)pnlSelectStar).set_Name("pnlSelectStar");
			((Control)pnlSelectStar).set_Size(new Size(228, 409));
			((Control)pnlSelectStar).set_TabIndex(16);
			((Control)panel8).set_BackColor(Color.LightCyan);
			panel8.set_BorderStyle((BorderStyle)2);
			((Control)panel8).get_Controls().Add((Control)(object)cmdPlotAll_Name);
			((Control)panel8).get_Controls().Add((Control)(object)cmdAstNameDec);
			((Control)panel8).get_Controls().Add((Control)(object)cmdAstNameInc);
			((Control)panel8).get_Controls().Add((Control)(object)lblAstNamesCount);
			((Control)panel8).get_Controls().Add((Control)(object)txtAsteroidName);
			((Control)panel8).get_Controls().Add((Control)(object)label11);
			((Control)panel8).get_Controls().Add((Control)(object)cmbAsteroidName);
			((Control)panel8).set_Location(new Point(3, 122));
			((Control)panel8).set_Name("panel8");
			((Control)panel8).set_Size(new Size(212, 71));
			((Control)panel8).set_TabIndex(25);
			((Control)cmdPlotAll_Name).set_BackColor(Color.Chartreuse);
			((Control)cmdPlotAll_Name).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdPlotAll_Name).set_Location(new Point(127, 17));
			((Control)cmdPlotAll_Name).set_Name("cmdPlotAll_Name");
			((Control)cmdPlotAll_Name).set_Size(new Size(55, 22));
			((Control)cmdPlotAll_Name).set_TabIndex(26);
			((Control)cmdPlotAll_Name).set_Text("Plot All");
			((ButtonBase)cmdPlotAll_Name).set_UseVisualStyleBackColor(false);
			((Control)cmdPlotAll_Name).add_Click((EventHandler)cmbPlotAll_Name_Click);
			((ButtonBase)cmdAstNameDec).set_Image((Image)Resources.arrow_Down_16xLG);
			((Control)cmdAstNameDec).set_Location(new Point(186, 46));
			((Control)cmdAstNameDec).set_Name("cmdAstNameDec");
			((Control)cmdAstNameDec).set_Size(new Size(17, 17));
			((Control)cmdAstNameDec).set_TabIndex(25);
			((ButtonBase)cmdAstNameDec).set_UseVisualStyleBackColor(true);
			((Control)cmdAstNameDec).add_Click((EventHandler)cmdAstNameDec_Click);
			((Control)cmdAstNameInc).set_BackColor(Color.LightGreen);
			((ButtonBase)cmdAstNameInc).set_Image((Image)Resources.arrow_Up_16xLG);
			((Control)cmdAstNameInc).set_Location(new Point(186, 28));
			((Control)cmdAstNameInc).set_Name("cmdAstNameInc");
			((Control)cmdAstNameInc).set_Size(new Size(17, 17));
			((Control)cmdAstNameInc).set_TabIndex(24);
			((ButtonBase)cmdAstNameInc).set_UseVisualStyleBackColor(false);
			((Control)cmdAstNameInc).add_Click((EventHandler)cmdAstNameInc_Click);
			((Control)lblAstNamesCount).set_AutoSize(true);
			((Control)lblAstNamesCount).set_Location(new Point(2, 21));
			((Control)lblAstNamesCount).set_Name("lblAstNamesCount");
			((Control)lblAstNamesCount).set_Size(new Size(14, 13));
			((Control)lblAstNamesCount).set_TabIndex(21);
			((Control)lblAstNamesCount).set_Text("#");
			lblAstNamesCount.set_TextAlign(ContentAlignment.MiddleRight);
			((Control)txtAsteroidName).set_Location(new Point(19, 18));
			((Control)txtAsteroidName).set_Name("txtAsteroidName");
			((Control)txtAsteroidName).set_Size(new Size(65, 20));
			((Control)txtAsteroidName).set_TabIndex(20);
			((Control)txtAsteroidName).add_TextChanged((EventHandler)txtAsteroidName_TextChanged);
			((Control)txtAsteroidName).add_KeyDown(new KeyEventHandler(txtAsteroidName_KeyDown));
			((Control)txtAsteroidName).add_Leave((EventHandler)txtAsteroidName_Leave);
			((Control)label11).set_AutoSize(true);
			((Control)label11).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label11).set_Location(new Point(21, 2));
			((Control)label11).set_Name("label11");
			((Control)label11).set_Size(new Size(156, 13));
			((Control)label11).set_TabIndex(18);
			((Control)label11).set_Text("Asteroids - select by name");
			cmbAsteroidName.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbAsteroidName).set_Font(new Font("Consolas", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbAsteroidName).set_FormattingEnabled(true);
			((Control)cmbAsteroidName).set_Location(new Point(5, 42));
			((Control)cmbAsteroidName).set_Name("cmbAsteroidName");
			((Control)cmbAsteroidName).set_Size(new Size(177, 21));
			((Control)cmbAsteroidName).set_TabIndex(16);
			cmbAsteroidName.add_SelectedIndexChanged((EventHandler)cmbAsteroidName_SelectedIndexChanged);
			((Control)panel2).set_BackColor(Color.LightYellow);
			panel2.set_BorderStyle((BorderStyle)2);
			((Control)panel2).get_Controls().Add((Control)(object)cmdAstDateDec);
			((Control)panel2).get_Controls().Add((Control)(object)cmdAstDateInc);
			((Control)panel2).get_Controls().Add((Control)(object)label5);
			((Control)panel2).get_Controls().Add((Control)(object)cmbByDate);
			((Control)panel2).set_Location(new Point(3, 195));
			((Control)panel2).set_Name("panel2");
			((Control)panel2).set_Size(new Size(211, 47));
			((Control)panel2).set_TabIndex(24);
			((ButtonBase)cmdAstDateDec).set_Image((Image)Resources.arrow_Down_16xLG);
			((Control)cmdAstDateDec).set_Location(new Point(186, 24));
			((Control)cmdAstDateDec).set_Name("cmdAstDateDec");
			((Control)cmdAstDateDec).set_Size(new Size(17, 17));
			((Control)cmdAstDateDec).set_TabIndex(27);
			((ButtonBase)cmdAstDateDec).set_UseVisualStyleBackColor(true);
			((Control)cmdAstDateDec).add_Click((EventHandler)cmdAstDateDec_Click);
			((Control)cmdAstDateInc).set_BackColor(Color.LightGreen);
			((ButtonBase)cmdAstDateInc).set_Image((Image)Resources.arrow_Up_16xLG);
			((Control)cmdAstDateInc).set_Location(new Point(186, 6));
			((Control)cmdAstDateInc).set_Name("cmdAstDateInc");
			((Control)cmdAstDateInc).set_Size(new Size(17, 17));
			((Control)cmdAstDateInc).set_TabIndex(26);
			((ButtonBase)cmdAstDateInc).set_UseVisualStyleBackColor(false);
			((Control)cmdAstDateInc).add_Click((EventHandler)cmdAstDateInc_Click);
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label5).set_Location(new Point(28, 2));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(153, 13));
			((Control)label5).set_TabIndex(25);
			((Control)label5).set_Text("Asteroids - Listed by Date");
			cmbByDate.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbByDate).set_Font(new Font("Consolas", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbByDate).set_FormattingEnabled(true);
			((Control)cmbByDate).set_Location(new Point(4, 19));
			((Control)cmbByDate).set_Name("cmbByDate");
			((Control)cmbByDate).set_Size(new Size(177, 21));
			((Control)cmbByDate).set_TabIndex(24);
			cmbByDate.add_SelectedIndexChanged((EventHandler)cmbByDate_SelectedIndexChanged);
			((Control)panel7).set_BackColor(Color.LavenderBlush);
			panel7.set_BorderStyle((BorderStyle)2);
			((Control)panel7).get_Controls().Add((Control)(object)cmbPlotAll_Star);
			((Control)panel7).get_Controls().Add((Control)(object)label8);
			((Control)panel7).get_Controls().Add((Control)(object)optU4);
			((Control)panel7).get_Controls().Add((Control)(object)optZC);
			((Control)panel7).get_Controls().Add((Control)(object)optTycho2);
			((Control)panel7).get_Controls().Add((Control)(object)optXZ);
			((Control)panel7).get_Controls().Add((Control)(object)optSAO);
			((Control)panel7).get_Controls().Add((Control)(object)optHip);
			((Control)panel7).get_Controls().Add((Control)(object)cmbStar);
			((Control)panel7).set_Location(new Point(3, 323));
			((Control)panel7).set_Name("panel7");
			((Control)panel7).set_Size(new Size(212, 81));
			((Control)panel7).set_TabIndex(23);
			((Control)cmbPlotAll_Star).set_BackColor(Color.Chartreuse);
			((Control)cmbPlotAll_Star).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmbPlotAll_Star).set_Location(new Point(149, 0));
			((Control)cmbPlotAll_Star).set_Name("cmbPlotAll_Star");
			((Control)cmbPlotAll_Star).set_Size(new Size(55, 22));
			((Control)cmbPlotAll_Star).set_TabIndex(25);
			((Control)cmbPlotAll_Star).set_Text("Plot All");
			((ButtonBase)cmbPlotAll_Star).set_UseVisualStyleBackColor(false);
			((Control)cmbPlotAll_Star).add_Click((EventHandler)cmbPlotAll_Star_Click);
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label8).set_Location(new Point(7, 2));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(79, 13));
			((Control)label8).set_TabIndex(18);
			((Control)label8).set_Text("Star number ");
			((Control)optU4).set_AutoSize(true);
			((Control)optU4).set_Location(new Point(18, 35));
			((Control)optU4).set_Name("optU4");
			((Control)optU4).set_Size(new Size(60, 17));
			((Control)optU4).set_TabIndex(3);
			((Control)optU4).set_Text("UCAC4");
			((ButtonBase)optU4).set_UseVisualStyleBackColor(true);
			optU4.add_CheckedChanged((EventHandler)optU4_CheckedChanged);
			((Control)optZC).set_AutoSize(true);
			((Control)optZC).set_Location(new Point(146, 35));
			((Control)optZC).set_Name("optZC");
			((Control)optZC).set_Size(new Size(39, 17));
			((Control)optZC).set_TabIndex(5);
			((Control)optZC).set_Text("ZC");
			((ButtonBase)optZC).set_UseVisualStyleBackColor(true);
			optZC.add_CheckedChanged((EventHandler)optZC_CheckedChanged);
			((Control)optTycho2).set_AutoSize(true);
			((Control)optTycho2).set_Location(new Point(146, 20));
			((Control)optTycho2).set_Name("optTycho2");
			((Control)optTycho2).set_Size(new Size(49, 17));
			((Control)optTycho2).set_TabIndex(2);
			((Control)optTycho2).set_Text("Tyc2");
			((ButtonBase)optTycho2).set_UseVisualStyleBackColor(true);
			optTycho2.add_CheckedChanged((EventHandler)optTycho2_CheckedChanged);
			((Control)optXZ).set_AutoSize(true);
			((Control)optXZ).set_Location(new Point(86, 35));
			((Control)optXZ).set_Name("optXZ");
			((Control)optXZ).set_Size(new Size(39, 17));
			((Control)optXZ).set_TabIndex(4);
			((Control)optXZ).set_Text("XZ");
			((ButtonBase)optXZ).set_UseVisualStyleBackColor(true);
			optXZ.add_CheckedChanged((EventHandler)optXZ_CheckedChanged);
			((Control)optSAO).set_AutoSize(true);
			((Control)optSAO).set_Location(new Point(86, 20));
			((Control)optSAO).set_Name("optSAO");
			((Control)optSAO).set_Size(new Size(47, 17));
			((Control)optSAO).set_TabIndex(1);
			((Control)optSAO).set_Text("SAO");
			((ButtonBase)optSAO).set_UseVisualStyleBackColor(true);
			optSAO.add_CheckedChanged((EventHandler)optSAO_CheckedChanged);
			((Control)optHip).set_AutoSize(true);
			((Control)optHip).set_Location(new Point(18, 20));
			((Control)optHip).set_Name("optHip");
			((Control)optHip).set_Size(new Size(41, 17));
			((Control)optHip).set_TabIndex(0);
			((Control)optHip).set_Text("Hip");
			((ButtonBase)optHip).set_UseVisualStyleBackColor(true);
			optHip.add_CheckedChanged((EventHandler)optHip_CheckedChanged);
			cmbStar.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbStar).set_Font(new Font("Consolas", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbStar).set_FormattingEnabled(true);
			((Control)cmbStar).set_Location(new Point(5, 54));
			((Control)cmbStar).set_Name("cmbStar");
			((Control)cmbStar).set_Size(new Size(198, 21));
			((Control)cmbStar).set_TabIndex(8);
			cmbStar.add_SelectedIndexChanged((EventHandler)cmbStar_SelectedIndexChanged);
			((Control)panel6).set_BackColor(Color.Lavender);
			panel6.set_BorderStyle((BorderStyle)2);
			((Control)panel6).get_Controls().Add((Control)(object)cmdPlotAll_Observer);
			((Control)panel6).get_Controls().Add((Control)(object)cmdCopy);
			((Control)panel6).get_Controls().Add((Control)(object)cmbObserver);
			((Control)panel6).get_Controls().Add((Control)(object)lblObserverCount);
			((Control)panel6).get_Controls().Add((Control)(object)label4);
			((Control)panel6).get_Controls().Add((Control)(object)lblIndexName);
			((Control)panel6).get_Controls().Add((Control)(object)txtObserver);
			((Control)panel6).set_Location(new Point(3, 245));
			((Control)panel6).set_Name("panel6");
			((Control)panel6).set_Size(new Size(212, 76));
			((Control)panel6).set_TabIndex(22);
			((Control)cmdPlotAll_Observer).set_BackColor(Color.Chartreuse);
			((Control)cmdPlotAll_Observer).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdPlotAll_Observer).set_Location(new Point(148, 0));
			((Control)cmdPlotAll_Observer).set_Name("cmdPlotAll_Observer");
			((Control)cmdPlotAll_Observer).set_Size(new Size(55, 22));
			((Control)cmdPlotAll_Observer).set_TabIndex(25);
			((Control)cmdPlotAll_Observer).set_Text("Plot All");
			((ButtonBase)cmdPlotAll_Observer).set_UseVisualStyleBackColor(false);
			((Control)cmdPlotAll_Observer).add_Click((EventHandler)cmbPlotAll_Observer_Click);
			((Control)cmdCopy).set_BackColor(Color.LemonChiffon);
			((Control)cmdCopy).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdCopy).set_Location(new Point(148, 23));
			((Control)cmdCopy).set_Name("cmdCopy");
			((Control)cmdCopy).set_Size(new Size(55, 24));
			((Control)cmdCopy).set_TabIndex(23);
			((Control)cmdCopy).set_Text("Copy list");
			((ButtonBase)cmdCopy).set_UseVisualStyleBackColor(false);
			((Control)cmdCopy).add_Click((EventHandler)cmdCopy_Click);
			cmbObserver.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbObserver).set_Font(new Font("Consolas", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbObserver).set_FormattingEnabled(true);
			((Control)cmbObserver).set_Location(new Point(5, 49));
			((Control)cmbObserver).set_Name("cmbObserver");
			((Control)cmbObserver).set_Size(new Size(198, 21));
			((Control)cmbObserver).set_TabIndex(22);
			cmbObserver.add_SelectedIndexChanged((EventHandler)cmbObserver_SelectedIndexChanged);
			((Control)lblObserverCount).set_AutoSize(true);
			((Control)lblObserverCount).set_Location(new Point(14, 28));
			((Control)lblObserverCount).set_Name("lblObserverCount");
			((Control)lblObserverCount).set_Size(new Size(14, 13));
			((Control)lblObserverCount).set_TabIndex(2);
			((Control)lblObserverCount).set_Text("#");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label4).set_Location(new Point(31, 0));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(92, 13));
			((Control)label4).set_TabIndex(21);
			((Control)label4).set_Text("Observer name");
			((Control)lblIndexName).set_AutoSize(true);
			((Control)lblIndexName).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblIndexName).set_Location(new Point(30, 12));
			((Control)lblIndexName).set_Name("lblIndexName");
			((Control)lblIndexName).set_Size(new Size(76, 13));
			((Control)lblIndexName).set_TabIndex(0);
			((Control)lblIndexName).set_Text("Name contains");
			((Control)txtObserver).set_Font(new Font("Consolas", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtObserver).set_Location(new Point(31, 25));
			((Control)txtObserver).set_Name("txtObserver");
			((Control)txtObserver).set_Size(new Size(96, 20));
			((Control)txtObserver).set_TabIndex(1);
			((Control)txtObserver).add_TextChanged((EventHandler)txtObserver_TextChanged);
			((Control)txtObserver).add_KeyDown(new KeyEventHandler(txtObserver_KeyDown));
			((Control)txtObserver).add_Leave((EventHandler)txtObserver_Leave);
			((Control)panel5).set_BackColor(Color.LightCyan);
			panel5.set_BorderStyle((BorderStyle)2);
			((Control)panel5).get_Controls().Add((Control)(object)cmdPlotAll_Number);
			((Control)panel5).get_Controls().Add((Control)(object)cmdAstNumDec);
			((Control)panel5).get_Controls().Add((Control)(object)cmdAstNumInc);
			((Control)panel5).get_Controls().Add((Control)(object)lblAsteroidCount);
			((Control)panel5).get_Controls().Add((Control)(object)txtAsteroidNumber);
			((Control)panel5).get_Controls().Add((Control)(object)label7);
			((Control)panel5).get_Controls().Add((Control)(object)cmbAsteroidNumber);
			((Control)panel5).set_Location(new Point(3, 49));
			((Control)panel5).set_Name("panel5");
			((Control)panel5).set_Size(new Size(212, 71));
			((Control)panel5).set_TabIndex(21);
			((Control)cmdPlotAll_Number).set_BackColor(Color.Chartreuse);
			((Control)cmdPlotAll_Number).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdPlotAll_Number).set_Location(new Point(126, 16));
			((Control)cmdPlotAll_Number).set_Name("cmdPlotAll_Number");
			((Control)cmdPlotAll_Number).set_Size(new Size(55, 22));
			((Control)cmdPlotAll_Number).set_TabIndex(24);
			((Control)cmdPlotAll_Number).set_Text("Plot All");
			((ButtonBase)cmdPlotAll_Number).set_UseVisualStyleBackColor(false);
			((Control)cmdPlotAll_Number).add_Click((EventHandler)cmdPlotAll_Number_Click);
			((ButtonBase)cmdAstNumDec).set_Image((Image)Resources.arrow_Down_16xLG);
			((Control)cmdAstNumDec).set_Location(new Point(184, 48));
			((Control)cmdAstNumDec).set_Name("cmdAstNumDec");
			((Control)cmdAstNumDec).set_Size(new Size(17, 17));
			((Control)cmdAstNumDec).set_TabIndex(23);
			((ButtonBase)cmdAstNumDec).set_UseVisualStyleBackColor(true);
			((Control)cmdAstNumDec).add_Click((EventHandler)cmdAstNumDec_Click);
			((Control)cmdAstNumInc).set_BackColor(Color.LightGreen);
			((ButtonBase)cmdAstNumInc).set_Image((Image)Resources.arrow_Up_16xLG);
			((Control)cmdAstNumInc).set_Location(new Point(184, 30));
			((Control)cmdAstNumInc).set_Name("cmdAstNumInc");
			((Control)cmdAstNumInc).set_Size(new Size(17, 17));
			((Control)cmdAstNumInc).set_TabIndex(22);
			((ButtonBase)cmdAstNumInc).set_UseVisualStyleBackColor(false);
			((Control)cmdAstNumInc).add_Click((EventHandler)cmdAstNumInc_Click);
			((Control)lblAsteroidCount).set_AutoSize(true);
			((Control)lblAsteroidCount).set_Location(new Point(1, 21));
			((Control)lblAsteroidCount).set_Name("lblAsteroidCount");
			((Control)lblAsteroidCount).set_Size(new Size(14, 13));
			((Control)lblAsteroidCount).set_TabIndex(21);
			((Control)lblAsteroidCount).set_Text("#");
			lblAsteroidCount.set_TextAlign(ContentAlignment.MiddleRight);
			((Control)txtAsteroidNumber).set_Location(new Point(18, 18));
			((Control)txtAsteroidNumber).set_Name("txtAsteroidNumber");
			((Control)txtAsteroidNumber).set_Size(new Size(65, 20));
			((Control)txtAsteroidNumber).set_TabIndex(20);
			((Control)txtAsteroidNumber).add_TextChanged((EventHandler)txtAsteroidNumber_TextChanged);
			((Control)txtAsteroidNumber).add_KeyDown(new KeyEventHandler(txtAsteroidNumber_KeyDown));
			((Control)txtAsteroidNumber).add_KeyPress(new KeyPressEventHandler(txtAsteroidNumber_KeyPress));
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label7).set_Location(new Point(21, 2));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(167, 13));
			((Control)label7).set_TabIndex(18);
			((Control)label7).set_Text("Asteroids - select by number");
			cmbAsteroidNumber.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbAsteroidNumber).set_Font(new Font("Consolas", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbAsteroidNumber).set_FormattingEnabled(true);
			((Control)cmbAsteroidNumber).set_Location(new Point(4, 42));
			((Control)cmbAsteroidNumber).set_Name("cmbAsteroidNumber");
			((Control)cmbAsteroidNumber).set_Size(new Size(177, 21));
			((Control)cmbAsteroidNumber).set_TabIndex(16);
			cmbAsteroidNumber.add_SelectedIndexChanged((EventHandler)cmbAsteroidNumber_SelectedIndexChanged);
			((Control)panel4).set_BackColor(Color.Honeydew);
			panel4.set_BorderStyle((BorderStyle)2);
			((Control)panel4).get_Controls().Add((Control)(object)cmdDown);
			((Control)panel4).get_Controls().Add((Control)(object)cmdUp);
			((Control)panel4).get_Controls().Add((Control)(object)lblRecords);
			((Control)panel4).get_Controls().Add((Control)(object)txtRecord);
			((Control)panel4).set_Location(new Point(3, 2));
			((Control)panel4).set_Name("panel4");
			((Control)panel4).set_Size(new Size(212, 45));
			((Control)panel4).set_TabIndex(20);
			((ButtonBase)cmdDown).set_Image((Image)Resources.arrow_Down_16xLG);
			((Control)cmdDown).set_Location(new Point(50, 19));
			((Control)cmdDown).set_Name("cmdDown");
			((Control)cmdDown).set_Size(new Size(17, 17));
			((Control)cmdDown).set_TabIndex(19);
			((ButtonBase)cmdDown).set_UseVisualStyleBackColor(true);
			((Control)cmdDown).add_Click((EventHandler)cmdDown_Click);
			((ButtonBase)cmdUp).set_Image((Image)Resources.arrow_Up_16xLG);
			((Control)cmdUp).set_Location(new Point(138, 19));
			((Control)cmdUp).set_Name("cmdUp");
			((Control)cmdUp).set_Size(new Size(17, 17));
			((Control)cmdUp).set_TabIndex(18);
			((ButtonBase)cmdUp).set_UseVisualStyleBackColor(true);
			((Control)cmdUp).add_Click((EventHandler)cmdUp_Click);
			((Control)lblRecords).set_AutoSize(true);
			((Control)lblRecords).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblRecords).set_Location(new Point(22, 2));
			((Control)lblRecords).set_Name("lblRecords");
			((Control)lblRecords).set_Size(new Size(165, 13));
			((Control)lblRecords).set_TabIndex(17);
			((Control)lblRecords).set_Text("Record number ( 1 to xxxxx)");
			((Control)txtRecord).set_Location(new Point(76, 18));
			((Control)txtRecord).set_Name("txtRecord");
			((Control)txtRecord).set_Size(new Size(56, 20));
			((Control)txtRecord).set_TabIndex(14);
			((Control)txtRecord).add_TextChanged((EventHandler)txtRecord_TextChanged);
			((Control)txtRecord).add_KeyDown(new KeyEventHandler(txtRecord_KeyDown));
			((Control)txtRecord).add_KeyPress(new KeyPressEventHandler(txtRecord_KeyPress));
			toolTip1.set_AutomaticDelay(100);
			toolTip1.set_IsBalloon(true);
			chkTimeHeight.set_AutoCheck(false);
			((Control)chkTimeHeight).set_AutoSize(true);
			((Control)chkTimeHeight).set_BackColor(Color.GreenYellow);
			chkTimeHeight.set_Checked(Settings.Default.LightCurveToolTip);
			chkTimeHeight.set_CheckState((CheckState)1);
			((Control)chkTimeHeight).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "LightCurveToolTip", true, (DataSourceUpdateMode)1));
			((Control)chkTimeHeight).set_Location(new Point(799, 4));
			((Control)chkTimeHeight).set_Name("chkTimeHeight");
			((Control)chkTimeHeight).set_Size(new Size(117, 17));
			((Control)chkTimeHeight).set_TabIndex(13);
			((Control)chkTimeHeight).set_Text("ShowTime + height");
			((ButtonBase)chkTimeHeight).set_UseVisualStyleBackColor(false);
			((Control)chkTimeHeight).add_Click((EventHandler)chkTimeHeight_Click);
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_BackColor(Color.Ivory);
			((Control)label9).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label9).set_Location(new Point(932, 7));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(189, 15));
			((Control)label9).set_TabIndex(17);
			((Control)label9).set_Text("Plot light curves - MAIN FILE");
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((ScrollableControl)this).set_AutoScrollMargin(new Size(4, 0));
			((Form)this).set_ClientSize(new Size(1141, 629));
			((Control)this).get_Controls().Add((Control)(object)label9);
			((Control)this).get_Controls().Add((Control)(object)pnlSelectStar);
			((Control)this).get_Controls().Add((Control)(object)chkTimeHeight);
			((Control)this).get_Controls().Add((Control)(object)panel1);
			((Control)this).get_Controls().Add((Control)(object)panelPlot);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationLightCurveViewer", true, (DataSourceUpdateMode)1));
			((Form)this).set_Icon((Icon)componentResourceManager.GetObject("$this.Icon"));
			((Form)this).set_Location(Settings.Default.LocationLightCurveViewer);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_MinimumSize(new Size(1000, 668));
			((Control)this).set_Name("LightCurveViewer");
			((Control)this).set_Text("Light Curve Viewer");
			((Form)this).add_FormClosing(new FormClosingEventHandler(LightCurveViewer_FormClosing));
			((Form)this).add_Load((EventHandler)LightCurveViewer_Load);
			((Control)this).add_Resize((EventHandler)LightCurveViewer_Resize);
			((Control)panelPlot).ResumeLayout(false);
			((ISupportInitialize)picPlot).EndInit();
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((ISupportInitialize)updnScale).EndInit();
			((Control)panel1).ResumeLayout(false);
			((Control)panel1).PerformLayout();
			((ISupportInitialize)SliderVerticalScale).EndInit();
			((ISupportInitialize)SliderOpacity).EndInit();
			((Control)panel3).ResumeLayout(false);
			((Control)panel10).ResumeLayout(false);
			((Control)panel10).PerformLayout();
			((Control)pnlStarByDate).ResumeLayout(false);
			((Control)pnlStarByDate).PerformLayout();
			((ISupportInitialize)SliderHeight).EndInit();
			((Control)pnlSelectStar).ResumeLayout(false);
			((Control)panel8).ResumeLayout(false);
			((Control)panel8).PerformLayout();
			((Control)panel2).ResumeLayout(false);
			((Control)panel2).PerformLayout();
			((Control)panel7).ResumeLayout(false);
			((Control)panel7).PerformLayout();
			((Control)panel6).ResumeLayout(false);
			((Control)panel6).PerformLayout();
			((Control)panel5).ResumeLayout(false);
			((Control)panel5).PerformLayout();
			((Control)panel4).ResumeLayout(false);
			((Control)panel4).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
