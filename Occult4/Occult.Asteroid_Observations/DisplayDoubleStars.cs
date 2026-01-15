using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Text;
using System.Windows.Forms;
using LightCurves;
using Occult.Properties;
using Occult.Star_Catalogues;

namespace Occult.Asteroid_Observations
{
	public class DisplayDoubleStars : Form
	{
		private string Header = "Count WDS Number       Star number         Mv1  Mv2  Soln   Sep (masec)     P.A.      RUWE *dia     Date          Asteroid               Publication";

		internal int TotalDoubles;

		internal int CurrentSort = 2;

		internal bool IncludeSolution = true;

		internal bool IncludeObservers = true;

		internal bool IncludeOnlyUnpublished;

		internal bool IncludeOnlyPublished;

		internal bool IncludeOnlyInWDS;

		internal bool ExcludeAllInWDS;

		internal bool FirstSolutionOnly;

		internal bool Include2Component = true;

		internal bool IncludeOneOnlyComponentObserved = true;

		internal bool WDS_ReportSubmissions;

		internal bool WDS_ReportSummary;

		internal bool Unsolved;

		internal string[] SortNames = new string[7] { "Asteroid name", "Asteroid number", "Date (most recent first)", "Date (oldest first)", "Star number", "Right Ascension", "RUWE" };

		private List<int> LightCurveRecordNumbers;

		private List<int> SubmittedRecordNumbers;

		private static DisplayData D_NearbyStars;

		private IContainer components;

		private MenuStrip menuStrip1;

		private ListBox lstDoubles;

		private ToolStripMenuItem withDoubleStarsToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem printPreviewToolStripMenuItem;

		private ToolStripMenuItem printToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripMenuItem sortToolStripMenuItem;

		private ToolStripMenuItem byStarNumberToolStripMenuItem;

		private ToolStripMenuItem byDateToolStripMenuItem;

		private ToolStripMenuItem byDateOldestFirstToolStripMenuItem;

		private ToolStripMenuItem byAsteroidNumberToolStripMenuItem;

		private ToolStripMenuItem byAsteroidNameToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private ToolStripMenuItem helpToolStripMenuItem;

		private Label label1;

		private ToolStripMenuItem copySelectedToolStripMenuItem;

		private ToolStripMenuItem byRUWEToolStripMenuItem;

		private ToolStripMenuItem byRightAscensionToolStripMenuItem;

		private ToolStripMenuItem displayIncludesToolStripMenuItem;

		private ToolStripMenuItem doubleStarSolutionsToolStripMenuItem;

		private ToolStripMenuItem observersToolStripMenuItem;

		private ToolStripMenuItem unpublishedEventsOnlyToolStripMenuItem;

		private ToolStripMenuItem limitNames;

		private ToolStripMenuItem toolStripMenuItem1;

		private ToolStripTextBox toolStripNameFilter;

		private ToolStripMenuItem toolStripMenuItem2;

		private ToolStripMenuItem limitToPublishedEventsOnlyToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator1;

		private ToolStripMenuItem firstSolutionOnlyToolStripMenuItem;

		private ToolStripMenuItem includeBothComponentsDetectedToolStripMenuItem;

		private ToolStripMenuItem includeOnlyOneComponentObservedToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator2;

		private ToolStripMenuItem asWDSSubmissionLineToolStripMenuItem;

		private ToolStripMenuItem asWDSSummaryLineToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator3;

		private ToolStripMenuItem excludeEventsInTheWDSToolStripMenuItem;

		private ToolStripMenuItem limitToEventsInTheWDSToolStripMenuItem;

		private ToolStripMenuItem unsolvedDoublesToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator4;

		private Label label2;

		private Label label3;

		private ToolStripMenuItem displaySaveLightCurvesToolStripMenuItem;

		public DisplayDoubleStars()
		{
			InitializeComponent();
			((Control)label1).set_Text(Header);
			IncludeSolution = (IncludeObservers = (Include2Component = (IncludeOneOnlyComponentObserved = true)));
			ToolStripMenuItem obj = doubleStarSolutionsToolStripMenuItem;
			bool @checked;
			observersToolStripMenuItem.set_Checked(@checked = true);
			obj.set_Checked(@checked);
			ToolStripMenuItem obj2 = includeBothComponentsDetectedToolStripMenuItem;
			includeOnlyOneComponentObservedToolStripMenuItem.set_Checked(@checked = true);
			obj2.set_Checked(@checked);
			ToolStripMenuItem obj3 = unpublishedEventsOnlyToolStripMenuItem;
			ToolStripMenuItem obj4 = limitToPublishedEventsOnlyToolStripMenuItem;
			bool flag;
			firstSolutionOnlyToolStripMenuItem.set_Checked(flag = false);
			obj4.set_Checked(@checked = flag);
			obj3.set_Checked(@checked);
			SetSortChecks(CurrentSort);
		}

		private void copyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(CollectEvents());
		}

		private void printPreviewToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.PrintPreviewText(CollectEvents());
		}

		private void printToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.PrintText(CollectEvents());
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings.Default.Save_AsteroidResults = Output.SavePredictionText(CollectEvents(), "Double stars from occultation observations", Settings.Default.Save_AsteroidResults);
		}

		private string CollectEvents()
		{
			StringBuilder stringBuilder = new StringBuilder();
			int count = lstDoubles.get_Items().get_Count();
			for (int i = 0; i < count; i++)
			{
				stringBuilder.AppendLine(lstDoubles.get_Items().get_Item(i).ToString());
			}
			stringBuilder.AppendLine("");
			return stringBuilder.ToString();
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private void byAsteroidNameToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SetSortChecks(0);
			Asteroid_Observations_Reports.DoublesList.Sort();
			if (WDS_ReportSubmissions | WDS_ReportSummary)
			{
				Display_WDS();
			}
			else
			{
				Display();
			}
		}

		private void byAsteroidNumberToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SetSortChecks(1);
			Asteroid_Observations_Reports.DoublesList.Sort();
			if (WDS_ReportSubmissions | WDS_ReportSummary)
			{
				Display_WDS();
			}
			else
			{
				Display();
			}
		}

		private void byDateToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SetSortChecks(2);
			Asteroid_Observations_Reports.DoublesList.Sort();
			if (WDS_ReportSubmissions | WDS_ReportSummary)
			{
				Display_WDS();
			}
			else
			{
				Display();
			}
		}

		private void byDateOldestFirstToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SetSortChecks(3);
			Asteroid_Observations_Reports.DoublesList.Sort();
			if (WDS_ReportSubmissions | WDS_ReportSummary)
			{
				Display_WDS();
			}
			else
			{
				Display();
			}
		}

		private void byStarNumberToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SetSortChecks(4);
			Asteroid_Observations_Reports.DoublesList.Sort();
			if (WDS_ReportSubmissions | WDS_ReportSummary)
			{
				Display_WDS();
			}
			else
			{
				Display();
			}
		}

		private void byRightAscensionToolStripMenuItem_Click_1(object sender, EventArgs e)
		{
			SetSortChecks(5);
			Asteroid_Observations_Reports.DoublesList.Sort();
			if (WDS_ReportSubmissions | WDS_ReportSummary)
			{
				Display_WDS();
			}
			else
			{
				Display();
			}
		}

		private void byRUWEToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SetSortChecks(6);
			Asteroid_Observations_Reports.DoublesList.Sort();
			if (WDS_ReportSubmissions | WDS_ReportSummary)
			{
				Display_WDS();
			}
			else
			{
				Display();
			}
		}

		private void SetSortChecks(int SortID)
		{
			AsteroidDoubleStars.SortField = SortID;
			byAsteroidNameToolStripMenuItem.set_Checked(SortID == 0);
			byAsteroidNumberToolStripMenuItem.set_Checked(SortID == 1);
			byDateToolStripMenuItem.set_Checked(SortID == 2);
			byDateOldestFirstToolStripMenuItem.set_Checked(SortID == 3);
			byStarNumberToolStripMenuItem.set_Checked(SortID == 4);
			byRightAscensionToolStripMenuItem.set_Checked(SortID == 5);
			byRUWEToolStripMenuItem.set_Checked(SortID == 6);
			CurrentSort = SortID;
		}

		public void Display()
		{
			int num = 0;
			string text = "     ";
			string text2 = "";
			string text3 = "";
			bool flag = false;
			((Control)label1).set_Text(Header);
			int num2 = 1;
			for (int i = 1; i < Asteroid_Observations_Reports.DoublesList.Count; i++)
			{
				if (Asteroid_Observations_Reports.DoublesList[i].StarID != Asteroid_Observations_Reports.DoublesList[i - 1].StarID)
				{
					num2++;
				}
			}
			lstDoubles.get_Items().Clear();
			lstDoubles.get_Items().Add((object)("".PadRight(25) + num2 + " double stars discovered in asteroidal occultations"));
			lstDoubles.get_Items().Add((object)"");
			string text4 = "";
			if (limitNames.get_Checked())
			{
				text4 = ". Limited to names containing '" + ((ToolStripItem)toolStripNameFilter).get_Text().Trim() + "'";
			}
			if (IncludeSolution & IncludeObservers)
			{
				lstDoubles.get_Items().Add((object)("List of double star solutions and observers. Sorted by " + SortNames[CurrentSort] + text4));
			}
			else if (IncludeSolution)
			{
				lstDoubles.get_Items().Add((object)("List of double star solutions. Sorted by " + SortNames[CurrentSort] + text4));
			}
			else
			{
				lstDoubles.get_Items().Add((object)("List of observers. Sorted by " + SortNames[CurrentSort] + text4));
			}
			if (!Include2Component)
			{
				lstDoubles.get_Items().Add((object)"");
				lstDoubles.get_Items().Add((object)"Stars where only one component was detected (light drop was much smaller than expected)");
			}
			lstDoubles.get_Items().Add((object)"");
			if (IncludeSolution)
			{
				lstDoubles.get_Items().Add((object)Header);
			}
			for (int j = 0; j < Asteroid_Observations_Reports.DoublesList.Count; j++)
			{
				bool flag2 = (Asteroid_Observations_Reports.DoublesList[j].Sep_mas < -999.0) | (Asteroid_Observations_Reports.DoublesList[j].PA_deg < 0.0);
				if ((flag2 & !Unsolved) || (!flag2 & Unsolved))
				{
					continue;
				}
				text3 = Asteroid_Observations_Reports.DoublesList[j].Observers;
				if ((limitNames.get_Checked() && !text3.ToLower().Contains(((ToolStripItem)toolStripNameFilter).get_Text().Trim().ToLower())) || (IncludeOnlyInWDS & !Asteroid_Observations_Reports.DoublesList[j].IsMatchedToWDS) || (ExcludeAllInWDS & Asteroid_Observations_Reports.DoublesList[j].IsMatchedToWDS) || (IncludeOnlyPublished & (Asteroid_Observations_Reports.DoublesList[j].JDSO_SubmitDate.Trim().Length == 0) & (Asteroid_Observations_Reports.DoublesList[j].JDSO_Vol_Num_Pg.Trim().Length == 0)) || (!Include2Component & !Asteroid_Observations_Reports.DoublesList[j].SingleComponentOnly) || (!IncludeOneOnlyComponentObserved & Asteroid_Observations_Reports.DoublesList[j].SingleComponentOnly))
				{
					continue;
				}
				string text5 = Asteroid_Observations_Reports.DoublesList[j].ToString();
				if (text5.Substring(0, 18) != text2)
				{
					num++;
					text = string.Format("{0,4}  ", num);
					if (AsteroidDoubleStars.SortField == 6 && Asteroid_Observations_Reports.DoublesList[j].RUWE > 1.4 && !flag)
					{
						lstDoubles.get_Items().Add((object)"".PadRight(110, '='));
						flag = true;
					}
					if ((IncludeOnlyUnpublished & (Asteroid_Observations_Reports.DoublesList[j].JDSO_SubmitDate.Trim().Length == 0)) | !IncludeOnlyUnpublished)
					{
						lstDoubles.get_Items().Add((object)"");
						if (IncludeSolution)
						{
							lstDoubles.get_Items().Add((object)(text + Asteroid_Observations_Reports.DoublesList[j].ToString()));
						}
						else
						{
							lstDoubles.get_Items().Add((object)(text + Asteroid_Observations_Reports.DoublesList[j].Star_Date));
						}
						text2 = text5.Substring(0, 13);
					}
				}
				for (int k = 1; k < 4; k++)
				{
					if (j + 1 >= Asteroid_Observations_Reports.DoublesList.Count)
					{
						break;
					}
					if (Asteroid_Observations_Reports.DoublesList[j + 1].ToString().Substring(0, 13) == text2)
					{
						if (((IncludeOnlyUnpublished & (Asteroid_Observations_Reports.DoublesList[j].JDSO_SubmitDate.Trim().Length == 0)) | !IncludeOnlyUnpublished) && (!FirstSolutionOnly & IncludeSolution))
						{
							lstDoubles.get_Items().Add((object)("  ..".PadRight(47) + Asteroid_Observations_Reports.DoublesList[j + 1].ToString().Substring(41, 39)));
						}
						j++;
					}
				}
				if (!IncludeObservers)
				{
					continue;
				}
				string text6 = "Observers: " + text3.Trim();
				int num3 = 108;
				if (!((IncludeOnlyUnpublished & (Asteroid_Observations_Reports.DoublesList[j].JDSO_SubmitDate.Trim().Length == 0)) | !IncludeOnlyUnpublished))
				{
					continue;
				}
				do
				{
					if (text6.Length < num3)
					{
						lstDoubles.get_Items().Add((object)text6);
						break;
					}
					int num4 = text6.LastIndexOf(",", num3 - 1);
					lstDoubles.get_Items().Add((object)text6.Substring(0, num4));
					text6 = text6.Substring(num4 + 1).Trim();
				}
				while (text6.Trim().Length > 5);
			}
			if (lstDoubles.get_Items().get_Count() < 7)
			{
				lstDoubles.get_Items().Add((object)"");
				lstDoubles.get_Items().Add((object)"No double star entries matching the criteria set");
			}
		}

		public void Display_WDS()
		{
			string text = "";
			int num = 0;
			lstDoubles.get_Items().Clear();
			if (WDS_ReportSummary)
			{
				((Control)label1).set_Text(AsteroidDoubleStars.WDS_SummaryHeader);
				lstDoubles.get_Items().Add((object)AsteroidDoubleStars.WDS_SummaryHeader);
			}
			if (WDS_ReportSubmissions)
			{
				((Control)label1).set_Text(AsteroidDoubleStars.WDS_SubmissionHeader);
				lstDoubles.get_Items().Add((object)AsteroidDoubleStars.WDS_SubmissionHeader);
			}
			for (int i = 0; i < Asteroid_Observations_Reports.DoublesList.Count; i++)
			{
				bool flag = (Asteroid_Observations_Reports.DoublesList[i].Sep_mas < -999.0) | (Asteroid_Observations_Reports.DoublesList[i].PA_deg < 0.0);
				if ((flag & !Unsolved) || (!flag & Unsolved) || (IncludeOnlyUnpublished & (Asteroid_Observations_Reports.DoublesList[i].JDSO_SubmitDate.Trim().Length > 0)) || (IncludeOnlyInWDS & !Asteroid_Observations_Reports.DoublesList[i].IsMatchedToWDS) || (ExcludeAllInWDS & Asteroid_Observations_Reports.DoublesList[i].IsMatchedToWDS))
				{
					continue;
				}
				if (Asteroid_Observations_Reports.DoublesList[i].WDS_id != text)
				{
					if (WDS_ReportSubmissions)
					{
						lstDoubles.get_Items().Add((object)"");
					}
					else if (num % 5 == 0)
					{
						lstDoubles.get_Items().Add((object)"");
					}
					num++;
					if (WDS_ReportSummary)
					{
						lstDoubles.get_Items().Add((object)Asteroid_Observations_Reports.DoublesList[i].WDS_SummaryLine());
					}
				}
				if (WDS_ReportSubmissions)
				{
					lstDoubles.get_Items().Add((object)Asteroid_Observations_Reports.DoublesList[i].WDS_SubmissionLine());
				}
				for (int j = 1; j < 4 && i + 1 < Asteroid_Observations_Reports.DoublesList.Count && Asteroid_Observations_Reports.DoublesList[i + 1].ToString().Substring(0, 13) == text; j++)
				{
					if (WDS_ReportSubmissions)
					{
						if ((IncludeOnlyUnpublished & (Asteroid_Observations_Reports.DoublesList[i + 1].JDSO_SubmitDate.Trim().Length > 0)) || (IncludeOnlyInWDS & !Asteroid_Observations_Reports.DoublesList[i + 1].IsMatchedToWDS) || (ExcludeAllInWDS & Asteroid_Observations_Reports.DoublesList[i + 1].IsMatchedToWDS))
						{
							continue;
						}
						lstDoubles.get_Items().Add((object)Asteroid_Observations_Reports.DoublesList[i + 1].WDS_SubmissionLine());
					}
					i++;
				}
				text = Asteroid_Observations_Reports.DoublesList[i].WDS_id;
			}
		}

		private void DisplayDoubleStars_Resize(object sender, EventArgs e)
		{
			if (((Control)this).get_Height() > 150)
			{
				((Control)lstDoubles).set_Height(((Control)this).get_Height() - 115);
			}
			if (((Control)this).get_Width() < 600)
			{
				((Control)this).set_Width(600);
			}
			((Control)lstDoubles).set_Width(((Control)this).get_Width() - 30);
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Double stars - Display");
		}

		private void DisplayDoubleStars_Load(object sender, EventArgs e)
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
		}

		private void copySelectedToolStripMenuItem_Click(object sender, EventArgs e)
		{
			string text = Header + "\r\n";
			foreach (object selectedItem in lstDoubles.get_SelectedItems())
			{
				text = text + selectedItem.ToString() + "\r\n";
			}
			try
			{
				Clipboard.SetText(text);
			}
			catch
			{
			}
		}

		private void limitNames_Click(object sender, EventArgs e)
		{
			limitNames.set_Checked(!limitNames.get_Checked());
			if (WDS_ReportSubmissions | WDS_ReportSummary)
			{
				Display_WDS();
			}
			else
			{
				Display();
			}
		}

		private void includeBothComponentsDetectedToolStripMenuItem_Click(object sender, EventArgs e)
		{
			bool include2Component;
			includeBothComponentsDetectedToolStripMenuItem.set_Checked(include2Component = !includeBothComponentsDetectedToolStripMenuItem.get_Checked());
			Include2Component = include2Component;
			if (!Include2Component & !IncludeOneOnlyComponentObserved)
			{
				includeOnlyOneComponentObservedToolStripMenuItem.set_Checked(include2Component = true);
				IncludeOneOnlyComponentObserved = include2Component;
			}
			if (WDS_ReportSubmissions | WDS_ReportSummary)
			{
				Display_WDS();
			}
			else
			{
				Display();
			}
		}

		private void includeOnlyOneComponentToolStripMenuItem_Click(object sender, EventArgs e)
		{
			bool includeOneOnlyComponentObserved;
			includeOnlyOneComponentObservedToolStripMenuItem.set_Checked(includeOneOnlyComponentObserved = !includeOnlyOneComponentObservedToolStripMenuItem.get_Checked());
			IncludeOneOnlyComponentObserved = includeOneOnlyComponentObserved;
			if (!Include2Component & !IncludeOneOnlyComponentObserved)
			{
				includeBothComponentsDetectedToolStripMenuItem.set_Checked(includeOneOnlyComponentObserved = true);
				Include2Component = includeOneOnlyComponentObserved;
			}
			if (WDS_ReportSubmissions | WDS_ReportSummary)
			{
				Display_WDS();
			}
			else
			{
				Display();
			}
		}

		private void lstDoubles_MouseDown(object sender, MouseEventArgs e)
		{
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_000b: Invalid comparison between Unknown and I4
			if ((int)e.get_Button() == 2097152)
			{
				DisplayStarChart();
				DisplayLightCurves(SaveImages: false);
				DisplayWDSentries();
				DisplayNearbyStars();
				DisplayEvent();
			}
		}

		private bool DisplayLightCurves(bool SaveImages)
		{
			LightCurveRecordNumbers = new List<int>();
			SubmittedRecordNumbers = new List<int>();
			int NumLightCurves = 0;
			int result = 0;
			try
			{
				LightData.LightCurveView.addTextToolStripMenuItem.set_Checked(false);
			}
			catch
			{
			}
			LightData.HeightRatioForSelectedLightLevel = 0f;
			LightData.HeaderExtraText = "";
			LightData.AddExtraHeader = false;
			LightData.HeaderExtraTime_UT1 = (LightData.HeaderExtraTime_UT2 = 0.0);
			LightData.AddTimeMarker1 = (LightData.AddTimeMarker2 = false);
			if (LightData.MainLightCurves.Count < 1)
			{
				LightData.ReadMainFile(Index: true, ReRead: true, ShowReadErrorMessages: false);
			}
			LightData.LoadLightCurvesReceived_Admin(IncludeEmbargoed: true, includeForReview: true, 0, ShowReadErrorMessages: true);
			try
			{
				string text = lstDoubles.get_SelectedItem().ToString();
				if ((!asWDSSubmissionLineToolStripMenuItem.get_Checked() & !asWDSSummaryLineToolStripMenuItem.get_Checked()) && doubleStarSolutionsToolStripMenuItem.get_Checked())
				{
					if (!int.TryParse(text.Substring(0, 4).PadRight(5), out var _))
					{
						return false;
					}
					try
					{
						((Form)LightCurveViewer.MultiPlot).Close();
					}
					catch
					{
					}
					int.TryParse(text.Substring(105, 6), out result);
					string asteroidName = text.Substring(112, 16).Trim();
					int.TryParse(text.Substring(92, 4), out var result3);
					int month = 1;
					string text2 = text.Substring(97, 3);
					for (int i = 1; i < 13; i++)
					{
						if (Utilities.ShortMonths[i] == text2)
						{
							month = i;
						}
					}
					int.TryParse(text.Substring(101, 2), out var result4);
					double jD_EventDate = Utilities.JD_from_Date(result3, month, result4);
					LightData.MainLightCurve_EntriesForAnObject(ref LightCurveRecordNumbers, ref NumLightCurves, result, asteroidName, jD_EventDate, DateToWithin4Hours: false);
					LightData.SubmittedLightCurve_EntriesForAnObject(ref SubmittedRecordNumbers, result, asteroidName, jD_EventDate, DateToWithin4Hours: false);
					if (LightCurveRecordNumbers.Count + SubmittedRecordNumbers.Count > 0)
					{
						new LightCurveViewer().MultiCurvePlot_Main_Submitted(ref LightCurveRecordNumbers, ref SubmittedRecordNumbers, ShowSkippedFrames: true, SaveImages);
					}
				}
			}
			catch
			{
				return false;
			}
			return true;
		}

		private bool DisplayWDSentries()
		{
			try
			{
				string text = lstDoubles.get_SelectedItem().ToString();
				if ((!asWDSSubmissionLineToolStripMenuItem.get_Checked() & !asWDSSummaryLineToolStripMenuItem.get_Checked()) && doubleStarSolutionsToolStripMenuItem.get_Checked())
				{
					if (!int.TryParse(text.Substring(0, 4).PadRight(5), out var _))
					{
						return false;
					}
					int.TryParse(text.Substring(6, 2), out var result2);
					int.TryParse(text.Substring(8, 2), out var result3);
					int.TryParse(text.Substring(10, 1), out var result4);
					double rA_hrs = (double)result2 + (double)result3 / 60.0 + (double)result4 / 600.0;
					int.TryParse(text.Substring(12, 2), out var result5);
					int.TryParse(text.Substring(14, 2), out var result6);
					double num = (double)result5 + (double)result6 / 60.0;
					if (text.Substring(11, 1) == "-")
					{
						num = 0.0 - num;
					}
					Interferometric_Plus_WDS.Find_WDS_IF_Matches(rA_hrs, num, HighPrecision: false, ShowResults: true, out WDS_Interferometer_Display.VariableID);
				}
			}
			catch
			{
				return false;
			}
			return true;
		}

		private bool DisplayNearbyStars()
		{
			double MagG = 0.0;
			double MagR = 0.0;
			double RA = 0.0;
			double Dec = 0.0;
			string StarID = "";
			int YearOfObs = 2016;
			int num = 30;
			if (!GetStarDetails(out StarID, out RA, out Dec, out MagG, out MagR, out YearOfObs))
			{
				return false;
			}
			Gaia.GetNearbyStarsFromCoords(EventDetails.StarCatwithNumber, RA, Dec, Gaia.MagGreen, YearOfObs - 2000, num, out var ExtraMagG, out var _, out var _, out var AllStars, out var AllNearby);
			try
			{
				((Control)D_NearbyStars).Show();
			}
			catch
			{
				D_NearbyStars = new DisplayData();
				((Control)D_NearbyStars).Show();
				((Form)D_NearbyStars).set_Location(Settings.Default.LocationD_NearbyStars);
			}
			double num2 = MagG + 2.0;
			if (num2 > 16.0)
			{
				num2 = 16.0;
			}
			AllNearby = "Stars within " + num + "\" of " + StarID + "\r\nbrighter than Mv " + string.Format("{0,1:f1}", num2) + "\r\n\r\n Sepn    Mv     Mr\r\n" + string.Format("Target  {0,5:f2}  {1,5:f2}  ", MagG, MagR) + "\r\n" + AllNearby;
			((Control)D_NearbyStars).set_Text("Nearby stars");
			((Control)D_NearbyStars.txtBox).set_Text(AllNearby);
			((Control)D_NearbyStars).set_Width(350);
			((Control)D_NearbyStars).set_Height(260);
			if (AllStars == 0)
			{
				D_NearbyStars.NoColors();
			}
			else if (EventDetails.MgStar - Utilities.CombinedMagnitude(EventDetails.MgStar, ExtraMagG) > 0.2)
			{
				D_NearbyStars.StartYellowRise();
			}
			else
			{
				D_NearbyStars.StartBeigeRise();
			}
			((TextBoxBase)D_NearbyStars.txtBox).set_SelectionStart(0);
			((TextBoxBase)D_NearbyStars.txtBox).Select(0, 0);
			((Control)D_NearbyStars).Focus();
			return true;
		}

		private bool DisplayEvent()
		{
			bool flag = true;
			try
			{
				string text = lstDoubles.get_SelectedItem().ToString();
				flag = text.Trim().Length > 5;
				if (flag)
				{
					flag = int.TryParse(text.Substring(0, 4).PadRight(5), out var _);
				}
				if (!flag)
				{
					TimedMessageDisplay timedMessageDisplay = new TimedMessageDisplay();
					((Control)timedMessageDisplay).set_Text("Wrong line selected");
					((Control)timedMessageDisplay.txtMessage).set_Text("You must select the line that identifies the star");
					((Control)timedMessageDisplay).Show();
					((Control)timedMessageDisplay).set_Left(((Control)this).get_Left() + 10);
					((Control)timedMessageDisplay).set_Top(((Control)this).get_Top() + 10);
					return false;
				}
				string asteroidNumber = text.Substring(110, 7);
				string asteroidName = text.Substring(118, 16).Trim();
				int.TryParse(text.Substring(97, 4), out var result2);
				int month = 1;
				string text2 = text.Substring(102, 3);
				for (int i = 1; i < 13; i++)
				{
					if (Utilities.ShortMonths[i] == text2)
					{
						month = i;
					}
				}
				int.TryParse(text.Substring(106, 2), out var result3);
				Data_and_Plots.ShowEditor();
				Data_and_Plots.Observations_Editor.DisplayEvent_ByDate_Number(result2, month, result3, asteroidNumber, asteroidName);
				return true;
			}
			catch
			{
				return false;
			}
		}

		private bool GetStarDetails(out string StarID, out double RA, out double Dec, out double MagG, out double MagR, out int YearOfObs)
		{
			bool result = false;
			RA = (Dec = (MagG = (MagR = 0.0)));
			StarID = "";
			YearOfObs = 2016;
			try
			{
				string text = lstDoubles.get_SelectedItem().ToString();
				if ((!asWDSSubmissionLineToolStripMenuItem.get_Checked() & !asWDSSummaryLineToolStripMenuItem.get_Checked()) && doubleStarSolutionsToolStripMenuItem.get_Checked())
				{
					if (!int.TryParse(text.Substring(0, 4).PadRight(5), out var _))
					{
						return false;
					}
					StarID = text.Substring(22, 20);
					int result3;
					string SourceFile;
					int result4;
					if (StarID.Contains("HIP"))
					{
						int.TryParse(StarID.Substring(3).Trim(), out result3);
						result = Gaia.GetHIPfromGaia(result3, out SourceFile);
					}
					else if (StarID.Contains("Tycho2"))
					{
						string[] array = StarID.Substring(6).Trim().Split(new char[1] { '-' });
						int.TryParse(array[0], out result3);
						int.TryParse(array[1], out result4);
						int.TryParse(array[2], out var result5);
						result = Gaia.GetTycho2fromGaiaDR2(result3, result4, result5, out SourceFile);
					}
					else if (StarID.Contains("UCAC4"))
					{
						string[] array2 = StarID.Substring(5).Trim().Split(new char[1] { '-' });
						int.TryParse(array2[0], out result3);
						int.TryParse(array2[1], out result4);
						result = Gaia.Get_U4_fromGaia_DR3_EDR3(result3, result4, out SourceFile);
					}
					MagG = Gaia.MagGreen;
					MagR = Gaia.MagRed;
					int.TryParse(text.Substring(92, 4), out YearOfObs);
					RA = Gaia.RA_rad + Gaia.PMRA_rad * ((double)YearOfObs - Gaia.Epoch_2000 - 2000.0);
					Dec = Gaia.Dec_rad + Gaia.PMDec_rad * ((double)YearOfObs - Gaia.Epoch_2000 - 2000.0);
					return result;
				}
				return false;
			}
			catch
			{
				return false;
			}
		}

		private void displaySaveLightCurvesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			DisplayLightCurves(SaveImages: true);
		}

		private void DisplayStarChart()
		{
			if (GetStarDetails(out var _, out var RA, out var Dec, out var _, out var _, out var _))
			{
				try
				{
					((Control)DisplayMPOccultations.Chart).Show();
				}
				catch
				{
					DisplayMPOccultations.Chart = new StarChart(SetCoords: false);
					((Control)DisplayMPOccultations.Chart).Show();
				}
				DisplayMPOccultations.Chart.CentreRA = RA;
				DisplayMPOccultations.Chart.CentreDec = Dec;
				DisplayMPOccultations.Chart.ShowObjectPath = false;
				DisplayMPOccultations.Chart.Use_Gaia = true;
				DisplayMPOccultations.Chart.SetStarChartAngluarSize(0);
				((Control)DisplayMPOccultations.Chart).Focus();
				DisplayMPOccultations.Chart.PlotChart();
			}
		}

		private void unsolvedDoublesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			bool unsolved;
			unsolvedDoublesToolStripMenuItem.set_Checked(unsolved = !unsolvedDoublesToolStripMenuItem.get_Checked());
			Unsolved = unsolved;
			if (WDS_ReportSubmissions | WDS_ReportSummary)
			{
				Display_WDS();
			}
			else
			{
				Display();
			}
		}

		private void asWDSSummaryLineToolStripMenuItem_Click(object sender, EventArgs e)
		{
			bool wDS_ReportSummary;
			asWDSSummaryLineToolStripMenuItem.set_Checked(wDS_ReportSummary = !asWDSSummaryLineToolStripMenuItem.get_Checked());
			WDS_ReportSummary = wDS_ReportSummary;
			if (WDS_ReportSubmissions | WDS_ReportSummary)
			{
				Display_WDS();
			}
			else
			{
				Display();
			}
		}

		private void asWDSSubmissionLineToolStripMenuItem_Click(object sender, EventArgs e)
		{
			bool wDS_ReportSubmissions;
			asWDSSubmissionLineToolStripMenuItem.set_Checked(wDS_ReportSubmissions = !asWDSSubmissionLineToolStripMenuItem.get_Checked());
			WDS_ReportSubmissions = wDS_ReportSubmissions;
			if (WDS_ReportSubmissions | WDS_ReportSummary)
			{
				Display_WDS();
			}
			else
			{
				Display();
			}
		}

		private void toolStripNameFilter_KeyDown(object sender, KeyEventArgs e)
		{
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_0008: Invalid comparison between Unknown and I4
			if ((int)e.get_KeyCode() == 13)
			{
				limitNames.set_Checked(true);
				if (WDS_ReportSubmissions | WDS_ReportSummary)
				{
					Display_WDS();
				}
				else
				{
					Display();
				}
			}
		}

		internal static void MatchToWDS()
		{
			double num = 0.0;
			double num2 = 0.0;
			double num3 = 0.0;
			string text = "";
			double num4 = 0.001;
			double num5 = 0.005;
			for (int i = 0; i < Asteroid_Observations_Reports.DoublesList.Count; i++)
			{
				Asteroid_Observations_Reports.DoublesList[i].IsMatchedToWDS = false;
			}
			using FileStream fileStream = new FileStream(Utilities.AppPath + "\\DownLoaded Files\\wds.dat", FileMode.Open, FileAccess.Read);
			BinaryReader binaryReader = new BinaryReader(fileStream);
			for (int j = 0; j < Asteroid_Observations_Reports.DoublesList.Count; j++)
			{
				if (Asteroid_Observations_Reports.DoublesList[j].WDS_id == text)
				{
					Asteroid_Observations_Reports.DoublesList[j].IsMatchedToWDS = Asteroid_Observations_Reports.DoublesList[j - 1].IsMatchedToWDS;
				}
				else
				{
					num = Asteroid_Observations_Reports.DoublesList[j].RA2000 * (180.0 / Math.PI) / 15.0;
					num2 = Asteroid_Observations_Reports.DoublesList[j].Dec2000 * (180.0 / Math.PI);
					num3 = Math.Abs(Asteroid_Observations_Reports.DoublesList[j].Sep_mas / 1000.0);
					int num6 = (int)(fileStream.Length / 132 - 1);
					int num7 = 0;
					int num8;
					do
					{
						num8 = (num6 + num7) / 2;
						fileStream.Seek(132 * num8, SeekOrigin.Begin);
						string text2 = new string(binaryReader.ReadChars(10));
						double num9 = double.Parse(text2.Substring(0, 2)) + double.Parse(text2.Substring(2, 3)) / 600.0;
						if (num > num9)
						{
							num7 = num8 + 1;
						}
						else
						{
							num6 = num8 - 1;
						}
					}
					while (num7 < num6);
					num8 -= 50;
					if (num8 < 0)
					{
						num8 = 0;
					}
					for (int k = 0; k < 400 && num8 + k < fileStream.Length / 132; k++)
					{
						fileStream.Seek(132 * (num8 + k), SeekOrigin.Begin);
						string text2 = new string(binaryReader.ReadChars(130));
						double num10 = double.Parse(text2.Substring(0, 2)) + double.Parse(text2.Substring(2, 3)) / 600.0;
						_ = text2.Substring(0, 5) == "05316";
						double num9;
						double num11;
						if (text2.Substring(112, 6).Trim().Length > 4)
						{
							num9 = double.Parse(text2.Substring(112, 2)) + double.Parse(text2.Substring(114, 2)) / 60.0 + double.Parse(text2.Substring(116, 5).Replace(" ", "0")) / 3600.0;
							num11 = double.Parse(text2.Substring(122, 2)) + double.Parse(text2.Substring(124, 2)) / 60.0 + double.Parse(text2.Substring(126, 4).Replace(" ", "0")) / 3600.0;
							if (text2.Substring(121, 1) == "-")
							{
								num11 = 0.0 - num11;
							}
						}
						else
						{
							num9 = num10;
							num11 = double.Parse(text2.Substring(6, 2)) + double.Parse(text2.Substring(8, 2)) / 60.0;
							if (text2.Substring(5, 1) == "-")
							{
								num11 = 0.0 - num11;
							}
						}
						if ((Math.Abs(num - num9) < num4) & (Math.Abs(num2 - num11) < num5))
						{
							double.TryParse(text2.Substring(46, 5), out var result);
							double.TryParse(text2.Substring(51, 6), out var result2);
							if (result < 0.2 + num3 || result2 < 0.2 + num3)
							{
								Asteroid_Observations_Reports.DoublesList[j].IsMatchedToWDS = true;
							}
							break;
						}
						if (num10 - num > num4)
						{
							break;
						}
					}
				}
				text = Asteroid_Observations_Reports.DoublesList[j].WDS_id;
			}
		}

		private void firstSolutionOnlyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			bool firstSolutionOnly;
			firstSolutionOnlyToolStripMenuItem.set_Checked(firstSolutionOnly = !firstSolutionOnlyToolStripMenuItem.get_Checked());
			FirstSolutionOnly = firstSolutionOnly;
			if (WDS_ReportSubmissions | WDS_ReportSummary)
			{
				Display_WDS();
			}
			else
			{
				Display();
			}
		}

		private void doubleStarSolutionsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			doubleStarSolutionsToolStripMenuItem.set_Checked(!doubleStarSolutionsToolStripMenuItem.get_Checked());
			if (!doubleStarSolutionsToolStripMenuItem.get_Checked())
			{
				observersToolStripMenuItem.set_Checked(true);
			}
			IncludeSolution = doubleStarSolutionsToolStripMenuItem.get_Checked();
			IncludeObservers = observersToolStripMenuItem.get_Checked();
			if (WDS_ReportSubmissions | WDS_ReportSummary)
			{
				Display_WDS();
			}
			else
			{
				Display();
			}
		}

		private void observersToolStripMenuItem_Click(object sender, EventArgs e)
		{
			observersToolStripMenuItem.set_Checked(!observersToolStripMenuItem.get_Checked());
			if (!observersToolStripMenuItem.get_Checked())
			{
				doubleStarSolutionsToolStripMenuItem.set_Checked(true);
			}
			IncludeSolution = doubleStarSolutionsToolStripMenuItem.get_Checked();
			IncludeObservers = observersToolStripMenuItem.get_Checked();
			if (WDS_ReportSubmissions | WDS_ReportSummary)
			{
				Display_WDS();
			}
			else
			{
				Display();
			}
		}

		private void limitToPublishedEventsOnlyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			bool includeOnlyPublished;
			limitToPublishedEventsOnlyToolStripMenuItem.set_Checked(includeOnlyPublished = !limitToPublishedEventsOnlyToolStripMenuItem.get_Checked());
			IncludeOnlyPublished = includeOnlyPublished;
			if (limitToPublishedEventsOnlyToolStripMenuItem.get_Checked() & doubleStarSolutionsToolStripMenuItem.get_Checked())
			{
				unpublishedEventsOnlyToolStripMenuItem.set_Checked(includeOnlyPublished = false);
				IncludeOnlyUnpublished = includeOnlyPublished;
			}
			if (WDS_ReportSubmissions | WDS_ReportSummary)
			{
				Display_WDS();
			}
			else
			{
				Display();
			}
		}

		private void unpublishedEventsOnlyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			bool includeOnlyUnpublished;
			unpublishedEventsOnlyToolStripMenuItem.set_Checked(includeOnlyUnpublished = !unpublishedEventsOnlyToolStripMenuItem.get_Checked());
			IncludeOnlyUnpublished = includeOnlyUnpublished;
			if (unpublishedEventsOnlyToolStripMenuItem.get_Checked() & limitToPublishedEventsOnlyToolStripMenuItem.get_Checked())
			{
				limitToPublishedEventsOnlyToolStripMenuItem.set_Checked(includeOnlyUnpublished = false);
				IncludeOnlyPublished = includeOnlyUnpublished;
			}
			if (WDS_ReportSubmissions | WDS_ReportSummary)
			{
				Display_WDS();
			}
			else
			{
				Display();
			}
		}

		private void excludeEventsInTheWDSToolStripMenuItem_Click(object sender, EventArgs e)
		{
			bool excludeAllInWDS;
			excludeEventsInTheWDSToolStripMenuItem.set_Checked(excludeAllInWDS = !excludeEventsInTheWDSToolStripMenuItem.get_Checked());
			ExcludeAllInWDS = excludeAllInWDS;
			if (ExcludeAllInWDS)
			{
				limitToEventsInTheWDSToolStripMenuItem.set_Checked(excludeAllInWDS = false);
				IncludeOnlyInWDS = excludeAllInWDS;
			}
			if (WDS_ReportSubmissions | WDS_ReportSummary)
			{
				Display_WDS();
			}
			else
			{
				Display();
			}
		}

		private void limitToEventsInTheWDSToolStripMenuItem_Click(object sender, EventArgs e)
		{
			bool includeOnlyInWDS;
			limitToEventsInTheWDSToolStripMenuItem.set_Checked(includeOnlyInWDS = !limitToEventsInTheWDSToolStripMenuItem.get_Checked());
			IncludeOnlyInWDS = includeOnlyInWDS;
			if (IncludeOnlyInWDS)
			{
				excludeEventsInTheWDSToolStripMenuItem.set_Checked(includeOnlyInWDS = false);
				ExcludeAllInWDS = includeOnlyInWDS;
			}
			if (WDS_ReportSubmissions | WDS_ReportSummary)
			{
				Display_WDS();
			}
			else
			{
				Display();
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
			//IL_1019: Unknown result type (might be due to invalid IL or missing references)
			//IL_1023: Expected O, but got Unknown
			//IL_1227: Unknown result type (might be due to invalid IL or missing references)
			//IL_1231: Expected O, but got Unknown
			//IL_148c: Unknown result type (might be due to invalid IL or missing references)
			//IL_1496: Expected O, but got Unknown
			menuStrip1 = new MenuStrip();
			withDoubleStarsToolStripMenuItem = new ToolStripMenuItem();
			copyToolStripMenuItem = new ToolStripMenuItem();
			copySelectedToolStripMenuItem = new ToolStripMenuItem();
			printPreviewToolStripMenuItem = new ToolStripMenuItem();
			printToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			sortToolStripMenuItem = new ToolStripMenuItem();
			byAsteroidNameToolStripMenuItem = new ToolStripMenuItem();
			byAsteroidNumberToolStripMenuItem = new ToolStripMenuItem();
			byDateToolStripMenuItem = new ToolStripMenuItem();
			byDateOldestFirstToolStripMenuItem = new ToolStripMenuItem();
			byStarNumberToolStripMenuItem = new ToolStripMenuItem();
			byRUWEToolStripMenuItem = new ToolStripMenuItem();
			byRightAscensionToolStripMenuItem = new ToolStripMenuItem();
			displayIncludesToolStripMenuItem = new ToolStripMenuItem();
			doubleStarSolutionsToolStripMenuItem = new ToolStripMenuItem();
			observersToolStripMenuItem = new ToolStripMenuItem();
			firstSolutionOnlyToolStripMenuItem = new ToolStripMenuItem();
			limitNames = new ToolStripMenuItem();
			toolStripSeparator1 = new ToolStripSeparator();
			includeBothComponentsDetectedToolStripMenuItem = new ToolStripMenuItem();
			includeOnlyOneComponentObservedToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator3 = new ToolStripSeparator();
			unpublishedEventsOnlyToolStripMenuItem = new ToolStripMenuItem();
			limitToPublishedEventsOnlyToolStripMenuItem = new ToolStripMenuItem();
			excludeEventsInTheWDSToolStripMenuItem = new ToolStripMenuItem();
			limitToEventsInTheWDSToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator2 = new ToolStripSeparator();
			asWDSSubmissionLineToolStripMenuItem = new ToolStripMenuItem();
			asWDSSummaryLineToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator4 = new ToolStripSeparator();
			unsolvedDoublesToolStripMenuItem = new ToolStripMenuItem();
			toolStripMenuItem1 = new ToolStripMenuItem();
			toolStripNameFilter = new ToolStripTextBox();
			toolStripMenuItem2 = new ToolStripMenuItem();
			displaySaveLightCurvesToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			lstDoubles = new ListBox();
			label1 = new Label();
			label2 = new Label();
			label3 = new Label();
			((Control)menuStrip1).SuspendLayout();
			((Control)this).SuspendLayout();
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[9]
			{
				(ToolStripItem)withDoubleStarsToolStripMenuItem,
				(ToolStripItem)sortToolStripMenuItem,
				(ToolStripItem)displayIncludesToolStripMenuItem,
				(ToolStripItem)toolStripMenuItem1,
				(ToolStripItem)toolStripNameFilter,
				(ToolStripItem)toolStripMenuItem2,
				(ToolStripItem)displaySaveLightCurvesToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(907, 27));
			((Control)menuStrip1).set_TabIndex(0);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withDoubleStarsToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[5]
			{
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)copySelectedToolStripMenuItem,
				(ToolStripItem)printPreviewToolStripMenuItem,
				(ToolStripItem)printToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem
			});
			((ToolStripItem)withDoubleStarsToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)withDoubleStarsToolStripMenuItem).set_Name("withDoubleStarsToolStripMenuItem");
			((ToolStripItem)withDoubleStarsToolStripMenuItem).set_Size(new Size(129, 23));
			((ToolStripItem)withDoubleStarsToolStripMenuItem).set_Text("with Double Stars...   ");
			((ToolStripItem)copyToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyToolStripMenuItem).set_Name("copyToolStripMenuItem");
			copyToolStripMenuItem.set_ShortcutKeys((Keys)131139);
			((ToolStripItem)copyToolStripMenuItem).set_Size(new Size(159, 22));
			((ToolStripItem)copyToolStripMenuItem).set_Text("&Copy all");
			((ToolStripItem)copyToolStripMenuItem).add_Click((EventHandler)copyToolStripMenuItem_Click);
			((ToolStripItem)copySelectedToolStripMenuItem).set_Name("copySelectedToolStripMenuItem");
			((ToolStripItem)copySelectedToolStripMenuItem).set_Size(new Size(159, 22));
			((ToolStripItem)copySelectedToolStripMenuItem).set_Text("Copy selected");
			((ToolStripItem)copySelectedToolStripMenuItem).add_Click((EventHandler)copySelectedToolStripMenuItem_Click);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Image((Image)Resources.PrintPreview);
			((ToolStripItem)printPreviewToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Name("printPreviewToolStripMenuItem");
			((ToolStripItem)printPreviewToolStripMenuItem).set_Size(new Size(159, 22));
			((ToolStripItem)printPreviewToolStripMenuItem).set_Text("Print preview");
			((ToolStripItem)printPreviewToolStripMenuItem).add_Click((EventHandler)printPreviewToolStripMenuItem_Click);
			((ToolStripItem)printToolStripMenuItem).set_Image((Image)Resources.Print);
			((ToolStripItem)printToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printToolStripMenuItem).set_Name("printToolStripMenuItem");
			printToolStripMenuItem.set_ShortcutKeys((Keys)131152);
			((ToolStripItem)printToolStripMenuItem).set_Size(new Size(159, 22));
			((ToolStripItem)printToolStripMenuItem).set_Text("&Print");
			((ToolStripItem)printToolStripMenuItem).add_Click((EventHandler)printToolStripMenuItem_Click);
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			saveToolStripMenuItem.set_ShortcutKeys((Keys)131155);
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(159, 22));
			((ToolStripItem)saveToolStripMenuItem).set_Text("&Save");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
			((ToolStripDropDownItem)sortToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[7]
			{
				(ToolStripItem)byAsteroidNameToolStripMenuItem,
				(ToolStripItem)byAsteroidNumberToolStripMenuItem,
				(ToolStripItem)byDateToolStripMenuItem,
				(ToolStripItem)byDateOldestFirstToolStripMenuItem,
				(ToolStripItem)byStarNumberToolStripMenuItem,
				(ToolStripItem)byRUWEToolStripMenuItem,
				(ToolStripItem)byRightAscensionToolStripMenuItem
			});
			((ToolStripItem)sortToolStripMenuItem).set_Image((Image)Resources.Sort);
			((ToolStripItem)sortToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)sortToolStripMenuItem).set_Name("sortToolStripMenuItem");
			((ToolStripItem)sortToolStripMenuItem).set_Size(new Size(87, 23));
			((ToolStripItem)sortToolStripMenuItem).set_Text("Sort by..   ");
			((ToolStripItem)byAsteroidNameToolStripMenuItem).set_Name("byAsteroidNameToolStripMenuItem");
			((ToolStripItem)byAsteroidNameToolStripMenuItem).set_Size(new Size(209, 22));
			((ToolStripItem)byAsteroidNameToolStripMenuItem).set_Text("Asteroid name");
			((ToolStripItem)byAsteroidNameToolStripMenuItem).add_Click((EventHandler)byAsteroidNameToolStripMenuItem_Click);
			((ToolStripItem)byAsteroidNumberToolStripMenuItem).set_Name("byAsteroidNumberToolStripMenuItem");
			((ToolStripItem)byAsteroidNumberToolStripMenuItem).set_Size(new Size(209, 22));
			((ToolStripItem)byAsteroidNumberToolStripMenuItem).set_Text("Asteroid number");
			((ToolStripItem)byAsteroidNumberToolStripMenuItem).add_Click((EventHandler)byAsteroidNumberToolStripMenuItem_Click);
			((ToolStripItem)byDateToolStripMenuItem).set_Name("byDateToolStripMenuItem");
			((ToolStripItem)byDateToolStripMenuItem).set_Size(new Size(209, 22));
			((ToolStripItem)byDateToolStripMenuItem).set_Text("Date (most recent first)");
			((ToolStripItem)byDateToolStripMenuItem).add_Click((EventHandler)byDateToolStripMenuItem_Click);
			((ToolStripItem)byDateOldestFirstToolStripMenuItem).set_Name("byDateOldestFirstToolStripMenuItem");
			((ToolStripItem)byDateOldestFirstToolStripMenuItem).set_Size(new Size(209, 22));
			((ToolStripItem)byDateOldestFirstToolStripMenuItem).set_Text("Date (oldest first)");
			((ToolStripItem)byDateOldestFirstToolStripMenuItem).add_Click((EventHandler)byDateOldestFirstToolStripMenuItem_Click);
			((ToolStripItem)byStarNumberToolStripMenuItem).set_Name("byStarNumberToolStripMenuItem");
			((ToolStripItem)byStarNumberToolStripMenuItem).set_Size(new Size(209, 22));
			((ToolStripItem)byStarNumberToolStripMenuItem).set_Text("Star number");
			((ToolStripItem)byStarNumberToolStripMenuItem).add_Click((EventHandler)byStarNumberToolStripMenuItem_Click);
			((ToolStripItem)byRUWEToolStripMenuItem).set_Name("byRUWEToolStripMenuItem");
			((ToolStripItem)byRUWEToolStripMenuItem).set_Size(new Size(209, 22));
			((ToolStripItem)byRUWEToolStripMenuItem).set_Text("RUWE");
			((ToolStripItem)byRUWEToolStripMenuItem).add_Click((EventHandler)byRUWEToolStripMenuItem_Click);
			((ToolStripItem)byRightAscensionToolStripMenuItem).set_Name("byRightAscensionToolStripMenuItem");
			((ToolStripItem)byRightAscensionToolStripMenuItem).set_Size(new Size(209, 22));
			((ToolStripItem)byRightAscensionToolStripMenuItem).set_Text("WDS identifier [RA + Dec]");
			((ToolStripItem)byRightAscensionToolStripMenuItem).add_Click((EventHandler)byRightAscensionToolStripMenuItem_Click_1);
			((ToolStripDropDownItem)displayIncludesToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[17]
			{
				(ToolStripItem)doubleStarSolutionsToolStripMenuItem,
				(ToolStripItem)observersToolStripMenuItem,
				(ToolStripItem)firstSolutionOnlyToolStripMenuItem,
				(ToolStripItem)limitNames,
				(ToolStripItem)toolStripSeparator1,
				(ToolStripItem)includeBothComponentsDetectedToolStripMenuItem,
				(ToolStripItem)includeOnlyOneComponentObservedToolStripMenuItem,
				(ToolStripItem)toolStripSeparator3,
				(ToolStripItem)unpublishedEventsOnlyToolStripMenuItem,
				(ToolStripItem)limitToPublishedEventsOnlyToolStripMenuItem,
				(ToolStripItem)excludeEventsInTheWDSToolStripMenuItem,
				(ToolStripItem)limitToEventsInTheWDSToolStripMenuItem,
				(ToolStripItem)toolStripSeparator2,
				(ToolStripItem)asWDSSubmissionLineToolStripMenuItem,
				(ToolStripItem)asWDSSummaryLineToolStripMenuItem,
				(ToolStripItem)toolStripSeparator4,
				(ToolStripItem)unsolvedDoublesToolStripMenuItem
			});
			((ToolStripItem)displayIncludesToolStripMenuItem).set_Name("displayIncludesToolStripMenuItem");
			((ToolStripItem)displayIncludesToolStripMenuItem).set_Size(new Size(78, 23));
			((ToolStripItem)displayIncludesToolStripMenuItem).set_Text("Display ...   ");
			((ToolStripItem)doubleStarSolutionsToolStripMenuItem).set_BackColor(Color.Honeydew);
			((ToolStripItem)doubleStarSolutionsToolStripMenuItem).set_Name("doubleStarSolutionsToolStripMenuItem");
			((ToolStripItem)doubleStarSolutionsToolStripMenuItem).set_Size(new Size(315, 22));
			((ToolStripItem)doubleStarSolutionsToolStripMenuItem).set_Text("Include   Double star solutions");
			((ToolStripItem)doubleStarSolutionsToolStripMenuItem).add_Click((EventHandler)doubleStarSolutionsToolStripMenuItem_Click);
			((ToolStripItem)observersToolStripMenuItem).set_BackColor(Color.Honeydew);
			((ToolStripItem)observersToolStripMenuItem).set_Name("observersToolStripMenuItem");
			((ToolStripItem)observersToolStripMenuItem).set_Size(new Size(315, 22));
			((ToolStripItem)observersToolStripMenuItem).set_Text("Include   Observers");
			((ToolStripItem)observersToolStripMenuItem).add_Click((EventHandler)observersToolStripMenuItem_Click);
			((ToolStripItem)firstSolutionOnlyToolStripMenuItem).set_BackColor(Color.Lavender);
			((ToolStripItem)firstSolutionOnlyToolStripMenuItem).set_Name("firstSolutionOnlyToolStripMenuItem");
			((ToolStripItem)firstSolutionOnlyToolStripMenuItem).set_Size(new Size(315, 22));
			((ToolStripItem)firstSolutionOnlyToolStripMenuItem).set_Text(" List  First solution only");
			((ToolStripItem)firstSolutionOnlyToolStripMenuItem).add_Click((EventHandler)firstSolutionOnlyToolStripMenuItem_Click);
			((ToolStripItem)limitNames).set_BackColor(Color.PapayaWhip);
			((ToolStripItem)limitNames).set_Name("limitNames");
			((ToolStripItem)limitNames).set_Size(new Size(315, 22));
			((ToolStripItem)limitNames).set_Text("Apply   Name filter");
			((ToolStripItem)limitNames).add_Click((EventHandler)limitNames_Click);
			((ToolStripItem)toolStripSeparator1).set_Name("toolStripSeparator1");
			((ToolStripItem)toolStripSeparator1).set_Size(new Size(312, 6));
			((ToolStripItem)includeBothComponentsDetectedToolStripMenuItem).set_BackColor(Color.PeachPuff);
			((ToolStripItem)includeBothComponentsDetectedToolStripMenuItem).set_Name("includeBothComponentsDetectedToolStripMenuItem");
			((ToolStripItem)includeBothComponentsDetectedToolStripMenuItem).set_Size(new Size(315, 22));
			((ToolStripItem)includeBothComponentsDetectedToolStripMenuItem).set_Text("Include    Both components observed");
			((ToolStripItem)includeBothComponentsDetectedToolStripMenuItem).add_Click((EventHandler)includeBothComponentsDetectedToolStripMenuItem_Click);
			((ToolStripItem)includeOnlyOneComponentObservedToolStripMenuItem).set_BackColor(Color.PeachPuff);
			((ToolStripItem)includeOnlyOneComponentObservedToolStripMenuItem).set_Name("includeOnlyOneComponentObservedToolStripMenuItem");
			((ToolStripItem)includeOnlyOneComponentObservedToolStripMenuItem).set_Size(new Size(315, 22));
			((ToolStripItem)includeOnlyOneComponentObservedToolStripMenuItem).set_Text("Include    Only one component observed");
			((ToolStripItem)includeOnlyOneComponentObservedToolStripMenuItem).add_Click((EventHandler)includeOnlyOneComponentToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator3).set_Name("toolStripSeparator3");
			((ToolStripItem)toolStripSeparator3).set_Size(new Size(312, 6));
			((ToolStripItem)unpublishedEventsOnlyToolStripMenuItem).set_BackColor(Color.LightCyan);
			((ToolStripItem)unpublishedEventsOnlyToolStripMenuItem).set_Font(new Font("Segoe UI", 9f, FontStyle.Bold));
			((ToolStripItem)unpublishedEventsOnlyToolStripMenuItem).set_ForeColor(Color.DarkGreen);
			((ToolStripItem)unpublishedEventsOnlyToolStripMenuItem).set_Name("unpublishedEventsOnlyToolStripMenuItem");
			((ToolStripItem)unpublishedEventsOnlyToolStripMenuItem).set_Size(new Size(315, 22));
			((ToolStripItem)unpublishedEventsOnlyToolStripMenuItem).set_Text("Exclude   Events published in the JDSO");
			((ToolStripItem)unpublishedEventsOnlyToolStripMenuItem).add_Click((EventHandler)unpublishedEventsOnlyToolStripMenuItem_Click);
			((ToolStripItem)limitToPublishedEventsOnlyToolStripMenuItem).set_BackColor(Color.LightCyan);
			((ToolStripItem)limitToPublishedEventsOnlyToolStripMenuItem).set_Font(new Font("Segoe UI", 9f, FontStyle.Bold));
			((ToolStripItem)limitToPublishedEventsOnlyToolStripMenuItem).set_ForeColor(Color.DarkRed);
			((ToolStripItem)limitToPublishedEventsOnlyToolStripMenuItem).set_Name("limitToPublishedEventsOnlyToolStripMenuItem");
			((ToolStripItem)limitToPublishedEventsOnlyToolStripMenuItem).set_Size(new Size(315, 22));
			((ToolStripItem)limitToPublishedEventsOnlyToolStripMenuItem).set_Text("Exclude   Events NOT published in the JDSO");
			((ToolStripItem)limitToPublishedEventsOnlyToolStripMenuItem).add_Click((EventHandler)limitToPublishedEventsOnlyToolStripMenuItem_Click);
			((ToolStripItem)excludeEventsInTheWDSToolStripMenuItem).set_BackColor(Color.PaleTurquoise);
			((ToolStripItem)excludeEventsInTheWDSToolStripMenuItem).set_Font(new Font("Segoe UI", 9f, FontStyle.Bold));
			((ToolStripItem)excludeEventsInTheWDSToolStripMenuItem).set_ForeColor(Color.DarkGreen);
			((ToolStripItem)excludeEventsInTheWDSToolStripMenuItem).set_Name("excludeEventsInTheWDSToolStripMenuItem");
			((ToolStripItem)excludeEventsInTheWDSToolStripMenuItem).set_Size(new Size(315, 22));
			((ToolStripItem)excludeEventsInTheWDSToolStripMenuItem).set_Text("Exclude   Events in the WDS");
			((ToolStripItem)excludeEventsInTheWDSToolStripMenuItem).add_Click((EventHandler)excludeEventsInTheWDSToolStripMenuItem_Click);
			((ToolStripItem)limitToEventsInTheWDSToolStripMenuItem).set_BackColor(Color.PaleTurquoise);
			((ToolStripItem)limitToEventsInTheWDSToolStripMenuItem).set_Font(new Font("Segoe UI", 9f, FontStyle.Bold));
			((ToolStripItem)limitToEventsInTheWDSToolStripMenuItem).set_ForeColor(Color.DarkRed);
			((ToolStripItem)limitToEventsInTheWDSToolStripMenuItem).set_Name("limitToEventsInTheWDSToolStripMenuItem");
			((ToolStripItem)limitToEventsInTheWDSToolStripMenuItem).set_Size(new Size(315, 22));
			((ToolStripItem)limitToEventsInTheWDSToolStripMenuItem).set_Text("Exclude   Events NOT in the WDS");
			((ToolStripItem)limitToEventsInTheWDSToolStripMenuItem).add_Click((EventHandler)limitToEventsInTheWDSToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator2).set_Name("toolStripSeparator2");
			((ToolStripItem)toolStripSeparator2).set_Size(new Size(312, 6));
			((ToolStripItem)asWDSSubmissionLineToolStripMenuItem).set_BackColor(Color.LightGreen);
			((ToolStripItem)asWDSSubmissionLineToolStripMenuItem).set_Font(new Font("Segoe UI", 9f, FontStyle.Bold));
			((ToolStripItem)asWDSSubmissionLineToolStripMenuItem).set_ForeColor(Color.Blue);
			((ToolStripItem)asWDSSubmissionLineToolStripMenuItem).set_Name("asWDSSubmissionLineToolStripMenuItem");
			((ToolStripItem)asWDSSubmissionLineToolStripMenuItem).set_Size(new Size(315, 22));
			((ToolStripItem)asWDSSubmissionLineToolStripMenuItem).set_Text("as WDS    Submission line");
			((ToolStripItem)asWDSSubmissionLineToolStripMenuItem).add_Click((EventHandler)asWDSSubmissionLineToolStripMenuItem_Click);
			((ToolStripItem)asWDSSummaryLineToolStripMenuItem).set_BackColor(Color.PaleGreen);
			((ToolStripItem)asWDSSummaryLineToolStripMenuItem).set_Font(new Font("Segoe UI", 9f, FontStyle.Bold));
			((ToolStripItem)asWDSSummaryLineToolStripMenuItem).set_ForeColor(Color.Blue);
			((ToolStripItem)asWDSSummaryLineToolStripMenuItem).set_Name("asWDSSummaryLineToolStripMenuItem");
			((ToolStripItem)asWDSSummaryLineToolStripMenuItem).set_Size(new Size(315, 22));
			((ToolStripItem)asWDSSummaryLineToolStripMenuItem).set_Text("as WDS    Summary line");
			((ToolStripItem)asWDSSummaryLineToolStripMenuItem).add_Click((EventHandler)asWDSSummaryLineToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator4).set_Name("toolStripSeparator4");
			((ToolStripItem)toolStripSeparator4).set_Size(new Size(312, 6));
			((ToolStripItem)unsolvedDoublesToolStripMenuItem).set_BackColor(Color.LightPink);
			((ToolStripItem)unsolvedDoublesToolStripMenuItem).set_Name("unsolvedDoublesToolStripMenuItem");
			((ToolStripItem)unsolvedDoublesToolStripMenuItem).set_Size(new Size(315, 22));
			((ToolStripItem)unsolvedDoublesToolStripMenuItem).set_Text("Unsolved doubles");
			((ToolStripItem)unsolvedDoublesToolStripMenuItem).add_Click((EventHandler)unsolvedDoublesToolStripMenuItem_Click);
			((ToolStripItem)toolStripMenuItem1).set_Name("toolStripMenuItem1");
			((ToolStripItem)toolStripMenuItem1).set_Size(new Size(78, 23));
			((ToolStripItem)toolStripMenuItem1).set_Text("Name filter");
			((ToolStripItem)toolStripNameFilter).set_BackColor(Color.PowderBlue);
			toolStripNameFilter.set_BorderStyle((BorderStyle)1);
			((ToolStripItem)toolStripNameFilter).set_Font(new Font("Segoe UI", 9f));
			((ToolStripItem)toolStripNameFilter).set_Name("toolStripNameFilter");
			((ToolStripItem)toolStripNameFilter).set_Size(new Size(100, 23));
			((ToolStripControlHost)toolStripNameFilter).add_KeyDown(new KeyEventHandler(toolStripNameFilter_KeyDown));
			((ToolStripItem)toolStripMenuItem2).set_Enabled(false);
			((ToolStripItem)toolStripMenuItem2).set_Name("toolStripMenuItem2");
			((ToolStripItem)toolStripMenuItem2).set_Size(new Size(40, 23));
			((ToolStripItem)toolStripMenuItem2).set_Text("       ");
			((ToolStripItem)displaySaveLightCurvesToolStripMenuItem).set_Name("displaySaveLightCurvesToolStripMenuItem");
			((ToolStripItem)displaySaveLightCurvesToolStripMenuItem).set_Size(new Size(173, 23));
			((ToolStripItem)displaySaveLightCurvesToolStripMenuItem).set_Text("Display && Save light curves    ");
			((ToolStripItem)displaySaveLightCurvesToolStripMenuItem).add_Click((EventHandler)displaySaveLightCurvesToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(75, 23));
			((ToolStripItem)helpToolStripMenuItem).set_Text(" &Help    ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(54, 23));
			((ToolStripItem)exitToolStripMenuItem).set_Text("E&xit");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((Control)lstDoubles).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstDoubles).set_FormattingEnabled(true);
			lstDoubles.set_HorizontalExtent(990);
			lstDoubles.set_HorizontalScrollbar(true);
			lstDoubles.set_ItemHeight(14);
			((Control)lstDoubles).set_Location(new Point(6, 71));
			((Control)lstDoubles).set_Name("lstDoubles");
			lstDoubles.set_ScrollAlwaysVisible(true);
			lstDoubles.set_SelectionMode((SelectionMode)3);
			((Control)lstDoubles).set_Size(new Size(889, 396));
			((Control)lstDoubles).set_TabIndex(1);
			((Control)lstDoubles).add_MouseDown(new MouseEventHandler(lstDoubles_MouseDown));
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(8, 52));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(588, 14));
			((Control)label1).set_TabIndex(2);
			((Control)label1).set_Text("Cnt  Star number         Soln   Sep (masec)      P.A.         Date         Asteroid");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label2).set_ForeColor(Color.Red);
			((Control)label2).set_Location(new Point(61, 33));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(202, 13));
			((Control)label2).set_TabIndex(4);
			((Control)label2).set_Text("Right-click on main solution line to display");
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_BackColor(Color.MistyRose);
			label3.set_BorderStyle((BorderStyle)2);
			((Control)label3).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label3).set_ForeColor(Color.MediumBlue);
			((Control)label3).set_Location(new Point(262, 33));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(325, 15));
			((Control)label3).set_TabIndex(5);
			((Control)label3).set_Text("Event  + WDS entry  +  Nearby stars  +  Star chart  +   Light Curves");
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(907, 488));
			((Control)this).get_Controls().Add((Control)(object)label3);
			((Control)this).get_Controls().Add((Control)(object)label2);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)lstDoubles);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationAsterShowDoubles", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_Location(Settings.Default.LocationAsterShowDoubles);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_Name("DisplayDoubleStars");
			((Control)this).set_Text("Display observed double stars");
			((Form)this).add_Load((EventHandler)DisplayDoubleStars_Load);
			((Control)this).add_Resize((EventHandler)DisplayDoubleStars_Resize);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
