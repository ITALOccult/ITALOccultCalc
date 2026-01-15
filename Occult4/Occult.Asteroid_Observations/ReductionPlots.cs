using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Drawing.Imaging;
using System.Globalization;
using System.IO;
using System.Text;
using System.Windows.Forms;
using Occult.Asteroids;
using Occult.Properties;

namespace Occult.Asteroid_Observations
{
	public class ReductionPlots : Form
	{
		private bool Loaded;

		private string SaveLocation = Utilities.AppPath + "\\Asteroid\\Results\\PDS Files\\" + DateTime.Today.Year + "\\Grahics";

		private const double Radian = 180.0 / Math.PI;

		private static int Kind;

		private int TotalNumRecords;

		private IContainer components;

		private Button cmdGeneratePlots;

		private GroupBox grpHistory;

		private RadioButton optByPlanets;

		private Label lblNumbers;

		private RadioButton optByRecords;

		private RadioButton optByNumber;

		private RadioButton optByName;

		private RadioButton optByDate;

		private ComboBox cmbNumberRange;

		private ComboBox cmbAlpha;

		private ComboBox cmbYearRange;

		private RadioButton optExcellent;

		private RadioButton optGood;

		private RadioButton optPoor;

		private ListBox lstHistorical;

		private Label label1;

		private GroupBox grpGenerate;

		private RadioButton optTime;

		private Button cmdOutputDirectory;

		private Label lblDirectory;

		private ComboBox cmbFileType;

		private Label label2;

		private ComboBox cmbPlotSize;

		private Label label3;

		private ListBox lstStarErrors;

		private Button cmdStarPositions;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripMenuItem withListToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem;

		private Label label4;

		private GroupBox grpStarPos;

		private ToolStripMenuItem printToolStripMenuItem;

		private Label label6;

		private CheckBox chkPDSWebPage;

		private Label label7;

		private CheckBox chkColorPaths;

		private CheckBox chkBackColor;

		private RadioButton optByClass;

		private ComboBox cmbAsteroidClasses;

		private CheckBox chkShapeModelsOnly;

		private CheckBox chkWhenVisible;

		private Label label5;

		private CheckBox chkSaveAstrometry;

		private Label lblPlotSettings;

		private CheckBox chkPredictedPath;

		public ReductionPlots(int Type)
		{
			InitializeComponent();
			for (int i = 2000; i <= DateTime.Today.Year; i++)
			{
				cmbYearRange.get_Items().Insert(0, (object)i.ToString());
			}
			((ListControl)cmbNumberRange).set_SelectedIndex(0);
			((ListControl)cmbAlpha).set_SelectedIndex(0);
			((ListControl)cmbYearRange).set_SelectedIndex(0);
			((ListControl)cmbFileType).set_SelectedIndex(3);
			((ListControl)cmbPlotSize).set_SelectedIndex(1);
			((ListControl)cmbAsteroidClasses).set_SelectedIndex(0);
			switch (Type)
			{
			case 0:
				((Control)grpGenerate).set_Visible(true);
				((Control)grpGenerate).set_Top(45);
				((Control)grpGenerate).set_Left(360);
				((ToolStripItem)withListToolStripMenuItem).set_Visible(false);
				((Control)this).set_Width(670);
				break;
			case 1:
				((Control)grpStarPos).set_Visible(true);
				((Control)grpStarPos).set_Top(45);
				((Control)grpStarPos).set_Left(360);
				((ToolStripItem)withListToolStripMenuItem).set_Visible(true);
				((Control)this).set_Width(740);
				break;
			}
			Kind = Type;
			Loaded = true;
		}

		private void ReductionPlots_Load(object sender, EventArgs e)
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
			DisplayIndex();
			if (Data_and_Plots.Historical_AllEvents.OccEvents.Count < 1)
			{
				Data_and_Plots.Historical_AllEvents.ReadObservationsFile(HistoricalFile: true, "");
			}
			((Control)lblDirectory).set_Text(SaveLocation);
			if (!Directory.Exists(SaveLocation))
			{
				Directory.CreateDirectory(SaveLocation);
			}
			TotalNumRecords = Data_and_Plots.Historical_AllEvents.OccEvents.Count;
		}

		private void cmdGeneratePlots_Click(object sender, EventArgs e)
		{
			//IL_0069: Unknown result type (might be due to invalid IL or missing references)
			//IL_006f: Invalid comparison between Unknown and I4
			//IL_0211: Unknown result type (might be due to invalid IL or missing references)
			//IL_0217: Invalid comparison between Unknown and I4
			int num = 1;
			if (optPoor.get_Checked())
			{
				num = 2;
			}
			if (optGood.get_Checked())
			{
				num = 3;
			}
			if (optExcellent.get_Checked())
			{
				num = 4;
			}
			Data_and_Plots.PDSPlots = chkPDSWebPage.get_Checked();
			if (Data_and_Plots.Observations_Editor != null && ((Control)Data_and_Plots.Observations_Editor).get_Visible())
			{
				if ((int)MessageBox.Show("To run this option, the asteroid observations \r\neditor will be closed.\r\n\r\nDo you want to continue?", "Editor open", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
				{
					((Form)Data_and_Plots.PlotForm).Close();
					return;
				}
				((Form)Data_and_Plots.Observations_Editor).Close();
				if (((Control)Data_and_Plots.PlotForm).get_Visible())
				{
					((Form)Data_and_Plots.PlotForm).Close();
				}
			}
			if (Directory.GetFiles(SaveLocation).Length != 0)
			{
				int num2 = Directory.GetFiles(SaveLocation, "*.jpg").Length;
				int num3 = Directory.GetFiles(SaveLocation, "*.bmp").Length;
				int num4 = Directory.GetFiles(SaveLocation, "*.gif").Length;
				int num5 = Directory.GetFiles(SaveLocation, "*.png").Length;
				int num6 = Directory.GetFiles(SaveLocation, "*.tif").Length;
				int num7 = Directory.GetFiles(SaveLocation, "*.txt").Length;
				string text = "The output directory :\r\n     " + SaveLocation + "\r\ncontains the following number of files, by file extension:\r\n\r\n";
				if (num2 > 0)
				{
					text += $"jpg :  {num2}\r\n";
				}
				if (num3 > 0)
				{
					text += $"bmp :  {num3}\r\n";
				}
				if (num4 > 0)
				{
					text += $"gif :  {num4}\r\n";
				}
				if (num5 > 0)
				{
					text += $"png :  {num5}\r\n";
				}
				if (num6 > 0)
				{
					text += $"tif :  {num6}\r\n";
				}
				if (num7 > 0)
				{
					text += $"txt :  {num7}\r\n";
				}
				if ((int)MessageBox.Show(text + "\r\nIf you proceed\r\n*  existing files may be overwritten, and\r\n*  existing files that are not overwritten will remain\r\n\r\nDo you want to proceed?", "Non-empty output directory", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
				{
					return;
				}
			}
			Data_and_Plots.ShowPlotForm();
			switch (((ListControl)cmbPlotSize).get_SelectedIndex())
			{
			case 0:
				Data_and_Plots.PlotForm.ResizeToWidthOfImage(480);
				break;
			case 1:
				Data_and_Plots.PlotForm.ResizeToWidthOfImage(640);
				break;
			case 2:
				Data_and_Plots.PlotForm.ResizeToWidthOfImage(768);
				break;
			case 3:
				Data_and_Plots.PlotForm.ResizeToWidthOfImage(1024);
				break;
			}
			for (int i = 0; i < lstHistorical.get_Items().get_Count(); i++)
			{
				int startRecord = Asteroid_Observations_Reports.HistoricalIndex[i].StartRecord;
				if (Data_and_Plots.Historical_AllEvents.GetEventQuality(startRecord) < num)
				{
					continue;
				}
				Data_and_Plots.Historical_AllEvents.DecodeAnEvent_Into_EventDetails(startRecord);
				Data_and_Plots.Parallax = Data_and_Plots.Historical_AllEvents.GetParallax(startRecord, out Data_and_Plots.dParallax);
				Data_and_Plots.FirstTimePlot = true;
				Data_and_Plots.Observations_InitialisePlot_Using_EventDetails();
				Data_and_Plots.GetPlotParametersFromForm();
				Data_and_Plots.PlotForm.SliderScale.set_Value(0);
				Data_and_Plots.PlotForm.optyPlotNormal.set_Checked(true);
				Data_and_Plots.PlotForm.mnuBlackBackground.set_Checked(chkBackColor.get_Checked());
				Data_and_Plots.PlotForm.mnuShowPredicted.set_Checked(chkPredictedPath.get_Checked());
				Data_and_Plots.PlotForm.optBoth.set_Checked(true);
				Data_and_Plots.WhiteBackground = !chkBackColor.get_Checked();
				Data_and_Plots.GrayBackground = false;
				Data_and_Plots.PlotForm.mnuPathsInColour.set_Checked(Data_and_Plots.ShowPathsIncolour = chkColorPaths.get_Checked());
				Data_and_Plots.ShowVisible = chkWhenVisible.get_Checked();
				if (EventDetails.AsteroidHasSatellite)
				{
					Data_and_Plots.PlotForm.mnuAlign.set_Checked(false);
					double num8 = EventDetails.Satellites[Data_and_Plots.SelectedSatellite].SatelliteSeparation / 1000.0 / EventDetails.Parallax;
					if (num8 > 0.62)
					{
						num8 = 0.62;
					}
					Data_and_Plots.PlotForm.SliderScale.set_Value((int)(-800.0 * num8));
					Data_and_Plots.PlotForm.optPlotx5.set_Checked(true);
					Data_and_Plots.ShowAlign = false;
				}
				else
				{
					Data_and_Plots.PlotForm.mnuAlign.set_Checked(EventDetails.StarIsDouble);
				}
				Data_and_Plots.FirstTimePlot = true;
				Data_and_Plots.PlotEventOnScreen();
				string text2 = SaveLocation + "\\" + Data_and_Plots.Historical_AllEvents.SaveFileName(startRecord);
				switch (((ListControl)cmbFileType).get_SelectedIndex())
				{
				case 0:
					Data_and_Plots.PlotForm.picPlot.get_Image().Save(text2 + ".jpg", ImageFormat.Jpeg);
					break;
				case 1:
					Data_and_Plots.PlotForm.picPlot.get_Image().Save(text2 + ".bmp", ImageFormat.Bmp);
					break;
				case 2:
					Data_and_Plots.PlotForm.picPlot.get_Image().Save(text2 + ".gif", ImageFormat.Gif);
					break;
				case 3:
					Data_and_Plots.PlotForm.picPlot.get_Image().Save(text2 + ".png", ImageFormat.Png);
					break;
				case 4:
					Data_and_Plots.PlotForm.picPlot.get_Image().Save(text2 + ".tif", ImageFormat.Tiff);
					break;
				}
				if (chkSaveAstrometry.get_Checked() && ((ListControl)Data_and_Plots.PlotForm.cmbQuality).get_SelectedIndex() > 1)
				{
					Asteroid_Observations_Reports.DisplayAstrometricSolution(IncludeConjunctionSolution: false, ReDoUpdate: false);
					string text3 = ((Control)Asteroid_Observations_Reports.DataBox.txtBox).get_Text();
					using StreamWriter streamWriter = new StreamWriter(text2 + ".txt");
					streamWriter.Write(text3);
				}
			}
			if (chkPDSWebPage.get_Checked())
			{
				using StreamWriter streamWriter2 = new StreamWriter(SaveLocation + "\\Graphics of selected occultations.html");
				string[] files = Directory.GetFiles(SaveLocation, "*.png");
				List<AsteroidID> list = new List<AsteroidID>();
				for (int j = 0; j < files.Length; j++)
				{
					AsteroidID asteroidID = new AsteroidID();
					asteroidID.ID = files[j];
					int num9 = files[j].IndexOf("_");
					int num10 = files[j].LastIndexOf("\\", num9);
					int result = 0;
					int.TryParse(files[j].Substring(num10 + 1, num9 - num10 - 1), out result);
					asteroidID.Number = result;
					list.Add(asteroidID);
				}
				list.Sort();
				string text4 = "The following graphics plot the " + files.Length + " best observed Asteroid occultations (which is about " + files.Length * 100 / TotalNumRecords + "% of all observed Asteroid occultation events).<p/>The plots have a filename convention concatenating the asteroid number, asteroid name or provisional designation, and occultation date.<p><h2>General features of the plot</strong></h2><ul><li>The plot is a sky-plane plot. North and East are indicated with 'N' and 'E'</li><li>The plotted chords represent when the star was visible</li><li>Chords where the star was occulted are drawn as an unbroken line. Chords corresponding to a 'Miss' observation are drawn as broken lines</li><li>The chords of each observer are drawn in different colors, to facilitate the visual matching of chords on opposite sides of the asteroid</li><li>For the best-observed events, the observations are drawn without any best-fit ellipse. This better enables the irregularities in the asteroid profile to be apparent.</li><li>For well-observed events, a best-fit ellipse is drawn through the observations. The ellipse illustrates the general shape of the asteroid profile.</li><li>A blue dot near the center of the plot indicates the location used to report astrometry from the event. It is typically located at the center of the fitted ellipse (whether the ellipse is shown or not). For some events the location is at the center of a fitted shape model (not shown), and represents the center of mass of the asteroid</li></ul><h2>Plot Heading</h2><p>The lines in the heading of each plot give the following information</p><strong>Line 1</strong><br>Object identification, event date, ellipse size and orientation, and associated uncertainties.<p/><strong>Line 2</strong><br>The offset of the shadow from the center of the earth, and associated uncertainty values.<p/><strong>Line 3</strong><br>When applicable, the Separation and Position Angle of the components of the double star, with associated uncertainty.<p/><strong>Important.</strong> In all lines, an uncertainty value is only given if there has been a meaningful calculation of the particular quantity. Where there is no uncertainty value, usually the particular value has been assumed.<p/><h2>Double stars</h2>If a double star is involved, the chords for both components are co-aligned. A representation of the double star solution, drawn on the same projected scale as the plot of the asteroid, is shown. When the double star solution is not unique, only one solution is displayed.<br><h2>Satellites</h2>If a satellite is involved, the plot shows both the main body and the satellite.</p><h2>Event coding</h2><h3>Event type</h3><ul><li>Disappearance events are plotted as red points.</li><li>Reappearance events are plotted as green points.</li><li>Miss events are plotted as broken lines.</li></ul><h3>Observing method</strong></h3><ul><li>Video/CCD observations are plotted with a small square.</li><li>Events recorded visually are plotted with a '+'</li><li>Miss events are plotted with a 'x'.</li></ul>";
				streamWriter2.Write("<head></head>\n");
				streamWriter2.Write("<body>\n");
				streamWriter2.Write("<h1>Graphics of selected Asteroid occultation observations.</h1>\n");
				streamWriter2.Write("<p><h2>" + DateTime.Now.Year + " " + CultureInfo.CurrentCulture.DateTimeFormat.GetMonthName(DateTime.Now.Month) + " </h2></p>\n");
				streamWriter2.Write("<h2>Introduction</h2>\n");
				streamWriter2.Write("<p>" + text4 + "</p>\n");
				streamWriter2.Write("<h2>Table of Images</h2>\n");
				streamWriter2.Write("<a id='toc'/>\n");
				for (int k = 0; k < list.Count; k++)
				{
					streamWriter2.Write("<div><a href='#" + Path.GetFileName(list[k].ID) + "'>" + Path.GetFileNameWithoutExtension(list[k].ID) + "</a></div>\n");
				}
				for (int l = 0; l < list.Count; l++)
				{
					streamWriter2.Write("<div style='page-break-before:always'><a id='" + Path.GetFileName(list[l].ID) + "'/><h2>" + Path.GetFileNameWithoutExtension(list[l].ID) + "</h2><img src='" + Path.GetFileName(list[l].ID) + "'/></div>\n");
				}
				streamWriter2.Write("</body>\n");
			}
			((Form)Data_and_Plots.PlotForm).Close();
		}

		private void cmbYearRange_SelectedIndexChanged(object sender, EventArgs e)
		{
			optByDate.set_Checked(true);
			DisplayIndex();
		}

		private void cmbNumberRange_SelectedIndexChanged(object sender, EventArgs e)
		{
			optByNumber.set_Checked(true);
			DisplayIndex();
		}

		private void cmbAlpha_SelectedIndexChanged(object sender, EventArgs e)
		{
			optByName.set_Checked(true);
			DisplayIndex();
		}

		private void cmbAsteroidClasses_SelectedIndexChanged(object sender, EventArgs e)
		{
			optByClass.set_Checked(true);
			DisplayIndex();
		}

		private void chkShapeModelsOnly_CheckedChanged(object sender, EventArgs e)
		{
			DisplayIndex();
		}

		private void optByDate_CheckedChanged(object sender, EventArgs e)
		{
			DisplayIndex();
		}

		private void optByName_CheckedChanged(object sender, EventArgs e)
		{
			DisplayIndex();
		}

		private void optByNumber_CheckedChanged(object sender, EventArgs e)
		{
			DisplayIndex();
		}

		private void optByClass_CheckedChanged(object sender, EventArgs e)
		{
			DisplayIndex();
		}

		private void optByRecords_CheckedChanged(object sender, EventArgs e)
		{
			DisplayIndex();
		}

		private void optByPlanets_CheckedChanged(object sender, EventArgs e)
		{
			DisplayIndex();
		}

		private void DisplayIndex()
		{
			int TotalNumber = 0;
			if (!Loaded)
			{
				return;
			}
			Asteroid_Observations_Reports.HistoricalIndex.Clear();
			string classTest = "X";
			if (optByClass.get_Checked())
			{
				if (AsteroidClassList.AClass.Count < 1)
				{
					AsteroidClassList.Fill_AllAsteroids();
				}
				classTest = cmbAsteroidClasses.get_Items().get_Item(((ListControl)cmbAsteroidClasses).get_SelectedIndex()).ToString();
			}
			if (Data_and_Plots.Historical_AllEvents.OccEvents.Count < 1)
			{
				Data_and_Plots.Historical_AllEvents.ReadObservationsFile(HistoricalFile: true, "");
			}
			for (int i = 0; i < Data_and_Plots.Historical_AllEvents.OccEvents.Count; i++)
			{
				AddToIndexList(Data_and_Plots.Historical_AllEvents.OccEvents[i].IndexLine + i.ToString().PadLeft(5), ref TotalNumber, classTest);
			}
			if (optByDate.get_Checked())
			{
				HistoricalIndexData.SortField = 2;
			}
			else if (optByName.get_Checked())
			{
				HistoricalIndexData.SortField = 0;
			}
			else if (optByNumber.get_Checked())
			{
				HistoricalIndexData.SortField = 1;
			}
			else if (optByClass.get_Checked())
			{
				if (((ListControl)cmbAsteroidClasses).get_SelectedIndex() < 0)
				{
					((ListControl)cmbAsteroidClasses).set_SelectedIndex(0);
				}
				HistoricalIndexData.SortField = 1;
			}
			else if (optByPlanets.get_Checked())
			{
				HistoricalIndexData.SortField = 4;
			}
			else
			{
				HistoricalIndexData.SortField = 3;
			}
			Asteroid_Observations_Reports.HistoricalIndex.Sort();
			lstHistorical.get_Items().Clear();
			for (int j = 0; j < Asteroid_Observations_Reports.HistoricalIndex.Count; j++)
			{
				lstHistorical.get_Items().Add((object)Asteroid_Observations_Reports.HistoricalIndex[j].ToString());
			}
			((Control)lblNumbers).set_Text(lstHistorical.get_Items().get_Count() + " / " + TotalNumber);
		}

		private void AddToIndexList(string IndexLine, ref int TotalNumber, string ClassTest)
		{
			HistoricalIndexData historicalIndexData = new HistoricalIndexData();
			historicalIndexData.ReadIndexLine(IndexLine);
			string text = cmbAlpha.get_Items().get_Item(((ListControl)cmbAlpha).get_SelectedIndex()).ToString()!.Trim();
			int num = cmbYearRange.get_Items().get_Count() - 1 - ((ListControl)cmbYearRange).get_SelectedIndex();
			int selectedIndex = ((ListControl)cmbNumberRange).get_SelectedIndex();
			TotalNumber++;
			if (chkShapeModelsOnly.get_Checked() & !historicalIndexData.HasShapeModel)
			{
				return;
			}
			if (optByDate.get_Checked())
			{
				switch (num)
				{
				case 0:
					Asteroid_Observations_Reports.HistoricalIndex.Add(historicalIndexData);
					break;
				case 1:
					if (historicalIndexData.Year < 1990)
					{
						Asteroid_Observations_Reports.HistoricalIndex.Add(historicalIndexData);
					}
					break;
				case 2:
					if ((historicalIndexData.Year > 1989) & (historicalIndexData.Year < 2000))
					{
						Asteroid_Observations_Reports.HistoricalIndex.Add(historicalIndexData);
					}
					break;
				default:
					if (historicalIndexData.Year == 1997 + num)
					{
						Asteroid_Observations_Reports.HistoricalIndex.Add(historicalIndexData);
					}
					break;
				}
			}
			else if (optByName.get_Checked())
			{
				if (text == "All")
				{
					Asteroid_Observations_Reports.HistoricalIndex.Add(historicalIndexData);
				}
				else if (text == historicalIndexData.AsteroidID.Substring(0, 1))
				{
					Asteroid_Observations_Reports.HistoricalIndex.Add(historicalIndexData);
				}
			}
			else if (optByNumber.get_Checked())
			{
				switch (selectedIndex)
				{
				case 0:
					Asteroid_Observations_Reports.HistoricalIndex.Add(historicalIndexData);
					break;
				case 1:
					if ((historicalIndexData.AsteroidNum > 0) & (historicalIndexData.AsteroidNum < 100))
					{
						Asteroid_Observations_Reports.HistoricalIndex.Add(historicalIndexData);
					}
					break;
				case 2:
					if ((historicalIndexData.AsteroidNum > 99) & (historicalIndexData.AsteroidNum < 200))
					{
						Asteroid_Observations_Reports.HistoricalIndex.Add(historicalIndexData);
					}
					break;
				case 3:
					if ((historicalIndexData.AsteroidNum > 199) & (historicalIndexData.AsteroidNum < 500))
					{
						Asteroid_Observations_Reports.HistoricalIndex.Add(historicalIndexData);
					}
					break;
				case 4:
					if ((historicalIndexData.AsteroidNum > 499) & (historicalIndexData.AsteroidNum < 1000))
					{
						Asteroid_Observations_Reports.HistoricalIndex.Add(historicalIndexData);
					}
					break;
				case 5:
					if (historicalIndexData.AsteroidNum > 999)
					{
						Asteroid_Observations_Reports.HistoricalIndex.Add(historicalIndexData);
					}
					break;
				default:
					if (historicalIndexData.AsteroidNum == 0)
					{
						Asteroid_Observations_Reports.HistoricalIndex.Add(historicalIndexData);
					}
					break;
				}
			}
			else if (optByClass.get_Checked())
			{
				if (AsteroidClassList.ClassOfAnAsteroid(historicalIndexData.AsteroidNum).Contains(ClassTest))
				{
					Asteroid_Observations_Reports.HistoricalIndex.Add(historicalIndexData);
				}
			}
			else if (optByPlanets.get_Checked())
			{
				if (historicalIndexData.AsteroidNumber.Contains("P"))
				{
					Asteroid_Observations_Reports.HistoricalIndex.Add(historicalIndexData);
				}
			}
			else
			{
				Asteroid_Observations_Reports.HistoricalIndex.Add(historicalIndexData);
			}
		}

		private void ReductionPlots_FormClosed(object sender, FormClosedEventArgs e)
		{
			Data_and_Plots.PDSPlots = false;
			((Component)this).Dispose();
		}

		private void cmdOutputDirectory_Click(object sender, EventArgs e)
		{
			//IL_0000: Unknown result type (might be due to invalid IL or missing references)
			//IL_0006: Expected O, but got Unknown
			//IL_0025: Unknown result type (might be due to invalid IL or missing references)
			//IL_002b: Invalid comparison between Unknown and I4
			FolderBrowserDialog val = new FolderBrowserDialog();
			val.set_Description("Select or create the directory that all the image files will be written to.");
			val.set_ShowNewFolderButton(true);
			val.set_SelectedPath(SaveLocation);
			if ((int)((CommonDialog)val).ShowDialog() == 1)
			{
				SaveLocation = val.get_SelectedPath();
			}
			((Control)lblDirectory).set_Text(SaveLocation);
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (((Control)grpGenerate).get_Visible())
			{
				Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Plot all Asteroid profiles");
			}
			else if (((Control)grpStarPos).get_Visible())
			{
				Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Poor star positions");
			}
		}

		private void cmdStarPositions_Click(object sender, EventArgs e)
		{
			string[] array = new string[10] { "Hip2", "GaiaDR1", "GaiaDR2", "GaiaEDR3", "GaiaDR3", "", "", "", "", "UBSC" };
			List<NonStars> list = new List<NonStars>();
			NonStars nonStars = new NonStars();
			double StarMag = 19.0;
			int num = 0;
			string AsteroidNumber = "";
			string AsteroidName = "";
			string SourceCode = "";
			if (((ListControl)cmbYearRange).get_SelectedIndex() != cmbYearRange.get_Items().get_Count() - 1)
			{
				((ListControl)cmbYearRange).set_SelectedIndex(cmbYearRange.get_Items().get_Count() - 1);
				optByDate.set_Checked(true);
				DisplayIndex();
			}
			lstStarErrors.get_Items().Clear();
			lstStarErrors.get_Items().Add((object)"Star positions not from Gaia EDR3");
			lstStarErrors.get_Items().Add((object)"");
			Cursor.set_Current(Cursors.get_WaitCursor());
			for (int i = 0; i < lstHistorical.get_Items().get_Count(); i++)
			{
				num = Asteroid_Observations_Reports.HistoricalIndex[i].StartRecord;
				Data_and_Plots.Historical_AllEvents.GetStarCoords(num, out var _, out var _, out SourceCode, out StarMag);
				if (SourceCode != "3")
				{
					Data_and_Plots.Historical_AllEvents.GetAsteroidID(num, out AsteroidNumber, out AsteroidName);
					nonStars = new NonStars();
					nonStars.Star = Data_and_Plots.Historical_AllEvents.GetStarID(num).PadRight(16);
					nonStars.Cat = Convert.ToInt32(SourceCode);
					nonStars.Line1 = Data_and_Plots.Historical_AllEvents.GetFormattedEventDate(num) + "  (" + AsteroidNumber.Trim() + ") " + AsteroidName;
					nonStars.Line2 = nonStars.Star + string.Format("   Mag = {0,2:f1}    ", StarMag) + array[Convert.ToInt32(SourceCode)];
					list.Add(nonStars);
				}
			}
			list.Sort();
			for (int j = 0; j < list.Count; j++)
			{
				lstStarErrors.get_Items().Add((object)(string.Format("{0,2}  ", j + 1) + list[j].Line1));
				lstStarErrors.get_Items().Add((object)list[j].Line2);
				lstStarErrors.get_Items().Add((object)"");
			}
			lstStarErrors.get_Items().Add((object)"Check completed");
			Cursor.set_Current(Cursors.get_Default());
		}

		private void copyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(CollectEvents());
		}

		private void printToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.PrintText(CollectEvents());
		}

		private string CollectEvents()
		{
			StringBuilder stringBuilder = new StringBuilder();
			if (((Control)grpStarPos).get_Visible())
			{
				int count = lstStarErrors.get_Items().get_Count();
				for (int i = 0; i < count; i++)
				{
					stringBuilder.AppendLine(lstStarErrors.get_Items().get_Item(i).ToString());
				}
			}
			stringBuilder.AppendLine("");
			return stringBuilder.ToString();
		}

		private void ReductionPlots_Resize(object sender, EventArgs e)
		{
			((Control)this).set_Width(740);
			GroupBox obj = grpHistory;
			GroupBox obj2 = grpGenerate;
			int num;
			((Control)grpStarPos).set_Height(num = ((Control)this).get_Height() - 88);
			int height;
			((Control)obj2).set_Height(height = num);
			((Control)obj).set_Height(height);
			((Control)lstHistorical).set_Height(((Control)this).get_Height() - 214);
			((Control)lstStarErrors).set_Height(((Control)this).get_Height() - 158);
		}

		private void chkPDSWebPage_MouseClick(object sender, MouseEventArgs e)
		{
			chkPDSWebPage.set_Checked(!chkPDSWebPage.get_Checked());
			if (chkPDSWebPage.get_Checked())
			{
				((Control)lblPlotSettings).set_Text("Plot settings are set automatically");
			}
			else
			{
				((Control)lblPlotSettings).set_Text("Uses plot settings from the plot form\r\nMake sure they are set for your needs");
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
			//IL_1938: Unknown result type (might be due to invalid IL or missing references)
			//IL_1942: Expected O, but got Unknown
			//IL_22f1: Unknown result type (might be due to invalid IL or missing references)
			//IL_22fb: Expected O, but got Unknown
			//IL_2363: Unknown result type (might be due to invalid IL or missing references)
			//IL_236d: Expected O, but got Unknown
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(ReductionPlots));
			cmdGeneratePlots = new Button();
			grpHistory = new GroupBox();
			chkShapeModelsOnly = new CheckBox();
			optByClass = new RadioButton();
			cmbAsteroidClasses = new ComboBox();
			lstHistorical = new ListBox();
			optByPlanets = new RadioButton();
			lblNumbers = new Label();
			optByRecords = new RadioButton();
			optByNumber = new RadioButton();
			optByName = new RadioButton();
			optByDate = new RadioButton();
			cmbNumberRange = new ComboBox();
			cmbAlpha = new ComboBox();
			cmbYearRange = new ComboBox();
			label1 = new Label();
			optExcellent = new RadioButton();
			optGood = new RadioButton();
			optPoor = new RadioButton();
			grpGenerate = new GroupBox();
			chkPredictedPath = new CheckBox();
			lblPlotSettings = new Label();
			label5 = new Label();
			chkSaveAstrometry = new CheckBox();
			chkWhenVisible = new CheckBox();
			chkColorPaths = new CheckBox();
			chkBackColor = new CheckBox();
			label7 = new Label();
			chkPDSWebPage = new CheckBox();
			label6 = new Label();
			label4 = new Label();
			optTime = new RadioButton();
			label3 = new Label();
			cmbPlotSize = new ComboBox();
			label2 = new Label();
			cmbFileType = new ComboBox();
			cmdOutputDirectory = new Button();
			lblDirectory = new Label();
			lstStarErrors = new ListBox();
			cmdStarPositions = new Button();
			menuStrip1 = new MenuStrip();
			withListToolStripMenuItem = new ToolStripMenuItem();
			copyToolStripMenuItem = new ToolStripMenuItem();
			printToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			grpStarPos = new GroupBox();
			((Control)grpHistory).SuspendLayout();
			((Control)grpGenerate).SuspendLayout();
			((Control)menuStrip1).SuspendLayout();
			((Control)grpStarPos).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)cmdGeneratePlots).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdGeneratePlots).set_Location(new Point(30, 471));
			((Control)cmdGeneratePlots).set_Name("cmdGeneratePlots");
			((Control)cmdGeneratePlots).set_Size(new Size(169, 35));
			((Control)cmdGeneratePlots).set_TabIndex(0);
			((Control)cmdGeneratePlots).set_Text("11. Create plots");
			((ButtonBase)cmdGeneratePlots).set_UseVisualStyleBackColor(true);
			((Control)cmdGeneratePlots).add_Click((EventHandler)cmdGeneratePlots_Click);
			((Control)grpHistory).get_Controls().Add((Control)(object)chkShapeModelsOnly);
			((Control)grpHistory).get_Controls().Add((Control)(object)optByClass);
			((Control)grpHistory).get_Controls().Add((Control)(object)cmbAsteroidClasses);
			((Control)grpHistory).get_Controls().Add((Control)(object)lstHistorical);
			((Control)grpHistory).get_Controls().Add((Control)(object)optByPlanets);
			((Control)grpHistory).get_Controls().Add((Control)(object)lblNumbers);
			((Control)grpHistory).get_Controls().Add((Control)(object)optByRecords);
			((Control)grpHistory).get_Controls().Add((Control)(object)optByNumber);
			((Control)grpHistory).get_Controls().Add((Control)(object)optByName);
			((Control)grpHistory).get_Controls().Add((Control)(object)optByDate);
			((Control)grpHistory).get_Controls().Add((Control)(object)cmbNumberRange);
			((Control)grpHistory).get_Controls().Add((Control)(object)cmbAlpha);
			((Control)grpHistory).get_Controls().Add((Control)(object)cmbYearRange);
			((Control)grpHistory).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpHistory).set_Location(new Point(23, 57));
			((Control)grpHistory).set_Name("grpHistory");
			((Control)grpHistory).set_Size(new Size(318, 517));
			((Control)grpHistory).set_TabIndex(5);
			grpHistory.set_TabStop(false);
			((Control)grpHistory).set_Text("Select events for output");
			((Control)chkShapeModelsOnly).set_AutoSize(true);
			((Control)chkShapeModelsOnly).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Italic, GraphicsUnit.Point, 0));
			((Control)chkShapeModelsOnly).set_Location(new Point(205, 13));
			((Control)chkShapeModelsOnly).set_Name("chkShapeModelsOnly");
			((Control)chkShapeModelsOnly).set_Size(new Size(94, 30));
			((Control)chkShapeModelsOnly).set_TabIndex(13);
			((Control)chkShapeModelsOnly).set_Text("Limit to\r\nShape Models");
			((ButtonBase)chkShapeModelsOnly).set_TextAlign(ContentAlignment.MiddleCenter);
			((ButtonBase)chkShapeModelsOnly).set_UseVisualStyleBackColor(true);
			chkShapeModelsOnly.add_CheckedChanged((EventHandler)chkShapeModelsOnly_CheckedChanged);
			((Control)optByClass).set_AutoSize(true);
			optByClass.set_CheckAlign(ContentAlignment.MiddleRight);
			((Control)optByClass).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optByClass).set_Location(new Point(20, 94));
			((Control)optByClass).set_Name("optByClass");
			((Control)optByClass).set_Size(new Size(64, 17));
			((Control)optByClass).set_TabIndex(11);
			((Control)optByClass).set_Text("by Class");
			((ButtonBase)optByClass).set_UseVisualStyleBackColor(true);
			optByClass.add_CheckedChanged((EventHandler)optByClass_CheckedChanged);
			cmbAsteroidClasses.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbAsteroidClasses).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbAsteroidClasses).set_FormattingEnabled(true);
			cmbAsteroidClasses.get_Items().AddRange(new object[7] { "Amor", "Apollo", "Aten", "Centaur", "PHA", "TNO", "Trojan" });
			((Control)cmbAsteroidClasses).set_Location(new Point(93, 91));
			cmbAsteroidClasses.set_MaxDropDownItems(20);
			((Control)cmbAsteroidClasses).set_Name("cmbAsteroidClasses");
			((Control)cmbAsteroidClasses).set_Size(new Size(95, 22));
			((Control)cmbAsteroidClasses).set_TabIndex(12);
			cmbAsteroidClasses.add_SelectedIndexChanged((EventHandler)cmbAsteroidClasses_SelectedIndexChanged);
			((Control)lstHistorical).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstHistorical).set_FormattingEnabled(true);
			lstHistorical.set_ItemHeight(14);
			((Control)lstHistorical).set_Location(new Point(10, 118));
			((Control)lstHistorical).set_Name("lstHistorical");
			((Control)lstHistorical).set_Size(new Size(298, 382));
			((Control)lstHistorical).set_TabIndex(10);
			((Control)optByPlanets).set_AutoSize(true);
			optByPlanets.set_CheckAlign(ContentAlignment.MiddleRight);
			((Control)optByPlanets).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optByPlanets).set_Location(new Point(206, 66));
			((Control)optByPlanets).set_Name("optByPlanets");
			((Control)optByPlanets).set_Size(new Size(82, 17));
			((Control)optByPlanets).set_TabIndex(9);
			((Control)optByPlanets).set_Text("Planets only");
			((ButtonBase)optByPlanets).set_UseVisualStyleBackColor(true);
			optByPlanets.add_CheckedChanged((EventHandler)optByPlanets_CheckedChanged);
			((Control)lblNumbers).set_AutoSize(true);
			((Control)lblNumbers).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblNumbers).set_Location(new Point(203, 101));
			((Control)lblNumbers).set_Name("lblNumbers");
			((Control)lblNumbers).set_Size(new Size(28, 15));
			((Control)lblNumbers).set_TabIndex(8);
			((Control)lblNumbers).set_Text("A/B");
			((Control)optByRecords).set_AutoSize(true);
			optByRecords.set_CheckAlign(ContentAlignment.MiddleRight);
			((Control)optByRecords).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optByRecords).set_Location(new Point(202, 44));
			((Control)optByRecords).set_Name("optByRecords");
			((Control)optByRecords).set_Size(new Size(86, 17));
			((Control)optByRecords).set_TabIndex(7);
			((Control)optByRecords).set_Text("by #Records");
			((ButtonBase)optByRecords).set_UseVisualStyleBackColor(true);
			optByRecords.add_CheckedChanged((EventHandler)optByRecords_CheckedChanged);
			((Control)optByNumber).set_AutoSize(true);
			optByNumber.set_CheckAlign(ContentAlignment.MiddleRight);
			((Control)optByNumber).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optByNumber).set_Location(new Point(8, 72));
			((Control)optByNumber).set_Name("optByNumber");
			((Control)optByNumber).set_Size(new Size(76, 17));
			((Control)optByNumber).set_TabIndex(5);
			((Control)optByNumber).set_Text("by Number");
			((ButtonBase)optByNumber).set_UseVisualStyleBackColor(true);
			optByNumber.add_CheckedChanged((EventHandler)optByNumber_CheckedChanged);
			((Control)optByName).set_AutoSize(true);
			optByName.set_CheckAlign(ContentAlignment.MiddleRight);
			((Control)optByName).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optByName).set_Location(new Point(17, 50));
			((Control)optByName).set_Name("optByName");
			((Control)optByName).set_Size(new Size(67, 17));
			((Control)optByName).set_TabIndex(3);
			((Control)optByName).set_Text("by Name");
			((ButtonBase)optByName).set_UseVisualStyleBackColor(true);
			optByName.add_CheckedChanged((EventHandler)optByName_CheckedChanged);
			((Control)optByDate).set_AutoSize(true);
			optByDate.set_CheckAlign(ContentAlignment.MiddleRight);
			optByDate.set_Checked(true);
			((Control)optByDate).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optByDate).set_Location(new Point(22, 28));
			((Control)optByDate).set_Name("optByDate");
			((Control)optByDate).set_Size(new Size(62, 17));
			((Control)optByDate).set_TabIndex(1);
			optByDate.set_TabStop(true);
			((Control)optByDate).set_Text("by Date");
			((ButtonBase)optByDate).set_UseVisualStyleBackColor(true);
			optByDate.add_CheckedChanged((EventHandler)optByDate_CheckedChanged);
			cmbNumberRange.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbNumberRange).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbNumberRange).set_FormattingEnabled(true);
			cmbNumberRange.get_Items().AddRange(new object[7] { "All", "1 - 99", "100- 199", "200 - 499", "500 - 999", ">1000", "Planets/Moons" });
			((Control)cmbNumberRange).set_Location(new Point(93, 69));
			cmbNumberRange.set_MaxDropDownItems(20);
			((Control)cmbNumberRange).set_Name("cmbNumberRange");
			((Control)cmbNumberRange).set_Size(new Size(95, 22));
			((Control)cmbNumberRange).set_TabIndex(6);
			cmbNumberRange.add_SelectedIndexChanged((EventHandler)cmbNumberRange_SelectedIndexChanged);
			cmbAlpha.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbAlpha).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbAlpha).set_FormattingEnabled(true);
			cmbAlpha.get_Items().AddRange(new object[37]
			{
				"All", "A", "B", "C", "D", "E", "F", "G", "H", "I",
				"J", "K", "L", "M", "N", "O", "P", "Q", "R", "S",
				"T", "U", "V", "W", "X", "Y", "Z", "1", "2", "3",
				"4", "5", "6", "7", "8", "9", "0"
			});
			((Control)cmbAlpha).set_Location(new Point(93, 47));
			cmbAlpha.set_MaxDropDownItems(30);
			((Control)cmbAlpha).set_Name("cmbAlpha");
			((Control)cmbAlpha).set_Size(new Size(47, 22));
			((Control)cmbAlpha).set_TabIndex(4);
			cmbAlpha.add_SelectedIndexChanged((EventHandler)cmbAlpha_SelectedIndexChanged);
			cmbYearRange.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbYearRange).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbYearRange).set_FormattingEnabled(true);
			cmbYearRange.get_Items().AddRange(new object[3] { "1990-1999", "1950-1989", "All" });
			((Control)cmbYearRange).set_Location(new Point(93, 25));
			cmbYearRange.set_MaxDropDownItems(20);
			((Control)cmbYearRange).set_Name("cmbYearRange");
			((Control)cmbYearRange).set_Size(new Size(88, 22));
			((Control)cmbYearRange).set_TabIndex(2);
			cmbYearRange.add_SelectedIndexChanged((EventHandler)cmbYearRange_SelectedIndexChanged);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(9, 36));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(42, 13));
			((Control)label1).set_TabIndex(3);
			((Control)label1).set_Text("Include");
			((Control)optExcellent).set_AutoSize(true);
			((Control)optExcellent).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optExcellent).set_Location(new Point(25, 101));
			((Control)optExcellent).set_Name("optExcellent");
			((Control)optExcellent).set_Size(new Size(197, 17));
			((Control)optExcellent).set_TabIndex(2);
			((Control)optExcellent).set_Text("Resolution better than shape models");
			((ButtonBase)optExcellent).set_UseVisualStyleBackColor(true);
			((Control)optGood).set_AutoSize(true);
			optGood.set_Checked(true);
			((Control)optGood).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optGood).set_Location(new Point(25, 84));
			((Control)optGood).set_Name("optGood");
			((Control)optGood).set_Size(new Size(200, 17));
			((Control)optGood).set_TabIndex(1);
			optGood.set_TabStop(true);
			((Control)optGood).set_Text("Reliable size. Can fit to shape models");
			((ButtonBase)optGood).set_UseVisualStyleBackColor(true);
			((Control)optPoor).set_AutoSize(true);
			((Control)optPoor).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optPoor).set_Location(new Point(25, 67));
			((Control)optPoor).set_Name("optPoor");
			((Control)optPoor).set_Size(new Size(155, 17));
			((Control)optPoor).set_TabIndex(0);
			((Control)optPoor).set_Text("Limits on size, but no shape");
			((ButtonBase)optPoor).set_UseVisualStyleBackColor(true);
			((Control)grpGenerate).get_Controls().Add((Control)(object)chkPredictedPath);
			((Control)grpGenerate).get_Controls().Add((Control)(object)lblPlotSettings);
			((Control)grpGenerate).get_Controls().Add((Control)(object)label5);
			((Control)grpGenerate).get_Controls().Add((Control)(object)chkSaveAstrometry);
			((Control)grpGenerate).get_Controls().Add((Control)(object)chkWhenVisible);
			((Control)grpGenerate).get_Controls().Add((Control)(object)chkColorPaths);
			((Control)grpGenerate).get_Controls().Add((Control)(object)chkBackColor);
			((Control)grpGenerate).get_Controls().Add((Control)(object)label7);
			((Control)grpGenerate).get_Controls().Add((Control)(object)chkPDSWebPage);
			((Control)grpGenerate).get_Controls().Add((Control)(object)label6);
			((Control)grpGenerate).get_Controls().Add((Control)(object)label4);
			((Control)grpGenerate).get_Controls().Add((Control)(object)optTime);
			((Control)grpGenerate).get_Controls().Add((Control)(object)label1);
			((Control)grpGenerate).get_Controls().Add((Control)(object)label3);
			((Control)grpGenerate).get_Controls().Add((Control)(object)optGood);
			((Control)grpGenerate).get_Controls().Add((Control)(object)cmbPlotSize);
			((Control)grpGenerate).get_Controls().Add((Control)(object)optExcellent);
			((Control)grpGenerate).get_Controls().Add((Control)(object)label2);
			((Control)grpGenerate).get_Controls().Add((Control)(object)optPoor);
			((Control)grpGenerate).get_Controls().Add((Control)(object)cmbFileType);
			((Control)grpGenerate).get_Controls().Add((Control)(object)cmdOutputDirectory);
			((Control)grpGenerate).get_Controls().Add((Control)(object)lblDirectory);
			((Control)grpGenerate).get_Controls().Add((Control)(object)cmdGeneratePlots);
			((Control)grpGenerate).set_Font(new Font("Microsoft Sans Serif", 9.75f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpGenerate).set_Location(new Point(353, 57));
			((Control)grpGenerate).set_Name("grpGenerate");
			((Control)grpGenerate).set_Size(new Size(228, 517));
			((Control)grpGenerate).set_TabIndex(12);
			grpGenerate.set_TabStop(false);
			((Control)grpGenerate).set_Text("Generate graphic files");
			((Control)grpGenerate).set_Visible(false);
			((Control)chkPredictedPath).set_AutoSize(true);
			((Control)chkPredictedPath).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)chkPredictedPath).set_Location(new Point(8, 397));
			((Control)chkPredictedPath).set_Name("chkPredictedPath");
			((Control)chkPredictedPath).set_Size(new Size(169, 17));
			((Control)chkPredictedPath).set_TabIndex(108);
			((Control)chkPredictedPath).set_Text("9. Include predicted path");
			((ButtonBase)chkPredictedPath).set_UseVisualStyleBackColor(true);
			((Control)lblPlotSettings).set_AutoSize(true);
			((Control)lblPlotSettings).set_BackColor(Color.MediumBlue);
			lblPlotSettings.set_BorderStyle((BorderStyle)2);
			((Control)lblPlotSettings).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblPlotSettings).set_ForeColor(Color.Gold);
			((Control)lblPlotSettings).set_Location(new Point(9, 437));
			((Control)lblPlotSettings).set_Name("lblPlotSettings");
			((Control)lblPlotSettings).set_Size(new Size(190, 17));
			((Control)lblPlotSettings).set_TabIndex(107);
			((Control)lblPlotSettings).set_Text("Plot settings are set automatically");
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label5).set_Location(new Point(7, 126));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(18, 13));
			((Control)label5).set_TabIndex(106);
			((Control)label5).set_Text("2.");
			((Control)chkSaveAstrometry).set_AutoSize(true);
			((Control)chkSaveAstrometry).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)chkSaveAstrometry).set_Location(new Point(25, 125));
			((Control)chkSaveAstrometry).set_Name("chkSaveAstrometry");
			((Control)chkSaveAstrometry).set_Size(new Size(195, 17));
			((Control)chkSaveAstrometry).set_TabIndex(105);
			((Control)chkSaveAstrometry).set_Text("also save Astrometric solution");
			((ButtonBase)chkSaveAstrometry).set_UseVisualStyleBackColor(true);
			((Control)chkWhenVisible).set_AutoSize(true);
			chkWhenVisible.set_Checked(true);
			chkWhenVisible.set_CheckState((CheckState)1);
			((Control)chkWhenVisible).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)chkWhenVisible).set_Location(new Point(8, 357));
			((Control)chkWhenVisible).set_Name("chkWhenVisible");
			((Control)chkWhenVisible).set_Size(new Size(172, 17));
			((Control)chkWhenVisible).set_TabIndex(104);
			((Control)chkWhenVisible).set_Text("7. Plot paths when visible");
			((ButtonBase)chkWhenVisible).set_UseVisualStyleBackColor(true);
			((Control)chkColorPaths).set_AutoSize(true);
			chkColorPaths.set_Checked(true);
			chkColorPaths.set_CheckState((CheckState)1);
			((Control)chkColorPaths).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)chkColorPaths).set_Location(new Point(8, 377));
			((Control)chkColorPaths).set_Name("chkColorPaths");
			((Control)chkColorPaths).set_Size(new Size(144, 17));
			((Control)chkColorPaths).set_TabIndex(103);
			((Control)chkColorPaths).set_Text("8. Plot paths in color");
			((ButtonBase)chkColorPaths).set_UseVisualStyleBackColor(true);
			((Control)chkBackColor).set_AutoSize(true);
			chkBackColor.set_Checked(true);
			chkBackColor.set_CheckState((CheckState)1);
			((Control)chkBackColor).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)chkBackColor).set_Location(new Point(8, 337));
			((Control)chkBackColor).set_Name("chkBackColor");
			((Control)chkBackColor).set_Size(new Size(188, 17));
			((Control)chkBackColor).set_TabIndex(102);
			((Control)chkBackColor).set_Text("6. Plot on Black background");
			((ButtonBase)chkBackColor).set_UseVisualStyleBackColor(true);
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Font(new Font("Microsoft Sans Serif", 8.75f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label7).set_Location(new Point(12, 316));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(196, 15));
			((Control)label7).set_TabIndex(101);
			((Control)label7).set_Text("640x640 avoids text overlaying plot");
			chkPDSWebPage.set_AutoCheck(false);
			((Control)chkPDSWebPage).set_AutoSize(true);
			chkPDSWebPage.set_Checked(true);
			chkPDSWebPage.set_CheckState((CheckState)1);
			((Control)chkPDSWebPage).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)chkPDSWebPage).set_Location(new Point(8, 417));
			((Control)chkPDSWebPage).set_Name("chkPDSWebPage");
			((Control)chkPDSWebPage).set_Size(new Size(214, 17));
			((Control)chkPDSWebPage).set_TabIndex(100);
			((Control)chkPDSWebPage).set_Text("10. Create PDS files + Web page");
			((ButtonBase)chkPDSWebPage).set_UseVisualStyleBackColor(true);
			((Control)chkPDSWebPage).add_MouseClick(new MouseEventHandler(chkPDSWebPage_MouseClick));
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Font(new Font("Microsoft Sans Serif", 8.75f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label6).set_Location(new Point(23, 254));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(141, 15));
			((Control)label6).set_TabIndex(99);
			((Control)label6).set_Text("NASA PDS requires .png");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label4).set_Location(new Point(6, 21));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(132, 13));
			((Control)label4).set_TabIndex(98);
			((Control)label4).set_Text("1. Set quality criterion");
			((Control)optTime).set_AutoSize(true);
			((Control)optTime).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optTime).set_Location(new Point(25, 50));
			((Control)optTime).set_Name("optTime");
			((Control)optTime).set_Size(new Size(173, 17));
			((Control)optTime).set_TabIndex(4);
			((Control)optTime).set_Text("Astrometry only. No reliable size");
			((ButtonBase)optTime).set_UseVisualStyleBackColor(true);
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label3).set_Location(new Point(6, 280));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(104, 26));
			((Control)label3).set_TabIndex(97);
			((Control)label3).set_Text("5. Set image size\r\n   (pixels)");
			label3.set_TextAlign(ContentAlignment.TopRight);
			cmbPlotSize.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbPlotSize).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbPlotSize).set_FormattingEnabled(true);
			cmbPlotSize.get_Items().AddRange(new object[4] { "480 x 480", "640 x 640", "768 x 768", "1024 x 1024" });
			((Control)cmbPlotSize).set_Location(new Point(111, 283));
			((Control)cmbPlotSize).set_Name("cmbPlotSize");
			((Control)cmbPlotSize).set_Size(new Size(93, 21));
			((Control)cmbPlotSize).set_TabIndex(96);
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label2).set_Location(new Point(6, 236));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(101, 13));
			((Control)label2).set_TabIndex(95);
			((Control)label2).set_Text("4. Set file format");
			cmbFileType.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbFileType).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbFileType).set_FormattingEnabled(true);
			cmbFileType.get_Items().AddRange(new object[5] { "JPEG", "BMP", "GIF", "PNG", "TIFF" });
			((Control)cmbFileType).set_Location(new Point(111, 233));
			((Control)cmbFileType).set_Name("cmbFileType");
			((Control)cmbFileType).set_Size(new Size(65, 21));
			((Control)cmbFileType).set_TabIndex(94);
			((Control)cmdOutputDirectory).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdOutputDirectory).set_Location(new Point(9, 151));
			((Control)cmdOutputDirectory).set_Name("cmdOutputDirectory");
			((Control)cmdOutputDirectory).set_Size(new Size(169, 27));
			((Control)cmdOutputDirectory).set_TabIndex(13);
			((Control)cmdOutputDirectory).set_Text("3. Set output directory");
			((ButtonBase)cmdOutputDirectory).set_UseVisualStyleBackColor(true);
			((Control)cmdOutputDirectory).add_Click((EventHandler)cmdOutputDirectory_Click);
			((Control)lblDirectory).set_Font(new Font("Microsoft Sans Serif", 6.75f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblDirectory).set_Location(new Point(9, 179));
			((Control)lblDirectory).set_Name("lblDirectory");
			((Control)lblDirectory).set_Size(new Size(195, 44));
			((Control)lblDirectory).set_TabIndex(14);
			((Control)lblDirectory).set_Text("label2");
			((Control)lstStarErrors).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstStarErrors).set_FormattingEnabled(true);
			lstStarErrors.set_ItemHeight(14);
			((Control)lstStarErrors).set_Location(new Point(10, 60));
			((Control)lstStarErrors).set_Name("lstStarErrors");
			((Control)lstStarErrors).set_Size(new Size(339, 438));
			((Control)lstStarErrors).set_TabIndex(99);
			((Control)cmdStarPositions).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdStarPositions).set_Location(new Point(104, 26));
			((Control)cmdStarPositions).set_Name("cmdStarPositions");
			((Control)cmdStarPositions).set_Size(new Size(121, 27));
			((Control)cmdStarPositions).set_TabIndex(100);
			((Control)cmdStarPositions).set_Text("List stars/events");
			((ButtonBase)cmdStarPositions).set_UseVisualStyleBackColor(true);
			((Control)cmdStarPositions).add_Click((EventHandler)cmdStarPositions_Click);
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)withListToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(1000, 24));
			((Control)menuStrip1).set_TabIndex(101);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withListToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)printToolStripMenuItem
			});
			((ToolStripItem)withListToolStripMenuItem).set_Name("withListToolStripMenuItem");
			((ToolStripItem)withListToolStripMenuItem).set_Size(new Size(63, 20));
			((ToolStripItem)withListToolStripMenuItem).set_Text("with List");
			((ToolStripItem)copyToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyToolStripMenuItem).set_Name("copyToolStripMenuItem");
			copyToolStripMenuItem.set_ShortcutKeys((Keys)131139);
			((ToolStripItem)copyToolStripMenuItem).set_Size(new Size(144, 22));
			((ToolStripItem)copyToolStripMenuItem).set_Text("Copy");
			((ToolStripItem)copyToolStripMenuItem).add_Click((EventHandler)copyToolStripMenuItem_Click);
			((ToolStripItem)printToolStripMenuItem).set_Image((Image)Resources.Print);
			((ToolStripItem)printToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printToolStripMenuItem).set_Name("printToolStripMenuItem");
			printToolStripMenuItem.set_ShortcutKeys((Keys)131152);
			((ToolStripItem)printToolStripMenuItem).set_Size(new Size(144, 22));
			((ToolStripItem)printToolStripMenuItem).set_Text("Print");
			((ToolStripItem)printToolStripMenuItem).add_Click((EventHandler)printToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(60, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("Help");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((Control)grpStarPos).get_Controls().Add((Control)(object)cmdStarPositions);
			((Control)grpStarPos).get_Controls().Add((Control)(object)lstStarErrors);
			((Control)grpStarPos).set_Font(new Font("Microsoft Sans Serif", 9.75f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpStarPos).set_Location(new Point(593, 57));
			((Control)grpStarPos).set_Name("grpStarPos");
			((Control)grpStarPos).set_Size(new Size(358, 517));
			((Control)grpStarPos).set_TabIndex(102);
			grpStarPos.set_TabStop(false);
			((Control)grpStarPos).set_Text("List of stars not sourced from Gaia DR3 or EDR3");
			((Control)grpStarPos).set_Visible(false);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(1000, 581));
			((Control)this).get_Controls().Add((Control)(object)grpStarPos);
			((Control)this).get_Controls().Add((Control)(object)grpGenerate);
			((Control)this).get_Controls().Add((Control)(object)grpHistory);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationAsterReductionPlots", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_Icon((Icon)componentResourceManager.GetObject("$this.Icon"));
			((Form)this).set_Location(Settings.Default.LocationAsterReductionPlots);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_Name("ReductionPlots");
			((Control)this).set_Text("Reduction plots");
			((Form)this).add_FormClosed(new FormClosedEventHandler(ReductionPlots_FormClosed));
			((Form)this).add_Load((EventHandler)ReductionPlots_Load);
			((Control)this).add_Resize((EventHandler)ReductionPlots_Resize);
			((Control)grpHistory).ResumeLayout(false);
			((Control)grpHistory).PerformLayout();
			((Control)grpGenerate).ResumeLayout(false);
			((Control)grpGenerate).PerformLayout();
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)grpStarPos).ResumeLayout(false);
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
