using System;
using System.ComponentModel;
using System.Diagnostics;
using System.Drawing;
using System.IO;
using System.Windows.Forms;
using GaiaDoubles;
using Occult.Asteroid_Observations;
using Occult.Asteroid_Predictions;
using Occult.Asteroids;
using Occult.File_Actions;
using Occult.Mapping;
using Occult.Properties;
using Occult.Star_Catalogues;

namespace Occult
{
	public class AsteroidPlotPath : Form
	{
		private readonly string AppPath;

		public static ToolStripMenuItem[] ScaleChecks;

		private static bool ShowToolTip;

		private static bool FormStarted;

		private bool GetToolTip = true;

		private double Longitude;

		private double Latitude;

		private const double Radian = 180.0 / Math.PI;

		private DisplayData DataBox;

		private static DisplayData NearbyStars;

		private PredictionLightCurve PredictionLightCurves;

		private IContainer components;

		public PictureBox picAsteroidPlot;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem nextEventToolStripMenuItem;

		private ToolStripMenuItem previousEventToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private ToolStripMenuItem redrawToolStripMenuItem;

		public ToolStripMenuItem scale01ToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator1;

		public ToolStripMenuItem scale1ToolStripMenuItem;

		public ToolStripMenuItem scale2ToolStripMenuItem;

		public ToolStripMenuItem scale4ToolStripMenuItem;

		public ToolStripMenuItem scale8ToolStripMenuItem;

		public ToolStripMenuItem scale16ToolStripMenuItem;

		public ToolStripMenuItem scale32ToolStripMenuItem;

		public ToolStripMenuItem scale001100ToolStripMenuItem;

		private ToolStripMenuItem withThisEventToolStripMenuItem;

		private ToolStripMenuItem pathCoordinatesToolStripMenuItem;

		public ToolStripMenuItem plotToolStripMenuItem;

		private Panel PanelExport;

		private Button cmdExportG;

		private Button cmdExportF;

		private Button cmdExportE;

		private Button cmdExportD;

		private Button cmdExportC;

		private Button cmdExportB;

		private Button cmdExportA;

		private ToolStripMenuItem optionsToolStripMenuItem;

		private ToolStripMenuItem exportButtonsDisplayedToolStripMenuItem;

		public ToolStripMenuItem colourToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator2;

		public NumericUpDown updnLatitude;

		private Label label1;

		private Label label2;

		public NumericUpDown updnLongitude;

		private ToolStripMenuItem includeStarChartToolStripMenuItem;

		private ToolStripMenuItem largeStarChartToolStripMenuItem;

		private Button cmdThis;

		public ToolStripMenuItem showPlanetConfigurationToolStripMenuItem;

		public Button cmdNext;

		public Button cmdPrevious;

		private ToolStripSeparator toolStripSeparator3;

		private Button cmdx2;

		private Button cmdx4;

		private Button cmdx8;

		private Button cmdx16;

		private Button cmdx32;

		private Button cmdx01;

		private Label label3;

		private ToolStripMenuItem showErrorEllipseToolStripMenuItem;

		private ToolTip toolTip1;

		private ToolStripMenuItem showCoordinatesInTootipToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem printToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripMenuItem printPreviewToolStripMenuItem;

		private Label label4;

		private Label label5;

		public ToolStripMenuItem siteDistancesFromThePathToolStripMenuItem;

		public ToolStripMenuItem planetContactTimesForMultipleLocationsToolStripMenuItem;

		private ToolStripMenuItem listOfPrepointStarsToolStripMenuItem;

		internal ProgressBar pBarPlot;

		internal ComboBox cmbSiteFiles;

		internal ComboBox cmbNames;

		private ToolStripMenuItem pathCoordinatesToolStripMenuItem1;

		private ToolStripMenuItem prepointStarsToolStripMenuItem;

		private ToolStripMenuItem starChartToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator5;

		private ToolStripMenuItem viewInGoogleEarthToolStripMenuItem;

		private ToolStripMenuItem saveAsGoogleEarthKMLFileToolStripMenuItem;

		private ToolStripMenuItem saveAsGToolStripMenuItem;

		private Label label6;

		internal ToolStripMenuItem planetAppearanceToolStripMenuItem;

		internal ToolStripMenuItem contactTimesForMultipleLocationsToolStripMenuItem;

		internal ToolStripMenuItem multiToolStripMenuItem;

		private ToolStripMenuItem viewStarInGoogleSkyToolStripMenuItem;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripMenuItem show1sigmaLinesAsContinuousToolStripMenuItem;

		private ToolStripMenuItem displayWDSInterferometricDataToolStripMenuItem;

		private ToolStripMenuItem displayWDSInterferometricDataToolStripMenuItem1;

		private ToolStripSeparator toolStripSeparator6;

		private ToolStripMenuItem compareStarCataloguePositionsToolStripMenuItem;

		private ToolStripMenuItem setPlotRegionToStandardSizesToolStripMenuItem;

		private ToolStripMenuItem x768ToolStripMenuItem;

		private ToolStripMenuItem x480ToolStripMenuItem;

		private ToolStripMenuItem x325ToolStripMenuItem;

		private ToolStripMenuItem x600ToolStripMenuItem;

		private ToolStripMenuItem x1024ToolStripMenuItem;

		private ToolStripMenuItem showRegionalWeatherPrediction16DayLimitToolStripMenuItem;

		private ToolStripMenuItem printToolStripMenuItem1;

		private ToolStripSeparator toolStripSeparator7;

		private ToolStripMenuItem displayMeasuredestimatedStellarDiameterToolStripMenuItem;

		private ToolStripMenuItem displayStarsDiameterFresnelDiffractionToolStripMenuItem;

		private ToolStripMenuItem compareStarCataloguePositionsToolStripMenuItem1;

		private ToolStripMenuItem showRegionalWeatherPrediction16DayLimitToolStripMenuItem1;

		private ToolStripSeparator toolStripSeparator8;

		private ToolStripSeparator toolStripSeparator10;

		private ToolStripMenuItem dAMITAsteroidShapesToolStripMenuItem;

		private ToolStripMenuItem dAMITAsteroidShapesToolStripMenuItem1;

		private ToolStripMenuItem lightCurveDataToolStripMenuItem1;

		private ToolStripMenuItem lightCurveDataToolStripMenuItem;

		private ToolStripMenuItem listRelativisticDisplacementToolStripMenuItem;

		private ToolStripMenuItem listRelativisticOffsetToolStripMenuItem;

		private ToolStripMenuItem getStarInCDSPortalToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator9;

		private ToolStripMenuItem plotStarUsingCDSSimPlayToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator11;

		private ToolStripMenuItem listStarDetailsThroughTheCDSPortalToolStripMenuItem;

		private ToolStripMenuItem plotStarUsingCDSSimPlayToolStripMenuItem1;

		private ToolStripMenuItem displayRingSystemToolStripMenuItem;

		private ToolStripMenuItem displayAsteroidsRingSystemToolStripMenuItem;

		private ToolStripMenuItem displayToolStripMenuItem;

		private ToolStripMenuItem displayAsteroidSatelliteOrbitToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator12;

		private ToolStripMenuItem viewInGoogleEarthToolStripMenuItem1;

		private ToolStripMenuItem saveAsGoogleEarthKMZFileToolStripMenuItem;

		private ToolStripMenuItem saveAsGoogleMapsHTMFileToolStripMenuItem;

		private ToolStripMenuItem viewStarInGoogleSkyToolStripMenuItem1;

		private ToolStripSeparator toolStripSeparator13;

		private ToolStripMenuItem listAsteroidIRDiametersToolStripMenuItem;

		private ToolStripMenuItem listAsteroidIRDiametersToolStripMenuItem1;

		private ToolStripMenuItem listPreviousOccultionsOfThisStarToolStripMenuItem;

		private ToolStripMenuItem listPreviousOccultionsOfThisStarToolStripMenuItem1;

		private ToolStripMenuItem drawStarChartInC2AToolStripMenuItem;

		private ToolStripMenuItem c2AStarChartToolStripMenuItem;

		private ToolStripMenuItem occultSettingsSetToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator14;

		private ToolStripMenuItem copyToolStripMenuItem1;

		private ToolStripMenuItem printPreviewToolStripMenuItem1;

		private ToolStripMenuItem saveToolStripMenuItem1;

		private Panel panelScale;

		private Panel panelLocation;

		private Panel panelExportPlusSites;

		private ToolStripMenuItem gEarthToolStripMenuItem;

		private ToolStripMenuItem pathToolStripMenuItem;

		private ToolStripMenuItem starChartToolStripMenuItem1;

		private ToolStripMenuItem prepointToolStripMenuItem;

		private ToolStripMenuItem multilocationToolStripMenuItem;

		internal ToolStripMenuItem planetConfigToolStripMenuItem;

		internal Button cmdx1;

		private ToolStripMenuItem scale03ToolStripMenuItem;

		private ToolStripMenuItem starChartUseEnhancedToolStripMenuItem;

		private ToolStripMenuItem copyOccultationElementsForThisEventToolStripMenuItem;

		public ToolStripMenuItem asteroidRingsContactTimesForMultipleLocationsToolStripMenuItem;

		public ContextMenuStrip ctxtMenu;

		public ToolStripMenuItem asteroidRingsContactTimesForMultipleLocdationsToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator15;

		private ToolStripMenuItem lightCurveSimulatorToolStripMenuItem;

		private RadioButton optTimeCentered;

		private ToolStripMenuItem copyOccultationElementsForThisEventToolStripMenuItem1;

		private ToolStripMenuItem gaiaDoublesDisplayNearbyToolStripMenuItem;

		private ToolStripMenuItem gaiaDoubleStarsnearbyToolStripMenuItem;

		private ToolStripMenuItem listNearbyStarsToolStripMenuItem;

		private ToolStripMenuItem displayPredictionLightCurveToolStripMenuItem;

		private ToolStripMenuItem displayPredictionLightCurveToolStripMenuItem1;

		private ToolStripMenuItem astrometryAtMinorPlanetCenterOccnSearchFor275ToolStripMenuItem;

		private ToolStripMenuItem astrometry_MPC;

		private ToolStripMenuItem asteroidDetailsToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator16;

		private ToolStripMenuItem starToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator17;

		private ToolStripMenuItem eventToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator18;

		private ToolStripMenuItem eventToolStripMenuItem1;

		private ToolStripSeparator toolStripSeparator19;

		private ToolStripMenuItem toolStripMenuItem2;

		private ToolStripSeparator toolStripSeparator20;

		private ToolStripMenuItem asteroidToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator21;

		private ToolStripMenuItem binaryAsteroidsToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator22;

		private ToolStripMenuItem otherToolStripMenuItem;

		private ToolStripMenuItem binaryAsteroidsToolStripMenuItem1;

		private ToolStripSeparator toolStripSeparator4;

		private ToolStripMenuItem otherToolStripMenuItem1;

		private ToolStripMenuItem toolStripNearbyStars;

		private ToolStripMenuItem jPLSmallBodyDatabaseForThisAsteroidOrCometToolStripMenuItem;

		private ToolStripMenuItem toolStripJPLsmallbodies;

		public AsteroidPlotPath()
		{
			InitializeComponent();
			AppPath = Utilities.AppPath;
		}

		private void AsteroidPlotPath_Load(object sender, EventArgs e)
		{
			//IL_011d: Unknown result type (might be due to invalid IL or missing references)
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion);
			FormStarted = false;
			if (((Control)this).get_Top() < -30000)
			{
				((Control)this).set_Top(0);
			}
			if ((double)((Control)this).get_Left() + (double)((Control)this).get_Width() / 1.2 < (double)Screen.GetWorkingArea((Control)(object)this).Left)
			{
				((Control)this).set_Left(Screen.GetWorkingArea((Control)(object)this).Left);
			}
			ScaleChecks = (ToolStripMenuItem[])(object)new ToolStripMenuItem[9] { scale001100ToolStripMenuItem, scale01ToolStripMenuItem, scale03ToolStripMenuItem, scale1ToolStripMenuItem, scale2ToolStripMenuItem, scale4ToolStripMenuItem, scale8ToolStripMenuItem, scale16ToolStripMenuItem, scale32ToolStripMenuItem };
			DisplayMPOccultations.DrawErrorEllipse = showErrorEllipseToolStripMenuItem.get_Checked();
			ShowToolTip = showCoordinatesInTootipToolStripMenuItem.get_Checked();
			colourToolStripMenuItem.set_Checked(!Settings.Default.BWFlag);
			DisplayMPOccultations.BWFlag = Settings.Default.BWFlag;
			((Form)this).set_WindowState(Settings.Default.AsteroidPlotPathWindow);
			Panel panelExport = PanelExport;
			Label obj = label5;
			ToolStripMenuItem obj2 = exportButtonsDisplayedToolStripMenuItem;
			bool asteroidExportEnabled;
			((ToolStripItem)exportButtonsDisplayedToolStripMenuItem).set_Visible(asteroidExportEnabled = Settings.Default.AsteroidExportEnabled);
			bool flag;
			obj2.set_Checked(flag = asteroidExportEnabled);
			bool visible;
			((Control)obj).set_Visible(visible = flag);
			((Control)panelExport).set_Visible(visible);
			SetLabelText();
			optTimeCentered.set_Checked(false);
			ComboBox obj3 = cmbSiteFiles;
			((Control)cmbNames).set_Visible(visible = !((Control)PanelExport).get_Visible());
			((Control)obj3).set_Visible(visible);
			string[] files = Directory.GetFiles(AppPath + "\\Sites", "*.site");
			foreach (string path in files)
			{
				cmbSiteFiles.get_Items().Add((object)Path.GetFileName(path));
			}
			cmbSiteFiles.get_Items().Add((object)"   don't plot");
			((ListControl)cmbSiteFiles).set_SelectedIndex(0);
			string siteFileForNormalMap = Settings.Default.SiteFileForNormalMap;
			for (int j = 0; j < cmbSiteFiles.get_Items().get_Count(); j++)
			{
				if (siteFileForNormalMap == cmbSiteFiles.get_Items().get_Item(j).ToString())
				{
					((ListControl)cmbSiteFiles).set_SelectedIndex(j);
					break;
				}
			}
			((ListControl)cmbNames).set_SelectedIndex(0);
			((ToolStripItem)viewInGoogleEarthToolStripMenuItem).set_Visible(Settings.Default.GoogleEarthInstalled);
			show1sigmaLinesAsContinuousToolStripMenuItem.set_Checked(Settings.Default.AsteroidPlot_SolidErrorLines);
			ToolStripMenuItem obj4 = c2AStarChartToolStripMenuItem;
			((ToolStripItem)drawStarChartInC2AToolStripMenuItem).set_Enabled(visible = Settings.Default.C2A_Path.Length > 2);
			((ToolStripItem)obj4).set_Enabled(visible);
			includeStarChartToolStripMenuItem.set_Checked(Settings.Default.IncludeStarChart_Prediction);
			FormStarted = true;
		}

		private void AsteroidPlotPath_FormClosing(object sender, FormClosingEventArgs e)
		{
			//IL_0006: Unknown result type (might be due to invalid IL or missing references)
			Settings.Default.AsteroidPlotPathWindow = ((Form)this).get_WindowState();
			if (DisplayMPOccultations.FileInUse)
			{
				DisplayMPOccultations.Elements.Close();
				DisplayMPOccultations.FileInUse = false;
			}
			try
			{
				Settings.Default.LocationIRDiameter = ((Form)Utilities.DataBoxDia).get_Location();
				((Form)Utilities.DataBoxDia).Close();
			}
			catch
			{
			}
			try
			{
				Settings.Default.LocationLightCurveData = ((Form)Asteroid_Observations_Reports.LightCurveData).get_Location();
				((Form)Utilities.DataBoxDia).Close();
			}
			catch
			{
			}
			try
			{
				Settings.Default.LocationShowStarDiameter = ((Form)DisplayMPOccultations.StarDiameter).get_Location();
				((Form)DisplayMPOccultations.StarDiameter).Close();
			}
			catch
			{
			}
			Interferometric_Plus_WDS.CloseInterferometerDisplay();
		}

		private void nextEventToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (((ToolStripItem)plotToolStripMenuItem).get_Enabled())
			{
				DisplayMPOccultations.PlotNextEvent();
			}
		}

		private void previousEventToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (((ToolStripItem)plotToolStripMenuItem).get_Enabled())
			{
				DisplayMPOccultations.PlotPreviousEvent();
			}
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			DisplayMPOccultations.End_PlotRoutine();
		}

		private void SetRedrawSiteCoords()
		{
			DisplayMPOccultations.RedrawLongitude = (double)updnLongitude.get_Value();
			DisplayMPOccultations.RedrawLatitude = (double)updnLatitude.get_Value();
			DisplayMPOccultations.UseRedrawSites = true;
			DisplayMPOccultations.DrawErrorEllipse = showErrorEllipseToolStripMenuItem.get_Checked();
		}

		public static void SetScaleCheckmarks(int Item)
		{
			for (int i = 0; i <= 7; i++)
			{
				ScaleChecks[i].set_Checked(Item == i);
			}
		}

		private void scale001100ToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SetScaleCheckmarks(0);
			DisplayMPOccultations.PlotMagnification = 0.01;
			SetRedrawSiteCoords();
			DisplayMPOccultations.PlotPath();
		}

		private void scale01ToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SetScaleCheckmarks(1);
			DisplayMPOccultations.PlotMagnification = 0.1;
			SetRedrawSiteCoords();
			DisplayMPOccultations.PlotPath();
		}

		private void scale03ToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SetScaleCheckmarks(2);
			DisplayMPOccultations.PlotMagnification = 1.0 / 3.0;
			SetRedrawSiteCoords();
			DisplayMPOccultations.PlotPath();
		}

		private void scale1ToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SetScaleCheckmarks(3);
			DisplayMPOccultations.PlotMagnification = 1.0;
			SetRedrawSiteCoords();
			DisplayMPOccultations.PlotPath();
		}

		private void scale2ToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SetScaleCheckmarks(4);
			DisplayMPOccultations.PlotMagnification = 2.0;
			SetRedrawSiteCoords();
			DisplayMPOccultations.PlotPath();
		}

		private void scale4ToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SetScaleCheckmarks(5);
			DisplayMPOccultations.PlotMagnification = 4.0;
			SetRedrawSiteCoords();
			DisplayMPOccultations.PlotPath();
		}

		private void scale8ToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SetScaleCheckmarks(6);
			DisplayMPOccultations.PlotMagnification = 8.0;
			SetRedrawSiteCoords();
			DisplayMPOccultations.PlotPath();
		}

		private void scale16ToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SetScaleCheckmarks(7);
			DisplayMPOccultations.PlotMagnification = 16.0;
			SetRedrawSiteCoords();
			DisplayMPOccultations.PlotPath();
		}

		private void scale32ToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SetScaleCheckmarks(8);
			DisplayMPOccultations.PlotMagnification = 32.0;
			SetRedrawSiteCoords();
			DisplayMPOccultations.PlotPath();
		}

		private void cmdx1_Click(object sender, EventArgs e)
		{
			scale1ToolStripMenuItem_Click(sender, e);
		}

		private void cmdx2_Click(object sender, EventArgs e)
		{
			scale2ToolStripMenuItem_Click(sender, e);
		}

		private void cmdx4_Click(object sender, EventArgs e)
		{
			scale4ToolStripMenuItem_Click(sender, e);
		}

		private void cmdx8_Click(object sender, EventArgs e)
		{
			scale8ToolStripMenuItem_Click(sender, e);
		}

		private void cmdx16_Click(object sender, EventArgs e)
		{
			scale16ToolStripMenuItem_Click(sender, e);
		}

		private void cmdx32_Click(object sender, EventArgs e)
		{
			scale32ToolStripMenuItem_Click(sender, e);
		}

		private void cmdx01_Click(object sender, EventArgs e)
		{
			scale01ToolStripMenuItem_Click(sender, e);
		}

		private void exportButtonsDisplayedToolStripMenuItem_Click(object sender, EventArgs e)
		{
			exportButtonsDisplayedToolStripMenuItem.set_Checked(!exportButtonsDisplayedToolStripMenuItem.get_Checked());
			((Control)PanelExport).set_Visible(exportButtonsDisplayedToolStripMenuItem.get_Checked());
			ComboBox obj = cmbSiteFiles;
			bool visible;
			((Control)cmbNames).set_Visible(visible = !((Control)PanelExport).get_Visible());
			((Control)obj).set_Visible(visible);
			ReSetExportButtons();
			SetLabelText();
		}

		private void SetLabelText()
		{
			if (((Control)PanelExport).get_Visible())
			{
				((Control)label5).set_Text("Export Elements");
			}
			else
			{
				((Control)label5).set_Text("Plot Sites && names");
			}
		}

		private void cmdExportA_Click(object sender, EventArgs e)
		{
			if (((Control)PanelExport).get_Visible())
			{
				ExportElements(".Export A");
			}
			((Control)cmdExportA).set_Enabled(false);
		}

		private void cmdExportB_Click(object sender, EventArgs e)
		{
			if (((Control)PanelExport).get_Visible())
			{
				ExportElements(".Export B");
			}
			((Control)cmdExportB).set_Enabled(false);
		}

		private void cmdExportC_Click(object sender, EventArgs e)
		{
			if (((Control)PanelExport).get_Visible())
			{
				ExportElements(".Export C");
			}
			((Control)cmdExportC).set_Enabled(false);
		}

		private void cmdExportD_Click(object sender, EventArgs e)
		{
			if (((Control)PanelExport).get_Visible())
			{
				ExportElements(".Export D");
			}
			((Control)cmdExportD).set_Enabled(false);
		}

		private void cmdExportE_Click(object sender, EventArgs e)
		{
			if (((Control)PanelExport).get_Visible())
			{
				ExportElements(".Export E");
			}
			((Control)cmdExportE).set_Enabled(false);
		}

		private void cmdExportF_Click(object sender, EventArgs e)
		{
			if (((Control)PanelExport).get_Visible())
			{
				ExportElements(".Export F");
			}
			((Control)cmdExportF).set_Enabled(false);
		}

		private void cmdExportG_Click(object sender, EventArgs e)
		{
			if (((Control)PanelExport).get_Visible())
			{
				ExportElements(".Export G");
			}
			((Control)cmdExportG).set_Enabled(false);
		}

		private static void ExportElements(string Extension)
		{
			//IL_0014: Unknown result type (might be due to invalid IL or missing references)
			if (DisplayMPOccultations.FileNameIn == null)
			{
				MessageBox.Show("Before you can use the Export Elements buttons, you must save at least some events listed on the List & Display form. \r\n\r\nReason? To set the Directory and base File name for the files that are to be saved.", "No file name", (MessageBoxButtons)0, (MessageBoxIcon)16);
				return;
			}
			string path = Path.GetDirectoryName(DisplayMPOccultations.FileNameIn) + "\\" + Path.GetFileNameWithoutExtension(DisplayMPOccultations.FileNameIn) + Extension + ".xml";
			if (!File.Exists(path))
			{
				using StreamWriter streamWriter = new StreamWriter(path);
				streamWriter.WriteLine("<Occultations>");
				streamWriter.WriteLine("</Occultations>");
			}
			string text = "";
			string text2 = "";
			using (StreamReader streamReader = new StreamReader(path))
			{
				do
				{
					text2 = streamReader.ReadLine();
					if (text2 != "</Occultations>")
					{
						text = text + text2 + "\r\n";
					}
				}
				while (!streamReader.EndOfStream);
			}
			using StreamWriter streamWriter2 = new StreamWriter(path);
			streamWriter2.Write(text);
			streamWriter2.Write(DisplayMPOccultations.OccElements[DisplayMPOccultations.Current_OccElements_Record].XML_Elements());
			streamWriter2.WriteLine("</Occultations>");
		}

		public void ReSetExportButtons()
		{
			((Control)cmdExportA).set_Enabled(true);
			((Control)cmdExportB).set_Enabled(true);
			((Control)cmdExportC).set_Enabled(true);
			((Control)cmdExportD).set_Enabled(true);
			((Control)cmdExportE).set_Enabled(true);
			((Control)cmdExportF).set_Enabled(true);
			((Control)cmdExportG).set_Enabled(true);
		}

		private void includeStarChartToolStripMenuItem_Click(object sender, EventArgs e)
		{
			includeStarChartToolStripMenuItem.set_Checked(!includeStarChartToolStripMenuItem.get_Checked());
			Settings.Default.IncludeStarChart_Prediction = includeStarChartToolStripMenuItem.get_Checked();
			DisplayMPOccultations.IncludeStarChart = includeStarChartToolStripMenuItem.get_Checked();
			DisplayMPOccultations.PlotPath();
		}

		private void colourToolStripMenuItem_Click(object sender, EventArgs e)
		{
			colourToolStripMenuItem.set_Checked(!colourToolStripMenuItem.get_Checked());
			bool bWFlag = (Settings.Default.BWFlag = !colourToolStripMenuItem.get_Checked());
			DisplayMPOccultations.BWFlag = bWFlag;
			DisplayMPOccultations.PlotPath();
		}

		private void AsteroidPlotPath_Resize(object sender, EventArgs e)
		{
			if (((Control)this).get_Width() < 805)
			{
				((Control)this).set_Width(805);
			}
			if (((Control)this).get_Height() < 500)
			{
				((Control)this).set_Height(500);
			}
			((Control)picAsteroidPlot).set_Width(((Control)this).get_Width() - 26);
			((Control)picAsteroidPlot).set_Height(((Control)this).get_Height() - 100);
			int num = (((Control)this).get_Width() - 805) / 3;
			if (num < 0)
			{
				num = 0;
			}
			((Control)panelScale).set_Left(5);
			((Control)panelLocation).set_Left(((Control)panelScale).get_Right() + num);
			((Control)label6).set_Left(((Control)panelLocation).get_Right() + num);
			((Control)panelExportPlusSites).set_Left(((Control)label6).get_Right() + num);
			((Control)optTimeCentered).set_Left(((Control)panelLocation).get_Right() + 2);
			if (FormStarted)
			{
				DisplayMPOccultations.PlotPath();
			}
		}

		private void pathCoordinatesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			DisplayMPOccultations.AsteroidPath((double)Settings.Default.AsteroidPathStepSize, UseLocalTopography: false, 0.0, 0.0);
			DisplayMPOccultations.DisplayAsteroidCoordinatesForm();
			((Control)DisplayMPOccultations.PathCoordsForm.cmdPlotInGE).set_Text("Draw listed MSL path in\r\nGoogle Earth");
			((Control)DisplayMPOccultations.PathCoordsForm.cmdPlotInGE).set_BackColor(((Control)DisplayMPOccultations.PathCoordsForm.cmdCompute).get_BackColor());
			DisplayMPOccultations.TopographyEnabledInPathCoords(!DisplayMPOccultations.OrbitSourceDate.Contains("INTG:"));
		}

		private void largeStarChartToolStripMenuItem_Click(object sender, EventArgs e)
		{
			DisplayMPOccultations.LargeStarChart();
		}

		private void cmdPrevious_Click(object sender, EventArgs e)
		{
			DisplayMPOccultations.PlotPreviousEvent();
		}

		private void cmdThis_Click(object sender, EventArgs e)
		{
			if (DisplayMPOccultations.ListAndDisplay != null)
			{
				updnLongitude.set_Value((decimal)(OccultationElements.SiteLongitudeTest * 57.295779));
				updnLatitude.set_Value((decimal)(OccultationElements.SiteLatitudeTest * 57.295779));
				SetRedrawSiteCoords();
				DisplayMPOccultations.PlotPath();
			}
		}

		private void cmbSiteFiles_SelectedIndexChanged(object sender, EventArgs e)
		{
			if (FormStarted)
			{
				Settings.Default.SiteFileForNormalMap = cmbSiteFiles.get_Items().get_Item(((ListControl)cmbSiteFiles).get_SelectedIndex()).ToString();
				DisplayMPOccultations.PlotPath();
			}
		}

		private void cmbNames_SelectedIndexChanged(object sender, EventArgs e)
		{
			if (FormStarted)
			{
				DisplayMPOccultations.PlotPath();
			}
		}

		private void cmdNext_Click(object sender, EventArgs e)
		{
			DisplayMPOccultations.PlotNextEvent();
		}

		private void showPlanetConfigurationToolStripMenuItem_Click(object sender, EventArgs e)
		{
			DisplayMPOccultations.ShowPlanetConfiguration();
		}

		private void picAsteroidPlot_MouseClick(object sender, MouseEventArgs e)
		{
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_000b: Invalid comparison between Unknown and I4
			if ((int)e.get_Button() == 1048576)
			{
				DisplayMPOccultations.UseSetErrorLocation = true;
				DisplayMPOccultations.XerrorSet = e.get_X();
				DisplayMPOccultations.YerrorSet = e.get_Y();
				SetRedrawSiteCoords();
				DisplayMPOccultations.PlotPath();
			}
		}

		private void showErrorEllipseToolStripMenuItem_Click(object sender, EventArgs e)
		{
			showErrorEllipseToolStripMenuItem.set_Checked(!showErrorEllipseToolStripMenuItem.get_Checked());
			SetRedrawSiteCoords();
			DisplayMPOccultations.PlotPath();
		}

		private void show1sigmaLinesAsContinuousToolStripMenuItem_Click(object sender, EventArgs e)
		{
			show1sigmaLinesAsContinuousToolStripMenuItem.set_Checked(!show1sigmaLinesAsContinuousToolStripMenuItem.get_Checked());
			Settings.Default.AsteroidPlot_SolidErrorLines = show1sigmaLinesAsContinuousToolStripMenuItem.get_Checked();
			DisplayMPOccultations.PlotPath();
		}

		private void picAsteroidPlot_MouseMove(object sender, MouseEventArgs e)
		{
			Maps.InverseGlobeCoords(out Longitude, out Latitude, e.get_X(), e.get_Y(), out var Valid);
			if (Valid & ShowToolTip)
			{
				GetToolTip = !GetToolTip;
				if (GetToolTip)
				{
					toolTip1.SetToolTip((Control)(object)picAsteroidPlot, Longitude.ToString("+0.0  ;-0.0  ") + Latitude.ToString("+0.0;-0.0"));
				}
			}
			else
			{
				toolTip1.Hide((IWin32Window)(object)picAsteroidPlot);
			}
		}

		private void picAsteroidPlot_DoubleClick(object sender, EventArgs e)
		{
			updnLongitude.set_Value((decimal)Longitude);
			updnLatitude.set_Value((decimal)Latitude);
			SetRedrawSiteCoords();
			DisplayMPOccultations.PlotPath();
		}

		private void showCoordinatesInTootipToolStripMenuItem_Click(object sender, EventArgs e)
		{
			showCoordinatesInTootipToolStripMenuItem.set_Checked(!showCoordinatesInTootipToolStripMenuItem.get_Checked());
			ShowToolTip = showCoordinatesInTootipToolStripMenuItem.get_Checked();
		}

		private void copyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetImage(picAsteroidPlot.get_Image());
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings.Default.Save_AsteroidPredictions = Output.SaveGraphic(picAsteroidPlot.get_Image(), DisplayMPOccultations.UTDate_Numeric.Trim() + " " + DisplayMPOccultations.AsteroidName.Trim(), Settings.Default.Save_AsteroidPredictions);
		}

		private void printToolStripMenuItem_Click(object sender, EventArgs e)
		{
			DisplayMPOccultations.PrintPathGraphic();
		}

		private void printPreviewToolStripMenuItem_Click(object sender, EventArgs e)
		{
			DisplayMPOccultations.PrintPreviewPathGraphic();
		}

		private void siteDistancesFromThePathToolStripMenuItem_Click(object sender, EventArgs e)
		{
			DisplayMPOccultations.Show_Multilocations();
		}

		private void planetContactTimesForMultipleLocationsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			DisplayMPOccultations.Show_PlanetContactTimes();
		}

		private void asteroidRingsContactTimesForMultipleLocationsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			DisplayMPOccultations.Show_PlanetContactTimes();
		}

		private void listOfPrepointStarsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			DisplayMPOccultations.Show_PrePointStars();
		}

		private void pathCoordinatesToolStripMenuItem1_Click(object sender, EventArgs e)
		{
			pathCoordinatesToolStripMenuItem_Click(sender, e);
		}

		private void multiToolStripMenuItem_Click(object sender, EventArgs e)
		{
			siteDistancesFromThePathToolStripMenuItem_Click(sender, e);
		}

		private void prepointStarsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			listOfPrepointStarsToolStripMenuItem_Click(sender, e);
		}

		private void starChartToolStripMenuItem_Click(object sender, EventArgs e)
		{
			DisplayMPOccultations.LargeStarChart();
		}

		private void planetAppearanceToolStripMenuItem_Click(object sender, EventArgs e)
		{
			showPlanetConfigurationToolStripMenuItem_Click(sender, e);
		}

		private void contactTimesForMultipleLocationsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			DisplayMPOccultations.Show_PlanetContactTimes();
		}

		private void asteroidRingsContactTimesForMultipleLocdationsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			DisplayMPOccultations.Show_PlanetContactTimes();
		}

		private void viewInGoogleEarthToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_001a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0032: Unknown result type (might be due to invalid IL or missing references)
			if (DisplayMPOccultations.PathCoordinates.Count < 1)
			{
				MessageBox.Show("KMZ file cannot be created, as\r\nno part of the path crosses the Earth", "No .KMZ file", (MessageBoxButtons)0, (MessageBoxIcon)16);
			}
			else if (!Utilities.InternetIsAvailable())
			{
				MessageBox.Show("This functionality requires internet access. You are not connected to the internet", "No internet");
			}
			else
			{
				DisplayMPOccultations.CreateGoogleEarthKMLFile(AppPath + "\\AutoGenerated Asteroids\\AsteroidPath.KML", DisplayMPOccultations.EventHeader.Substring(15).Trim(), Auto: true, View: true, cmbSiteFiles.get_Items().get_Item(((ListControl)cmbSiteFiles).get_SelectedIndex()).ToString());
			}
		}

		private void viewInGoogleEarthToolStripMenuItem1_Click(object sender, EventArgs e)
		{
			//IL_001a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0032: Unknown result type (might be due to invalid IL or missing references)
			if (DisplayMPOccultations.PathCoordinates.Count < 1)
			{
				MessageBox.Show("KMZ file cannot be created, as\r\nno part of the path crosses the Earth", "No .KMZ file", (MessageBoxButtons)0, (MessageBoxIcon)16);
			}
			else if (!Utilities.InternetIsAvailable())
			{
				MessageBox.Show("This functionality requires internet access. You are not connected to the internet", "No internet");
			}
			else
			{
				DisplayMPOccultations.CreateGoogleEarthKMLFile(AppPath + "\\AutoGenerated Asteroids\\AsteroidPath.KML", DisplayMPOccultations.EventHeader.Substring(15).Trim(), Auto: true, View: true, cmbSiteFiles.get_Items().get_Item(((ListControl)cmbSiteFiles).get_SelectedIndex()).ToString());
			}
		}

		private void saveAsGoogleEarthKMLFileToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_001a: Unknown result type (might be due to invalid IL or missing references)
			if (DisplayMPOccultations.PathCoordinates.Count < 1)
			{
				MessageBox.Show("KMZ file cannot be created, as\r\nno part of the path crosses the Earth", "No .KMZ file", (MessageBoxButtons)0, (MessageBoxIcon)16);
			}
			else
			{
				DisplayMPOccultations.CreateGoogleEarthKMLFile(DisplayMPOccultations.EventHeader.Substring(15).Trim(), DisplayMPOccultations.EventHeader.Substring(15).Trim(), Auto: false, View: false, "");
			}
		}

		private void saveAsGoogleEarthKMZFileToolStripMenuItem_Click(object sender, EventArgs e)
		{
			DisplayMPOccultations.CreateGoogleEarthKMLFile(DisplayMPOccultations.EventHeader.Substring(15).Trim(), DisplayMPOccultations.EventHeader.Substring(15).Trim(), Auto: false, View: false, "");
		}

		private void saveAsGToolStripMenuItem_Click(object sender, EventArgs e)
		{
			DisplayMPOccultations.CreateGoogleMapHTMFile(DisplayMPOccultations.EventHeader.Substring(15).Trim().Replace(" ", "_"), DisplayMPOccultations.EventHeader, Auto: false);
		}

		private void saveAsGoogleMapsHTMFileToolStripMenuItem_Click(object sender, EventArgs e)
		{
			DisplayMPOccultations.CreateGoogleMapHTMFile(DisplayMPOccultations.EventHeader.Substring(15).Trim().Replace(" ", "_"), DisplayMPOccultations.EventHeader, Auto: false);
		}

		private void viewStarInGoogleSkyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			if (!Utilities.InternetIsAvailable())
			{
				MessageBox.Show("This functionality requires internet access. You are not connected to the internet", "No internet");
			}
			else
			{
				GoogleEarth.CreateGoogleSkyKMLFile(DisplayMPOccultations.StarRA_2000 * (180.0 / Math.PI), DisplayMPOccultations.StarDec_2000 * (180.0 / Math.PI), 15, AppPath + "\\AutoGenerated Asteroids\\ViewGoogleSky.KML");
			}
		}

		private void viewStarInGoogleSkyToolStripMenuItem1_Click(object sender, EventArgs e)
		{
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			if (!Utilities.InternetIsAvailable())
			{
				MessageBox.Show("This functionality requires internet access. You are not connected to the internet", "No internet");
			}
			else
			{
				GoogleEarth.CreateGoogleSkyKMLFile(DisplayMPOccultations.StarRA_2000 * (180.0 / Math.PI), DisplayMPOccultations.StarDec_2000 * (180.0 / Math.PI), 15, AppPath + "\\AutoGenerated Asteroids\\ViewGoogleSky.KML");
			}
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Asteroid - World plot");
		}

		private void displayWDSInterferometricDataToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Interferometric_Plus_WDS.Find_WDS_IF_Matches(DisplayMPOccultations.StarRA_2000 * (180.0 / Math.PI) / 15.0, DisplayMPOccultations.StarDec_2000 * (180.0 / Math.PI), HighPrecision: true, ShowResults: true, out WDS_Interferometer_Display.VariableID);
		}

		private void displayWDSInterferometricDataToolStripMenuItem1_Click(object sender, EventArgs e)
		{
			Interferometric_Plus_WDS.Find_WDS_IF_Matches(DisplayMPOccultations.StarRA_2000 * (180.0 / Math.PI) / 15.0, DisplayMPOccultations.StarDec_2000 * (180.0 / Math.PI), HighPrecision: true, ShowResults: true, out WDS_Interferometer_Display.VariableID);
		}

		private void compareStarCataloguePositionsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			CompareStarCataloguePositions();
		}

		private void compareStarCataloguePositionsToolStripMenuItem1_Click(object sender, EventArgs e)
		{
			CompareStarCataloguePositions();
		}

		private static void CompareStarCataloguePositions()
		{
			//IL_02bb: Unknown result type (might be due to invalid IL or missing references)
			//IL_0340: Unknown result type (might be due to invalid IL or missing references)
			GetStarPosition.ShowCompareCatalogues(fromAsteroidPrediction: true);
			Application.DoEvents();
			((Control)GetStarPosition.CompareCatalogues.lblEventDetails).set_Text("Date : " + DisplayMPOccultations.UTDate + "\r\n\r\nAsteroid : " + DisplayMPOccultations.AsteroidName + "\r\n\r\nStar : " + DisplayMPOccultations.StarNo);
			((Control)GetStarPosition.CompareCatalogues.txtRAh).set_Text(DisplayMPOccultations.RA.Substring(0, 2));
			((Control)GetStarPosition.CompareCatalogues.txtRAm).set_Text(DisplayMPOccultations.RA.Substring(3, 2));
			((Control)GetStarPosition.CompareCatalogues.txtRAs).set_Text(DisplayMPOccultations.RA.Substring(6));
			((Control)GetStarPosition.CompareCatalogues.txtD).set_Text(DisplayMPOccultations.Dec.Substring(0, 3).Replace(" ", ""));
			((Control)GetStarPosition.CompareCatalogues.txtm).set_Text(DisplayMPOccultations.Dec.Substring(4, 2));
			((Control)GetStarPosition.CompareCatalogues.txts).set_Text(DisplayMPOccultations.Dec.Substring(7));
			((Control)GetStarPosition.CompareCatalogues.txtMv).set_Text(string.Format("{0,1:f1}", DisplayMPOccultations.Mv));
			if (DisplayMPOccultations.Mp < 30.0)
			{
				((Control)GetStarPosition.CompareCatalogues.txtMp).set_Text(string.Format("{0,1:f1}", DisplayMPOccultations.Mp));
			}
			else
			{
				((Control)GetStarPosition.CompareCatalogues.txtMp).set_Text("...");
			}
			GetStarPosition.CompareCatalogues.updnDiameter.set_Value((decimal)(DisplayMPOccultations.AsteroidDiameter / 0.7253 / DisplayMPOccultations.DistanceToAsteroid));
			((Control)GetStarPosition.CompareCatalogues.txtDuration).set_Text(DisplayMPOccultations.MaxDurn.ToString());
			GetStarPosition.CompareCatalogues.updnMotionPA.set_Value((decimal)(Math.Atan2(DisplayMPOccultations.DeltaX, DisplayMPOccultations.DeltaY) * (180.0 / Math.PI)));
			if (GetStarPosition.CompareCatalogues.updnMotionPA.get_Value() < 0m)
			{
				GetStarPosition.CompareCatalogues.updnMotionPA.set_Value(GetStarPosition.CompareCatalogues.updnMotionPA.get_Value() + 360m);
			}
			GetStarPosition.CompareCatalogues.updnEpoch.set_Value(2000m);
			decimal num = (decimal)Utilities.BesselianYear(Utilities.JD_from_Date(DisplayMPOccultations.EventYear, DisplayMPOccultations.EventMonth, DisplayMPOccultations.EventDay));
			if (num < GetStarPosition.CompareCatalogues.updnEpoch.get_Minimum())
			{
				MessageBox.Show("Epoch for comparison was before " + GetStarPosition.CompareCatalogues.updnEpoch.get_Minimum() + ", and has been set to " + GetStarPosition.CompareCatalogues.updnEpoch.get_Minimum(), "Invalid Epoch", (MessageBoxButtons)0, (MessageBoxIcon)64);
				GetStarPosition.CompareCatalogues.updnEpoch.set_Value(GetStarPosition.CompareCatalogues.updnEpoch.get_Minimum());
			}
			else if (num > GetStarPosition.CompareCatalogues.updnEpoch.get_Maximum())
			{
				MessageBox.Show("Epoch for comparison was after " + GetStarPosition.CompareCatalogues.updnEpoch.get_Maximum() + ", and has been set to " + GetStarPosition.CompareCatalogues.updnEpoch.get_Maximum(), "Invalid Epoch", (MessageBoxButtons)0, (MessageBoxIcon)64);
				GetStarPosition.CompareCatalogues.updnEpoch.set_Value(GetStarPosition.CompareCatalogues.updnEpoch.get_Maximum());
			}
			else
			{
				GetStarPosition.CompareCatalogues.updnEpoch.set_Value(num);
			}
			Application.DoEvents();
			GetStarPosition.CompareCatalogues.FindInSpecificCatalogues(DownLoadVizier: true);
		}

		private void x768ToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SetResize(1024, 768);
		}

		private void SetResize(int x, int y)
		{
			FormStarted = false;
			((Control)this).set_Width(x + 32);
			((Control)this).set_Height(y + 98);
			FormStarted = true;
			DisplayMPOccultations.PlotPath();
		}

		private void x480ToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SetResize(760, 480);
		}

		private void x325ToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SetResize(640, 480);
		}

		private void x600ToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SetResize(800, 600);
		}

		private void x1024ToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SetResize(1280, 1024);
		}

		private void showRegionalWeatherPrediction16DayLimitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			RegionalWeatherPrediction();
		}

		private void showRegionalWeatherPrediction16DayLimitToolStripMenuItem1_Click(object sender, EventArgs e)
		{
			RegionalWeatherPrediction();
		}

		private static void RegionalWeatherPrediction()
		{
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			if (!Utilities.InternetIsAvailable())
			{
				MessageBox.Show("This functionality requires internet access. You are not connected to the internet", "No internet");
				return;
			}
			APanelWeatherInfo.ShowCloudMap();
			DateTime dateTime = (((int)DisplayMPOccultations.MidTime > 23) ? new DateTime(DisplayMPOccultations.EventYear, DisplayMPOccultations.EventMonth, (int)DisplayMPOccultations.EventDay + 1, 0, 0, 0) : (((int)DisplayMPOccultations.MidTime >= 0) ? new DateTime(DisplayMPOccultations.EventYear, DisplayMPOccultations.EventMonth, (int)DisplayMPOccultations.EventDay, (int)DisplayMPOccultations.MidTime, 0, 0) : new DateTime(DisplayMPOccultations.EventYear, DisplayMPOccultations.EventMonth, (int)DisplayMPOccultations.EventDay, 0, 0, 0)));
			APanelWeatherInfo.Cloud_Map.SetRequestDate(dateTime.AddHours(1.0).AddMinutes(30.0));
		}

		private void printToolStripMenuItem1_Click(object sender, EventArgs e)
		{
			DisplayMPOccultations.PrintPathGraphic();
		}

		private void displayMeasuredestimatedStellarDiameterToolStripMenuItem_Click(object sender, EventArgs e)
		{
			DisplayStarDiameter();
		}

		private void displayStarsDiameterFresnelDiffractionToolStripMenuItem_Click(object sender, EventArgs e)
		{
			DisplayStarDiameter();
		}

		private static void DisplayStarDiameter()
		{
			double num = 0.0;
			double rAhrs = DisplayMPOccultations.StarRA_2000 * (180.0 / Math.PI) / 15.0;
			double decDeg = DisplayMPOccultations.StarDec_2000 * (180.0 / Math.PI);
			double mv = DisplayMPOccultations.Mv;
			double mp = DisplayMPOccultations.Mp;
			double mr = DisplayMPOccultations.Mr;
			int GaiaVersion = 0;
			bool UsedGaia = false;
			DisplayMPOccultations.Show_StarDiameter();
			DisplayMPOccultations.StarDiameter.lstDiameter.get_Items().Clear();
			DisplayMPOccultations.StarDiameter.lstDiameter.get_Items().Add((object)(DisplayMPOccultations.AsteroidName.Trim() + " occults " + DisplayMPOccultations.StarNo.Trim() + ", on " + DisplayMPOccultations.UTDate));
			DisplayMPOccultations.StarDiameter.lstDiameter.get_Items().Add((object)"");
			DisplayMPOccultations.StarDiameter.lstDiameter.get_Items().Add((object)"Star Diameter");
			string text = DisplayMPOccultations.StarNo.Trim();
			num = DisplayMPOccultations.StellarDiameter_mas / 1000.0;
			double RA;
			double Dec;
			double pmRA;
			double pmDec;
			double MagV;
			double MagB;
			double MagR;
			double Parallax_asec;
			double Epoch;
			ulong GaiaSourceID;
			string SourceFile;
			double RadialVelocity;
			double UncertRA;
			double UncertDec;
			double UncertPMRA;
			double UncertPMDec;
			double StarReliability;
			int DuplicateSource;
			int NoGaiaPM;
			int GaiaPMfromUCAC;
			if (text.Contains("UCAC4"))
			{
				int num2 = text.IndexOf(" ");
				int num3 = text.IndexOf("-");
				int.TryParse(text.Substring(num2 + 1, num3 - num2 - 1).Trim(), out var result);
				int.TryParse(text.Substring(num3 + 1).Trim(), out var result2);
				if (GetStarPosition.GetUCAC4Position(result, result2, Parallax_IfNotInGaia_TryHipparcos: false, out RA, out Dec, out pmRA, out pmDec, out MagV, out MagB, out MagR, out Parallax_asec, out Epoch, out UsedGaia, out GaiaVersion, out GaiaSourceID, out SourceFile, out RadialVelocity, out UncertRA, out UncertDec, out UncertPMRA, out UncertPMDec, out var StarDiameter_mas, out StarReliability, out DuplicateSource, out NoGaiaPM, out GaiaPMfromUCAC))
				{
					num = StarDiameter_mas / 1000.0;
				}
			}
			else if (text.Contains("TYC"))
			{
				int num4 = text.IndexOf(" ");
				int num5 = text.IndexOf("-");
				int num6 = text.IndexOf("-", num5 + 1);
				int.TryParse(text.Substring(num4 + 1, num5 - num4 - 1).Trim(), out var result3);
				int.TryParse(text.Substring(num5 + 1, num6 - num5 - 1).Trim(), out var result4);
				int.TryParse(text.Substring(num6 + 1).Trim(), out var result5);
				if (GetStarPosition.GetTycho2Position(result3, result4, result5, out StarReliability, out UncertPMDec, out UncertPMRA, out UncertDec, out UncertRA, out RadialVelocity, out Epoch, out Parallax_asec, out MagR, out UsedGaia, out GaiaVersion, out GaiaSourceID, out SourceFile, out MagB, out MagV, out pmDec, out pmRA, out Dec, out var StarDiameter_mas2, out RA, out GaiaPMfromUCAC, out NoGaiaPM, out DuplicateSource))
				{
					num = StarDiameter_mas2 / 1000.0;
				}
			}
			else if (text.Contains("HIP"))
			{
				int num7 = text.IndexOf(" ");
				int num8 = text.IndexOf(" ", num7 + 1);
				int result6;
				if (num8 < 0)
				{
					int.TryParse(text.Substring(num7 + 1).Trim(), out result6);
				}
				else
				{
					int.TryParse(text.Substring(num7 + 1, num8 - num7 - 1).Trim(), out result6);
				}
				if (GetStarPosition.GetHipparcosPosition(result6, out RA, out Dec, out pmRA, out pmDec, out MagV, out MagB, out MagR, out Parallax_asec, out Epoch, out UsedGaia, out GaiaVersion, out GaiaSourceID, out SourceFile, out RadialVelocity, out UncertRA, out UncertDec, out UncertPMRA, out UncertPMDec, out var StarDiameter_mas3, out StarReliability, out DuplicateSource, out NoGaiaPM, out GaiaPMfromUCAC, out var _))
				{
					num = StarDiameter_mas3 / 1000.0;
				}
			}
			string Basis;
			int NumMeasures;
			bool InvalidDiameter;
			double num9 = Utilities.StarDiameter_CHARM2_CADARS(rAhrs, decDeg, mv, mp, mr, out Basis, out NumMeasures, out InvalidDiameter);
			if (num9 > 0.001)
			{
				num = num9;
			}
			else
			{
				Basis = "Gaia DR3";
				NumMeasures = 0;
			}
			double num10 = num / DisplayMPOccultations.AsteroidAngularDiameterMAS * 1000.0;
			double num11 = num10 * DisplayMPOccultations.MaxDurn / (1.0 + num10);
			double num12 = num11 / num;
			if (num == 0.0)
			{
				num12 = 1.0;
			}
			if (num > 0.0)
			{
				if (NumMeasures > 0)
				{
					DisplayMPOccultations.StarDiameter.lstDiameter.get_Items().Add((object)($"    Dia: {num:.0000}\"" + " [" + Basis + string.Format(", {0,1} measures]", NumMeasures)));
				}
				else
				{
					DisplayMPOccultations.StarDiameter.lstDiameter.get_Items().Add((object)($"    Dia: {num:.0000}\"" + " [" + Basis + "]"));
				}
				DisplayMPOccultations.StarDiameter.lstDiameter.get_Items().Add((object)string.Format("          = {0,2:f0}% of the asteroid's diameter", num10 * 100.0));
				if (num10 > 0.2)
				{
					DisplayMPOccultations.StarDiameter.lstDiameter.get_Items().Add((object)"  *** WARNING - special processing required ***");
				}
				if (num10 > 1.0)
				{
					DisplayMPOccultations.StarDiameter.lstDiameter.get_Items().Add((object)"  *** WARNING: Occultation is ANNULAR. Mag drop adjusted ***");
				}
				if (num11 < 0.02)
				{
					DisplayMPOccultations.StarDiameter.lstDiameter.get_Items().Add((object)$"  => Fades caused by the star diameter are not expected.");
				}
				else
				{
					DisplayMPOccultations.StarDiameter.lstDiameter.get_Items().Add((object)string.Format("  => fades of about {0,1:F1} secs might be expected.", num11 * 0.7));
				}
			}
			else if (UsedGaia)
			{
				DisplayMPOccultations.StarDiameter.lstDiameter.get_Items().Add((object)("    Dia < 0.0001\" [" + Basis + "]"));
				DisplayMPOccultations.StarDiameter.lstDiameter.get_Items().Add((object)$"  => Fades caused by the star diameter are not expected.");
			}
			double mas;
			double num13 = Utilities.FresnelLength_m(DisplayMPOccultations.DistanceToAsteroid, 600.0, 150.0, out mas);
			DisplayMPOccultations.StarDiameter.lstDiameter.get_Items().Add((object)"");
			DisplayMPOccultations.StarDiameter.lstDiameter.get_Items().Add((object)"Fresnel diffraction");
			DisplayMPOccultations.StarDiameter.lstDiameter.get_Items().Add((object)string.Format(" Fresnel length = {0,1:f0} m,  {1,1:f2} mas", num13, mas));
			DisplayMPOccultations.StarDiameter.lstDiameter.get_Items().Add((object)string.Format(" diffraction for light drop of 2 mag (to 16%) = {0,1:F1} mas", mas));
			DisplayMPOccultations.StarDiameter.lstDiameter.get_Items().Add((object)string.Format("  => fades of about {0,1:F2} secs might be expected", num12 / 1000.0 * mas));
			if (DisplayMPOccultations.Mdrop > 2.0)
			{
				DisplayMPOccultations.StarDiameter.lstDiameter.get_Items().Add((object)string.Format(" diffraction for light drop of 4 mag (to 2.5%) = {0,1:F1} mas", 2.54 * mas));
				DisplayMPOccultations.StarDiameter.lstDiameter.get_Items().Add((object)string.Format("  => fades of about {0,1:F2} secs might be expected", num12 * mas / 1000.0 * 2.54));
			}
			if (DisplayMPOccultations.Mdrop > 4.0)
			{
				DisplayMPOccultations.StarDiameter.lstDiameter.get_Items().Add((object)string.Format(" diffraction for light drop of 6 mag (to 0.4%) = {0,1:F1} mas", 6.4 * mas));
				DisplayMPOccultations.StarDiameter.lstDiameter.get_Items().Add((object)string.Format("  => fades of about {0,1:F2} secs might be expected", num12 * mas / 1000.0 * 6.4));
			}
			DisplayMPOccultations.StarDiameter.lstDiameter.get_Items().Add((object)"                * * *");
			((Control)DisplayMPOccultations.StarDiameter).Focus();
		}

		private void dAMITAsteroidShapesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ShowShapeModels();
		}

		private void dAMITAsteroidShapesToolStripMenuItem1_Click(object sender, EventArgs e)
		{
			ShowShapeModels();
		}

		private void ShowShapeModels()
		{
			//IL_00db: Unknown result type (might be due to invalid IL or missing references)
			Asteroid_Observations_Reports.ShowShapeModels();
			((Control)Asteroid_Observations_Reports.Display_ShapeModels.txtYear).set_Text(DisplayMPOccultations.EventYear.ToString());
			((Control)Asteroid_Observations_Reports.Display_ShapeModels.txtMonth).set_Text(DisplayMPOccultations.EventMonth.ToString());
			((Control)Asteroid_Observations_Reports.Display_ShapeModels.txtDay).set_Text(DisplayMPOccultations.EventDay.ToString());
			double midTime = DisplayMPOccultations.MidTime;
			((Control)Asteroid_Observations_Reports.Display_ShapeModels.txtHrs).set_Text(Math.Floor(midTime).ToString());
			((Control)Asteroid_Observations_Reports.Display_ShapeModels.txtMin).set_Text(string.Format("{0,1:f0}", (midTime - Math.Floor(midTime)) * 60.0));
			Asteroid_Observations_Reports.Display_ShapeModels.plotOnEarthPlaneToolStripMenuItem.set_Checked(true);
			DisplayShapeModels.PlotOnEarthPlane = true;
			if (Asteroid_Observations_Reports.Display_ShapeModels.SetAsteroid(DisplayMPOccultations.AsteroidNumber))
			{
				Asteroid_Observations_Reports.Display_ShapeModels.GetModels();
			}
			else
			{
				MessageBox.Show("Asteroid is not in DAMIT database");
			}
			Application.DoEvents();
		}

		private void lightCurveDataToolStripMenuItem1_Click(object sender, EventArgs e)
		{
			LightCurveData();
		}

		private void lightCurveDataToolStripMenuItem_Click(object sender, EventArgs e)
		{
			LightCurveData();
		}

		private void LightCurveData()
		{
			Asteroid_Observations_Reports.ShowLightCurveData(DisplayMPOccultations.AsteroidNumber.ToString(), DisplayMPOccultations.AsteroidName.Trim());
		}

		private void listRelativisticDisplacementToolStripMenuItem_Click(object sender, EventArgs e)
		{
			DisplayMPOccultations.ListRelativisticOffset();
		}

		private void listRelativisticOffsetToolStripMenuItem_Click(object sender, EventArgs e)
		{
			DisplayMPOccultations.ListRelativisticOffset();
		}

		private void getStarInCDSPortalToolStripMenuItem_Click(object sender, EventArgs e)
		{
			CDS_Portal.CDSPortal(getCDSargument());
		}

		private void listStarDetailsThroughTheCDSPortalToolStripMenuItem_Click(object sender, EventArgs e)
		{
			CDS_Portal.CDSPortal(getCDSargument());
		}

		private void plotStarUsingCDSSimPlayToolStripMenuItem_Click(object sender, EventArgs e)
		{
			CDS_Portal.SimPlay(getCDSargument());
		}

		private void plotStarUsingCDSSimPlayToolStripMenuItem1_Click(object sender, EventArgs e)
		{
			CDS_Portal.SimPlay(getCDSargument());
		}

		private string getCDSargument()
		{
			string[] array = new string[6] { "HIP", "TYC", "UCAC2", "2UCAC", "UCAC3", "3UCAC" };
			string text = DisplayMPOccultations.StarNo.Trim().ToUpper();
			for (int i = 0; i < array.GetUpperBound(0); i++)
			{
				if (text.Contains(array[i]))
				{
					return text;
				}
			}
			text = Utilities.DEGtoDMS(DisplayMPOccultations.StarRA_2000 * (180.0 / Math.PI) / 15.0, 2, 1, MinutesOnly: false).Trim();
			text = ((!(DisplayMPOccultations.StarDec_2000 < 0.0)) ? (text + " +") : (text + " -"));
			return text + Utilities.DEGtoDMS(Math.Abs(DisplayMPOccultations.StarDec_2000 * (180.0 / Math.PI)), 2, 0, MinutesOnly: false).Trim();
		}

		private void displayRingSystemToolStripMenuItem_Click(object sender, EventArgs e)
		{
			DisplayMPOccultations.ShowAsteroidRings = !DisplayMPOccultations.ShowAsteroidRings;
			ToolStripMenuItem obj = displayRingSystemToolStripMenuItem;
			bool showAsteroidRings;
			displayAsteroidsRingSystemToolStripMenuItem.set_Checked(showAsteroidRings = DisplayMPOccultations.ShowAsteroidRings);
			obj.set_Checked(showAsteroidRings);
			DisplayMPOccultations.PlotPath();
		}

		private void displayAsteroidsRingSystemToolStripMenuItem_Click(object sender, EventArgs e)
		{
			DisplayMPOccultations.ShowAsteroidRings = !DisplayMPOccultations.ShowAsteroidRings;
			ToolStripMenuItem obj = displayRingSystemToolStripMenuItem;
			bool showAsteroidRings;
			displayAsteroidsRingSystemToolStripMenuItem.set_Checked(showAsteroidRings = DisplayMPOccultations.ShowAsteroidRings);
			obj.set_Checked(showAsteroidRings);
			DisplayMPOccultations.PlotPath();
		}

		private void displayToolStripMenuItem_Click(object sender, EventArgs e)
		{
			DisplayAsteroidSatteliteOrbits();
		}

		private void listAsteroidIRDiametersToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (DisplayMPOccultations.AsteroidNumber > 0)
			{
				Utilities.Display_IR_AsteroidDiameter(DisplayMPOccultations.AsteroidNumber, ShowInForm: true, out var _);
			}
		}

		private void listAsteroidIRDiametersToolStripMenuItem1_Click(object sender, EventArgs e)
		{
			if (DisplayMPOccultations.AsteroidNumber > 0)
			{
				Utilities.Display_IR_AsteroidDiameter(DisplayMPOccultations.AsteroidNumber, ShowInForm: true, out var _);
			}
		}

		private void displayAsteroidSatelliteOrbitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			DisplayAsteroidSatteliteOrbits();
		}

		private void DisplayAsteroidSatteliteOrbits()
		{
		}

		private void listPreviousOccultionsOfThisStarToolStripMenuItem1_Click(object sender, EventArgs e)
		{
			ShowPreviousOccultations();
		}

		private void drawStarChartInC2AToolStripMenuItem_Click(object sender, EventArgs e)
		{
			C2A.DrawC2AStarChart();
		}

		private void c2AStarChartToolStripMenuItem_Click(object sender, EventArgs e)
		{
			C2A.DrawC2AStarChart();
		}

		private void occultSettingsSetToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0005: Unknown result type (might be due to invalid IL or missing references)
			((Form)new Defaults()).ShowDialog();
		}

		private void copyToolStripMenuItem1_Click(object sender, EventArgs e)
		{
			Clipboard.SetImage(picAsteroidPlot.get_Image());
		}

		private void printPreviewToolStripMenuItem1_Click(object sender, EventArgs e)
		{
			DisplayMPOccultations.PrintPreviewPathGraphic();
		}

		private void saveToolStripMenuItem1_Click(object sender, EventArgs e)
		{
			Settings.Default.Save_AsteroidPredictions = Output.SaveGraphic(picAsteroidPlot.get_Image(), DisplayMPOccultations.UTDate_Numeric.Trim() + " " + DisplayMPOccultations.AsteroidName.Trim(), Settings.Default.Save_AsteroidPredictions);
		}

		private void gEarthToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_001a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0032: Unknown result type (might be due to invalid IL or missing references)
			if (DisplayMPOccultations.PathCoordinates.Count < 1)
			{
				MessageBox.Show("KMZ file cannot be created, as\r\nno part of the path crosses the Earth", "No .KMZ file", (MessageBoxButtons)0, (MessageBoxIcon)16);
			}
			else if (!Utilities.InternetIsAvailable())
			{
				MessageBox.Show("This functionality requires internet access. You are not connected to the internet", "No internet");
			}
			else
			{
				DisplayMPOccultations.CreateGoogleEarthKMLFile(AppPath + "\\AutoGenerated Asteroids\\AsteroidPath.KML", DisplayMPOccultations.EventHeader.Substring(15).Trim(), Auto: true, View: true, cmbSiteFiles.get_Items().get_Item(((ListControl)cmbSiteFiles).get_SelectedIndex()).ToString());
			}
		}

		private void pathToolStripMenuItem_Click(object sender, EventArgs e)
		{
			pathCoordinatesToolStripMenuItem_Click(sender, e);
		}

		private void starChartToolStripMenuItem1_Click(object sender, EventArgs e)
		{
			DisplayMPOccultations.LargeStarChart();
		}

		private void prepointToolStripMenuItem_Click(object sender, EventArgs e)
		{
			listOfPrepointStarsToolStripMenuItem_Click(sender, e);
		}

		private void multilocationToolStripMenuItem_Click(object sender, EventArgs e)
		{
			siteDistancesFromThePathToolStripMenuItem_Click(sender, e);
		}

		private void planetConfigToolStripMenuItem_Click(object sender, EventArgs e)
		{
			showPlanetConfigurationToolStripMenuItem_Click(sender, e);
		}

		private void starChartUseEnhancedToolStripMenuItem_Click(object sender, EventArgs e)
		{
			starChartUseEnhancedToolStripMenuItem.set_Checked(!starChartUseEnhancedToolStripMenuItem.get_Checked());
			Settings.Default.AsteroidStarChartEnhanced = starChartUseEnhancedToolStripMenuItem.get_Checked();
			DisplayMPOccultations.GetEphemerisForStarChart();
			DisplayMPOccultations.PlotPath();
		}

		private void copyOccultationElementsForThisEventToolStripMenuItem_Click(object sender, EventArgs e)
		{
			DisplayMPOccultations.CopyCurrentOccultationElements();
		}

		private void lightCurveSimulatorToolStripMenuItem_Click(object sender, EventArgs e)
		{
			DisplayMPOccultations.Show_PredictionLightCurves(ForAnEvent: true);
			DisplayMPOccultations.PredictionLightCurves.optEllipse.set_Checked(true);
			NumericUpDown updnAsteroidDiameter_mas = DisplayMPOccultations.PredictionLightCurves.updnAsteroidDiameter_mas;
			decimal value;
			DisplayMPOccultations.PredictionLightCurves.updnMinor_mas.set_Value(value = (decimal)DisplayMPOccultations.AsteroidAngularDiameterMAS);
			updnAsteroidDiameter_mas.set_Value(value);
			DisplayMPOccultations.PredictionLightCurves.updnParallax.set_Value((decimal)(8.794143836182533 / DisplayMPOccultations.DistanceToAsteroid));
			DisplayMPOccultations.PredictionLightCurves.updnAsteroidMag.set_Value((decimal)DisplayMPOccultations.MAsteroid);
			NumericUpDown updnAsteroidDia_km = DisplayMPOccultations.PredictionLightCurves.updnAsteroidDia_km;
			DisplayMPOccultations.PredictionLightCurves.updnMinor_km.set_Value(value = (decimal)DisplayMPOccultations.AsteroidDiameter);
			updnAsteroidDia_km.set_Value(value);
			DisplayMPOccultations.PredictionLightCurves.updnStarDiameter.set_Value((decimal)DisplayMPOccultations.StellarDiameter_mas);
			DisplayMPOccultations.PredictionLightCurves.updnStarMag.set_Value((decimal)DisplayMPOccultations.Mv);
			DisplayMPOccultations.PredictionLightCurves.updn_masPERsec.set_Value((decimal)(DisplayMPOccultations.n * 8.794143836182533 / DisplayMPOccultations.DistanceToAsteroid / 3.6));
			int num = Convert.ToInt32(Math.Atan2(DisplayMPOccultations.DeltaX, DisplayMPOccultations.DeltaY) * (180.0 / Math.PI));
			if (num < 0)
			{
				num += 360;
			}
			DisplayMPOccultations.PredictionLightCurves.PAofPathDirection = num;
			((Control)DisplayMPOccultations.PredictionLightCurves.lblPathPA).set_Text(num + "°");
			double num2 = DisplayMPOccultations.Xatmin * DisplayMPOccultations.Xatmin + DisplayMPOccultations.YatMin * DisplayMPOccultations.YatMin;
			if (num2 > 0.9)
			{
				DisplayMPOccultations.PredictionLightCurves.TopoKmRatio = 1f;
				DisplayMPOccultations.PredictionLightCurves.chkTopoDistances.set_Checked(false);
				((Control)DisplayMPOccultations.PredictionLightCurves.chkTopoDistances).set_Enabled(false);
			}
			else
			{
				DisplayMPOccultations.PredictionLightCurves.TopoKmRatio = 1f / (float)Math.Sqrt(1.0 - num2);
				((Control)DisplayMPOccultations.PredictionLightCurves.chkTopoDistances).set_Enabled(true);
			}
			((Control)DisplayMPOccultations.PredictionLightCurves.lblEvent).set_Text(DisplayMPOccultations.AsteroidName + "   occults   " + DisplayMPOccultations.StarNo + ",   " + DisplayMPOccultations.UTDate);
			DisplayMPOccultations.PredictionLightCurves.ObjectID = DisplayMPOccultations.AsteroidNumber.ToString();
			DisplayMPOccultations.PredictionLightCurves.ReSize();
			DisplayMPOccultations.PredictionLightCurves.Plot(GenerateModel: true);
		}

		private void optSiteCentered_Click(object sender, EventArgs e)
		{
			optTimeCentered.set_Checked(!optTimeCentered.get_Checked());
			if (DisplayMPOccultations.ListAndDisplay != null)
			{
				DisplayMPOccultations.CenteredUsingTimeOnPath = optTimeCentered.get_Checked();
				SetRedrawSiteCoords();
				DisplayMPOccultations.PlotPath();
			}
		}

		private void copyOccultationElementsForThisEventToolStripMenuItem1_Click(object sender, EventArgs e)
		{
			DisplayMPOccultations.CopyCurrentOccultationElements();
		}

		private void gaiaDoubleStarsnearbyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Gaia_Doubles();
		}

		private void gaiaDoublesDisplayNearbyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Gaia_Doubles();
		}

		private void Gaia_Doubles()
		{
			GaiaDoubles.Elements.Show_GaiaDoubles();
			string text = Utilities.DEGtoDMS(DisplayMPOccultations.StarRA_2000 * (180.0 / Math.PI) / 15.0, 2, 2, MinutesOnly: false);
			string text2 = Utilities.DEGtoDMS(DisplayMPOccultations.StarDec_2000 * (180.0 / Math.PI), 3, 1, MinutesOnly: false);
			((Control)GaiaDoubles.Elements.GaiaDoubles.txtRAh).set_Text(text.Substring(0, 2));
			((Control)GaiaDoubles.Elements.GaiaDoubles.txtRAm).set_Text(text.Substring(3, 2));
			((Control)GaiaDoubles.Elements.GaiaDoubles.txtRAs).set_Text(text.Substring(6, 5));
			((Control)GaiaDoubles.Elements.GaiaDoubles.txtDecD).set_Text(text2.Substring(0, 3));
			((Control)GaiaDoubles.Elements.GaiaDoubles.txtDecM).set_Text(text2.Substring(4, 2));
			((Control)GaiaDoubles.Elements.GaiaDoubles.txtDecS).set_Text(text2.Substring(7, 4));
			GaiaDoubles.Elements.GaiaDoubles.DisplayDoubles_Within15arcmin();
		}

		private void displayPredictionLightCurveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			DisplayPredictionLightCurve();
		}

		private void DisplayPredictionLightCurve()
		{
			try
			{
				((Control)PredictionLightCurves).Show();
			}
			catch
			{
				PredictionLightCurves = new PredictionLightCurve();
				((Control)PredictionLightCurves).Show();
			}
			((Control)PredictionLightCurves).Focus();
			((Control)PredictionLightCurves).set_Text("Light curve prediction:  " + DisplayMPOccultations.AsteroidName + "  occults  " + DisplayMPOccultations.StarNo + "  on  " + DisplayMPOccultations.PredictionDate.Substring(14));
			((Control)PredictionLightCurves.updn_masPERsec).set_Enabled(false);
			((Control)PredictionLightCurves.updnParallax).set_Enabled(false);
			((Control)PredictionLightCurves.chkTopoDistances).set_Enabled(true);
			string Basis;
			int NumMeasures;
			bool InvalidDiameter;
			double num = Utilities.StarDiameter_CHARM2_CADARS(DisplayMPOccultations.StarRA_2000 * (180.0 / Math.PI) / 15.0, DisplayMPOccultations.StarDec_2000 * (180.0 / Math.PI), DisplayMPOccultations.Mv, DisplayMPOccultations.Mp, DisplayMPOccultations.Mr, out Basis, out NumMeasures, out InvalidDiameter);
			_ = num / (DisplayMPOccultations.AsteroidDiameter / 725.3 / DisplayMPOccultations.DistanceToAsteroid) * DisplayMPOccultations.MaxDurn / num;
			_ = 0.0;
			if (DisplayMPOccultations.AsteroidAngularDiameterMAS > 400.0)
			{
				DisplayMPOccultations.AsteroidAngularDiameterMAS = 400.0;
			}
			NumericUpDown updnMinor_mas = PredictionLightCurves.updnMinor_mas;
			decimal value;
			PredictionLightCurves.updnAsteroidDiameter_mas.set_Value(value = (decimal)DisplayMPOccultations.AsteroidAngularDiameterMAS);
			updnMinor_mas.set_Value(value);
			NumericUpDown updnMinor_km = PredictionLightCurves.updnMinor_km;
			PredictionLightCurves.updnAsteroidDia_km.set_Value(value = (decimal)DisplayMPOccultations.AsteroidDiameter);
			updnMinor_km.set_Value(value);
			PredictionLightCurves.updnParallax.set_Value((decimal)(8.794 / DisplayMPOccultations.DistanceToAsteroid));
			PredictionLightCurves.updnAsteroidMag.set_Value((decimal)DisplayMPOccultations.MAsteroid);
			if (num < 1E-05)
			{
				num = 1E-05;
			}
			PredictionLightCurves.updnStarDiameter.set_Value((decimal)(num * 1000.0));
			PredictionLightCurves.updnStarMag.set_Value((decimal)DisplayMPOccultations.Mv);
			PredictionLightCurves.updn_masPERsec.set_Value((decimal)(8.794 / DisplayMPOccultations.DistanceToAsteroid * DisplayMPOccultations.n / 3.6));
			((Control)PredictionLightCurves.updn_masPERsec).set_Enabled(true);
			((Control)PredictionLightCurves.updnParallax).set_Enabled(true);
			((Control)PredictionLightCurves.chkTopoDistances).set_Enabled(false);
			PredictionLightCurves.TopoKmRatio = 1f;
			((Control)PredictionLightCurves.lblEvent).set_Text("Non-specific event lightcurve");
			PredictionLightCurves.ObjectID = DisplayMPOccultations.AsteroidName;
			PredictionLightCurves.Plot(GenerateModel: true);
		}

		private void displayPredictionLightCurveToolStripMenuItem1_Click(object sender, EventArgs e)
		{
			DisplayPredictionLightCurve();
		}

		private void astrometryAtMinorPlanetCenterOccnSearchFor275ToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (DisplayMPOccultations.AsteroidNumber > 0)
			{
				string text = DisplayMPOccultations.AsteroidNumber.ToString();
				Process.Start("https://www.minorplanetcenter.net/db_search/show_object?object_id=" + text);
			}
		}

		private void astrometry_MPC_Click(object sender, EventArgs e)
		{
			if (DisplayMPOccultations.AsteroidNumber > 0)
			{
				string text = DisplayMPOccultations.AsteroidNumber.ToString();
				Process.Start("https://www.minorplanetcenter.net/db_search/show_object?object_id=" + text);
			}
		}

		private void binaryAsteroidsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			BinaryAsteroids.BinaryAsteroidDetails(DisplayMPOccultations.AsteroidNumber, DisplayMPOccultations.AsteroidName);
		}

		private void binaryAsteroidsToolStripMenuItem1_Click(object sender, EventArgs e)
		{
			BinaryAsteroids.BinaryAsteroidDetails(DisplayMPOccultations.AsteroidNumber, DisplayMPOccultations.AsteroidName);
		}

		private void toolStripNearbyStars_Click(object sender, EventArgs e)
		{
			ListNearbyStars();
		}

		private void listNearbyStarsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ListNearbyStars();
		}

		private static void ListNearbyStars()
		{
			Gaia.GetGaiaCatalogueForNearbyStars();
			string text = DisplayMPOccultations.StarNo;
			int num = text.IndexOf("(");
			if (num > 0)
			{
				text = text.Substring(0, num - 1).Trim();
			}
			Gaia.GetNearbyStarsFromCoords(text, DisplayMPOccultations.StarRA_2000, DisplayMPOccultations.StarDec_2000, DisplayMPOccultations.Mv, Utilities.BesselianYear(DisplayMPOccultations.EventJD) - 2000.0, 30.0, out var _, out var _, out var _, out var _, out var AllNearby);
			try
			{
				((Control)NearbyStars).Show();
			}
			catch
			{
				NearbyStars = new DisplayData();
				((Control)NearbyStars).Show();
			}
			AllNearby = "Stars within  30\" of\r\n" + DisplayMPOccultations.StarNo + "\r\n\r\n Sepn    Mv     Mr\r\n\r\n" + string.Format("Target  {0,5:f2}  {1,5:f2}\r\n", DisplayMPOccultations.Mv, DisplayMPOccultations.Mr) + AllNearby;
			((Control)NearbyStars).set_Text("Nearby stars");
			((Control)NearbyStars.txtBox).set_Text(AllNearby);
			((Control)NearbyStars).set_Width(320);
			((Control)NearbyStars).set_Height(260);
			NearbyStars.NoColors();
			((TextBoxBase)NearbyStars.txtBox).set_SelectionStart(0);
			((TextBoxBase)NearbyStars.txtBox).Select(0, 0);
			((Control)NearbyStars).Focus();
		}

		private void jPLSmallBodyDatabaseForThisAsteroidOrCometToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Get_JPL_SmallBodiesDatabase();
		}

		private void toolStripJPLsmallbodies_Click(object sender, EventArgs e)
		{
			Get_JPL_SmallBodiesDatabase();
		}

		private static void Get_JPL_SmallBodiesDatabase()
		{
			string text = "";
			if (!(DisplayMPOccultations.ObjectID.PadRight(2).Substring(0, 2) == " P"))
			{
				if (DisplayMPOccultations.ObjectName.Contains("P/") | DisplayMPOccultations.ObjectName.Contains("C/"))
				{
					int length = DisplayMPOccultations.ObjectName.IndexOf("/");
					text = DisplayMPOccultations.ObjectName.Substring(0, length);
				}
				else if (DisplayMPOccultations.AsteroidNumber > 0)
				{
					text = DisplayMPOccultations.AsteroidNumber.ToString();
				}
				else if (DisplayMPOccultations.ObjectName.Length > 0)
				{
					text = DisplayMPOccultations.ObjectName;
				}
				if (text.Length > 1)
				{
					Process.Start("https://ssd.jpl.nasa.gov/tools/sbdb_lookup.html#/?sstr=" + text);
				}
			}
		}

		private void listPreviousOccultionsOfThisStarToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ShowPreviousOccultations();
		}

		private void ShowPreviousOccultations()
		{
			try
			{
				((Control)DataBox).Show();
			}
			catch
			{
				DataBox = new DisplayData();
				((Control)DataBox).Show();
			}
			((ToolStripItem)DataBox.cmdCancel).set_Visible(false);
			((ToolStripItem)DataBox.cmdOK).set_Visible(false);
			((Control)DataBox.txtBox).set_Text(Asteroid_Observations_Reports.FindAsteroidOccultedStars(DisplayMPOccultations.StarRA_2000, DisplayMPOccultations.StarDec_2000, out var _, out var _));
			((Control)DataBox).set_Width(550);
			((Control)DataBox).set_Height(160);
			((Control)DataBox).set_Text("Previous occultations");
			((TextBoxBase)DataBox.txtBox).Select(0, 0);
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
			//IL_04f2: Unknown result type (might be due to invalid IL or missing references)
			//IL_04fc: Expected O, but got Unknown
			//IL_04fd: Unknown result type (might be due to invalid IL or missing references)
			//IL_0507: Expected O, but got Unknown
			//IL_0508: Unknown result type (might be due to invalid IL or missing references)
			//IL_0512: Expected O, but got Unknown
			//IL_0513: Unknown result type (might be due to invalid IL or missing references)
			//IL_051d: Expected O, but got Unknown
			//IL_051e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0528: Expected O, but got Unknown
			//IL_0529: Unknown result type (might be due to invalid IL or missing references)
			//IL_0533: Expected O, but got Unknown
			//IL_053a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0544: Expected O, but got Unknown
			//IL_0545: Unknown result type (might be due to invalid IL or missing references)
			//IL_054f: Expected O, but got Unknown
			//IL_0550: Unknown result type (might be due to invalid IL or missing references)
			//IL_055a: Expected O, but got Unknown
			//IL_055b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0565: Expected O, but got Unknown
			//IL_0566: Unknown result type (might be due to invalid IL or missing references)
			//IL_0570: Expected O, but got Unknown
			//IL_0571: Unknown result type (might be due to invalid IL or missing references)
			//IL_057b: Expected O, but got Unknown
			//IL_057c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0586: Expected O, but got Unknown
			//IL_0587: Unknown result type (might be due to invalid IL or missing references)
			//IL_0591: Expected O, but got Unknown
			//IL_0592: Unknown result type (might be due to invalid IL or missing references)
			//IL_059c: Expected O, but got Unknown
			//IL_059d: Unknown result type (might be due to invalid IL or missing references)
			//IL_05a7: Expected O, but got Unknown
			//IL_05a8: Unknown result type (might be due to invalid IL or missing references)
			//IL_05b2: Expected O, but got Unknown
			//IL_05b3: Unknown result type (might be due to invalid IL or missing references)
			//IL_05bd: Expected O, but got Unknown
			//IL_05be: Unknown result type (might be due to invalid IL or missing references)
			//IL_05c8: Expected O, but got Unknown
			//IL_05c9: Unknown result type (might be due to invalid IL or missing references)
			//IL_05d3: Expected O, but got Unknown
			//IL_05d4: Unknown result type (might be due to invalid IL or missing references)
			//IL_05de: Expected O, but got Unknown
			//IL_05df: Unknown result type (might be due to invalid IL or missing references)
			//IL_05e9: Expected O, but got Unknown
			//IL_05ea: Unknown result type (might be due to invalid IL or missing references)
			//IL_05f4: Expected O, but got Unknown
			//IL_05f5: Unknown result type (might be due to invalid IL or missing references)
			//IL_05ff: Expected O, but got Unknown
			//IL_0600: Unknown result type (might be due to invalid IL or missing references)
			//IL_060a: Expected O, but got Unknown
			//IL_060b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0615: Expected O, but got Unknown
			//IL_0616: Unknown result type (might be due to invalid IL or missing references)
			//IL_0620: Expected O, but got Unknown
			//IL_0621: Unknown result type (might be due to invalid IL or missing references)
			//IL_062b: Expected O, but got Unknown
			//IL_062c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0636: Expected O, but got Unknown
			//IL_0637: Unknown result type (might be due to invalid IL or missing references)
			//IL_0641: Expected O, but got Unknown
			//IL_0642: Unknown result type (might be due to invalid IL or missing references)
			//IL_064c: Expected O, but got Unknown
			//IL_064d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0657: Expected O, but got Unknown
			//IL_0658: Unknown result type (might be due to invalid IL or missing references)
			//IL_0662: Expected O, but got Unknown
			//IL_0663: Unknown result type (might be due to invalid IL or missing references)
			//IL_066d: Expected O, but got Unknown
			//IL_066e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0678: Expected O, but got Unknown
			//IL_0679: Unknown result type (might be due to invalid IL or missing references)
			//IL_0683: Expected O, but got Unknown
			//IL_0684: Unknown result type (might be due to invalid IL or missing references)
			//IL_068e: Expected O, but got Unknown
			//IL_068f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0699: Expected O, but got Unknown
			//IL_069a: Unknown result type (might be due to invalid IL or missing references)
			//IL_06a4: Expected O, but got Unknown
			//IL_06a5: Unknown result type (might be due to invalid IL or missing references)
			//IL_06af: Expected O, but got Unknown
			//IL_06b0: Unknown result type (might be due to invalid IL or missing references)
			//IL_06ba: Expected O, but got Unknown
			//IL_06bb: Unknown result type (might be due to invalid IL or missing references)
			//IL_06c5: Expected O, but got Unknown
			//IL_06c6: Unknown result type (might be due to invalid IL or missing references)
			//IL_06d0: Expected O, but got Unknown
			//IL_06d1: Unknown result type (might be due to invalid IL or missing references)
			//IL_06db: Expected O, but got Unknown
			//IL_06dc: Unknown result type (might be due to invalid IL or missing references)
			//IL_06e6: Expected O, but got Unknown
			//IL_06e7: Unknown result type (might be due to invalid IL or missing references)
			//IL_06f1: Expected O, but got Unknown
			//IL_06f2: Unknown result type (might be due to invalid IL or missing references)
			//IL_06fc: Expected O, but got Unknown
			//IL_06fd: Unknown result type (might be due to invalid IL or missing references)
			//IL_0707: Expected O, but got Unknown
			//IL_0708: Unknown result type (might be due to invalid IL or missing references)
			//IL_0712: Expected O, but got Unknown
			//IL_0713: Unknown result type (might be due to invalid IL or missing references)
			//IL_071d: Expected O, but got Unknown
			//IL_071e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0728: Expected O, but got Unknown
			//IL_0729: Unknown result type (might be due to invalid IL or missing references)
			//IL_0733: Expected O, but got Unknown
			//IL_0734: Unknown result type (might be due to invalid IL or missing references)
			//IL_073e: Expected O, but got Unknown
			//IL_073f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0749: Expected O, but got Unknown
			//IL_074a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0754: Expected O, but got Unknown
			//IL_0755: Unknown result type (might be due to invalid IL or missing references)
			//IL_075f: Expected O, but got Unknown
			//IL_0760: Unknown result type (might be due to invalid IL or missing references)
			//IL_076a: Expected O, but got Unknown
			//IL_076b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0775: Expected O, but got Unknown
			//IL_0776: Unknown result type (might be due to invalid IL or missing references)
			//IL_0780: Expected O, but got Unknown
			//IL_0781: Unknown result type (might be due to invalid IL or missing references)
			//IL_078b: Expected O, but got Unknown
			//IL_078c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0796: Expected O, but got Unknown
			//IL_0797: Unknown result type (might be due to invalid IL or missing references)
			//IL_07a1: Expected O, but got Unknown
			//IL_07a2: Unknown result type (might be due to invalid IL or missing references)
			//IL_07ac: Expected O, but got Unknown
			//IL_07ad: Unknown result type (might be due to invalid IL or missing references)
			//IL_07b7: Expected O, but got Unknown
			//IL_07b8: Unknown result type (might be due to invalid IL or missing references)
			//IL_07c2: Expected O, but got Unknown
			//IL_54d1: Unknown result type (might be due to invalid IL or missing references)
			//IL_54db: Expected O, but got Unknown
			//IL_54e8: Unknown result type (might be due to invalid IL or missing references)
			//IL_54f2: Expected O, but got Unknown
			//IL_5621: Unknown result type (might be due to invalid IL or missing references)
			//IL_562b: Expected O, but got Unknown
			//IL_5693: Unknown result type (might be due to invalid IL or missing references)
			//IL_569d: Expected O, but got Unknown
			components = new Container();
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(AsteroidPlotPath));
			menuStrip1 = new MenuStrip();
			withThisEventToolStripMenuItem = new ToolStripMenuItem();
			eventToolStripMenuItem1 = new ToolStripMenuItem();
			toolStripSeparator19 = new ToolStripSeparator();
			largeStarChartToolStripMenuItem = new ToolStripMenuItem();
			pathCoordinatesToolStripMenuItem = new ToolStripMenuItem();
			siteDistancesFromThePathToolStripMenuItem = new ToolStripMenuItem();
			listOfPrepointStarsToolStripMenuItem = new ToolStripMenuItem();
			drawStarChartInC2AToolStripMenuItem = new ToolStripMenuItem();
			showPlanetConfigurationToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator3 = new ToolStripSeparator();
			asteroidRingsContactTimesForMultipleLocationsToolStripMenuItem = new ToolStripMenuItem();
			planetContactTimesForMultipleLocationsToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator8 = new ToolStripSeparator();
			toolStripMenuItem2 = new ToolStripMenuItem();
			toolStripSeparator20 = new ToolStripSeparator();
			displayStarsDiameterFresnelDiffractionToolStripMenuItem = new ToolStripMenuItem();
			displayWDSInterferometricDataToolStripMenuItem = new ToolStripMenuItem();
			gaiaDoubleStarsnearbyToolStripMenuItem = new ToolStripMenuItem();
			toolStripNearbyStars = new ToolStripMenuItem();
			listPreviousOccultionsOfThisStarToolStripMenuItem = new ToolStripMenuItem();
			displayPredictionLightCurveToolStripMenuItem1 = new ToolStripMenuItem();
			viewStarInGoogleSkyToolStripMenuItem1 = new ToolStripMenuItem();
			compareStarCataloguePositionsToolStripMenuItem1 = new ToolStripMenuItem();
			listStarDetailsThroughTheCDSPortalToolStripMenuItem = new ToolStripMenuItem();
			plotStarUsingCDSSimPlayToolStripMenuItem1 = new ToolStripMenuItem();
			listRelativisticDisplacementToolStripMenuItem = new ToolStripMenuItem();
			astrometry_MPC = new ToolStripMenuItem();
			toolStripSeparator11 = new ToolStripSeparator();
			asteroidToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator21 = new ToolStripSeparator();
			listAsteroidIRDiametersToolStripMenuItem = new ToolStripMenuItem();
			jPLSmallBodyDatabaseForThisAsteroidOrCometToolStripMenuItem = new ToolStripMenuItem();
			binaryAsteroidsToolStripMenuItem = new ToolStripMenuItem();
			displayAsteroidSatelliteOrbitToolStripMenuItem = new ToolStripMenuItem();
			displayRingSystemToolStripMenuItem = new ToolStripMenuItem();
			lightCurveDataToolStripMenuItem1 = new ToolStripMenuItem();
			dAMITAsteroidShapesToolStripMenuItem1 = new ToolStripMenuItem();
			toolStripSeparator22 = new ToolStripSeparator();
			otherToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator10 = new ToolStripSeparator();
			showRegionalWeatherPrediction16DayLimitToolStripMenuItem1 = new ToolStripMenuItem();
			viewInGoogleEarthToolStripMenuItem1 = new ToolStripMenuItem();
			saveAsGoogleEarthKMZFileToolStripMenuItem = new ToolStripMenuItem();
			saveAsGoogleMapsHTMFileToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator13 = new ToolStripSeparator();
			copyToolStripMenuItem = new ToolStripMenuItem();
			copyOccultationElementsForThisEventToolStripMenuItem = new ToolStripMenuItem();
			printPreviewToolStripMenuItem = new ToolStripMenuItem();
			printToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			optionsToolStripMenuItem = new ToolStripMenuItem();
			occultSettingsSetToolStripMenuItem = new ToolStripMenuItem();
			exportButtonsDisplayedToolStripMenuItem = new ToolStripMenuItem();
			includeStarChartToolStripMenuItem = new ToolStripMenuItem();
			starChartUseEnhancedToolStripMenuItem = new ToolStripMenuItem();
			showErrorEllipseToolStripMenuItem = new ToolStripMenuItem();
			showCoordinatesInTootipToolStripMenuItem = new ToolStripMenuItem();
			show1sigmaLinesAsContinuousToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator2 = new ToolStripSeparator();
			colourToolStripMenuItem = new ToolStripMenuItem();
			setPlotRegionToStandardSizesToolStripMenuItem = new ToolStripMenuItem();
			x325ToolStripMenuItem = new ToolStripMenuItem();
			x480ToolStripMenuItem = new ToolStripMenuItem();
			x600ToolStripMenuItem = new ToolStripMenuItem();
			x768ToolStripMenuItem = new ToolStripMenuItem();
			x1024ToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator15 = new ToolStripSeparator();
			lightCurveSimulatorToolStripMenuItem = new ToolStripMenuItem();
			redrawToolStripMenuItem = new ToolStripMenuItem();
			scale001100ToolStripMenuItem = new ToolStripMenuItem();
			scale01ToolStripMenuItem = new ToolStripMenuItem();
			scale03ToolStripMenuItem = new ToolStripMenuItem();
			scale1ToolStripMenuItem = new ToolStripMenuItem();
			scale2ToolStripMenuItem = new ToolStripMenuItem();
			scale4ToolStripMenuItem = new ToolStripMenuItem();
			scale8ToolStripMenuItem = new ToolStripMenuItem();
			scale16ToolStripMenuItem = new ToolStripMenuItem();
			scale32ToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator1 = new ToolStripSeparator();
			plotToolStripMenuItem = new ToolStripMenuItem();
			nextEventToolStripMenuItem = new ToolStripMenuItem();
			previousEventToolStripMenuItem = new ToolStripMenuItem();
			gEarthToolStripMenuItem = new ToolStripMenuItem();
			pathToolStripMenuItem = new ToolStripMenuItem();
			starChartToolStripMenuItem1 = new ToolStripMenuItem();
			prepointToolStripMenuItem = new ToolStripMenuItem();
			multilocationToolStripMenuItem = new ToolStripMenuItem();
			planetConfigToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			PanelExport = new Panel();
			cmdExportG = new Button();
			cmdExportF = new Button();
			cmdExportE = new Button();
			cmdExportD = new Button();
			cmdExportC = new Button();
			cmdExportB = new Button();
			cmdExportA = new Button();
			updnLongitude = new NumericUpDown();
			updnLatitude = new NumericUpDown();
			label1 = new Label();
			label2 = new Label();
			cmdThis = new Button();
			cmdx1 = new Button();
			cmdx2 = new Button();
			cmdx4 = new Button();
			cmdx8 = new Button();
			cmdx16 = new Button();
			cmdx32 = new Button();
			cmdx01 = new Button();
			label3 = new Label();
			toolTip1 = new ToolTip(components);
			optTimeCentered = new RadioButton();
			cmdPrevious = new Button();
			cmdNext = new Button();
			label4 = new Label();
			label5 = new Label();
			ctxtMenu = new ContextMenuStrip(components);
			eventToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator18 = new ToolStripSeparator();
			starChartToolStripMenuItem = new ToolStripMenuItem();
			pathCoordinatesToolStripMenuItem1 = new ToolStripMenuItem();
			multiToolStripMenuItem = new ToolStripMenuItem();
			prepointStarsToolStripMenuItem = new ToolStripMenuItem();
			c2AStarChartToolStripMenuItem = new ToolStripMenuItem();
			planetAppearanceToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator12 = new ToolStripSeparator();
			asteroidRingsContactTimesForMultipleLocdationsToolStripMenuItem = new ToolStripMenuItem();
			contactTimesForMultipleLocationsToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator6 = new ToolStripSeparator();
			starToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator17 = new ToolStripSeparator();
			displayMeasuredestimatedStellarDiameterToolStripMenuItem = new ToolStripMenuItem();
			displayWDSInterferometricDataToolStripMenuItem1 = new ToolStripMenuItem();
			gaiaDoublesDisplayNearbyToolStripMenuItem = new ToolStripMenuItem();
			listNearbyStarsToolStripMenuItem = new ToolStripMenuItem();
			displayPredictionLightCurveToolStripMenuItem = new ToolStripMenuItem();
			viewStarInGoogleSkyToolStripMenuItem = new ToolStripMenuItem();
			compareStarCataloguePositionsToolStripMenuItem = new ToolStripMenuItem();
			getStarInCDSPortalToolStripMenuItem = new ToolStripMenuItem();
			plotStarUsingCDSSimPlayToolStripMenuItem = new ToolStripMenuItem();
			listRelativisticOffsetToolStripMenuItem = new ToolStripMenuItem();
			astrometryAtMinorPlanetCenterOccnSearchFor275ToolStripMenuItem = new ToolStripMenuItem();
			listPreviousOccultionsOfThisStarToolStripMenuItem1 = new ToolStripMenuItem();
			toolStripSeparator9 = new ToolStripSeparator();
			asteroidDetailsToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator16 = new ToolStripSeparator();
			listAsteroidIRDiametersToolStripMenuItem1 = new ToolStripMenuItem();
			binaryAsteroidsToolStripMenuItem1 = new ToolStripMenuItem();
			displayToolStripMenuItem = new ToolStripMenuItem();
			displayAsteroidsRingSystemToolStripMenuItem = new ToolStripMenuItem();
			lightCurveDataToolStripMenuItem = new ToolStripMenuItem();
			dAMITAsteroidShapesToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator4 = new ToolStripSeparator();
			otherToolStripMenuItem1 = new ToolStripMenuItem();
			toolStripSeparator7 = new ToolStripSeparator();
			showRegionalWeatherPrediction16DayLimitToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator5 = new ToolStripSeparator();
			viewInGoogleEarthToolStripMenuItem = new ToolStripMenuItem();
			saveAsGoogleEarthKMLFileToolStripMenuItem = new ToolStripMenuItem();
			saveAsGToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator14 = new ToolStripSeparator();
			copyToolStripMenuItem1 = new ToolStripMenuItem();
			copyOccultationElementsForThisEventToolStripMenuItem1 = new ToolStripMenuItem();
			printPreviewToolStripMenuItem1 = new ToolStripMenuItem();
			printToolStripMenuItem1 = new ToolStripMenuItem();
			saveToolStripMenuItem1 = new ToolStripMenuItem();
			pBarPlot = new ProgressBar();
			cmbSiteFiles = new ComboBox();
			cmbNames = new ComboBox();
			label6 = new Label();
			panelScale = new Panel();
			panelLocation = new Panel();
			panelExportPlusSites = new Panel();
			picAsteroidPlot = new PictureBox();
			toolStripJPLsmallbodies = new ToolStripMenuItem();
			((Control)menuStrip1).SuspendLayout();
			((Control)PanelExport).SuspendLayout();
			((ISupportInitialize)updnLongitude).BeginInit();
			((ISupportInitialize)updnLatitude).BeginInit();
			((Control)ctxtMenu).SuspendLayout();
			((Control)panelScale).SuspendLayout();
			((Control)panelLocation).SuspendLayout();
			((Control)panelExportPlusSites).SuspendLayout();
			((ISupportInitialize)picAsteroidPlot).BeginInit();
			((Control)this).SuspendLayout();
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[12]
			{
				(ToolStripItem)withThisEventToolStripMenuItem,
				(ToolStripItem)optionsToolStripMenuItem,
				(ToolStripItem)redrawToolStripMenuItem,
				(ToolStripItem)plotToolStripMenuItem,
				(ToolStripItem)gEarthToolStripMenuItem,
				(ToolStripItem)pathToolStripMenuItem,
				(ToolStripItem)starChartToolStripMenuItem1,
				(ToolStripItem)prepointToolStripMenuItem,
				(ToolStripItem)multilocationToolStripMenuItem,
				(ToolStripItem)planetConfigToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			menuStrip1.set_ShowItemToolTips(true);
			((Control)menuStrip1).set_Size(new Size(830, 24));
			((Control)menuStrip1).set_TabIndex(1);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withThisEventToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[49]
			{
				(ToolStripItem)eventToolStripMenuItem1,
				(ToolStripItem)toolStripSeparator19,
				(ToolStripItem)largeStarChartToolStripMenuItem,
				(ToolStripItem)pathCoordinatesToolStripMenuItem,
				(ToolStripItem)siteDistancesFromThePathToolStripMenuItem,
				(ToolStripItem)listOfPrepointStarsToolStripMenuItem,
				(ToolStripItem)drawStarChartInC2AToolStripMenuItem,
				(ToolStripItem)showPlanetConfigurationToolStripMenuItem,
				(ToolStripItem)toolStripSeparator3,
				(ToolStripItem)asteroidRingsContactTimesForMultipleLocationsToolStripMenuItem,
				(ToolStripItem)planetContactTimesForMultipleLocationsToolStripMenuItem,
				(ToolStripItem)toolStripSeparator8,
				(ToolStripItem)toolStripMenuItem2,
				(ToolStripItem)toolStripSeparator20,
				(ToolStripItem)displayStarsDiameterFresnelDiffractionToolStripMenuItem,
				(ToolStripItem)displayWDSInterferometricDataToolStripMenuItem,
				(ToolStripItem)gaiaDoubleStarsnearbyToolStripMenuItem,
				(ToolStripItem)toolStripNearbyStars,
				(ToolStripItem)listPreviousOccultionsOfThisStarToolStripMenuItem,
				(ToolStripItem)displayPredictionLightCurveToolStripMenuItem1,
				(ToolStripItem)viewStarInGoogleSkyToolStripMenuItem1,
				(ToolStripItem)compareStarCataloguePositionsToolStripMenuItem1,
				(ToolStripItem)listStarDetailsThroughTheCDSPortalToolStripMenuItem,
				(ToolStripItem)plotStarUsingCDSSimPlayToolStripMenuItem1,
				(ToolStripItem)listRelativisticDisplacementToolStripMenuItem,
				(ToolStripItem)astrometry_MPC,
				(ToolStripItem)toolStripSeparator11,
				(ToolStripItem)asteroidToolStripMenuItem,
				(ToolStripItem)toolStripSeparator21,
				(ToolStripItem)listAsteroidIRDiametersToolStripMenuItem,
				(ToolStripItem)jPLSmallBodyDatabaseForThisAsteroidOrCometToolStripMenuItem,
				(ToolStripItem)binaryAsteroidsToolStripMenuItem,
				(ToolStripItem)displayAsteroidSatelliteOrbitToolStripMenuItem,
				(ToolStripItem)displayRingSystemToolStripMenuItem,
				(ToolStripItem)lightCurveDataToolStripMenuItem1,
				(ToolStripItem)dAMITAsteroidShapesToolStripMenuItem1,
				(ToolStripItem)toolStripSeparator22,
				(ToolStripItem)otherToolStripMenuItem,
				(ToolStripItem)toolStripSeparator10,
				(ToolStripItem)showRegionalWeatherPrediction16DayLimitToolStripMenuItem1,
				(ToolStripItem)viewInGoogleEarthToolStripMenuItem1,
				(ToolStripItem)saveAsGoogleEarthKMZFileToolStripMenuItem,
				(ToolStripItem)saveAsGoogleMapsHTMFileToolStripMenuItem,
				(ToolStripItem)toolStripSeparator13,
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)copyOccultationElementsForThisEventToolStripMenuItem,
				(ToolStripItem)printPreviewToolStripMenuItem,
				(ToolStripItem)printToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem
			});
			((ToolStripItem)withThisEventToolStripMenuItem).set_Image((Image)Resources.GrazeBW);
			((ToolStripItem)withThisEventToolStripMenuItem).set_Name("withThisEventToolStripMenuItem");
			((ToolStripItem)withThisEventToolStripMenuItem).set_Size(new Size(121, 20));
			((ToolStripItem)withThisEventToolStripMenuItem).set_Text("with this Event...");
			((ToolStripItem)eventToolStripMenuItem1).set_BackColor(Color.Cornsilk);
			((ToolStripItem)eventToolStripMenuItem1).set_Enabled(false);
			((ToolStripItem)eventToolStripMenuItem1).set_Font(new Font("Segoe UI", 12f, FontStyle.Bold));
			((ToolStripItem)eventToolStripMenuItem1).set_Name("eventToolStripMenuItem1");
			((ToolStripItem)eventToolStripMenuItem1).set_Size(new Size(356, 26));
			((ToolStripItem)eventToolStripMenuItem1).set_Text("Event");
			((ToolStripItem)toolStripSeparator19).set_Name("toolStripSeparator19");
			((ToolStripItem)toolStripSeparator19).set_Size(new Size(353, 6));
			((ToolStripItem)largeStarChartToolStripMenuItem).set_Image((Image)Resources.Stars3);
			((ToolStripItem)largeStarChartToolStripMenuItem).set_Name("largeStarChartToolStripMenuItem");
			largeStarChartToolStripMenuItem.set_ShortcutKeys((Keys)118);
			((ToolStripItem)largeStarChartToolStripMenuItem).set_Size(new Size(356, 26));
			((ToolStripItem)largeStarChartToolStripMenuItem).set_Text("Large star chart");
			((ToolStripItem)largeStarChartToolStripMenuItem).add_Click((EventHandler)largeStarChartToolStripMenuItem_Click);
			((ToolStripItem)pathCoordinatesToolStripMenuItem).set_Image((Image)Resources.Paths);
			((ToolStripItem)pathCoordinatesToolStripMenuItem).set_Name("pathCoordinatesToolStripMenuItem");
			pathCoordinatesToolStripMenuItem.set_ShortcutKeys((Keys)122);
			((ToolStripItem)pathCoordinatesToolStripMenuItem).set_Size(new Size(356, 26));
			((ToolStripItem)pathCoordinatesToolStripMenuItem).set_Text("Path coordinates");
			((ToolStripItem)pathCoordinatesToolStripMenuItem).add_Click((EventHandler)pathCoordinatesToolStripMenuItem_Click);
			((ToolStripItem)siteDistancesFromThePathToolStripMenuItem).set_Image((Image)Resources.MultiLocation);
			((ToolStripItem)siteDistancesFromThePathToolStripMenuItem).set_Name("siteDistancesFromThePathToolStripMenuItem");
			siteDistancesFromThePathToolStripMenuItem.set_ShortcutKeys((Keys)123);
			((ToolStripItem)siteDistancesFromThePathToolStripMenuItem).set_Size(new Size(356, 26));
			((ToolStripItem)siteDistancesFromThePathToolStripMenuItem).set_Text("Path distances for multiple locations");
			((ToolStripItem)siteDistancesFromThePathToolStripMenuItem).add_Click((EventHandler)siteDistancesFromThePathToolStripMenuItem_Click);
			((ToolStripItem)listOfPrepointStarsToolStripMenuItem).set_Image((Image)Resources.POINT06);
			((ToolStripItem)listOfPrepointStarsToolStripMenuItem).set_Name("listOfPrepointStarsToolStripMenuItem");
			((ToolStripItem)listOfPrepointStarsToolStripMenuItem).set_Size(new Size(356, 26));
			((ToolStripItem)listOfPrepointStarsToolStripMenuItem).set_Text("Pre-point stars");
			((ToolStripItem)listOfPrepointStarsToolStripMenuItem).add_Click((EventHandler)listOfPrepointStarsToolStripMenuItem_Click);
			((ToolStripItem)drawStarChartInC2AToolStripMenuItem).set_Image((Image)Resources.c2aw1);
			((ToolStripItem)drawStarChartInC2AToolStripMenuItem).set_Name("drawStarChartInC2AToolStripMenuItem");
			drawStarChartInC2AToolStripMenuItem.set_ShortcutKeys((Keys)131122);
			((ToolStripItem)drawStarChartInC2AToolStripMenuItem).set_Size(new Size(356, 26));
			((ToolStripItem)drawStarChartInC2AToolStripMenuItem).set_Text("C2A star chart");
			((ToolStripItem)drawStarChartInC2AToolStripMenuItem).add_Click((EventHandler)drawStarChartInC2AToolStripMenuItem_Click);
			((ToolStripItem)showPlanetConfigurationToolStripMenuItem).set_Image((Image)Resources.Satellite);
			((ToolStripItem)showPlanetConfigurationToolStripMenuItem).set_Name("showPlanetConfigurationToolStripMenuItem");
			showPlanetConfigurationToolStripMenuItem.set_ShortcutKeys((Keys)120);
			((ToolStripItem)showPlanetConfigurationToolStripMenuItem).set_Size(new Size(356, 26));
			((ToolStripItem)showPlanetConfigurationToolStripMenuItem).set_Text("Planet appearance");
			((ToolStripItem)showPlanetConfigurationToolStripMenuItem).add_Click((EventHandler)showPlanetConfigurationToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator3).set_Name("toolStripSeparator3");
			((ToolStripItem)toolStripSeparator3).set_Size(new Size(353, 6));
			((ToolStripItem)asteroidRingsContactTimesForMultipleLocationsToolStripMenuItem).set_Image((Image)Resources.Ring1);
			((ToolStripItem)asteroidRingsContactTimesForMultipleLocationsToolStripMenuItem).set_Name("asteroidRingsContactTimesForMultipleLocationsToolStripMenuItem");
			((ToolStripItem)asteroidRingsContactTimesForMultipleLocationsToolStripMenuItem).set_Size(new Size(356, 26));
			((ToolStripItem)asteroidRingsContactTimesForMultipleLocationsToolStripMenuItem).set_Text("Asteroid rings contact times for multiple locations");
			((ToolStripItem)asteroidRingsContactTimesForMultipleLocationsToolStripMenuItem).add_Click((EventHandler)asteroidRingsContactTimesForMultipleLocationsToolStripMenuItem_Click);
			((ToolStripItem)planetContactTimesForMultipleLocationsToolStripMenuItem).set_Image((Image)Resources.Planet);
			((ToolStripItem)planetContactTimesForMultipleLocationsToolStripMenuItem).set_Name("planetContactTimesForMultipleLocationsToolStripMenuItem");
			((ToolStripItem)planetContactTimesForMultipleLocationsToolStripMenuItem).set_Size(new Size(356, 26));
			((ToolStripItem)planetContactTimesForMultipleLocationsToolStripMenuItem).set_Text("Planet contact times for multiple locations");
			((ToolStripItem)planetContactTimesForMultipleLocationsToolStripMenuItem).add_Click((EventHandler)planetContactTimesForMultipleLocationsToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator8).set_Name("toolStripSeparator8");
			((ToolStripItem)toolStripSeparator8).set_Size(new Size(353, 6));
			((ToolStripItem)toolStripMenuItem2).set_BackColor(Color.PaleTurquoise);
			((ToolStripItem)toolStripMenuItem2).set_Enabled(false);
			((ToolStripItem)toolStripMenuItem2).set_Font(new Font("Segoe UI", 12f, FontStyle.Bold));
			((ToolStripItem)toolStripMenuItem2).set_Name("toolStripMenuItem2");
			((ToolStripItem)toolStripMenuItem2).set_Size(new Size(356, 26));
			((ToolStripItem)toolStripMenuItem2).set_Text("Star");
			((ToolStripItem)toolStripSeparator20).set_Name("toolStripSeparator20");
			((ToolStripItem)toolStripSeparator20).set_Size(new Size(353, 6));
			((ToolStripItem)displayStarsDiameterFresnelDiffractionToolStripMenuItem).set_Image((Image)Resources.Fresnel);
			((ToolStripItem)displayStarsDiameterFresnelDiffractionToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)displayStarsDiameterFresnelDiffractionToolStripMenuItem).set_Name("displayStarsDiameterFresnelDiffractionToolStripMenuItem");
			((ToolStripItem)displayStarsDiameterFresnelDiffractionToolStripMenuItem).set_Size(new Size(356, 26));
			((ToolStripItem)displayStarsDiameterFresnelDiffractionToolStripMenuItem).set_Text("Star's diameter && Fresnel diffraction");
			((ToolStripItem)displayStarsDiameterFresnelDiffractionToolStripMenuItem).add_Click((EventHandler)displayStarsDiameterFresnelDiffractionToolStripMenuItem_Click);
			((ToolStripItem)displayWDSInterferometricDataToolStripMenuItem).set_Image((Image)Resources.services);
			((ToolStripItem)displayWDSInterferometricDataToolStripMenuItem).set_Name("displayWDSInterferometricDataToolStripMenuItem");
			((ToolStripItem)displayWDSInterferometricDataToolStripMenuItem).set_Size(new Size(356, 26));
			((ToolStripItem)displayWDSInterferometricDataToolStripMenuItem).set_Text("Double stars (WDS && 6th IF catalog),  Variable stars");
			((ToolStripItem)displayWDSInterferometricDataToolStripMenuItem).add_Click((EventHandler)displayWDSInterferometricDataToolStripMenuItem_Click);
			((ToolStripItem)gaiaDoubleStarsnearbyToolStripMenuItem).set_Image((Image)Resources.Gaia);
			((ToolStripItem)gaiaDoubleStarsnearbyToolStripMenuItem).set_Name("gaiaDoubleStarsnearbyToolStripMenuItem");
			((ToolStripItem)gaiaDoubleStarsnearbyToolStripMenuItem).set_Size(new Size(356, 26));
			((ToolStripItem)gaiaDoubleStarsnearbyToolStripMenuItem).set_Text("Gaia double stars (nearby)");
			((ToolStripItem)gaiaDoubleStarsnearbyToolStripMenuItem).add_Click((EventHandler)gaiaDoubleStarsnearbyToolStripMenuItem_Click);
			((ToolStripItem)toolStripNearbyStars).set_Name("toolStripNearbyStars");
			((ToolStripItem)toolStripNearbyStars).set_Size(new Size(356, 26));
			((ToolStripItem)toolStripNearbyStars).set_Text("Nearby stars");
			((ToolStripItem)toolStripNearbyStars).add_Click((EventHandler)toolStripNearbyStars_Click);
			((ToolStripItem)listPreviousOccultionsOfThisStarToolStripMenuItem).set_Image((Image)Resources.Previous_48x_0000);
			((ToolStripItem)listPreviousOccultionsOfThisStarToolStripMenuItem).set_Name("listPreviousOccultionsOfThisStarToolStripMenuItem");
			((ToolStripItem)listPreviousOccultionsOfThisStarToolStripMenuItem).set_Size(new Size(356, 26));
			((ToolStripItem)listPreviousOccultionsOfThisStarToolStripMenuItem).set_Text("List previous occultions of this star");
			((ToolStripItem)listPreviousOccultionsOfThisStarToolStripMenuItem).add_Click((EventHandler)listPreviousOccultionsOfThisStarToolStripMenuItem_Click);
			((ToolStripItem)displayPredictionLightCurveToolStripMenuItem1).set_Image((Image)Resources.GrazeBW);
			((ToolStripItem)displayPredictionLightCurveToolStripMenuItem1).set_Name("displayPredictionLightCurveToolStripMenuItem1");
			((ToolStripItem)displayPredictionLightCurveToolStripMenuItem1).set_Size(new Size(356, 26));
			((ToolStripItem)displayPredictionLightCurveToolStripMenuItem1).set_Text("Display prediction light curve");
			((ToolStripItem)displayPredictionLightCurveToolStripMenuItem1).add_Click((EventHandler)displayPredictionLightCurveToolStripMenuItem1_Click);
			((ToolStripItem)viewStarInGoogleSkyToolStripMenuItem1).set_Image((Image)Resources.sky_mo);
			((ToolStripItem)viewStarInGoogleSkyToolStripMenuItem1).set_Name("viewStarInGoogleSkyToolStripMenuItem1");
			viewStarInGoogleSkyToolStripMenuItem1.set_ShortcutKeys((Keys)131143);
			((ToolStripItem)viewStarInGoogleSkyToolStripMenuItem1).set_Size(new Size(356, 26));
			((ToolStripItem)viewStarInGoogleSkyToolStripMenuItem1).set_Text("View star in GoogleSky");
			((ToolStripItem)viewStarInGoogleSkyToolStripMenuItem1).add_Click((EventHandler)viewStarInGoogleSkyToolStripMenuItem1_Click);
			((ToolStripItem)compareStarCataloguePositionsToolStripMenuItem1).set_Image((Image)Resources.vizier_40x35);
			((ToolStripItem)compareStarCataloguePositionsToolStripMenuItem1).set_Name("compareStarCataloguePositionsToolStripMenuItem1");
			((ToolStripItem)compareStarCataloguePositionsToolStripMenuItem1).set_Size(new Size(356, 26));
			((ToolStripItem)compareStarCataloguePositionsToolStripMenuItem1).set_Text("Compare star catalogue positions");
			((ToolStripItem)compareStarCataloguePositionsToolStripMenuItem1).add_Click((EventHandler)compareStarCataloguePositionsToolStripMenuItem1_Click);
			((ToolStripItem)listStarDetailsThroughTheCDSPortalToolStripMenuItem).set_Image((Image)Resources.cds_icon);
			((ToolStripItem)listStarDetailsThroughTheCDSPortalToolStripMenuItem).set_Name("listStarDetailsThroughTheCDSPortalToolStripMenuItem");
			((ToolStripItem)listStarDetailsThroughTheCDSPortalToolStripMenuItem).set_Size(new Size(356, 26));
			((ToolStripItem)listStarDetailsThroughTheCDSPortalToolStripMenuItem).set_Text("List star details through the CDS Portal");
			((ToolStripItem)listStarDetailsThroughTheCDSPortalToolStripMenuItem).add_Click((EventHandler)listStarDetailsThroughTheCDSPortalToolStripMenuItem_Click);
			((ToolStripItem)plotStarUsingCDSSimPlayToolStripMenuItem1).set_Image((Image)Resources.cds_icon);
			((ToolStripItem)plotStarUsingCDSSimPlayToolStripMenuItem1).set_Name("plotStarUsingCDSSimPlayToolStripMenuItem1");
			((ToolStripItem)plotStarUsingCDSSimPlayToolStripMenuItem1).set_Size(new Size(356, 26));
			((ToolStripItem)plotStarUsingCDSSimPlayToolStripMenuItem1).set_Text("Plot star using CDS SimPlay");
			((ToolStripItem)plotStarUsingCDSSimPlayToolStripMenuItem1).add_Click((EventHandler)plotStarUsingCDSSimPlayToolStripMenuItem1_Click);
			((ToolStripItem)listRelativisticDisplacementToolStripMenuItem).set_Image((Image)Resources.searchweb);
			((ToolStripItem)listRelativisticDisplacementToolStripMenuItem).set_Name("listRelativisticDisplacementToolStripMenuItem");
			((ToolStripItem)listRelativisticDisplacementToolStripMenuItem).set_Size(new Size(356, 26));
			((ToolStripItem)listRelativisticDisplacementToolStripMenuItem).set_Text("List relativistic displacement");
			((ToolStripItem)listRelativisticDisplacementToolStripMenuItem).add_Click((EventHandler)listRelativisticDisplacementToolStripMenuItem_Click);
			((ToolStripItem)astrometry_MPC).set_Font(new Font("Segoe UI", 7f, FontStyle.Bold));
			((ToolStripItem)astrometry_MPC).set_ForeColor(Color.MidnightBlue);
			((ToolStripItem)astrometry_MPC).set_Image((Image)Resources.Ruler_48x);
			((ToolStripItem)astrometry_MPC).set_Name("astrometry_MPC");
			((ToolStripItem)astrometry_MPC).set_Size(new Size(356, 26));
			((ToolStripItem)astrometry_MPC).set_Text("Astrometry at Minor Planet Center  [Occn : search for '275 ']");
			((ToolStripItem)astrometry_MPC).set_TextAlign(ContentAlignment.MiddleLeft);
			((ToolStripItem)astrometry_MPC).add_Click((EventHandler)astrometry_MPC_Click);
			((ToolStripItem)toolStripSeparator11).set_Name("toolStripSeparator11");
			((ToolStripItem)toolStripSeparator11).set_Size(new Size(353, 6));
			((ToolStripItem)asteroidToolStripMenuItem).set_BackColor(Color.LightGreen);
			((ToolStripItem)asteroidToolStripMenuItem).set_Enabled(false);
			((ToolStripItem)asteroidToolStripMenuItem).set_Font(new Font("Segoe UI", 12f, FontStyle.Bold));
			((ToolStripItem)asteroidToolStripMenuItem).set_Name("asteroidToolStripMenuItem");
			((ToolStripItem)asteroidToolStripMenuItem).set_Size(new Size(356, 26));
			((ToolStripItem)asteroidToolStripMenuItem).set_Text("Asteroid");
			((ToolStripItem)toolStripSeparator21).set_Name("toolStripSeparator21");
			((ToolStripItem)toolStripSeparator21).set_Size(new Size(353, 6));
			((ToolStripItem)listAsteroidIRDiametersToolStripMenuItem).set_Image((Image)Resources.Dimension_32xSM_0000);
			((ToolStripItem)listAsteroidIRDiametersToolStripMenuItem).set_Name("listAsteroidIRDiametersToolStripMenuItem");
			((ToolStripItem)listAsteroidIRDiametersToolStripMenuItem).set_Size(new Size(356, 26));
			((ToolStripItem)listAsteroidIRDiametersToolStripMenuItem).set_Text("Asteroid IR diameters ;  diameters from H0");
			((ToolStripItem)listAsteroidIRDiametersToolStripMenuItem).add_Click((EventHandler)listAsteroidIRDiametersToolStripMenuItem_Click);
			((ToolStripItem)jPLSmallBodyDatabaseForThisAsteroidOrCometToolStripMenuItem).set_Name("jPLSmallBodyDatabaseForThisAsteroidOrCometToolStripMenuItem");
			((ToolStripItem)jPLSmallBodyDatabaseForThisAsteroidOrCometToolStripMenuItem).set_Size(new Size(356, 26));
			((ToolStripItem)jPLSmallBodyDatabaseForThisAsteroidOrCometToolStripMenuItem).set_Text("JPL Small Body Database for this asteroid or comet");
			((ToolStripItem)jPLSmallBodyDatabaseForThisAsteroidOrCometToolStripMenuItem).add_Click((EventHandler)jPLSmallBodyDatabaseForThisAsteroidOrCometToolStripMenuItem_Click);
			((ToolStripItem)binaryAsteroidsToolStripMenuItem).set_Name("binaryAsteroidsToolStripMenuItem");
			((ToolStripItem)binaryAsteroidsToolStripMenuItem).set_Size(new Size(356, 26));
			((ToolStripItem)binaryAsteroidsToolStripMenuItem).set_Text("Binary asteroids");
			((ToolStripItem)binaryAsteroidsToolStripMenuItem).add_Click((EventHandler)binaryAsteroidsToolStripMenuItem_Click);
			((ToolStripItem)displayAsteroidSatelliteOrbitToolStripMenuItem).set_Image((Image)Resources.logo_IMCCE_t);
			((ToolStripItem)displayAsteroidSatelliteOrbitToolStripMenuItem).set_Name("displayAsteroidSatelliteOrbitToolStripMenuItem");
			((ToolStripItem)displayAsteroidSatelliteOrbitToolStripMenuItem).set_Size(new Size(356, 26));
			((ToolStripItem)displayAsteroidSatelliteOrbitToolStripMenuItem).set_Text("Asteroid satellite orbit using Miriade");
			((ToolStripItem)displayAsteroidSatelliteOrbitToolStripMenuItem).add_Click((EventHandler)displayAsteroidSatelliteOrbitToolStripMenuItem_Click);
			displayRingSystemToolStripMenuItem.set_Checked(true);
			displayRingSystemToolStripMenuItem.set_CheckState((CheckState)1);
			((ToolStripItem)displayRingSystemToolStripMenuItem).set_Image((Image)Resources.blankcd);
			((ToolStripItem)displayRingSystemToolStripMenuItem).set_Name("displayRingSystemToolStripMenuItem");
			((ToolStripItem)displayRingSystemToolStripMenuItem).set_Size(new Size(356, 26));
			((ToolStripItem)displayRingSystemToolStripMenuItem).set_Text("Asteroid's ring && moon systems");
			((ToolStripItem)displayRingSystemToolStripMenuItem).add_Click((EventHandler)displayRingSystemToolStripMenuItem_Click);
			((ToolStripItem)lightCurveDataToolStripMenuItem1).set_Image((Image)Resources.GRAPH07);
			((ToolStripItem)lightCurveDataToolStripMenuItem1).set_Name("lightCurveDataToolStripMenuItem1");
			((ToolStripItem)lightCurveDataToolStripMenuItem1).set_Size(new Size(356, 26));
			((ToolStripItem)lightCurveDataToolStripMenuItem1).set_Text("Light curve data");
			((ToolStripItem)lightCurveDataToolStripMenuItem1).add_Click((EventHandler)lightCurveDataToolStripMenuItem1_Click);
			((ToolStripItem)dAMITAsteroidShapesToolStripMenuItem1).set_Image((Image)Resources.Asteroid);
			((ToolStripItem)dAMITAsteroidShapesToolStripMenuItem1).set_Name("dAMITAsteroidShapesToolStripMenuItem1");
			((ToolStripItem)dAMITAsteroidShapesToolStripMenuItem1).set_Size(new Size(356, 26));
			((ToolStripItem)dAMITAsteroidShapesToolStripMenuItem1).set_Text("Shape models");
			((ToolStripItem)dAMITAsteroidShapesToolStripMenuItem1).add_Click((EventHandler)dAMITAsteroidShapesToolStripMenuItem1_Click);
			((ToolStripItem)toolStripSeparator22).set_Name("toolStripSeparator22");
			((ToolStripItem)toolStripSeparator22).set_Size(new Size(353, 6));
			((ToolStripItem)otherToolStripMenuItem).set_BackColor(Color.Thistle);
			((ToolStripItem)otherToolStripMenuItem).set_Enabled(false);
			((ToolStripItem)otherToolStripMenuItem).set_Font(new Font("Segoe UI", 12f, FontStyle.Bold));
			((ToolStripItem)otherToolStripMenuItem).set_Name("otherToolStripMenuItem");
			((ToolStripItem)otherToolStripMenuItem).set_Size(new Size(356, 26));
			((ToolStripItem)otherToolStripMenuItem).set_Text("Other");
			((ToolStripItem)toolStripSeparator10).set_Name("toolStripSeparator10");
			((ToolStripItem)toolStripSeparator10).set_Size(new Size(353, 6));
			((ToolStripItem)showRegionalWeatherPrediction16DayLimitToolStripMenuItem1).set_Image((Image)Resources.CLOUD);
			((ToolStripItem)showRegionalWeatherPrediction16DayLimitToolStripMenuItem1).set_Name("showRegionalWeatherPrediction16DayLimitToolStripMenuItem1");
			((ToolStripItem)showRegionalWeatherPrediction16DayLimitToolStripMenuItem1).set_Size(new Size(356, 26));
			((ToolStripItem)showRegionalWeatherPrediction16DayLimitToolStripMenuItem1).set_Text("Show regional weather prediction (16 day limit)");
			((ToolStripItem)showRegionalWeatherPrediction16DayLimitToolStripMenuItem1).add_Click((EventHandler)showRegionalWeatherPrediction16DayLimitToolStripMenuItem1_Click);
			((ToolStripItem)viewInGoogleEarthToolStripMenuItem1).set_Image((Image)Resources.google_earth);
			((ToolStripItem)viewInGoogleEarthToolStripMenuItem1).set_Name("viewInGoogleEarthToolStripMenuItem1");
			((ToolStripItem)viewInGoogleEarthToolStripMenuItem1).set_Size(new Size(356, 26));
			((ToolStripItem)viewInGoogleEarthToolStripMenuItem1).set_Text("View in GoogleEarth");
			((ToolStripItem)viewInGoogleEarthToolStripMenuItem1).add_Click((EventHandler)viewInGoogleEarthToolStripMenuItem1_Click);
			((ToolStripItem)saveAsGoogleEarthKMZFileToolStripMenuItem).set_Image((Image)Resources.google_earth);
			((ToolStripItem)saveAsGoogleEarthKMZFileToolStripMenuItem).set_Name("saveAsGoogleEarthKMZFileToolStripMenuItem");
			((ToolStripItem)saveAsGoogleEarthKMZFileToolStripMenuItem).set_Size(new Size(356, 26));
			((ToolStripItem)saveAsGoogleEarthKMZFileToolStripMenuItem).set_Text("Save as GoogleEarth KMZ file");
			((ToolStripItem)saveAsGoogleEarthKMZFileToolStripMenuItem).add_Click((EventHandler)saveAsGoogleEarthKMZFileToolStripMenuItem_Click);
			((ToolStripItem)saveAsGoogleMapsHTMFileToolStripMenuItem).set_Image((Image)Resources.mm_20_red);
			((ToolStripItem)saveAsGoogleMapsHTMFileToolStripMenuItem).set_Name("saveAsGoogleMapsHTMFileToolStripMenuItem");
			((ToolStripItem)saveAsGoogleMapsHTMFileToolStripMenuItem).set_Size(new Size(356, 26));
			((ToolStripItem)saveAsGoogleMapsHTMFileToolStripMenuItem).set_Text("Save as GoogleMaps HTM file");
			((ToolStripItem)saveAsGoogleMapsHTMFileToolStripMenuItem).add_Click((EventHandler)saveAsGoogleMapsHTMFileToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator13).set_Name("toolStripSeparator13");
			((ToolStripItem)toolStripSeparator13).set_Size(new Size(353, 6));
			((ToolStripItem)copyToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyToolStripMenuItem).set_Name("copyToolStripMenuItem");
			copyToolStripMenuItem.set_ShortcutKeys((Keys)131139);
			((ToolStripItem)copyToolStripMenuItem).set_Size(new Size(356, 26));
			((ToolStripItem)copyToolStripMenuItem).set_Text("&Copy");
			((ToolStripItem)copyToolStripMenuItem).add_Click((EventHandler)copyToolStripMenuItem_Click);
			((ToolStripItem)copyOccultationElementsForThisEventToolStripMenuItem).set_Image((Image)Resources.CopyItem_32x);
			((ToolStripItem)copyOccultationElementsForThisEventToolStripMenuItem).set_Name("copyOccultationElementsForThisEventToolStripMenuItem");
			((ToolStripItem)copyOccultationElementsForThisEventToolStripMenuItem).set_Size(new Size(356, 26));
			((ToolStripItem)copyOccultationElementsForThisEventToolStripMenuItem).set_Text("Copy occultation elements for this event");
			((ToolStripItem)copyOccultationElementsForThisEventToolStripMenuItem).add_Click((EventHandler)copyOccultationElementsForThisEventToolStripMenuItem_Click);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Image((Image)Resources.PrintPreview);
			((ToolStripItem)printPreviewToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Name("printPreviewToolStripMenuItem");
			((ToolStripItem)printPreviewToolStripMenuItem).set_Size(new Size(356, 26));
			((ToolStripItem)printPreviewToolStripMenuItem).set_Text("Print Preview");
			((ToolStripItem)printPreviewToolStripMenuItem).add_Click((EventHandler)printPreviewToolStripMenuItem_Click);
			((ToolStripItem)printToolStripMenuItem).set_Image((Image)Resources.Print);
			((ToolStripItem)printToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printToolStripMenuItem).set_Name("printToolStripMenuItem");
			printToolStripMenuItem.set_ShortcutKeys((Keys)131152);
			((ToolStripItem)printToolStripMenuItem).set_Size(new Size(356, 26));
			((ToolStripItem)printToolStripMenuItem).set_Text("&Print");
			((ToolStripItem)printToolStripMenuItem).add_Click((EventHandler)printToolStripMenuItem_Click);
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			saveToolStripMenuItem.set_ShortcutKeys((Keys)131155);
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(356, 26));
			((ToolStripItem)saveToolStripMenuItem).set_Text("&Save");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
			((ToolStripDropDownItem)optionsToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[12]
			{
				(ToolStripItem)occultSettingsSetToolStripMenuItem,
				(ToolStripItem)exportButtonsDisplayedToolStripMenuItem,
				(ToolStripItem)includeStarChartToolStripMenuItem,
				(ToolStripItem)starChartUseEnhancedToolStripMenuItem,
				(ToolStripItem)showErrorEllipseToolStripMenuItem,
				(ToolStripItem)showCoordinatesInTootipToolStripMenuItem,
				(ToolStripItem)show1sigmaLinesAsContinuousToolStripMenuItem,
				(ToolStripItem)toolStripSeparator2,
				(ToolStripItem)colourToolStripMenuItem,
				(ToolStripItem)setPlotRegionToStandardSizesToolStripMenuItem,
				(ToolStripItem)toolStripSeparator15,
				(ToolStripItem)lightCurveSimulatorToolStripMenuItem
			});
			((ToolStripItem)optionsToolStripMenuItem).set_Name("optionsToolStripMenuItem");
			((ToolStripItem)optionsToolStripMenuItem).set_Size(new Size(70, 20));
			((ToolStripItem)optionsToolStripMenuItem).set_Text("Options...");
			((ToolStripItem)occultSettingsSetToolStripMenuItem).set_Image((Image)Resources.OCCULT);
			((ToolStripItem)occultSettingsSetToolStripMenuItem).set_Name("occultSettingsSetToolStripMenuItem");
			((ToolStripItem)occultSettingsSetToolStripMenuItem).set_Size(new Size(253, 22));
			((ToolStripItem)occultSettingsSetToolStripMenuItem).set_Text("User Settings - set");
			((ToolStripItem)occultSettingsSetToolStripMenuItem).add_Click((EventHandler)occultSettingsSetToolStripMenuItem_Click);
			exportButtonsDisplayedToolStripMenuItem.set_Checked(true);
			exportButtonsDisplayedToolStripMenuItem.set_CheckState((CheckState)1);
			((ToolStripItem)exportButtonsDisplayedToolStripMenuItem).set_Name("exportButtonsDisplayedToolStripMenuItem");
			((ToolStripItem)exportButtonsDisplayedToolStripMenuItem).set_Size(new Size(253, 22));
			((ToolStripItem)exportButtonsDisplayedToolStripMenuItem).set_Text("Export buttons - enabled");
			((ToolStripItem)exportButtonsDisplayedToolStripMenuItem).add_Click((EventHandler)exportButtonsDisplayedToolStripMenuItem_Click);
			includeStarChartToolStripMenuItem.set_Checked(Settings.Default.IncludeStarChart_Prediction);
			includeStarChartToolStripMenuItem.set_CheckState((CheckState)1);
			((ToolStripItem)includeStarChartToolStripMenuItem).set_Name("includeStarChartToolStripMenuItem");
			((ToolStripItem)includeStarChartToolStripMenuItem).set_Size(new Size(253, 22));
			((ToolStripItem)includeStarChartToolStripMenuItem).set_Text("Include star chart");
			((ToolStripItem)includeStarChartToolStripMenuItem).add_Click((EventHandler)includeStarChartToolStripMenuItem_Click);
			starChartUseEnhancedToolStripMenuItem.set_Checked(Settings.Default.AsteroidStarChartEnhanced);
			((ToolStripItem)starChartUseEnhancedToolStripMenuItem).set_Name("starChartUseEnhancedToolStripMenuItem");
			((ToolStripItem)starChartUseEnhancedToolStripMenuItem).set_Size(new Size(253, 22));
			((ToolStripItem)starChartUseEnhancedToolStripMenuItem).set_Text("... use enhanced star chart");
			((ToolStripItem)starChartUseEnhancedToolStripMenuItem).add_Click((EventHandler)starChartUseEnhancedToolStripMenuItem_Click);
			showErrorEllipseToolStripMenuItem.set_Checked(true);
			showErrorEllipseToolStripMenuItem.set_CheckState((CheckState)1);
			((ToolStripItem)showErrorEllipseToolStripMenuItem).set_Name("showErrorEllipseToolStripMenuItem");
			((ToolStripItem)showErrorEllipseToolStripMenuItem).set_Size(new Size(253, 22));
			((ToolStripItem)showErrorEllipseToolStripMenuItem).set_Text("Show error ellipse");
			((ToolStripItem)showErrorEllipseToolStripMenuItem).add_Click((EventHandler)showErrorEllipseToolStripMenuItem_Click);
			showCoordinatesInTootipToolStripMenuItem.set_Checked(true);
			showCoordinatesInTootipToolStripMenuItem.set_CheckState((CheckState)1);
			((ToolStripItem)showCoordinatesInTootipToolStripMenuItem).set_Name("showCoordinatesInTootipToolStripMenuItem");
			((ToolStripItem)showCoordinatesInTootipToolStripMenuItem).set_Size(new Size(253, 22));
			((ToolStripItem)showCoordinatesInTootipToolStripMenuItem).set_Text("Show coordinates in tooltip");
			((ToolStripItem)showCoordinatesInTootipToolStripMenuItem).add_Click((EventHandler)showCoordinatesInTootipToolStripMenuItem_Click);
			((ToolStripItem)show1sigmaLinesAsContinuousToolStripMenuItem).set_Name("show1sigmaLinesAsContinuousToolStripMenuItem");
			((ToolStripItem)show1sigmaLinesAsContinuousToolStripMenuItem).set_Size(new Size(253, 22));
			((ToolStripItem)show1sigmaLinesAsContinuousToolStripMenuItem).set_Text("Show 1-sigma lines as continuous");
			((ToolStripItem)show1sigmaLinesAsContinuousToolStripMenuItem).add_Click((EventHandler)show1sigmaLinesAsContinuousToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator2).set_Name("toolStripSeparator2");
			((ToolStripItem)toolStripSeparator2).set_Size(new Size(250, 6));
			((ToolStripItem)colourToolStripMenuItem).set_Name("colourToolStripMenuItem");
			((ToolStripItem)colourToolStripMenuItem).set_Size(new Size(253, 22));
			((ToolStripItem)colourToolStripMenuItem).set_Text("Plot in Colour");
			((ToolStripItem)colourToolStripMenuItem).add_Click((EventHandler)colourToolStripMenuItem_Click);
			((ToolStripDropDownItem)setPlotRegionToStandardSizesToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[5]
			{
				(ToolStripItem)x325ToolStripMenuItem,
				(ToolStripItem)x480ToolStripMenuItem,
				(ToolStripItem)x600ToolStripMenuItem,
				(ToolStripItem)x768ToolStripMenuItem,
				(ToolStripItem)x1024ToolStripMenuItem
			});
			((ToolStripItem)setPlotRegionToStandardSizesToolStripMenuItem).set_Name("setPlotRegionToStandardSizesToolStripMenuItem");
			((ToolStripItem)setPlotRegionToStandardSizesToolStripMenuItem).set_Size(new Size(253, 22));
			((ToolStripItem)setPlotRegionToStandardSizesToolStripMenuItem).set_Text("Set plot region to standard sizes");
			((ToolStripItem)x325ToolStripMenuItem).set_Name("x325ToolStripMenuItem");
			((ToolStripItem)x325ToolStripMenuItem).set_Size(new Size(134, 22));
			((ToolStripItem)x325ToolStripMenuItem).set_Text("640 x 480");
			((ToolStripItem)x325ToolStripMenuItem).add_Click((EventHandler)x325ToolStripMenuItem_Click);
			((ToolStripItem)x480ToolStripMenuItem).set_Name("x480ToolStripMenuItem");
			((ToolStripItem)x480ToolStripMenuItem).set_Size(new Size(134, 22));
			((ToolStripItem)x480ToolStripMenuItem).set_Text("760 x 480");
			((ToolStripItem)x480ToolStripMenuItem).add_Click((EventHandler)x480ToolStripMenuItem_Click);
			((ToolStripItem)x600ToolStripMenuItem).set_Name("x600ToolStripMenuItem");
			((ToolStripItem)x600ToolStripMenuItem).set_Size(new Size(134, 22));
			((ToolStripItem)x600ToolStripMenuItem).set_Text("800 x 600");
			((ToolStripItem)x600ToolStripMenuItem).add_Click((EventHandler)x600ToolStripMenuItem_Click);
			((ToolStripItem)x768ToolStripMenuItem).set_Name("x768ToolStripMenuItem");
			((ToolStripItem)x768ToolStripMenuItem).set_Size(new Size(134, 22));
			((ToolStripItem)x768ToolStripMenuItem).set_Text("1024 x 768");
			((ToolStripItem)x768ToolStripMenuItem).add_Click((EventHandler)x768ToolStripMenuItem_Click);
			((ToolStripItem)x1024ToolStripMenuItem).set_Name("x1024ToolStripMenuItem");
			((ToolStripItem)x1024ToolStripMenuItem).set_Size(new Size(134, 22));
			((ToolStripItem)x1024ToolStripMenuItem).set_Text("1280 x 1024");
			((ToolStripItem)x1024ToolStripMenuItem).add_Click((EventHandler)x1024ToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator15).set_Name("toolStripSeparator15");
			((ToolStripItem)toolStripSeparator15).set_Size(new Size(250, 6));
			((ToolStripItem)lightCurveSimulatorToolStripMenuItem).set_Image((Image)Resources.LightCurveSimulator1);
			((ToolStripItem)lightCurveSimulatorToolStripMenuItem).set_Name("lightCurveSimulatorToolStripMenuItem");
			((ToolStripItem)lightCurveSimulatorToolStripMenuItem).set_Size(new Size(253, 22));
			((ToolStripItem)lightCurveSimulatorToolStripMenuItem).set_Text("show Light curve simulator");
			((ToolStripItem)lightCurveSimulatorToolStripMenuItem).add_Click((EventHandler)lightCurveSimulatorToolStripMenuItem_Click);
			((ToolStripDropDownItem)redrawToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[10]
			{
				(ToolStripItem)scale001100ToolStripMenuItem,
				(ToolStripItem)scale01ToolStripMenuItem,
				(ToolStripItem)scale03ToolStripMenuItem,
				(ToolStripItem)scale1ToolStripMenuItem,
				(ToolStripItem)scale2ToolStripMenuItem,
				(ToolStripItem)scale4ToolStripMenuItem,
				(ToolStripItem)scale8ToolStripMenuItem,
				(ToolStripItem)scale16ToolStripMenuItem,
				(ToolStripItem)scale32ToolStripMenuItem,
				(ToolStripItem)toolStripSeparator1
			});
			((ToolStripItem)redrawToolStripMenuItem).set_Name("redrawToolStripMenuItem");
			((ToolStripItem)redrawToolStripMenuItem).set_Size(new Size(67, 20));
			((ToolStripItem)redrawToolStripMenuItem).set_Text("Redraw...");
			((ToolStripItem)scale001100ToolStripMenuItem).set_Name("scale001100ToolStripMenuItem");
			((ToolStripItem)scale001100ToolStripMenuItem).set_Size(new Size(194, 22));
			((ToolStripItem)scale001100ToolStripMenuItem).set_Text("Scale = 0.01| 100");
			((ToolStripItem)scale001100ToolStripMenuItem).add_Click((EventHandler)scale001100ToolStripMenuItem_Click);
			((ToolStripItem)scale01ToolStripMenuItem).set_Name("scale01ToolStripMenuItem");
			((ToolStripItem)scale01ToolStripMenuItem).set_Size(new Size(194, 22));
			((ToolStripItem)scale01ToolStripMenuItem).set_Text("Scale = 0.1  | 10");
			((ToolStripItem)scale01ToolStripMenuItem).add_Click((EventHandler)scale01ToolStripMenuItem_Click);
			((ToolStripItem)scale03ToolStripMenuItem).set_Name("scale03ToolStripMenuItem");
			((ToolStripItem)scale03ToolStripMenuItem).set_Size(new Size(194, 22));
			((ToolStripItem)scale03ToolStripMenuItem).set_Text("Scale = 0.3  | 3.3");
			((ToolStripItem)scale03ToolStripMenuItem).add_Click((EventHandler)scale03ToolStripMenuItem_Click);
			scale1ToolStripMenuItem.set_Checked(true);
			scale1ToolStripMenuItem.set_CheckState((CheckState)1);
			((ToolStripItem)scale1ToolStripMenuItem).set_Name("scale1ToolStripMenuItem");
			scale1ToolStripMenuItem.set_ShortcutKeys((Keys)113);
			((ToolStripItem)scale1ToolStripMenuItem).set_Size(new Size(194, 22));
			((ToolStripItem)scale1ToolStripMenuItem).set_Text("Scale =  1    | 1");
			((ToolStripItem)scale1ToolStripMenuItem).add_Click((EventHandler)scale1ToolStripMenuItem_Click);
			((ToolStripItem)scale2ToolStripMenuItem).set_Name("scale2ToolStripMenuItem");
			scale2ToolStripMenuItem.set_ShortcutKeys((Keys)114);
			((ToolStripItem)scale2ToolStripMenuItem).set_Size(new Size(194, 22));
			((ToolStripItem)scale2ToolStripMenuItem).set_Text("Scale =  2    | 0.5");
			((ToolStripItem)scale2ToolStripMenuItem).add_Click((EventHandler)scale2ToolStripMenuItem_Click);
			((ToolStripItem)scale4ToolStripMenuItem).set_Name("scale4ToolStripMenuItem");
			scale4ToolStripMenuItem.set_ShortcutKeys((Keys)115);
			((ToolStripItem)scale4ToolStripMenuItem).set_Size(new Size(194, 22));
			((ToolStripItem)scale4ToolStripMenuItem).set_Text("Scale =  4    | 0.25");
			((ToolStripItem)scale4ToolStripMenuItem).add_Click((EventHandler)scale4ToolStripMenuItem_Click);
			((ToolStripItem)scale8ToolStripMenuItem).set_Name("scale8ToolStripMenuItem");
			scale8ToolStripMenuItem.set_ShortcutKeys((Keys)116);
			((ToolStripItem)scale8ToolStripMenuItem).set_Size(new Size(194, 22));
			((ToolStripItem)scale8ToolStripMenuItem).set_Text("Scale =  8    | 0.125");
			((ToolStripItem)scale8ToolStripMenuItem).add_Click((EventHandler)scale8ToolStripMenuItem_Click);
			((ToolStripItem)scale16ToolStripMenuItem).set_Name("scale16ToolStripMenuItem");
			scale16ToolStripMenuItem.set_ShortcutKeys((Keys)117);
			((ToolStripItem)scale16ToolStripMenuItem).set_Size(new Size(194, 22));
			((ToolStripItem)scale16ToolStripMenuItem).set_Text("Scale = 16   | 0.0625");
			((ToolStripItem)scale16ToolStripMenuItem).add_Click((EventHandler)scale16ToolStripMenuItem_Click);
			((ToolStripItem)scale32ToolStripMenuItem).set_Name("scale32ToolStripMenuItem");
			((ToolStripItem)scale32ToolStripMenuItem).set_Size(new Size(194, 22));
			((ToolStripItem)scale32ToolStripMenuItem).set_Text("Scale = 32   | 0.0312");
			((ToolStripItem)scale32ToolStripMenuItem).add_Click((EventHandler)scale32ToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator1).set_Name("toolStripSeparator1");
			((ToolStripItem)toolStripSeparator1).set_Size(new Size(191, 6));
			((ToolStripDropDownItem)plotToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)nextEventToolStripMenuItem,
				(ToolStripItem)previousEventToolStripMenuItem
			});
			((ToolStripItem)plotToolStripMenuItem).set_Name("plotToolStripMenuItem");
			((ToolStripItem)plotToolStripMenuItem).set_Size(new Size(84, 20));
			((ToolStripItem)plotToolStripMenuItem).set_Text("Move to...    ");
			((ToolStripItem)plotToolStripMenuItem).set_ToolTipText("Move to previous or following event");
			((ToolStripItem)nextEventToolStripMenuItem).set_Image((Image)Resources.BuilderDialog_add);
			((ToolStripItem)nextEventToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)nextEventToolStripMenuItem).set_Name("nextEventToolStripMenuItem");
			nextEventToolStripMenuItem.set_ShortcutKeys((Keys)119);
			((ToolStripItem)nextEventToolStripMenuItem).set_Size(new Size(202, 22));
			((ToolStripItem)nextEventToolStripMenuItem).set_Text("Next event");
			((ToolStripItem)nextEventToolStripMenuItem).add_Click((EventHandler)nextEventToolStripMenuItem_Click);
			((ToolStripItem)previousEventToolStripMenuItem).set_Image((Image)Resources.BuilderDialog_remove);
			((ToolStripItem)previousEventToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)previousEventToolStripMenuItem).set_Name("previousEventToolStripMenuItem");
			previousEventToolStripMenuItem.set_ShortcutKeys((Keys)65655);
			((ToolStripItem)previousEventToolStripMenuItem).set_Size(new Size(202, 22));
			((ToolStripItem)previousEventToolStripMenuItem).set_Text("Previous Event");
			((ToolStripItem)previousEventToolStripMenuItem).add_Click((EventHandler)previousEventToolStripMenuItem_Click);
			((ToolStripItem)gEarthToolStripMenuItem).set_Image((Image)Resources.google_earth);
			((ToolStripItem)gEarthToolStripMenuItem).set_Name("gEarthToolStripMenuItem");
			((ToolStripItem)gEarthToolStripMenuItem).set_Size(new Size(28, 20));
			((ToolStripItem)gEarthToolStripMenuItem).set_ToolTipText("Display event in GoogleEarth");
			((ToolStripItem)gEarthToolStripMenuItem).add_Click((EventHandler)gEarthToolStripMenuItem_Click);
			((ToolStripItem)pathToolStripMenuItem).set_Image((Image)Resources.Paths);
			((ToolStripItem)pathToolStripMenuItem).set_Name("pathToolStripMenuItem");
			((ToolStripItem)pathToolStripMenuItem).set_Size(new Size(28, 20));
			((ToolStripItem)pathToolStripMenuItem).set_ToolTipText("Path coordinates");
			((ToolStripItem)pathToolStripMenuItem).add_Click((EventHandler)pathToolStripMenuItem_Click);
			((ToolStripItem)starChartToolStripMenuItem1).set_Image((Image)Resources.Stars3);
			((ToolStripItem)starChartToolStripMenuItem1).set_Name("starChartToolStripMenuItem1");
			((ToolStripItem)starChartToolStripMenuItem1).set_Size(new Size(28, 20));
			((ToolStripItem)starChartToolStripMenuItem1).set_ToolTipText("Draw star chart");
			((ToolStripItem)starChartToolStripMenuItem1).add_Click((EventHandler)starChartToolStripMenuItem1_Click);
			((ToolStripItem)prepointToolStripMenuItem).set_Image((Image)Resources.POINT06);
			((ToolStripItem)prepointToolStripMenuItem).set_Name("prepointToolStripMenuItem");
			((ToolStripItem)prepointToolStripMenuItem).set_Size(new Size(28, 20));
			((ToolStripItem)prepointToolStripMenuItem).set_ToolTipText("Pre-point stars");
			((ToolStripItem)prepointToolStripMenuItem).add_Click((EventHandler)prepointToolStripMenuItem_Click);
			((ToolStripItem)multilocationToolStripMenuItem).set_Image((Image)Resources.MultiLocation);
			((ToolStripItem)multilocationToolStripMenuItem).set_Name("multilocationToolStripMenuItem");
			((ToolStripItem)multilocationToolStripMenuItem).set_Size(new Size(28, 20));
			((ToolStripItem)multilocationToolStripMenuItem).set_ToolTipText("MultiLocation prediction");
			((ToolStripItem)multilocationToolStripMenuItem).add_Click((EventHandler)multilocationToolStripMenuItem_Click);
			((ToolStripItem)planetConfigToolStripMenuItem).set_Image((Image)Resources.Planet);
			((ToolStripItem)planetConfigToolStripMenuItem).set_Name("planetConfigToolStripMenuItem");
			((ToolStripItem)planetConfigToolStripMenuItem).set_Size(new Size(56, 20));
			((ToolStripItem)planetConfigToolStripMenuItem).set_Text("       ");
			((ToolStripItem)planetConfigToolStripMenuItem).set_ToolTipText("Planet configuration");
			((ToolStripItem)planetConfigToolStripMenuItem).add_Click((EventHandler)planetConfigToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(60, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("Help");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(54, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("E&xit");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((Control)PanelExport).get_Controls().Add((Control)(object)cmdExportG);
			((Control)PanelExport).get_Controls().Add((Control)(object)cmdExportF);
			((Control)PanelExport).get_Controls().Add((Control)(object)cmdExportE);
			((Control)PanelExport).get_Controls().Add((Control)(object)cmdExportD);
			((Control)PanelExport).get_Controls().Add((Control)(object)cmdExportC);
			((Control)PanelExport).get_Controls().Add((Control)(object)cmdExportB);
			((Control)PanelExport).get_Controls().Add((Control)(object)cmdExportA);
			((Control)PanelExport).set_Location(new Point(2, 12));
			((Control)PanelExport).set_Name("PanelExport");
			((Control)PanelExport).set_Size(new Size(171, 27));
			((Control)PanelExport).set_TabIndex(2);
			((Control)cmdExportG).set_Font(new Font("Tahoma", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdExportG).set_Location(new Point(147, 3));
			((Control)cmdExportG).set_Name("cmdExportG");
			((Control)cmdExportG).set_Size(new Size(21, 21));
			((Control)cmdExportG).set_TabIndex(6);
			((Control)cmdExportG).set_Text("&G");
			((ButtonBase)cmdExportG).set_TextAlign(ContentAlignment.TopCenter);
			toolTip1.SetToolTip((Control)(object)cmdExportG, "Export to G");
			((ButtonBase)cmdExportG).set_UseVisualStyleBackColor(true);
			((Control)cmdExportG).add_Click((EventHandler)cmdExportG_Click);
			((Control)cmdExportF).set_Font(new Font("Tahoma", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdExportF).set_Location(new Point(123, 3));
			((Control)cmdExportF).set_Name("cmdExportF");
			((Control)cmdExportF).set_Size(new Size(21, 21));
			((Control)cmdExportF).set_TabIndex(5);
			((Control)cmdExportF).set_Text("&F");
			((ButtonBase)cmdExportF).set_TextAlign(ContentAlignment.TopCenter);
			toolTip1.SetToolTip((Control)(object)cmdExportF, "Export to F");
			((ButtonBase)cmdExportF).set_UseVisualStyleBackColor(true);
			((Control)cmdExportF).add_Click((EventHandler)cmdExportF_Click);
			((Control)cmdExportE).set_Font(new Font("Tahoma", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdExportE).set_Location(new Point(99, 3));
			((Control)cmdExportE).set_Name("cmdExportE");
			((Control)cmdExportE).set_Size(new Size(21, 21));
			((Control)cmdExportE).set_TabIndex(4);
			((Control)cmdExportE).set_Text("&E");
			((ButtonBase)cmdExportE).set_TextAlign(ContentAlignment.TopCenter);
			toolTip1.SetToolTip((Control)(object)cmdExportE, "Export to E");
			((ButtonBase)cmdExportE).set_UseVisualStyleBackColor(true);
			((Control)cmdExportE).add_Click((EventHandler)cmdExportE_Click);
			((Control)cmdExportD).set_Font(new Font("Tahoma", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdExportD).set_Location(new Point(75, 3));
			((Control)cmdExportD).set_Name("cmdExportD");
			((Control)cmdExportD).set_Size(new Size(21, 21));
			((Control)cmdExportD).set_TabIndex(3);
			((Control)cmdExportD).set_Text("&D");
			((ButtonBase)cmdExportD).set_TextAlign(ContentAlignment.TopCenter);
			toolTip1.SetToolTip((Control)(object)cmdExportD, "Export to D");
			((ButtonBase)cmdExportD).set_UseVisualStyleBackColor(true);
			((Control)cmdExportD).add_Click((EventHandler)cmdExportD_Click);
			((Control)cmdExportC).set_Font(new Font("Tahoma", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdExportC).set_Location(new Point(51, 3));
			((Control)cmdExportC).set_Name("cmdExportC");
			((Control)cmdExportC).set_Size(new Size(21, 21));
			((Control)cmdExportC).set_TabIndex(2);
			((Control)cmdExportC).set_Text("&C");
			((ButtonBase)cmdExportC).set_TextAlign(ContentAlignment.TopCenter);
			toolTip1.SetToolTip((Control)(object)cmdExportC, "Export to C");
			((ButtonBase)cmdExportC).set_UseVisualStyleBackColor(true);
			((Control)cmdExportC).add_Click((EventHandler)cmdExportC_Click);
			((Control)cmdExportB).set_Font(new Font("Tahoma", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdExportB).set_Location(new Point(27, 3));
			((Control)cmdExportB).set_Name("cmdExportB");
			((Control)cmdExportB).set_Size(new Size(21, 21));
			((Control)cmdExportB).set_TabIndex(1);
			((Control)cmdExportB).set_Text("&B");
			((ButtonBase)cmdExportB).set_TextAlign(ContentAlignment.TopCenter);
			toolTip1.SetToolTip((Control)(object)cmdExportB, "Export to B");
			((ButtonBase)cmdExportB).set_UseVisualStyleBackColor(true);
			((Control)cmdExportB).add_Click((EventHandler)cmdExportB_Click);
			((Control)cmdExportA).set_Font(new Font("Tahoma", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdExportA).set_Location(new Point(3, 3));
			((Control)cmdExportA).set_Name("cmdExportA");
			((Control)cmdExportA).set_Size(new Size(21, 21));
			((Control)cmdExportA).set_TabIndex(0);
			((Control)cmdExportA).set_Text("&A");
			((ButtonBase)cmdExportA).set_TextAlign(ContentAlignment.TopCenter);
			toolTip1.SetToolTip((Control)(object)cmdExportA, "Export to A");
			((ButtonBase)cmdExportA).set_UseVisualStyleBackColor(true);
			((Control)cmdExportA).add_Click((EventHandler)cmdExportA_Click);
			updnLongitude.set_DecimalPlaces(1);
			((Control)updnLongitude).set_Location(new Point(70, 14));
			updnLongitude.set_Maximum(new decimal(new int[4] { 180, 0, 0, 0 }));
			updnLongitude.set_Minimum(new decimal(new int[4] { 180, 0, 0, -2147483648 }));
			((Control)updnLongitude).set_Name("updnLongitude");
			((Control)updnLongitude).set_Size(new Size(61, 20));
			((Control)updnLongitude).set_TabIndex(3);
			updnLatitude.set_DecimalPlaces(1);
			((Control)updnLatitude).set_Location(new Point(180, 14));
			updnLatitude.set_Maximum(new decimal(new int[4] { 89, 0, 0, 0 }));
			updnLatitude.set_Minimum(new decimal(new int[4] { 89, 0, 0, -2147483648 }));
			((Control)updnLatitude).set_Name("updnLatitude");
			((Control)updnLatitude).set_Size(new Size(58, 20));
			((Control)updnLatitude).set_TabIndex(4);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(1, 16));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(71, 13));
			((Control)label1).set_TabIndex(5);
			((Control)label1).set_Text("Site longitude");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(135, 16));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(45, 13));
			((Control)label2).set_TabIndex(6);
			((Control)label2).set_Text("Latitude");
			((Control)cmdThis).set_Font(new Font("Tahoma", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdThis).set_Location(new Point(26, 10));
			((Control)cmdThis).set_Name("cmdThis");
			((Control)cmdThis).set_Size(new Size(21, 21));
			((Control)cmdThis).set_TabIndex(9);
			((Control)cmdThis).set_Text("||");
			toolTip1.SetToolTip((Control)(object)cmdThis, "Redraw");
			((ButtonBase)cmdThis).set_UseVisualStyleBackColor(true);
			((Control)cmdThis).add_Click((EventHandler)cmdThis_Click);
			((Control)cmdx1).set_Font(new Font("Tahoma", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdx1).set_Location(new Point(71, 10));
			((Control)cmdx1).set_Name("cmdx1");
			((Control)cmdx1).set_Size(new Size(16, 21));
			((Control)cmdx1).set_TabIndex(10);
			((Control)cmdx1).set_Text("1");
			toolTip1.SetToolTip((Control)(object)cmdx1, "redraw - scale = 1");
			((ButtonBase)cmdx1).set_UseVisualStyleBackColor(true);
			((Control)cmdx1).add_Click((EventHandler)cmdx1_Click);
			((Control)cmdx2).set_Font(new Font("Tahoma", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdx2).set_Location(new Point(87, 10));
			((Control)cmdx2).set_Name("cmdx2");
			((Control)cmdx2).set_Size(new Size(16, 21));
			((Control)cmdx2).set_TabIndex(11);
			((Control)cmdx2).set_Text("2");
			toolTip1.SetToolTip((Control)(object)cmdx2, "redraw - scale = 2");
			((ButtonBase)cmdx2).set_UseVisualStyleBackColor(true);
			((Control)cmdx2).add_Click((EventHandler)cmdx2_Click);
			((Control)cmdx4).set_Font(new Font("Tahoma", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdx4).set_Location(new Point(103, 10));
			((Control)cmdx4).set_Name("cmdx4");
			((Control)cmdx4).set_Size(new Size(16, 21));
			((Control)cmdx4).set_TabIndex(12);
			((Control)cmdx4).set_Text("4");
			toolTip1.SetToolTip((Control)(object)cmdx4, "redraw - scale = 4");
			((ButtonBase)cmdx4).set_UseVisualStyleBackColor(true);
			((Control)cmdx4).add_Click((EventHandler)cmdx4_Click);
			((Control)cmdx8).set_Font(new Font("Tahoma", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdx8).set_Location(new Point(119, 10));
			((Control)cmdx8).set_Name("cmdx8");
			((Control)cmdx8).set_Size(new Size(16, 21));
			((Control)cmdx8).set_TabIndex(13);
			((Control)cmdx8).set_Text("8");
			toolTip1.SetToolTip((Control)(object)cmdx8, "redraw - scale = 8");
			((ButtonBase)cmdx8).set_UseVisualStyleBackColor(true);
			((Control)cmdx8).add_Click((EventHandler)cmdx8_Click);
			((Control)cmdx16).set_Font(new Font("Tahoma", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdx16).set_Location(new Point(135, 10));
			((Control)cmdx16).set_Name("cmdx16");
			((Control)cmdx16).set_Size(new Size(24, 21));
			((Control)cmdx16).set_TabIndex(14);
			((Control)cmdx16).set_Text("16");
			toolTip1.SetToolTip((Control)(object)cmdx16, "redraw - scale = 16");
			((ButtonBase)cmdx16).set_UseVisualStyleBackColor(true);
			((Control)cmdx16).add_Click((EventHandler)cmdx16_Click);
			((Control)cmdx32).set_Font(new Font("Tahoma", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdx32).set_Location(new Point(159, 10));
			((Control)cmdx32).set_Name("cmdx32");
			((Control)cmdx32).set_Size(new Size(24, 21));
			((Control)cmdx32).set_TabIndex(15);
			((Control)cmdx32).set_Text("32");
			toolTip1.SetToolTip((Control)(object)cmdx32, "redraw - scale = 32");
			((ButtonBase)cmdx32).set_UseVisualStyleBackColor(true);
			((Control)cmdx32).add_Click((EventHandler)cmdx32_Click);
			((Control)cmdx01).set_Font(new Font("Tahoma", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdx01).set_Location(new Point(47, 10));
			((Control)cmdx01).set_Name("cmdx01");
			((Control)cmdx01).set_Size(new Size(24, 21));
			((Control)cmdx01).set_TabIndex(16);
			((Control)cmdx01).set_Text(".1");
			toolTip1.SetToolTip((Control)(object)cmdx01, "redraw - scale = 0.1");
			((ButtonBase)cmdx01).set_UseVisualStyleBackColor(true);
			((Control)cmdx01).add_Click((EventHandler)cmdx01_Click);
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label3).set_Location(new Point(45, 0));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(190, 13));
			((Control)label3).set_TabIndex(17);
			((Control)label3).set_Text("Double-Click on map to set site location");
			toolTip1.set_AutoPopDelay(5000);
			toolTip1.set_InitialDelay(100);
			toolTip1.set_ReshowDelay(100);
			optTimeCentered.set_AutoCheck(false);
			((Control)optTimeCentered).set_AutoSize(true);
			((Control)optTimeCentered).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optTimeCentered).set_Location(new Point(472, 23));
			((Control)optTimeCentered).set_Name("optTimeCentered");
			((Control)optTimeCentered).set_Size(new Size(84, 17));
			((Control)optTimeCentered).set_TabIndex(33);
			optTimeCentered.set_TabStop(true);
			((Control)optTimeCentered).set_Text("Time centred");
			toolTip1.SetToolTip((Control)(object)optTimeCentered, "When checked, orients the map using the\r\ntime along the path at the site location.\r\n\r\nThe map will always be oriented using the \r\ntime along the path, if the event duration >1hr.");
			((ButtonBase)optTimeCentered).set_UseVisualStyleBackColor(true);
			((Control)optTimeCentered).add_Click((EventHandler)optSiteCentered_Click);
			((Control)cmdPrevious).set_BackColor(SystemColors.Control);
			((Control)cmdPrevious).set_Font(new Font("Arial Black", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((ButtonBase)cmdPrevious).set_Image((Image)Resources.arrow_Previous_16xLG_color);
			((Control)cmdPrevious).set_Location(new Point(2, 10));
			((Control)cmdPrevious).set_Name("cmdPrevious");
			((Control)cmdPrevious).set_Size(new Size(24, 21));
			((Control)cmdPrevious).set_TabIndex(8);
			((ButtonBase)cmdPrevious).set_TextAlign(ContentAlignment.TopCenter);
			toolTip1.SetToolTip((Control)(object)cmdPrevious, "Move to previous");
			((ButtonBase)cmdPrevious).set_UseVisualStyleBackColor(false);
			((Control)cmdPrevious).add_Click((EventHandler)cmdPrevious_Click);
			((Control)cmdNext).set_BackColor(SystemColors.Control);
			((Control)cmdNext).set_Font(new Font("Arial Black", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((ButtonBase)cmdNext).set_Image((Image)Resources.arrow_Next_16xLG_color);
			((Control)cmdNext).set_Location(new Point(183, 10));
			((Control)cmdNext).set_Name("cmdNext");
			((Control)cmdNext).set_Size(new Size(24, 21));
			((Control)cmdNext).set_TabIndex(7);
			((ButtonBase)cmdNext).set_TextAlign(ContentAlignment.TopCenter);
			toolTip1.SetToolTip((Control)(object)cmdNext, "Move to next");
			((ButtonBase)cmdNext).set_UseVisualStyleBackColor(false);
			((Control)cmdNext).add_Click((EventHandler)cmdNext_Click);
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label4).set_Location(new Point(91, 21));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(52, 13));
			((Control)label4).set_TabIndex(20);
			((Control)label4).set_Text("Plot scale");
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_BackColor(SystemColors.Control);
			((Control)label5).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label5).set_Location(new Point(46, 1));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(82, 13));
			((Control)label5).set_TabIndex(21);
			((Control)label5).set_Text("Export elements");
			((Control)ctxtMenu).set_Font(new Font("Segoe UI", 9f));
			((ToolStrip)ctxtMenu).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[50]
			{
				(ToolStripItem)eventToolStripMenuItem,
				(ToolStripItem)toolStripSeparator18,
				(ToolStripItem)starChartToolStripMenuItem,
				(ToolStripItem)pathCoordinatesToolStripMenuItem1,
				(ToolStripItem)multiToolStripMenuItem,
				(ToolStripItem)prepointStarsToolStripMenuItem,
				(ToolStripItem)c2AStarChartToolStripMenuItem,
				(ToolStripItem)planetAppearanceToolStripMenuItem,
				(ToolStripItem)toolStripSeparator12,
				(ToolStripItem)asteroidRingsContactTimesForMultipleLocdationsToolStripMenuItem,
				(ToolStripItem)contactTimesForMultipleLocationsToolStripMenuItem,
				(ToolStripItem)toolStripSeparator6,
				(ToolStripItem)starToolStripMenuItem,
				(ToolStripItem)toolStripSeparator17,
				(ToolStripItem)displayMeasuredestimatedStellarDiameterToolStripMenuItem,
				(ToolStripItem)displayWDSInterferometricDataToolStripMenuItem1,
				(ToolStripItem)gaiaDoublesDisplayNearbyToolStripMenuItem,
				(ToolStripItem)listNearbyStarsToolStripMenuItem,
				(ToolStripItem)displayPredictionLightCurveToolStripMenuItem,
				(ToolStripItem)viewStarInGoogleSkyToolStripMenuItem,
				(ToolStripItem)compareStarCataloguePositionsToolStripMenuItem,
				(ToolStripItem)getStarInCDSPortalToolStripMenuItem,
				(ToolStripItem)plotStarUsingCDSSimPlayToolStripMenuItem,
				(ToolStripItem)listRelativisticOffsetToolStripMenuItem,
				(ToolStripItem)astrometryAtMinorPlanetCenterOccnSearchFor275ToolStripMenuItem,
				(ToolStripItem)listPreviousOccultionsOfThisStarToolStripMenuItem1,
				(ToolStripItem)toolStripSeparator9,
				(ToolStripItem)asteroidDetailsToolStripMenuItem,
				(ToolStripItem)toolStripSeparator16,
				(ToolStripItem)listAsteroidIRDiametersToolStripMenuItem1,
				(ToolStripItem)toolStripJPLsmallbodies,
				(ToolStripItem)binaryAsteroidsToolStripMenuItem1,
				(ToolStripItem)displayToolStripMenuItem,
				(ToolStripItem)displayAsteroidsRingSystemToolStripMenuItem,
				(ToolStripItem)lightCurveDataToolStripMenuItem,
				(ToolStripItem)dAMITAsteroidShapesToolStripMenuItem,
				(ToolStripItem)toolStripSeparator4,
				(ToolStripItem)otherToolStripMenuItem1,
				(ToolStripItem)toolStripSeparator7,
				(ToolStripItem)showRegionalWeatherPrediction16DayLimitToolStripMenuItem,
				(ToolStripItem)toolStripSeparator5,
				(ToolStripItem)viewInGoogleEarthToolStripMenuItem,
				(ToolStripItem)saveAsGoogleEarthKMLFileToolStripMenuItem,
				(ToolStripItem)saveAsGToolStripMenuItem,
				(ToolStripItem)toolStripSeparator14,
				(ToolStripItem)copyToolStripMenuItem1,
				(ToolStripItem)copyOccultationElementsForThisEventToolStripMenuItem1,
				(ToolStripItem)printPreviewToolStripMenuItem1,
				(ToolStripItem)printToolStripMenuItem1,
				(ToolStripItem)saveToolStripMenuItem1
			});
			((Control)ctxtMenu).set_Name("ctxtMenu");
			((Control)ctxtMenu).set_Size(new Size(357, 1126));
			((ToolStripItem)eventToolStripMenuItem).set_BackColor(Color.Cornsilk);
			((ToolStripItem)eventToolStripMenuItem).set_Enabled(false);
			((ToolStripItem)eventToolStripMenuItem).set_Font(new Font("Segoe UI", 12f, FontStyle.Bold));
			((ToolStripItem)eventToolStripMenuItem).set_Name("eventToolStripMenuItem");
			((ToolStripItem)eventToolStripMenuItem).set_Size(new Size(356, 26));
			((ToolStripItem)eventToolStripMenuItem).set_Text("Event");
			((ToolStripItem)toolStripSeparator18).set_Name("toolStripSeparator18");
			((ToolStripItem)toolStripSeparator18).set_Size(new Size(353, 6));
			((ToolStripItem)starChartToolStripMenuItem).set_Image((Image)Resources.Stars3);
			((ToolStripItem)starChartToolStripMenuItem).set_Name("starChartToolStripMenuItem");
			starChartToolStripMenuItem.set_ShortcutKeys((Keys)118);
			((ToolStripItem)starChartToolStripMenuItem).set_Size(new Size(356, 26));
			((ToolStripItem)starChartToolStripMenuItem).set_Text("Large star chart");
			((ToolStripItem)starChartToolStripMenuItem).add_Click((EventHandler)starChartToolStripMenuItem_Click);
			((ToolStripItem)pathCoordinatesToolStripMenuItem1).set_Image((Image)Resources.Paths);
			((ToolStripItem)pathCoordinatesToolStripMenuItem1).set_Name("pathCoordinatesToolStripMenuItem1");
			pathCoordinatesToolStripMenuItem1.set_ShortcutKeys((Keys)122);
			((ToolStripItem)pathCoordinatesToolStripMenuItem1).set_Size(new Size(356, 26));
			((ToolStripItem)pathCoordinatesToolStripMenuItem1).set_Text("Path coordinates");
			((ToolStripItem)pathCoordinatesToolStripMenuItem1).add_Click((EventHandler)pathCoordinatesToolStripMenuItem1_Click);
			((ToolStripItem)multiToolStripMenuItem).set_Image((Image)Resources.MultiLocation);
			((ToolStripItem)multiToolStripMenuItem).set_Name("multiToolStripMenuItem");
			multiToolStripMenuItem.set_ShortcutKeys((Keys)123);
			((ToolStripItem)multiToolStripMenuItem).set_Size(new Size(356, 26));
			((ToolStripItem)multiToolStripMenuItem).set_Text("Path distances for multiple locations");
			((ToolStripItem)multiToolStripMenuItem).add_Click((EventHandler)multiToolStripMenuItem_Click);
			((ToolStripItem)prepointStarsToolStripMenuItem).set_Image((Image)Resources.POINT06);
			((ToolStripItem)prepointStarsToolStripMenuItem).set_Name("prepointStarsToolStripMenuItem");
			((ToolStripItem)prepointStarsToolStripMenuItem).set_Size(new Size(356, 26));
			((ToolStripItem)prepointStarsToolStripMenuItem).set_Text("Pre-point stars");
			((ToolStripItem)prepointStarsToolStripMenuItem).add_Click((EventHandler)prepointStarsToolStripMenuItem_Click);
			((ToolStripItem)c2AStarChartToolStripMenuItem).set_Image((Image)Resources.c2aw);
			((ToolStripItem)c2AStarChartToolStripMenuItem).set_Name("c2AStarChartToolStripMenuItem");
			c2AStarChartToolStripMenuItem.set_ShortcutKeys((Keys)131122);
			((ToolStripItem)c2AStarChartToolStripMenuItem).set_Size(new Size(356, 26));
			((ToolStripItem)c2AStarChartToolStripMenuItem).set_Text("C2A star chart");
			((ToolStripItem)c2AStarChartToolStripMenuItem).add_Click((EventHandler)c2AStarChartToolStripMenuItem_Click);
			((ToolStripItem)planetAppearanceToolStripMenuItem).set_Image((Image)Resources.Satellite);
			((ToolStripItem)planetAppearanceToolStripMenuItem).set_Name("planetAppearanceToolStripMenuItem");
			planetAppearanceToolStripMenuItem.set_ShortcutKeys((Keys)120);
			((ToolStripItem)planetAppearanceToolStripMenuItem).set_Size(new Size(356, 26));
			((ToolStripItem)planetAppearanceToolStripMenuItem).set_Text("Planet appearance");
			((ToolStripItem)planetAppearanceToolStripMenuItem).add_Click((EventHandler)planetAppearanceToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator12).set_Name("toolStripSeparator12");
			((ToolStripItem)toolStripSeparator12).set_Size(new Size(353, 6));
			((ToolStripItem)asteroidRingsContactTimesForMultipleLocdationsToolStripMenuItem).set_Image((Image)Resources.Ring1);
			((ToolStripItem)asteroidRingsContactTimesForMultipleLocdationsToolStripMenuItem).set_Name("asteroidRingsContactTimesForMultipleLocdationsToolStripMenuItem");
			((ToolStripItem)asteroidRingsContactTimesForMultipleLocdationsToolStripMenuItem).set_Size(new Size(356, 26));
			((ToolStripItem)asteroidRingsContactTimesForMultipleLocdationsToolStripMenuItem).set_Text("Asteroid rings contact times for multiple locations");
			((ToolStripItem)asteroidRingsContactTimesForMultipleLocdationsToolStripMenuItem).add_Click((EventHandler)asteroidRingsContactTimesForMultipleLocdationsToolStripMenuItem_Click);
			((ToolStripItem)contactTimesForMultipleLocationsToolStripMenuItem).set_Image((Image)Resources.Planet);
			((ToolStripItem)contactTimesForMultipleLocationsToolStripMenuItem).set_Name("contactTimesForMultipleLocationsToolStripMenuItem");
			((ToolStripItem)contactTimesForMultipleLocationsToolStripMenuItem).set_Size(new Size(356, 26));
			((ToolStripItem)contactTimesForMultipleLocationsToolStripMenuItem).set_Text("Planet contact times for multiple locations");
			((ToolStripItem)contactTimesForMultipleLocationsToolStripMenuItem).add_Click((EventHandler)contactTimesForMultipleLocationsToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator6).set_Name("toolStripSeparator6");
			((ToolStripItem)toolStripSeparator6).set_Size(new Size(353, 6));
			((ToolStripItem)starToolStripMenuItem).set_BackColor(Color.PowderBlue);
			((ToolStripItem)starToolStripMenuItem).set_Enabled(false);
			((ToolStripItem)starToolStripMenuItem).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((ToolStripItem)starToolStripMenuItem).set_Name("starToolStripMenuItem");
			((ToolStripItem)starToolStripMenuItem).set_Size(new Size(356, 26));
			((ToolStripItem)starToolStripMenuItem).set_Text("Star");
			((ToolStripItem)toolStripSeparator17).set_Name("toolStripSeparator17");
			((ToolStripItem)toolStripSeparator17).set_Size(new Size(353, 6));
			((ToolStripItem)displayMeasuredestimatedStellarDiameterToolStripMenuItem).set_Image((Image)Resources.Fresnel);
			((ToolStripItem)displayMeasuredestimatedStellarDiameterToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)displayMeasuredestimatedStellarDiameterToolStripMenuItem).set_Name("displayMeasuredestimatedStellarDiameterToolStripMenuItem");
			((ToolStripItem)displayMeasuredestimatedStellarDiameterToolStripMenuItem).set_Size(new Size(356, 26));
			((ToolStripItem)displayMeasuredestimatedStellarDiameterToolStripMenuItem).set_Text("Star's diameter && Fresnel diffraction");
			((ToolStripItem)displayMeasuredestimatedStellarDiameterToolStripMenuItem).add_Click((EventHandler)displayMeasuredestimatedStellarDiameterToolStripMenuItem_Click);
			((ToolStripItem)displayWDSInterferometricDataToolStripMenuItem1).set_Image((Image)Resources.services);
			((ToolStripItem)displayWDSInterferometricDataToolStripMenuItem1).set_Name("displayWDSInterferometricDataToolStripMenuItem1");
			((ToolStripItem)displayWDSInterferometricDataToolStripMenuItem1).set_Size(new Size(356, 26));
			((ToolStripItem)displayWDSInterferometricDataToolStripMenuItem1).set_Text("Double stars (WDS && 6th IF catalog), Variable data");
			((ToolStripItem)displayWDSInterferometricDataToolStripMenuItem1).add_Click((EventHandler)displayWDSInterferometricDataToolStripMenuItem1_Click);
			((ToolStripItem)gaiaDoublesDisplayNearbyToolStripMenuItem).set_Image((Image)Resources.Gaia);
			((ToolStripItem)gaiaDoublesDisplayNearbyToolStripMenuItem).set_Name("gaiaDoublesDisplayNearbyToolStripMenuItem");
			((ToolStripItem)gaiaDoublesDisplayNearbyToolStripMenuItem).set_Size(new Size(356, 26));
			((ToolStripItem)gaiaDoublesDisplayNearbyToolStripMenuItem).set_Text("Gaia double stars (nearby)");
			((ToolStripItem)gaiaDoublesDisplayNearbyToolStripMenuItem).add_Click((EventHandler)gaiaDoublesDisplayNearbyToolStripMenuItem_Click);
			((ToolStripItem)listNearbyStarsToolStripMenuItem).set_Name("listNearbyStarsToolStripMenuItem");
			((ToolStripItem)listNearbyStarsToolStripMenuItem).set_Size(new Size(356, 26));
			((ToolStripItem)listNearbyStarsToolStripMenuItem).set_Text("Near-by stars");
			((ToolStripItem)listNearbyStarsToolStripMenuItem).add_Click((EventHandler)listNearbyStarsToolStripMenuItem_Click);
			((ToolStripItem)displayPredictionLightCurveToolStripMenuItem).set_Image((Image)Resources.GrazeBW);
			((ToolStripItem)displayPredictionLightCurveToolStripMenuItem).set_Name("displayPredictionLightCurveToolStripMenuItem");
			((ToolStripItem)displayPredictionLightCurveToolStripMenuItem).set_Size(new Size(356, 26));
			((ToolStripItem)displayPredictionLightCurveToolStripMenuItem).set_Text("Prediction light curve");
			((ToolStripItem)displayPredictionLightCurveToolStripMenuItem).add_Click((EventHandler)displayPredictionLightCurveToolStripMenuItem_Click);
			((ToolStripItem)viewStarInGoogleSkyToolStripMenuItem).set_Image((Image)Resources.sky_mo);
			((ToolStripItem)viewStarInGoogleSkyToolStripMenuItem).set_Name("viewStarInGoogleSkyToolStripMenuItem");
			viewStarInGoogleSkyToolStripMenuItem.set_ShortcutKeys((Keys)131143);
			((ToolStripItem)viewStarInGoogleSkyToolStripMenuItem).set_Size(new Size(356, 26));
			((ToolStripItem)viewStarInGoogleSkyToolStripMenuItem).set_Text("View star in GoogleSky");
			((ToolStripItem)viewStarInGoogleSkyToolStripMenuItem).add_Click((EventHandler)viewStarInGoogleSkyToolStripMenuItem_Click);
			((ToolStripItem)compareStarCataloguePositionsToolStripMenuItem).set_Image((Image)Resources.vizier_40x35);
			((ToolStripItem)compareStarCataloguePositionsToolStripMenuItem).set_Name("compareStarCataloguePositionsToolStripMenuItem");
			((ToolStripItem)compareStarCataloguePositionsToolStripMenuItem).set_Size(new Size(356, 26));
			((ToolStripItem)compareStarCataloguePositionsToolStripMenuItem).set_Text("Compare star catalogue positions using VizieR");
			((ToolStripItem)compareStarCataloguePositionsToolStripMenuItem).add_Click((EventHandler)compareStarCataloguePositionsToolStripMenuItem_Click);
			((ToolStripItem)getStarInCDSPortalToolStripMenuItem).set_Image((Image)Resources.cds_icon);
			((ToolStripItem)getStarInCDSPortalToolStripMenuItem).set_Name("getStarInCDSPortalToolStripMenuItem");
			((ToolStripItem)getStarInCDSPortalToolStripMenuItem).set_Size(new Size(356, 26));
			((ToolStripItem)getStarInCDSPortalToolStripMenuItem).set_Text("List star details through the CDS Portal");
			((ToolStripItem)getStarInCDSPortalToolStripMenuItem).add_Click((EventHandler)getStarInCDSPortalToolStripMenuItem_Click);
			((ToolStripItem)plotStarUsingCDSSimPlayToolStripMenuItem).set_Image((Image)Resources.cds_icon);
			((ToolStripItem)plotStarUsingCDSSimPlayToolStripMenuItem).set_Name("plotStarUsingCDSSimPlayToolStripMenuItem");
			((ToolStripItem)plotStarUsingCDSSimPlayToolStripMenuItem).set_Size(new Size(356, 26));
			((ToolStripItem)plotStarUsingCDSSimPlayToolStripMenuItem).set_Text("Plot star using CDS SimPlay");
			((ToolStripItem)plotStarUsingCDSSimPlayToolStripMenuItem).add_Click((EventHandler)plotStarUsingCDSSimPlayToolStripMenuItem_Click);
			((ToolStripItem)listRelativisticOffsetToolStripMenuItem).set_Image((Image)Resources.searchweb);
			((ToolStripItem)listRelativisticOffsetToolStripMenuItem).set_Name("listRelativisticOffsetToolStripMenuItem");
			((ToolStripItem)listRelativisticOffsetToolStripMenuItem).set_Size(new Size(356, 26));
			((ToolStripItem)listRelativisticOffsetToolStripMenuItem).set_Text("Gravitational light deflection");
			((ToolStripItem)listRelativisticOffsetToolStripMenuItem).add_Click((EventHandler)listRelativisticOffsetToolStripMenuItem_Click);
			((ToolStripItem)astrometryAtMinorPlanetCenterOccnSearchFor275ToolStripMenuItem).set_Font(new Font("Segoe UI", 7f, FontStyle.Bold));
			((ToolStripItem)astrometryAtMinorPlanetCenterOccnSearchFor275ToolStripMenuItem).set_ForeColor(Color.MidnightBlue);
			((ToolStripItem)astrometryAtMinorPlanetCenterOccnSearchFor275ToolStripMenuItem).set_Image((Image)Resources.Ruler_48x);
			((ToolStripItem)astrometryAtMinorPlanetCenterOccnSearchFor275ToolStripMenuItem).set_Name("astrometryAtMinorPlanetCenterOccnSearchFor275ToolStripMenuItem");
			((ToolStripItem)astrometryAtMinorPlanetCenterOccnSearchFor275ToolStripMenuItem).set_Size(new Size(356, 26));
			((ToolStripItem)astrometryAtMinorPlanetCenterOccnSearchFor275ToolStripMenuItem).set_Text("Astrometry at Minor Planet Center  [Occn : search for '275 ']");
			((ToolStripItem)astrometryAtMinorPlanetCenterOccnSearchFor275ToolStripMenuItem).add_Click((EventHandler)astrometryAtMinorPlanetCenterOccnSearchFor275ToolStripMenuItem_Click);
			((ToolStripItem)listPreviousOccultionsOfThisStarToolStripMenuItem1).set_Image((Image)Resources.Previous_48x_0000);
			((ToolStripItem)listPreviousOccultionsOfThisStarToolStripMenuItem1).set_Name("listPreviousOccultionsOfThisStarToolStripMenuItem1");
			((ToolStripItem)listPreviousOccultionsOfThisStarToolStripMenuItem1).set_Size(new Size(356, 26));
			((ToolStripItem)listPreviousOccultionsOfThisStarToolStripMenuItem1).set_Text("List previous occultions of this star");
			((ToolStripItem)listPreviousOccultionsOfThisStarToolStripMenuItem1).add_Click((EventHandler)listPreviousOccultionsOfThisStarToolStripMenuItem1_Click);
			((ToolStripItem)toolStripSeparator9).set_Name("toolStripSeparator9");
			((ToolStripItem)toolStripSeparator9).set_Size(new Size(353, 6));
			((ToolStripItem)asteroidDetailsToolStripMenuItem).set_BackColor(Color.PaleGreen);
			((ToolStripItem)asteroidDetailsToolStripMenuItem).set_Enabled(false);
			((ToolStripItem)asteroidDetailsToolStripMenuItem).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((ToolStripItem)asteroidDetailsToolStripMenuItem).set_Name("asteroidDetailsToolStripMenuItem");
			((ToolStripItem)asteroidDetailsToolStripMenuItem).set_Size(new Size(356, 26));
			((ToolStripItem)asteroidDetailsToolStripMenuItem).set_Text("Asteroid");
			((ToolStripItem)toolStripSeparator16).set_Name("toolStripSeparator16");
			((ToolStripItem)toolStripSeparator16).set_Size(new Size(353, 6));
			((ToolStripItem)listAsteroidIRDiametersToolStripMenuItem1).set_Image((Image)Resources.Dimension_32xSM_0000);
			((ToolStripItem)listAsteroidIRDiametersToolStripMenuItem1).set_Name("listAsteroidIRDiametersToolStripMenuItem1");
			((ToolStripItem)listAsteroidIRDiametersToolStripMenuItem1).set_Size(new Size(356, 26));
			((ToolStripItem)listAsteroidIRDiametersToolStripMenuItem1).set_Text("Asteroid IR diameters ;   diameters from H0");
			((ToolStripItem)listAsteroidIRDiametersToolStripMenuItem1).add_Click((EventHandler)listAsteroidIRDiametersToolStripMenuItem1_Click);
			((ToolStripItem)binaryAsteroidsToolStripMenuItem1).set_Name("binaryAsteroidsToolStripMenuItem1");
			((ToolStripItem)binaryAsteroidsToolStripMenuItem1).set_Size(new Size(356, 26));
			((ToolStripItem)binaryAsteroidsToolStripMenuItem1).set_Text("Binary asteroids");
			((ToolStripItem)binaryAsteroidsToolStripMenuItem1).add_Click((EventHandler)binaryAsteroidsToolStripMenuItem1_Click);
			((ToolStripItem)displayToolStripMenuItem).set_Image((Image)Resources.logo_IMCCE_t);
			((ToolStripItem)displayToolStripMenuItem).set_Name("displayToolStripMenuItem");
			((ToolStripItem)displayToolStripMenuItem).set_Size(new Size(356, 26));
			((ToolStripItem)displayToolStripMenuItem).set_Text("Asteroid satellite orbits using Miriade");
			((ToolStripItem)displayToolStripMenuItem).add_Click((EventHandler)displayToolStripMenuItem_Click);
			displayAsteroidsRingSystemToolStripMenuItem.set_Checked(true);
			displayAsteroidsRingSystemToolStripMenuItem.set_CheckState((CheckState)1);
			((ToolStripItem)displayAsteroidsRingSystemToolStripMenuItem).set_Image((Image)Resources.blankcd);
			((ToolStripItem)displayAsteroidsRingSystemToolStripMenuItem).set_Name("displayAsteroidsRingSystemToolStripMenuItem");
			((ToolStripItem)displayAsteroidsRingSystemToolStripMenuItem).set_Size(new Size(356, 26));
			((ToolStripItem)displayAsteroidsRingSystemToolStripMenuItem).set_Text("Asteroid's ring and moon system");
			((ToolStripItem)displayAsteroidsRingSystemToolStripMenuItem).add_Click((EventHandler)displayAsteroidsRingSystemToolStripMenuItem_Click);
			((ToolStripItem)lightCurveDataToolStripMenuItem).set_Image((Image)Resources.GRAPH07);
			((ToolStripItem)lightCurveDataToolStripMenuItem).set_Name("lightCurveDataToolStripMenuItem");
			((ToolStripItem)lightCurveDataToolStripMenuItem).set_Size(new Size(356, 26));
			((ToolStripItem)lightCurveDataToolStripMenuItem).set_Text("Light curve data");
			((ToolStripItem)lightCurveDataToolStripMenuItem).add_Click((EventHandler)lightCurveDataToolStripMenuItem_Click);
			((ToolStripItem)dAMITAsteroidShapesToolStripMenuItem).set_Image((Image)Resources.Asteroid);
			((ToolStripItem)dAMITAsteroidShapesToolStripMenuItem).set_Name("dAMITAsteroidShapesToolStripMenuItem");
			((ToolStripItem)dAMITAsteroidShapesToolStripMenuItem).set_Size(new Size(356, 26));
			((ToolStripItem)dAMITAsteroidShapesToolStripMenuItem).set_Text("Shape models");
			((ToolStripItem)dAMITAsteroidShapesToolStripMenuItem).add_Click((EventHandler)dAMITAsteroidShapesToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator4).set_Name("toolStripSeparator4");
			((ToolStripItem)toolStripSeparator4).set_Size(new Size(353, 6));
			((ToolStripItem)otherToolStripMenuItem1).set_BackColor(Color.Thistle);
			((ToolStripItem)otherToolStripMenuItem1).set_Enabled(false);
			((ToolStripItem)otherToolStripMenuItem1).set_Font(new Font("Segoe UI", 12f, FontStyle.Bold));
			((ToolStripItem)otherToolStripMenuItem1).set_Name("otherToolStripMenuItem1");
			((ToolStripItem)otherToolStripMenuItem1).set_Size(new Size(356, 26));
			((ToolStripItem)otherToolStripMenuItem1).set_Text("Other");
			((ToolStripItem)toolStripSeparator7).set_Name("toolStripSeparator7");
			((ToolStripItem)toolStripSeparator7).set_Size(new Size(353, 6));
			((ToolStripItem)showRegionalWeatherPrediction16DayLimitToolStripMenuItem).set_Image((Image)Resources.CLOUD);
			((ToolStripItem)showRegionalWeatherPrediction16DayLimitToolStripMenuItem).set_Name("showRegionalWeatherPrediction16DayLimitToolStripMenuItem");
			((ToolStripItem)showRegionalWeatherPrediction16DayLimitToolStripMenuItem).set_Size(new Size(356, 26));
			((ToolStripItem)showRegionalWeatherPrediction16DayLimitToolStripMenuItem).set_Text("Show regional weather prediction (16 day limit)");
			((ToolStripItem)showRegionalWeatherPrediction16DayLimitToolStripMenuItem).add_Click((EventHandler)showRegionalWeatherPrediction16DayLimitToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator5).set_Name("toolStripSeparator5");
			((ToolStripItem)toolStripSeparator5).set_Size(new Size(353, 6));
			((ToolStripItem)viewInGoogleEarthToolStripMenuItem).set_Image((Image)Resources.google_earth);
			((ToolStripItem)viewInGoogleEarthToolStripMenuItem).set_Name("viewInGoogleEarthToolStripMenuItem");
			((ToolStripItem)viewInGoogleEarthToolStripMenuItem).set_Size(new Size(356, 26));
			((ToolStripItem)viewInGoogleEarthToolStripMenuItem).set_Text("View in GoogleEarth");
			((ToolStripItem)viewInGoogleEarthToolStripMenuItem).add_Click((EventHandler)viewInGoogleEarthToolStripMenuItem_Click);
			((ToolStripItem)saveAsGoogleEarthKMLFileToolStripMenuItem).set_Image((Image)Resources.google_earth);
			((ToolStripItem)saveAsGoogleEarthKMLFileToolStripMenuItem).set_Name("saveAsGoogleEarthKMLFileToolStripMenuItem");
			((ToolStripItem)saveAsGoogleEarthKMLFileToolStripMenuItem).set_Size(new Size(356, 26));
			((ToolStripItem)saveAsGoogleEarthKMLFileToolStripMenuItem).set_Text("Save as GoogleEarth KMZ file");
			((ToolStripItem)saveAsGoogleEarthKMLFileToolStripMenuItem).add_Click((EventHandler)saveAsGoogleEarthKMLFileToolStripMenuItem_Click);
			((ToolStripItem)saveAsGToolStripMenuItem).set_Image((Image)Resources.mm_20_red);
			((ToolStripItem)saveAsGToolStripMenuItem).set_Name("saveAsGToolStripMenuItem");
			((ToolStripItem)saveAsGToolStripMenuItem).set_Size(new Size(356, 26));
			((ToolStripItem)saveAsGToolStripMenuItem).set_Text("Save as GoogleMaps HTM file");
			((ToolStripItem)saveAsGToolStripMenuItem).add_Click((EventHandler)saveAsGToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator14).set_Name("toolStripSeparator14");
			((ToolStripItem)toolStripSeparator14).set_Size(new Size(353, 6));
			((ToolStripItem)copyToolStripMenuItem1).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem1).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyToolStripMenuItem1).set_Name("copyToolStripMenuItem1");
			copyToolStripMenuItem1.set_ShortcutKeys((Keys)131139);
			((ToolStripItem)copyToolStripMenuItem1).set_Size(new Size(356, 26));
			((ToolStripItem)copyToolStripMenuItem1).set_Text("Copy");
			((ToolStripItem)copyToolStripMenuItem1).add_Click((EventHandler)copyToolStripMenuItem1_Click);
			((ToolStripItem)copyOccultationElementsForThisEventToolStripMenuItem1).set_Image((Image)Resources.CopyItem_32x);
			((ToolStripItem)copyOccultationElementsForThisEventToolStripMenuItem1).set_Name("copyOccultationElementsForThisEventToolStripMenuItem1");
			((ToolStripItem)copyOccultationElementsForThisEventToolStripMenuItem1).set_Size(new Size(356, 26));
			((ToolStripItem)copyOccultationElementsForThisEventToolStripMenuItem1).set_Text("Copy occultation elements for this event");
			((ToolStripItem)copyOccultationElementsForThisEventToolStripMenuItem1).add_Click((EventHandler)copyOccultationElementsForThisEventToolStripMenuItem1_Click);
			((ToolStripItem)printPreviewToolStripMenuItem1).set_Image((Image)Resources.PrintPreview);
			((ToolStripItem)printPreviewToolStripMenuItem1).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printPreviewToolStripMenuItem1).set_Name("printPreviewToolStripMenuItem1");
			((ToolStripItem)printPreviewToolStripMenuItem1).set_Size(new Size(356, 26));
			((ToolStripItem)printPreviewToolStripMenuItem1).set_Text("Print Preview");
			((ToolStripItem)printPreviewToolStripMenuItem1).add_Click((EventHandler)printPreviewToolStripMenuItem1_Click);
			((ToolStripItem)printToolStripMenuItem1).set_Image((Image)Resources.Print);
			((ToolStripItem)printToolStripMenuItem1).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printToolStripMenuItem1).set_Name("printToolStripMenuItem1");
			printToolStripMenuItem1.set_ShortcutKeys((Keys)131152);
			((ToolStripItem)printToolStripMenuItem1).set_Size(new Size(356, 26));
			((ToolStripItem)printToolStripMenuItem1).set_Text("Print");
			((ToolStripItem)printToolStripMenuItem1).add_Click((EventHandler)printToolStripMenuItem1_Click);
			((ToolStripItem)saveToolStripMenuItem1).set_Image((Image)Resources.Save);
			((ToolStripItem)saveToolStripMenuItem1).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveToolStripMenuItem1).set_Name("saveToolStripMenuItem1");
			saveToolStripMenuItem1.set_ShortcutKeys((Keys)131155);
			((ToolStripItem)saveToolStripMenuItem1).set_Size(new Size(356, 26));
			((ToolStripItem)saveToolStripMenuItem1).set_Text("Save");
			((ToolStripItem)saveToolStripMenuItem1).add_Click((EventHandler)saveToolStripMenuItem1_Click);
			((Control)pBarPlot).set_Location(new Point(695, 8));
			((Control)pBarPlot).set_Name("pBarPlot");
			((Control)pBarPlot).set_Size(new Size(122, 10));
			((Control)pBarPlot).set_TabIndex(26);
			((Control)pBarPlot).set_Visible(false);
			((Control)cmbSiteFiles).set_Anchor((AnchorStyles)1);
			((ListControl)cmbSiteFiles).set_FormattingEnabled(true);
			((Control)cmbSiteFiles).set_Location(new Point(6, 14));
			((Control)cmbSiteFiles).set_Name("cmbSiteFiles");
			((Control)cmbSiteFiles).set_Size(new Size(99, 21));
			cmbSiteFiles.set_Sorted(true);
			((Control)cmbSiteFiles).set_TabIndex(27);
			cmbSiteFiles.add_SelectedIndexChanged((EventHandler)cmbSiteFiles_SelectedIndexChanged);
			((Control)cmbNames).set_Anchor((AnchorStyles)1);
			((ListControl)cmbNames).set_FormattingEnabled(true);
			cmbNames.get_Items().AddRange(new object[4] { "never", "all", "many", "some" });
			((Control)cmbNames).set_Location(new Point(112, 14));
			((Control)cmbNames).set_Name("cmbNames");
			((Control)cmbNames).set_Size(new Size(55, 21));
			((Control)cmbNames).set_TabIndex(28);
			cmbNames.add_SelectedIndexChanged((EventHandler)cmbNames_SelectedIndexChanged);
			((Control)label6).set_Anchor((AnchorStyles)1);
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Font(new Font("Microsoft Sans Serif", 7.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label6).set_Location(new Point(478, 43));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(162, 13));
			((Control)label6).set_TabIndex(29);
			((Control)label6).set_Text("Right-click for menu options");
			((Control)panelScale).set_Anchor((AnchorStyles)1);
			((Control)panelScale).get_Controls().Add((Control)(object)cmdx01);
			((Control)panelScale).get_Controls().Add((Control)(object)cmdx32);
			((Control)panelScale).get_Controls().Add((Control)(object)cmdx16);
			((Control)panelScale).get_Controls().Add((Control)(object)cmdx8);
			((Control)panelScale).get_Controls().Add((Control)(object)cmdx4);
			((Control)panelScale).get_Controls().Add((Control)(object)cmdx2);
			((Control)panelScale).get_Controls().Add((Control)(object)cmdx1);
			((Control)panelScale).get_Controls().Add((Control)(object)cmdThis);
			((Control)panelScale).get_Controls().Add((Control)(object)cmdPrevious);
			((Control)panelScale).get_Controls().Add((Control)(object)cmdNext);
			((Control)panelScale).set_Location(new Point(5, 23));
			((Control)panelScale).set_Name("panelScale");
			((Control)panelScale).set_Size(new Size(210, 32));
			((Control)panelScale).set_TabIndex(30);
			((Control)panelLocation).set_Anchor((AnchorStyles)1);
			((Control)panelLocation).get_Controls().Add((Control)(object)label2);
			((Control)panelLocation).get_Controls().Add((Control)(object)updnLatitude);
			((Control)panelLocation).get_Controls().Add((Control)(object)updnLongitude);
			((Control)panelLocation).get_Controls().Add((Control)(object)label3);
			((Control)panelLocation).get_Controls().Add((Control)(object)label1);
			((Control)panelLocation).set_Location(new Point(232, 23));
			((Control)panelLocation).set_Name("panelLocation");
			((Control)panelLocation).set_Size(new Size(238, 34));
			((Control)panelLocation).set_TabIndex(31);
			((Control)panelExportPlusSites).set_Anchor((AnchorStyles)1);
			((Control)panelExportPlusSites).get_Controls().Add((Control)(object)label5);
			((Control)panelExportPlusSites).get_Controls().Add((Control)(object)PanelExport);
			((Control)panelExportPlusSites).get_Controls().Add((Control)(object)cmbSiteFiles);
			((Control)panelExportPlusSites).get_Controls().Add((Control)(object)cmbNames);
			((Control)panelExportPlusSites).set_Location(new Point(646, 21));
			((Control)panelExportPlusSites).set_Name("panelExportPlusSites");
			((Control)panelExportPlusSites).set_Size(new Size(175, 42));
			((Control)panelExportPlusSites).set_TabIndex(32);
			((Control)picAsteroidPlot).set_ContextMenuStrip(ctxtMenu);
			((Control)picAsteroidPlot).set_Location(new Point(5, 57));
			((Control)picAsteroidPlot).set_Name("picAsteroidPlot");
			((Control)picAsteroidPlot).set_Size(new Size(820, 585));
			picAsteroidPlot.set_TabIndex(0);
			picAsteroidPlot.set_TabStop(false);
			((Control)picAsteroidPlot).add_DoubleClick((EventHandler)picAsteroidPlot_DoubleClick);
			((Control)picAsteroidPlot).add_MouseClick(new MouseEventHandler(picAsteroidPlot_MouseClick));
			((Control)picAsteroidPlot).add_MouseMove(new MouseEventHandler(picAsteroidPlot_MouseMove));
			((ToolStripItem)toolStripJPLsmallbodies).set_Name("toolStripJPLsmallbodies");
			((ToolStripItem)toolStripJPLsmallbodies).set_Size(new Size(356, 26));
			((ToolStripItem)toolStripJPLsmallbodies).set_Text("JPL Small Bodies Database for this asteroid or comet");
			((ToolStripItem)toolStripJPLsmallbodies).add_Click((EventHandler)toolStripJPLsmallbodies_Click);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(830, 649));
			((Control)this).get_Controls().Add((Control)(object)pBarPlot);
			((Control)this).get_Controls().Add((Control)(object)picAsteroidPlot);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_Controls().Add((Control)(object)label4);
			((Control)this).get_Controls().Add((Control)(object)panelExportPlusSites);
			((Control)this).get_Controls().Add((Control)(object)panelLocation);
			((Control)this).get_Controls().Add((Control)(object)panelScale);
			((Control)this).get_Controls().Add((Control)(object)label6);
			((Control)this).get_Controls().Add((Control)(object)optTimeCentered);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationAsterPlotPath", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_Icon((Icon)componentResourceManager.GetObject("$this.Icon"));
			((Form)this).set_Location(Settings.Default.LocationAsterPlotPath);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_Name("AsteroidPlotPath");
			((Control)this).set_Text("Asteroid - predicted path");
			((Form)this).add_FormClosing(new FormClosingEventHandler(AsteroidPlotPath_FormClosing));
			((Form)this).add_Load((EventHandler)AsteroidPlotPath_Load);
			((Control)this).add_Resize((EventHandler)AsteroidPlotPath_Resize);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)PanelExport).ResumeLayout(false);
			((ISupportInitialize)updnLongitude).EndInit();
			((ISupportInitialize)updnLatitude).EndInit();
			((Control)ctxtMenu).ResumeLayout(false);
			((Control)panelScale).ResumeLayout(false);
			((Control)panelLocation).ResumeLayout(false);
			((Control)panelLocation).PerformLayout();
			((Control)panelExportPlusSites).ResumeLayout(false);
			((Control)panelExportPlusSites).PerformLayout();
			((ISupportInitialize)picAsteroidPlot).EndInit();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
