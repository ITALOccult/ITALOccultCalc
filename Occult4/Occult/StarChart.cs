using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Configuration;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Drawing.Printing;
using System.IO;
using System.Windows.Forms;
using Occult.Properties;
using Occult.Star_Catalogues;

namespace Occult
{
	public class StarChart : Form
	{
		private readonly string AppPath;

		public double CentreRA;

		public double CentreDec;

		public bool ShowObjectPath;

		private const double Radian = 180.0 / Math.PI;

		private static bool ShowToolTip = true;

		private static bool FormStarted = false;

		private static bool UsingFullNomad;

		private static string NomadPath;

		private static float ChartHeightDegrees = 6f;

		private static float ChartWidth;

		private static float ChartHeight;

		private static float ChartSideLength;

		private static bool ShowInputFields;

		private static bool IsResizing = false;

		private static bool ChartPlotting = false;

		private bool GetToolTip = true;

		private static int ChartSizeIndex = 4;

		private static int StarCatalogIndex = 3;

		private static int MagLimit = 16;

		private static double RotateDeg = 0.0;

		private static bool VisualMag = true;

		private static bool GridLines = true;

		private static bool FlipHorizontal = false;

		private static bool FlipVertical = false;

		internal bool Use_Gaia = true;

		private static bool Use_Gaia16 = false;

		private static bool Use_Gaia14 = false;

		private static bool Use_UCAC4 = false;

		private static bool Use_NOMAD = false;

		private static bool Use_PPMXL = false;

		private static int xPos_Stars = 0;

		private static int yPos_Stars = 0;

		private static List<CompStars> CStars = new List<CompStars>();

		private IContainer components;

		private PictureBox picStars;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem standardChartsToolStripMenuItem;

		private ToolStripMenuItem sizeToolStripMenuItem;

		private ToolStripMenuItem magnitudeToolStripMenuItem;

		private ToolStripMenuItem toolStripMenuItem_AllVisual;

		private ToolStripMenuItem toolStripMenuItemMag16;

		private ToolStripMenuItem toolStripMenuItemMag14;

		private ToolStripMenuItem toolStripMenuItemMag13;

		private ToolStripMenuItem toolStripMenuItemMag12;

		private ToolStripMenuItem toolStripMenuItemMag11;

		private ToolStripMenuItem toolStripMenuItemMag10;

		private ToolStripMenuItem toolStripMenuItemMag9;

		private ToolStripMenuItem toolStripMenuItemMag8;

		private ToolStripMenuItem toolStripMenuItemMag7;

		private ToolStripMenuItem toolStripMenuItemMag6;

		private ToolStripMenuItem withPlotToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private ToolStripMenuItem x1DegToolStripMenuItem;

		private ToolStripMenuItem x2DegToolStripMenuItem;

		private ToolStripMenuItem x3DegToolStripMenuItem;

		private ToolStripMenuItem x4DegToolStripMenuItem;

		private ToolStripMenuItem x3DegMag120ToolStripMenuItem;

		private ToolStripMenuItem x10DegMag85ToolStripMenuItem;

		private ToolStripMenuItem x15DegMag70ToolStripMenuItem;

		private ToolStripMenuItem x5DegToolStripMenuItem;

		private ToolStripMenuItem x6DegToolStripMenuItem;

		private ToolStripMenuItem x8DegToolStripMenuItem;

		private ToolStripMenuItem x10DegToolStripMenuItem;

		private ToolStripMenuItem x12DegToolStripMenuItem;

		private ToolStripMenuItem x15DegToolStripMenuItem;

		private ToolStripMenuItem x20DegToolStripMenuItem;

		private ToolStripMenuItem flipHorizontallyToolStripMenuItem;

		private ToolStripMenuItem toolStripMenuItemMag15;

		private ToolStripMenuItem x5DegMag105ToolStripMenuItem;

		private ToolTip toolTip1;

		private ToolStripMenuItem showCoordinatesInTooltipToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator1;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem printToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripMenuItem printPreviewToolStripMenuItem;

		private ToolStripMenuItem toolStripMenuItemMag21;

		private ToolStripMenuItem toolStripMenuItemMag19;

		private ToolStripMenuItem toolStripMenuItemMag18;

		private ToolStripMenuItem toolStripMenuItemMag17;

		private ToolStripMenuItem toolStripMenuItemMag20;

		private ToolStripMenuItem x40mToolStripMenuItem;

		private ToolStripMenuItem x30mToolStripMenuItem;

		private ToolStripMenuItem x15mToolStripMenuItem;

		private Panel PanelCentre;

		private Label label13;

		private TextBox txtRM1;

		private TextBox txtRH1;

		private Label label15;

		private TextBox txtDD1;

		private TextBox txtDM1;

		private Label label8;

		private Label label18;

		private Label label17;

		private Button cmdPlot;

		private Label label16;

		private ToolStripMenuItem displayInGoogleSkyToolStripMenuItem;

		private Button GoogleSky;

		private ToolStripMenuItem helpToolStripMenuItem;

		public ToolStripMenuItem blackOnWhiteToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator3;

		private ToolStripMenuItem catalogueToolStripMenuItem;

		private ToolStripMenuItem GaiaToolStripMenuItem;

		private ToolStripMenuItem uCAC4ToolStripMenuItem;

		private ToolStripMenuItem nOMADToolStripMenuItem;

		private ToolStripMenuItem pPMXLToolStripMenuItem;

		private ToolStripMenuItem toolStripMenuItem_All;

		private ToolStripSeparator toolStripSeparator4;

		private ToolStripMenuItem setStarCatalogPathsToolStripMenuItem;

		internal TrackBar trackOpacity;

		private ToolStripMenuItem flipVerticallyToolStripMenuItem;

		internal TrackBar tBarRotate;

		private Label lblAngle;

		private Label label1;

		private Label label2;

		private Label lblRedraw;

		private Label label3;

		private Label label4;

		private Label label5;

		private Label label6;

		private Label label7;

		private Label label9;

		private Label label10;

		private Label label11;

		private Label label12;

		private ToolStripMenuItem x05DegMag160ToolStripMenuItem;

		private ToolStripMenuItem x1DegMag140ToolStripMenuItem;

		private Button cmdPlusHalf;

		private Button cmdMinusHalf;

		private Label label14;

		private Label label19;

		private ToolStripMenuItem x12mToolStripMenuItem;

		private ToolStripMenuItem x18mToolStripMenuItem;

		private ToolStripMenuItem x24mToolStripMenuItem;

		private ToolStripMenuItem x50mToolStripMenuItem;

		private ToolStripMenuItem x10mToolStripMenuItem;

		private Label label20;

		private ToolStripMenuItem storeSettingsToolStripMenuItem;

		private ToolStripMenuItem applyStoredToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator2;

		private ToolStripSeparator toolStripSeparator5;

		private CheckBox chkOnTop;

		private Panel panel1;

		private Panel panel2;

		private Label label21;

		private Label lblStarID;

		private Label lblSelected;

		private ToolStripMenuItem recenterToolStripMenuItem;

		private ToolStripMenuItem comparisonStarDetailsToolStripMenuItem;

		private Panel pnlCompStars;

		private ListBox lstStars;

		private Button button1;

		private Button cmdAddCurrent;

		private Button cmdClear;

		private Button cmdCopy;

		private Label label22;

		public StarChart(bool SetCoords)
		{
			InitializeComponent();
			AppPath = Utilities.AppPath;
			ShowInputFields = SetCoords;
		}

		private void StarChart_Load(object sender, EventArgs e)
		{
			CStars = new List<CompStars>();
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			if (((Control)this).get_Top() < -30000)
			{
				((Control)this).set_Top(0);
			}
			if ((double)((Control)this).get_Left() + (double)((Control)this).get_Width() / 1.2 < (double)Screen.GetWorkingArea((Control)(object)this).Left)
			{
				((Control)this).set_Left(Screen.GetWorkingArea((Control)(object)this).Left);
			}
			if (ShowInputFields)
			{
				((Control)picStars).set_Top(70);
			}
			else
			{
				((Control)picStars).set_Top(30);
			}
			((Control)PanelCentre).set_Visible(ShowInputFields);
			tBarRotate.set_Value(0);
			RotateDeg = 0.0;
			Gaia.GetAvailableGaiaCatalogues();
			((ToolStripItem)GaiaToolStripMenuItem).set_Enabled(Gaia.GaiaPrimaryFiles.Count > 0);
			((ToolStripItem)uCAC4ToolStripMenuItem).set_Enabled(File.Exists(Settings.Default.UCAC4_Path + "\\u4b\\z900"));
			((ToolStripItem)pPMXLToolStripMenuItem).set_Enabled(File.Exists(Settings.Default.PPMXL_Path + "\\n89d.dat"));
			((ToolStripItem)nOMADToolStripMenuItem).set_Enabled(File.Exists(Settings.Default.NOMAD_Path + "\\000\\m001.cat"));
			NomadPath = Settings.Default.NOMAD_Path;
			if (!NomadPath.ToLower().Contains("nomad") | !File.Exists(NomadPath + "\\090\\m0900.cat"))
			{
				NomadPath = Settings.Default.NOMAD_Short_path;
			}
			if (NomadPath.ToLower().Contains("nomad1"))
			{
				UsingFullNomad = true;
			}
			MagLimit = Settings.Default.StarChartMagLimitIndex;
			SetMagnitudeLimit(MagLimit);
			ChartSizeIndex = Settings.Default.StarChartSizeIndex;
			SetStarChartAngluarSize(ChartSizeIndex);
			recenterToolStripMenuItem.set_Checked(Settings.Default.StarMapDoubleClick);
			((ToolStripItem)displayInGoogleSkyToolStripMenuItem).set_Enabled(Settings.Default.GoogleEarthInstalled);
			if (Settings.Default.StarChartUseGaia)
			{
				StarCatalogIndex = 3;
			}
			else if (Settings.Default.StarChartUseUCAC4)
			{
				StarCatalogIndex = 4;
			}
			else if (Settings.Default.StarChartUsePPMXL)
			{
				StarCatalogIndex = 5;
			}
			else if (Settings.Default.StarChartUseNomad)
			{
				StarCatalogIndex = 6;
			}
			else
			{
				StarCatalogIndex = 3;
			}
			SetCatalogueChecks(StarCatalogIndex);
			ApplyStoredSettings();
			FormStarted = true;
			StarChart_Resize(sender, e);
		}

		private void printToolStripMenuItem_Click(object sender, EventArgs e)
		{
			PrintDocument printDocument = new PrintDocument();
			printDocument.DefaultPageSettings.Landscape = false;
			printDocument.PrintPage += PrintChart;
			printDocument.Print();
		}

		private void printPreviewToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0000: Unknown result type (might be due to invalid IL or missing references)
			//IL_000b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0030: Unknown result type (might be due to invalid IL or missing references)
			PrintPreviewDialog val = new PrintPreviewDialog();
			PrintDocument printDocument = new PrintDocument();
			val.set_Document(printDocument);
			printDocument.DefaultPageSettings.Landscape = false;
			printDocument.PrintPage += PrintChart;
			((Form)val).ShowDialog();
		}

		private void PrintChart(object sender, PrintPageEventArgs e)
		{
			Graphics graphics = e.Graphics;
			int width = e.MarginBounds.Width;
			DisplayMPOccultations.DrawAsteroidStarChart(ChartHeight: e.MarginBounds.Height, formGraphics: graphics, Printer: true, TopLeftX: e.MarginBounds.Left, TopLeftY: e.MarginBounds.Top + 40, ChartWidth: width, CentreRA: CentreRA, CentreDec: CentreDec, ChartHeightDegrees: ChartHeightDegrees, MagLimit: MagLimit, VisualMag: VisualMag, GridLines: GridLines, RotateAngle: RotateDeg, FlipHorizontal: FlipHorizontal, FlipVertical: FlipVertical, DrawInBW: false, Use_Gaia: StarCatalogIndex < 4, use_UCAC4: StarCatalogIndex == 4, Use_NOMAD: StarCatalogIndex == 6, Use_PPMXL: StarCatalogIndex == 5);
			Font font = new Font("Times New Roman", 12f, FontStyle.Bold);
			float width2 = graphics.MeasureString(((Control)this).get_Text(), font).Width;
			graphics.DrawString(((Control)this).get_Text(), font, Brushes.Black, (float)e.MarginBounds.Left + ((float)width - width2) / 2f, e.MarginBounds.Top);
		}

		public void PlotChart()
		{
			//IL_003d: Unknown result type (might be due to invalid IL or missing references)
			//IL_005d: Unknown result type (might be due to invalid IL or missing references)
			//IL_00a2: Unknown result type (might be due to invalid IL or missing references)
			if (!FormStarted)
			{
				return;
			}
			if (Use_NOMAD & (ChartHeightDegrees > 3f))
			{
				if (UsingFullNomad & (ChartHeightDegrees > 1f))
				{
					MessageBox.Show("The plot area is too large for plotting from NOMAD_Short.\r\n\r\nSelect a plot area of 1 degree or less", "Correct plot area", (MessageBoxButtons)0, (MessageBoxIcon)48);
					return;
				}
				if (ChartHeightDegrees > 3f)
				{
					MessageBox.Show("The plot area is too large for plotting from NOMAD_Short.\r\n\r\nSelect a plot area of 3 degrees or less", "Correct plot area", (MessageBoxButtons)0, (MessageBoxIcon)48);
					return;
				}
			}
			if (Math.Abs(CentreDec * (180.0 / Math.PI)) + (double)ChartHeightDegrees / 2.0 > 89.7)
			{
				MessageBox.Show("The edge of the plot goes too close to the Pole to plot.", "Correct plot area", (MessageBoxButtons)0, (MessageBoxIcon)48);
				return;
			}
			if (ShowInputFields)
			{
				GetChartCenter();
			}
			ChartWidth = ((Control)picStars).get_Width();
			ChartHeight = ((Control)picStars).get_Height();
			ChartSideLength = ChartWidth;
			Bitmap image = new Bitmap((int)ChartWidth, (int)ChartHeight);
			Graphics graphics = Graphics.FromImage(image);
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			if (Settings.Default.GraphicsSmoothed)
			{
				graphics.SmoothingMode = SmoothingMode.AntiAlias;
			}
			graphics.Clear(Color.White);
			double compStarRA = 0.0;
			double num = 100.0;
			if (((Control)pnlCompStars).get_Visible() && ((ListControl)lstStars).get_SelectedIndex() >= 0)
			{
				string text = lstStars.get_Items().get_Item(((ListControl)lstStars).get_SelectedIndex()).ToString();
				compStarRA = double.Parse(text.Substring(55, 2)) + double.Parse(text.Substring(59, 2)) / 60.0 + double.Parse(text.Substring(63, 5)) / 3600.0;
				num = double.Parse(text.Substring(76, 2)) + double.Parse(text.Substring(80, 2)) / 60.0 + double.Parse(text.Substring(84, 4)) / 3600.0;
				if (text.Substring(75, 1) == "-")
				{
					num = 0.0 - num;
				}
			}
			DisplayMPOccultations.DrawAsteroidStarChart(graphics, Printer: false, 0f, 0f, ChartWidth, ChartHeight, CentreRA, CentreDec, ChartHeightDegrees, MagLimit, VisualMag, GridLines, RotateDeg, FlipHorizontal, FlipVertical, blackOnWhiteToolStripMenuItem.get_Checked(), StarCatalogIndex < 4, StarCatalogIndex == 4, StarCatalogIndex == 6, StarCatalogIndex == 5, compStarRA, num);
			picStars.set_Image((Image)image);
			graphics.Dispose();
			string text2 = ((!(ChartHeightDegrees > 1f)) ? (Utilities.Constellation(CentreRA, CentreDec, AbbreviatedName: false) + string.Format(".   Plot {0,2:F0}' x {0,2:F0}', to Mag ", ChartHeightDegrees * 60f) + string.Format("{0,3:F1}", MagLimit)) : (Utilities.Constellation(CentreRA, CentreDec, AbbreviatedName: false) + string.Format(".   Plot {0,2:F0}° x{0,2:F0}°, to Mag ", ChartHeightDegrees) + string.Format("{0,3:F1}", MagLimit)));
			if (ShowObjectPath)
			{
				((Control)this).set_Text(DisplayMPOccultations.UTDate + ": " + DisplayMPOccultations.AsteroidName.Trim() + ", in " + text2);
			}
			else
			{
				((Control)this).set_Text(text2);
			}
			((Control)this).set_Cursor(Cursors.get_Default());
		}

		private void StarChart_Resize(object sender, EventArgs e)
		{
			if (IsResizing)
			{
				return;
			}
			IsResizing = true;
			((Control)this).set_Height(((Control)this).get_Width() + 91);
			if (((Control)this).get_Width() >= 40)
			{
				((Control)PanelCentre).set_Top(68);
				if (ShowInputFields)
				{
					((Control)this).set_Height(((Control)this).get_Height() + 22);
				}
				((Control)picStars).set_Width(((Control)this).get_Width() - 26);
				((Control)picStars).set_Height(((Control)this).get_Width() - 26);
				((Control)picStars).set_Top(((Control)this).get_Height() - 44 - ((Control)picStars).get_Height());
				if ((double)(3600f * ChartHeightDegrees / ChartSideLength) > 3.0)
				{
					((Control)lblStarID).set_Text("not available");
				}
				else if (((Control)lblStarID).get_Text() == "not available")
				{
					((Control)lblStarID).set_Text("x");
				}
				if (FormStarted)
				{
					PlotChart();
					IsResizing = false;
				}
			}
		}

		private void UnCheckedStandard()
		{
			x05DegMag160ToolStripMenuItem.set_Checked(false);
			x1DegMag140ToolStripMenuItem.set_Checked(false);
			x3DegMag120ToolStripMenuItem.set_Checked(false);
			x5DegMag105ToolStripMenuItem.set_Checked(false);
			x10DegMag85ToolStripMenuItem.set_Checked(false);
			x15DegMag70ToolStripMenuItem.set_Checked(false);
		}

		private void UnCheckSize()
		{
			x10mToolStripMenuItem.set_Checked(false);
			x12mToolStripMenuItem.set_Checked(false);
			x15mToolStripMenuItem.set_Checked(false);
			x18mToolStripMenuItem.set_Checked(false);
			x24mToolStripMenuItem.set_Checked(false);
			x30mToolStripMenuItem.set_Checked(false);
			x40mToolStripMenuItem.set_Checked(false);
			x50mToolStripMenuItem.set_Checked(false);
			x1DegToolStripMenuItem.set_Checked(false);
			x2DegToolStripMenuItem.set_Checked(false);
			x3DegToolStripMenuItem.set_Checked(false);
			x4DegToolStripMenuItem.set_Checked(false);
			x5DegToolStripMenuItem.set_Checked(false);
			x6DegToolStripMenuItem.set_Checked(false);
			x8DegToolStripMenuItem.set_Checked(false);
			x10DegToolStripMenuItem.set_Checked(false);
			x12DegToolStripMenuItem.set_Checked(false);
			x15DegToolStripMenuItem.set_Checked(false);
			x20DegToolStripMenuItem.set_Checked(false);
			UnCheckedStandard();
		}

		private void EnableSize()
		{
			((ToolStripItem)x15mToolStripMenuItem).set_Enabled(Use_NOMAD | Use_UCAC4 | Use_Gaia);
			((ToolStripItem)x30mToolStripMenuItem).set_Enabled(Use_NOMAD | Use_UCAC4 | Use_Gaia);
			((ToolStripItem)x40mToolStripMenuItem).set_Enabled(Use_NOMAD | Use_UCAC4 | Use_Gaia);
			((ToolStripItem)x1DegToolStripMenuItem).set_Enabled(Use_NOMAD | Use_UCAC4 | Use_Gaia);
			((ToolStripItem)x2DegToolStripMenuItem).set_Enabled(Use_NOMAD | Use_UCAC4 | Use_Gaia);
			((ToolStripItem)x3DegToolStripMenuItem).set_Enabled(Use_NOMAD | Use_UCAC4 | Use_Gaia);
			((ToolStripItem)x4DegToolStripMenuItem).set_Enabled(Use_Gaia);
			((ToolStripItem)x5DegToolStripMenuItem).set_Enabled(Use_Gaia);
			((ToolStripItem)x6DegToolStripMenuItem).set_Enabled(Use_Gaia);
			((ToolStripItem)x8DegToolStripMenuItem).set_Enabled(Use_Gaia);
			((ToolStripItem)x10DegToolStripMenuItem).set_Enabled(Use_Gaia);
			((ToolStripItem)x12DegToolStripMenuItem).set_Enabled(Use_Gaia);
			((ToolStripItem)x15DegToolStripMenuItem).set_Enabled(Use_Gaia);
			((ToolStripItem)x20DegToolStripMenuItem).set_Enabled(Use_Gaia);
		}

		private void uncheckMagnitudes()
		{
			toolStripMenuItemMag21.set_Checked(false);
			toolStripMenuItem_All.set_Checked(false);
			toolStripMenuItemMag20.set_Checked(false);
			toolStripMenuItemMag19.set_Checked(false);
			toolStripMenuItemMag18.set_Checked(false);
			toolStripMenuItemMag17.set_Checked(false);
			toolStripMenuItemMag16.set_Checked(false);
			toolStripMenuItemMag15.set_Checked(false);
			toolStripMenuItemMag14.set_Checked(false);
			toolStripMenuItemMag13.set_Checked(false);
			toolStripMenuItemMag12.set_Checked(false);
			toolStripMenuItemMag11.set_Checked(false);
			toolStripMenuItemMag10.set_Checked(false);
			toolStripMenuItemMag9.set_Checked(false);
			toolStripMenuItemMag8.set_Checked(false);
			toolStripMenuItemMag7.set_Checked(false);
			toolStripMenuItemMag6.set_Checked(false);
			UnCheckedStandard();
		}

		internal void SetMagnitudeLimit(int Index)
		{
			uncheckMagnitudes();
			switch (Index)
			{
			case 6:
				toolStripMenuItemMag6.set_Checked(true);
				break;
			case 7:
				toolStripMenuItemMag7.set_Checked(true);
				break;
			case 8:
				toolStripMenuItemMag8.set_Checked(true);
				break;
			case 9:
				toolStripMenuItemMag9.set_Checked(true);
				break;
			case 10:
				toolStripMenuItemMag10.set_Checked(true);
				break;
			case 11:
				toolStripMenuItemMag11.set_Checked(true);
				break;
			case 12:
				toolStripMenuItemMag12.set_Checked(true);
				break;
			case 13:
				toolStripMenuItemMag13.set_Checked(true);
				break;
			case 14:
				toolStripMenuItemMag14.set_Checked(true);
				break;
			case 15:
				toolStripMenuItemMag15.set_Checked(true);
				break;
			case 16:
				toolStripMenuItemMag16.set_Checked(true);
				break;
			case 17:
				toolStripMenuItemMag17.set_Checked(true);
				break;
			case 18:
				toolStripMenuItemMag18.set_Checked(true);
				break;
			case 19:
				toolStripMenuItemMag19.set_Checked(true);
				break;
			case 20:
				toolStripMenuItemMag20.set_Checked(true);
				break;
			case 21:
				toolStripMenuItemMag21.set_Checked(true);
				break;
			case 22:
				toolStripMenuItem_All.set_Checked(true);
				break;
			}
			PlotChart();
		}

		private void toolStripMenuItem_All_Click(object sender, EventArgs e)
		{
			MagLimit = 22;
			SetMagnitudeLimit(MagLimit);
		}

		private void allToolStripMenuItem1_Click(object sender, EventArgs e)
		{
			MagLimit = 21;
			SetMagnitudeLimit(MagLimit);
		}

		private void toolStripMenuItem16_Click(object sender, EventArgs e)
		{
			MagLimit = 20;
			SetMagnitudeLimit(MagLimit);
		}

		private void toolStripMenuItem15_Click(object sender, EventArgs e)
		{
			MagLimit = 19;
			SetMagnitudeLimit(MagLimit);
		}

		private void toolStripMenuItem14_Click(object sender, EventArgs e)
		{
			MagLimit = 18;
			SetMagnitudeLimit(MagLimit);
		}

		private void toolStripMenuItem13_Click(object sender, EventArgs e)
		{
			MagLimit = 17;
			SetMagnitudeLimit(MagLimit);
		}

		private void allToolStripMenuItem_Click(object sender, EventArgs e)
		{
			MagLimit = 16;
			SetMagnitudeLimit(MagLimit);
		}

		private void toolStripMenuItem12_Click(object sender, EventArgs e)
		{
			MagLimit = 15;
			SetMagnitudeLimit(MagLimit);
		}

		private void toolStripMenuItem2_Click(object sender, EventArgs e)
		{
			MagLimit = 14;
			SetMagnitudeLimit(MagLimit);
		}

		private void toolStripMenuItem3_Click(object sender, EventArgs e)
		{
			MagLimit = 13;
			SetMagnitudeLimit(MagLimit);
		}

		private void toolStripMenuItem4_Click(object sender, EventArgs e)
		{
			MagLimit = 12;
			SetMagnitudeLimit(MagLimit);
		}

		private void toolStripMenuItem5_Click(object sender, EventArgs e)
		{
			MagLimit = 11;
			SetMagnitudeLimit(MagLimit);
		}

		private void toolStripMenuItem6_Click(object sender, EventArgs e)
		{
			MagLimit = 10;
			SetMagnitudeLimit(MagLimit);
		}

		private void toolStripMenuItem7_Click(object sender, EventArgs e)
		{
			MagLimit = 9;
			SetMagnitudeLimit(MagLimit);
		}

		private void toolStripMenuItem8_Click(object sender, EventArgs e)
		{
			MagLimit = 8;
			SetMagnitudeLimit(MagLimit);
			toolStripMenuItemMag8.set_Checked(true);
		}

		private void toolStripMenuItem9_Click(object sender, EventArgs e)
		{
			MagLimit = 7;
			SetMagnitudeLimit(MagLimit);
		}

		private void toolStripMenuItem10_Click(object sender, EventArgs e)
		{
			MagLimit = 6;
			SetMagnitudeLimit(MagLimit);
		}

		private void x05DegMag160ToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SetSize_x50DegMag160();
		}

		internal void SetSize_x50DegMag160()
		{
			ChartHeightDegrees = 0.5f;
			MagLimit = 16;
			UnCheckSize();
			uncheckMagnitudes();
			x05DegMag160ToolStripMenuItem.set_Checked(true);
			toolStripMenuItemMag16.set_Checked(true);
			x30mToolStripMenuItem.set_Checked(true);
			PlotChart();
		}

		private void x1DegMag140ToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ChartHeightDegrees = 1f;
			MagLimit = 14;
			UnCheckSize();
			uncheckMagnitudes();
			x1DegMag140ToolStripMenuItem.set_Checked(true);
			toolStripMenuItemMag14.set_Checked(true);
			x1DegToolStripMenuItem.set_Checked(true);
			PlotChart();
		}

		private void x3DegMag120ToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ChartHeightDegrees = 3f;
			MagLimit = 12;
			UnCheckSize();
			uncheckMagnitudes();
			x3DegMag120ToolStripMenuItem.set_Checked(true);
			toolStripMenuItemMag12.set_Checked(true);
			x3DegToolStripMenuItem.set_Checked(true);
			PlotChart();
		}

		private void x5DegMag105ToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ChartHeightDegrees = 5f;
			MagLimit = 10;
			UnCheckSize();
			uncheckMagnitudes();
			x5DegMag105ToolStripMenuItem.set_Checked(true);
			toolStripMenuItemMag10.set_Checked(true);
			x5DegToolStripMenuItem.set_Checked(true);
			PlotChart();
		}

		private void x10DegMag85ToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ChartHeightDegrees = 10f;
			MagLimit = 8;
			UnCheckSize();
			uncheckMagnitudes();
			x10DegMag85ToolStripMenuItem.set_Checked(true);
			toolStripMenuItemMag8.set_Checked(true);
			x10DegToolStripMenuItem.set_Checked(true);
			PlotChart();
		}

		private void x15DegMag70ToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ChartHeightDegrees = 15f;
			MagLimit = 7;
			UnCheckSize();
			uncheckMagnitudes();
			x15DegMag70ToolStripMenuItem.set_Checked(true);
			toolStripMenuItemMag7.set_Checked(true);
			x15DegToolStripMenuItem.set_Checked(true);
			PlotChart();
		}

		internal void SetStarChartAngluarSize(int Index)
		{
			UnCheckSize();
			switch (Index)
			{
			case 0:
				ChartHeightDegrees = 1f / 6f;
				x10mToolStripMenuItem.set_Checked(true);
				break;
			case 1:
				ChartHeightDegrees = 0.2f;
				x12mToolStripMenuItem.set_Checked(true);
				break;
			case 2:
				ChartHeightDegrees = 0.25f;
				x15mToolStripMenuItem.set_Checked(true);
				break;
			case 3:
				ChartHeightDegrees = 0.3f;
				x18mToolStripMenuItem.set_Checked(true);
				break;
			case 4:
				ChartHeightDegrees = 0.4f;
				x24mToolStripMenuItem.set_Checked(true);
				break;
			case 5:
				ChartHeightDegrees = 0.5f;
				x30mToolStripMenuItem.set_Checked(true);
				break;
			case 6:
				ChartHeightDegrees = 2f / 3f;
				x40mToolStripMenuItem.set_Checked(true);
				break;
			case 7:
				ChartHeightDegrees = 5f / 6f;
				x50mToolStripMenuItem.set_Checked(true);
				break;
			case 8:
				ChartHeightDegrees = 1f;
				x1DegToolStripMenuItem.set_Checked(true);
				break;
			case 9:
				ChartHeightDegrees = 2f;
				x2DegToolStripMenuItem.set_Checked(true);
				break;
			case 10:
				ChartHeightDegrees = 3f;
				x3DegToolStripMenuItem.set_Checked(true);
				break;
			case 11:
				ChartHeightDegrees = 4f;
				x4DegToolStripMenuItem.set_Checked(true);
				break;
			case 12:
				ChartHeightDegrees = 5f;
				x5DegToolStripMenuItem.set_Checked(true);
				break;
			case 13:
				ChartHeightDegrees = 6f;
				x6DegToolStripMenuItem.set_Checked(true);
				break;
			case 14:
				ChartHeightDegrees = 8f;
				x8DegToolStripMenuItem.set_Checked(true);
				break;
			case 15:
				ChartHeightDegrees = 10f;
				x10DegToolStripMenuItem.set_Checked(true);
				break;
			case 16:
				ChartHeightDegrees = 12f;
				x12DegToolStripMenuItem.set_Checked(true);
				break;
			case 17:
				ChartHeightDegrees = 15f;
				x15DegToolStripMenuItem.set_Checked(true);
				break;
			case 18:
				ChartHeightDegrees = 20f;
				x20DegToolStripMenuItem.set_Checked(true);
				break;
			}
			PlotChart();
		}

		private void x10mToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ChartSizeIndex = 0;
			SetStarChartAngluarSize(ChartSizeIndex);
		}

		private void x12mToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ChartSizeIndex = 1;
			SetStarChartAngluarSize(ChartSizeIndex);
		}

		private void x15mToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ChartSizeIndex = 2;
			SetStarChartAngluarSize(ChartSizeIndex);
		}

		private void x18mToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ChartSizeIndex = 3;
			SetStarChartAngluarSize(ChartSizeIndex);
		}

		private void x24mToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ChartSizeIndex = 4;
			SetStarChartAngluarSize(ChartSizeIndex);
		}

		private void x30mToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ChartSizeIndex = 5;
			SetStarChartAngluarSize(ChartSizeIndex);
		}

		private void x40mToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ChartSizeIndex = 6;
			SetStarChartAngluarSize(ChartSizeIndex);
		}

		private void x50mToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ChartSizeIndex = 7;
			SetStarChartAngluarSize(ChartSizeIndex);
		}

		private void x1DegToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ChartSizeIndex = 8;
			SetStarChartAngluarSize(ChartSizeIndex);
		}

		private void x2DegToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ChartSizeIndex = 9;
			SetStarChartAngluarSize(ChartSizeIndex);
		}

		private void x3DegToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ChartSizeIndex = 10;
			SetStarChartAngluarSize(ChartSizeIndex);
		}

		private void toolStripMenuItem11_Click(object sender, EventArgs e)
		{
			ChartSizeIndex = 11;
			SetStarChartAngluarSize(ChartSizeIndex);
		}

		private void x5DegToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ChartSizeIndex = 12;
			SetStarChartAngluarSize(ChartSizeIndex);
		}

		private void x6DegToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ChartSizeIndex = 13;
			SetStarChartAngluarSize(ChartSizeIndex);
		}

		private void x8DegToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ChartSizeIndex = 14;
			SetStarChartAngluarSize(ChartSizeIndex);
		}

		private void x10DegToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ChartSizeIndex = 15;
			SetStarChartAngluarSize(ChartSizeIndex);
		}

		private void x12DegToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ChartSizeIndex = 16;
			SetStarChartAngluarSize(ChartSizeIndex);
		}

		private void x15DegToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ChartSizeIndex = 17;
			SetStarChartAngluarSize(ChartSizeIndex);
		}

		private void x20DegToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ChartSizeIndex = 18;
			SetStarChartAngluarSize(ChartSizeIndex);
		}

		private void visualToolStripMenuItem_Click(object sender, EventArgs e)
		{
			toolStripMenuItem_AllVisual.set_Checked(!toolStripMenuItem_AllVisual.get_Checked());
			VisualMag = toolStripMenuItem_AllVisual.get_Checked();
			PlotChart();
		}

		private void showCoordinatesInTooltipToolStripMenuItem_Click(object sender, EventArgs e)
		{
			showCoordinatesInTooltipToolStripMenuItem.set_Checked(!showCoordinatesInTooltipToolStripMenuItem.get_Checked());
			ShowToolTip = showCoordinatesInTooltipToolStripMenuItem.get_Checked();
		}

		private void flipHorizontallyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			flipHorizontallyToolStripMenuItem.set_Checked(!flipHorizontallyToolStripMenuItem.get_Checked());
			FlipHorizontal = flipHorizontallyToolStripMenuItem.get_Checked();
			PlotChart();
		}

		private void flipVerticallyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			flipVerticallyToolStripMenuItem.set_Checked(!flipVerticallyToolStripMenuItem.get_Checked());
			FlipVertical = flipVerticallyToolStripMenuItem.get_Checked();
			PlotChart();
		}

		private void blackOnWhiteToolStripMenuItem_Click(object sender, EventArgs e)
		{
			blackOnWhiteToolStripMenuItem.set_Checked(!blackOnWhiteToolStripMenuItem.get_Checked());
			PlotChart();
		}

		private void applyStoredToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ApplyStoredSettings();
		}

		private void ApplyStoredSettings()
		{
			showCoordinatesInTooltipToolStripMenuItem.set_Checked(ShowToolTip = Settings.Default.StarChartCoords);
			blackOnWhiteToolStripMenuItem.set_Checked(Settings.Default.StarChartBWFlag);
			bool starChartFlipHorizontal;
			flipHorizontallyToolStripMenuItem.set_Checked(starChartFlipHorizontal = Settings.Default.StarChartFlipHorizontal);
			FlipHorizontal = starChartFlipHorizontal;
			flipVerticallyToolStripMenuItem.set_Checked(starChartFlipHorizontal = Settings.Default.StarChartFlipVertical);
			FlipVertical = starChartFlipHorizontal;
			tBarRotate.set_Value(Settings.Default.StarChartRotate);
			toolStripMenuItem_AllVisual.set_Checked(Settings.Default.StarChartVisual);
			((Control)this).set_Width(Settings.Default.StarChartWidth);
			chkOnTop.set_Checked(starChartFlipHorizontal = Settings.Default.StarChartTopMost);
			((Form)this).set_TopMost(starChartFlipHorizontal);
			ChartSizeIndex = Settings.Default.StarChartSizeIndex;
			SetStarChartAngluarSize(ChartSizeIndex);
			MagLimit = Settings.Default.StarChartMagLimitIndex;
			SetMagnitudeLimit(MagLimit);
			StarCatalogIndex = Settings.Default.StarChartCatalogIndex;
			SetCatalogueChecks(StarCatalogIndex);
			if (Settings.Default.StarChartUseGaia16)
			{
				Settings.Default.StarChartUseGaia16 = false;
				Settings.Default.StarChartUseGaia = true;
			}
			if (Settings.Default.StarChartUseGaia14)
			{
				Settings.Default.StarChartUseGaia14 = false;
				Settings.Default.StarChartUseGaia = true;
			}
			GaiaToolStripMenuItem.set_Checked(Settings.Default.StarChartUseGaia);
			uCAC4ToolStripMenuItem.set_Checked(Settings.Default.StarChartUseUCAC4);
			pPMXLToolStripMenuItem.set_Checked(Settings.Default.StarChartUsePPMXL);
			nOMADToolStripMenuItem.set_Checked(Settings.Default.StarChartUseNomad);
			IsResizing = false;
			RotateChart();
		}

		private void storeSettingsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0017: Unknown result type (might be due to invalid IL or missing references)
			//IL_001d: Invalid comparison between Unknown and I4
			if ((int)MessageBox.Show("Do you want to save the current settings for:\r\nShowing coords, Black-on-white, Flip ↕ & ↔, Rotation,\r\nuse Visual mag, Form width, and Form-on-top status", "Confirm save", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 7)
			{
				Settings.Default.StarChartCoords = showCoordinatesInTooltipToolStripMenuItem.get_Checked();
				Settings.Default.StarChartBWFlag = blackOnWhiteToolStripMenuItem.get_Checked();
				Settings.Default.StarChartFlipHorizontal = flipHorizontallyToolStripMenuItem.get_Checked();
				Settings.Default.StarChartFlipVertical = flipVerticallyToolStripMenuItem.get_Checked();
				Settings.Default.StarChartRotate = tBarRotate.get_Value();
				Settings.Default.StarChartVisual = toolStripMenuItem_AllVisual.get_Checked();
				Settings.Default.StarChartWidth = ((Control)this).get_Width();
				Settings.Default.StarChartTopMost = chkOnTop.get_Checked();
				Settings.Default.StarChartMagLimitIndex = MagLimit;
				Settings.Default.StarChartSizeIndex = ChartSizeIndex;
				Settings.Default.StarChartCatalogIndex = StarCatalogIndex;
				Settings.Default.StarChartUseGaia = GaiaToolStripMenuItem.get_Checked();
				Settings.Default.StarChartUseUCAC4 = uCAC4ToolStripMenuItem.get_Checked();
				Settings.Default.StarChartUsePPMXL = pPMXLToolStripMenuItem.get_Checked();
				Settings.Default.StarChartUseNomad = nOMADToolStripMenuItem.get_Checked();
				((SettingsBase)Settings.Default).Save();
			}
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
		}

		private void picStars_MouseMove(object sender, MouseEventArgs e)
		{
			if (IsResizing)
			{
				return;
			}
			if (ShowToolTip)
			{
				double num = (double)((ChartWidth / 2f - (float)e.get_X()) / ChartSideLength * ChartHeightDegrees) / (180.0 / Math.PI);
				if (FlipHorizontal)
				{
					num = 0.0 - num;
				}
				double num2 = (double)((ChartHeight / 2f - (float)e.get_Y()) / ChartSideLength * ChartHeightDegrees) / (180.0 / Math.PI);
				if (FlipVertical)
				{
					num2 = 0.0 - num2;
				}
				Utilities.RotateXY(num, num2, 0.0 - RotateDeg, out var x, out var y);
				double num3 = 1.0 - y * Math.Tan(CentreDec);
				double num4 = Math.Atan(x / num3 / Math.Cos(CentreDec));
				double num5 = num4 + CentreRA;
				if (num5 < 0.0)
				{
					num5 += Math.PI * 2.0;
				}
				double num6 = Math.Atan(Math.Cos(num4) * (y + Math.Tan(CentreDec)) / num3);
				GetToolTip = !GetToolTip;
				if (GetToolTip)
				{
					if (ChartHeightDegrees > 1f)
					{
						toolTip1.SetToolTip((Control)(object)picStars, Utilities.Constellation(num5, num6, AbbreviatedName: true) + ": " + Utilities.DEGtoDMS(num5 * (180.0 / Math.PI) / 15.0, 2, 0, MinutesOnly: false) + ", " + Utilities.DEGtoDMS(num6 * (180.0 / Math.PI), 3, 1, MinutesOnly: true));
					}
					else
					{
						toolTip1.SetToolTip((Control)(object)picStars, Utilities.Constellation(num5, num6, AbbreviatedName: true) + ": " + Utilities.DEGtoDMS(num5 * (180.0 / Math.PI) / 15.0, 2, 1, MinutesOnly: false) + ", " + Utilities.DEGtoDMS(num6 * (180.0 / Math.PI), 3, 0, MinutesOnly: false));
					}
				}
			}
			else
			{
				toolTip1.Hide((IWin32Window)(object)picStars);
			}
		}

		private void copyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetImage(picStars.get_Image());
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (ShowObjectPath)
			{
				Settings.Default.Save_Stars = Output.SaveGraphic(picStars.get_Image(), DisplayMPOccultations.UTDate + DisplayMPOccultations.AsteroidName.Trim() + " Finder chart", Settings.Default.Save_Stars);
			}
			else
			{
				Settings.Default.Save_AsteroidPredictions = Output.SaveGraphic(picStars.get_Image(), "Finder chart at RA " + Utilities.DEGtoDMS(CentreRA * (180.0 / Math.PI) / 15.0, 2, 0, MinutesOnly: false).Trim() + " Dec " + Utilities.DEGtoDMS(CentreDec * (180.0 / Math.PI), 3, 0, MinutesOnly: false).Trim(), Settings.Default.Save_AsteroidPredictions);
			}
		}

		private void StarChart_FormClosed(object sender, FormClosedEventArgs e)
		{
			((Component)this).Dispose();
		}

		private void cmdPlot_Click(object sender, EventArgs e)
		{
			GetChartCenter();
			PlotChart();
		}

		private void GetChartCenter()
		{
			if (!double.TryParse(((Control)txtRH1).get_Text(), out var result))
			{
				result = 0.0;
			}
			if (result > 23.0 || result < 0.0)
			{
				result = 0.0;
			}
			if (!double.TryParse(((Control)txtRM1).get_Text(), out var result2))
			{
				result2 = 0.0;
			}
			if (result2 >= 60.0 || result2 < 0.0)
			{
				result2 = 0.0;
			}
			if (!double.TryParse(((Control)txtDD1).get_Text(), out var result3))
			{
				result3 = 0.0;
			}
			if (Math.Abs(result3) > 89.0)
			{
				result3 = 89.0;
			}
			if (!double.TryParse(((Control)txtDM1).get_Text(), out var result4))
			{
				result4 = 0.0;
			}
			if (result4 >= 60.0 || result4 < 0.0)
			{
				result4 = 0.0;
			}
			double num = 15.0 * (Math.Abs(result) + result2 / 60.0) / (180.0 / Math.PI);
			double num2 = (Math.Abs(result3) + result4 / 60.0) / (180.0 / Math.PI);
			if (((Control)txtRH1).get_Text().Contains("-"))
			{
				num = 0.0 - num;
			}
			if (num2 > 75.0)
			{
				num2 = 75.0;
			}
			if (((Control)txtDD1).get_Text().Contains("-"))
			{
				num2 = 0.0 - num2;
			}
			CentreRA = num;
			CentreDec = num2;
		}

		private void tBarRotate_MouseUp(object sender, MouseEventArgs e)
		{
			RotateChart();
		}

		private void RotateChart()
		{
			RotateDeg = (double)(-tBarRotate.get_Value()) / 5.0;
			((Control)lblAngle).set_Text($"{RotateDeg:+#.0;-#.0;0.0}°");
			((Control)tBarRotate).set_Enabled(false);
			((Control)lblRedraw).set_Visible(true);
			Application.DoEvents();
			PlotChart();
			((Control)lblRedraw).set_Visible(false);
			((Control)tBarRotate).set_Enabled(true);
		}

		private void tBarRotate_MouseMove(object sender, MouseEventArgs e)
		{
			toolTip1.SetToolTip((Control)(object)tBarRotate, $"{(double)(-tBarRotate.get_Value()) / 5.0:+#.0;-#.0;0.0}°");
		}

		private void cmdPlusHalf_MouseClick(object sender, MouseEventArgs e)
		{
			if (tBarRotate.get_Value() != tBarRotate.get_Minimum())
			{
				TrackBar obj = tBarRotate;
				obj.set_Value(obj.get_Value() - 1);
				RotateChart();
			}
		}

		private void cmdMinusHalf_Click(object sender, EventArgs e)
		{
			if (tBarRotate.get_Value() != tBarRotate.get_Maximum())
			{
				TrackBar obj = tBarRotate;
				obj.set_Value(obj.get_Value() + 1);
				RotateChart();
			}
		}

		private void trackOpacity_ValueChanged(object sender, EventArgs e)
		{
			((Form)this).set_Opacity((double)trackOpacity.get_Value() / 100.0);
			if (trackOpacity.get_Value() > 99)
			{
				Color lightSeaGreen;
				((ToolStrip)menuStrip1).set_BackColor(lightSeaGreen = Color.LightSeaGreen);
				((Control)this).set_BackColor(lightSeaGreen);
				TrackBar obj = trackOpacity;
				((Control)tBarRotate).set_BackColor(lightSeaGreen = Color.LightYellow);
				((Control)obj).set_BackColor(lightSeaGreen);
			}
			else
			{
				MenuStrip obj2 = menuStrip1;
				TrackBar obj3 = trackOpacity;
				Color color;
				((Control)tBarRotate).set_BackColor(color = Color.FromArgb(255, 255, 150 + trackOpacity.get_Value(), 0));
				Color color2;
				((Control)obj3).set_BackColor(color2 = color);
				Color lightSeaGreen;
				((ToolStrip)obj2).set_BackColor(lightSeaGreen = color2);
				((Control)this).set_BackColor(lightSeaGreen);
				TrackBar obj4 = trackOpacity;
				((Control)tBarRotate).set_BackColor(lightSeaGreen = Color.LightSeaGreen);
				((Control)obj4).set_BackColor(lightSeaGreen);
			}
		}

		private void displayInGoogleSkyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			GoogleSkyPlot();
		}

		private void setStarCatalogPathsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0005: Unknown result type (might be due to invalid IL or missing references)
			((Form)new Defaults()).ShowDialog();
		}

		private void GoogleSky_Click(object sender, EventArgs e)
		{
			GoogleSkyPlot();
		}

		private void chkOnTop_Click(object sender, EventArgs e)
		{
			chkOnTop.set_Checked(!chkOnTop.get_Checked());
			((Form)this).set_TopMost(chkOnTop.get_Checked());
		}

		private void picStars_MouseDoubleClick(object sender, MouseEventArgs e)
		{
			if (Settings.Default.StarMapDoubleClick)
			{
				_ = 3600f * ChartHeightDegrees / ChartSideLength;
				double x = (double)((ChartWidth / 2f - (float)e.get_X()) / ChartSideLength * ChartHeightDegrees) / (180.0 / Math.PI);
				double y = (double)((ChartHeight / 2f - (float)e.get_Y()) / ChartSideLength * ChartHeightDegrees) / (180.0 / Math.PI);
				Utilities.RotateXY(x, y, 0.0 - RotateDeg, out var x2, out var y2);
				if (FlipHorizontal)
				{
					x2 = 0.0 - x2;
				}
				if (FlipVertical)
				{
					y2 = 0.0 - y2;
				}
				double num = 1.0 - y2 * Math.Tan(CentreDec);
				double num2 = Math.Atan(x2 / num / Math.Cos(CentreDec));
				double num3 = num2 + CentreRA;
				if (num3 < 0.0)
				{
					num3 += Math.PI * 2.0;
				}
				double num4 = Math.Atan(Math.Cos(num2) * (y2 + Math.Tan(CentreDec)) / num);
				string text = Utilities.DEGtoDMS(num3 * (180.0 / Math.PI) / 15.0, 2, 2, MinutesOnly: true);
				((Control)txtRH1).set_Text(text.Substring(0, 2).Trim());
				((Control)txtRM1).set_Text(text.Substring(3).Trim());
				string text2 = Utilities.DEGtoDMS(num4 * (180.0 / Math.PI), 3, 1, MinutesOnly: true);
				((Control)txtDD1).set_Text(text2.Substring(0, 3).Trim());
				((Control)txtDM1).set_Text(text2.Substring(4).Trim());
				GetChartCenter();
				PlotChart();
			}
		}

		private void recenterToolStripMenuItem_Click(object sender, EventArgs e)
		{
			recenterToolStripMenuItem.set_Checked(!recenterToolStripMenuItem.get_Checked());
			Settings.Default.StarMapDoubleClick = recenterToolStripMenuItem.get_Checked();
		}

		private void comparisonStarDetailsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			comparisonStarDetailsToolStripMenuItem.set_Checked(!comparisonStarDetailsToolStripMenuItem.get_Checked());
			((Control)pnlCompStars).set_Visible(comparisonStarDetailsToolStripMenuItem.get_Checked());
			((Control)pnlCompStars).set_Left(((Control)picStars).get_Left() + 100);
			((Control)pnlCompStars).set_Top(((Control)picStars).get_Top() + 200);
			PlotChart();
		}

		private void pnlCompStars_MouseDown(object sender, MouseEventArgs e)
		{
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_000b: Invalid comparison between Unknown and I4
			if ((int)e.get_Button() == 1048576)
			{
				xPos_Stars = e.get_X();
				yPos_Stars = e.get_Y();
			}
		}

		private void pnlCompStars_MouseHover(object sender, EventArgs e)
		{
			Cursor.set_Current(Cursors.get_SizeAll());
		}

		private void pnlCompStars_MouseMove(object sender, MouseEventArgs e)
		{
			//IL_000e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0018: Invalid comparison between Unknown and I4
			Panel val = (Panel)((sender is Panel) ? sender : null);
			if (val != null && (int)e.get_Button() == 1048576)
			{
				((Control)val).set_Top(((Control)val).get_Top() + (e.get_Y() - yPos_Stars));
				if (((Control)val).get_Bottom() < 8)
				{
					((Control)val).set_Top(8 - ((Control)val).get_Height());
				}
				if (((Control)val).get_Top() > ((Control)this).get_Height() - 46)
				{
					((Control)val).set_Top(((Control)this).get_Height() - 46);
				}
				((Control)val).set_Left(((Control)val).get_Left() + (e.get_X() - xPos_Stars));
				if (((Control)val).get_Right() < 8)
				{
					((Control)val).set_Left(8 - ((Control)val).get_Width());
				}
				if (((Control)val).get_Left() > ((Control)this).get_Width() - 25)
				{
					((Control)val).set_Left(((Control)this).get_Width() - 25);
				}
			}
		}

		private void cmdAddCurrent_Click(object sender, EventArgs e)
		{
			if (((Control)lblStarID).get_Text().Length > 10)
			{
				CompStars compStars = new CompStars();
				compStars.Entry = ((Control)lblStarID).get_Text().PadLeft(89);
				CStars.Add(compStars);
				PopulateListStars();
			}
		}

		private void cmdClear_Click(object sender, EventArgs e)
		{
			CStars.Clear();
			PopulateListStars();
		}

		private void button1_Click(object sender, EventArgs e)
		{
			if (((ListControl)lstStars).get_SelectedIndex() > 0)
			{
				CStars.RemoveAt(((ListControl)lstStars).get_SelectedIndex());
				PopulateListStars();
			}
		}

		private void PopulateListStars()
		{
			lstStars.get_Items().Clear();
			CStars.Sort();
			for (int i = 0; i < CStars.Count; i++)
			{
				lstStars.get_Items().Add((object)CStars[i].Entry);
			}
			PlotChart();
		}

		private void cmdCopy_Click(object sender, EventArgs e)
		{
			if (lstStars.get_Items().get_Count() > 0)
			{
				string text = "";
				for (int i = 0; i < lstStars.get_Items().get_Count(); i++)
				{
					text = text + lstStars.get_Items().get_Item(i).ToString() + "\r\n";
				}
				Clipboard.SetText(text);
			}
		}

		private void lstStars_SelectedIndexChanged(object sender, EventArgs e)
		{
			if (((ListControl)lstStars).get_SelectedIndex() >= 0)
			{
				((Control)lblStarID).set_Text(lstStars.get_Items().get_Item(((ListControl)lstStars).get_SelectedIndex()).ToString());
			}
			if (((Control)lblStarID).get_Text().Contains("UCAC"))
			{
				((Control)lblStarID).set_BackColor(Color.Yellow);
			}
			else if (((Control)lblStarID).get_Text().Contains("TYC"))
			{
				((Control)lblStarID).set_BackColor(Color.LightGreen);
			}
			else if (((Control)lblStarID).get_Text().Contains("HIP"))
			{
				((Control)lblStarID).set_BackColor(Color.Cyan);
			}
			else
			{
				((Control)lblStarID).set_BackColor(Color.LightSalmon);
			}
			PlotChart();
		}

		private void pnlCompStars_MouseEnter(object sender, EventArgs e)
		{
			((Control)pnlCompStars).Focus();
		}

		private void GoogleSkyPlot()
		{
			int width_in_arcMins = (int)(ChartHeightDegrees * 60f);
			if (ChartHeightDegrees > 2f)
			{
				width_in_arcMins = 120;
			}
			if (ShowInputFields)
			{
				GetChartCenter();
			}
			GoogleEarth.CreateGoogleSkyKMLFile(CentreRA * (180.0 / Math.PI), CentreDec * (180.0 / Math.PI), width_in_arcMins, AppPath + "\\AutoGenerated Asteroids\\ViewGoogleSky.KML");
		}

		private void picStars_MouseUp(object sender, MouseEventArgs e)
		{
			//IL_000c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0016: Invalid comparison between Unknown and I4
			double num = 1.0;
			if ((int)e.get_Button() != 1048576)
			{
				return;
			}
			num = 3600f * ChartHeightDegrees / ChartSideLength;
			double x = (double)((ChartWidth / 2f - (float)e.get_X()) / ChartSideLength * ChartHeightDegrees) / (180.0 / Math.PI);
			double y = (double)((ChartHeight / 2f - (float)e.get_Y()) / ChartSideLength * ChartHeightDegrees) / (180.0 / Math.PI);
			Utilities.RotateXY(x, y, 0.0 - RotateDeg, out var x2, out var y2);
			if (FlipHorizontal)
			{
				x2 = 0.0 - x2;
			}
			if (FlipVertical)
			{
				y2 = 0.0 - y2;
			}
			double num2 = 1.0 - y2 * Math.Tan(CentreDec);
			double num3 = Math.Atan(x2 / num2 / Math.Cos(CentreDec));
			double num4 = num3 + CentreRA;
			if (num4 < 0.0)
			{
				num4 += Math.PI * 2.0;
			}
			double num5 = Math.Atan(Math.Cos(num3) * (y2 + Math.Tan(CentreDec)) / num2);
			string text = Utilities.DEGtoDMS(num4 * (180.0 / Math.PI) / 15.0, 2, 1, MinutesOnly: false, IncludeLeadingZeros: true, IncludePlusSymbol: false, IncludeDMS: false, IncludeHMS: false, IncludeHourDegLeadingZero: true).Replace(" ", "");
			string text2 = Utilities.DEGtoDMS(num5 * (180.0 / Math.PI), 3, 0, MinutesOnly: false, IncludeLeadingZeros: true, IncludePlusSymbol: true, IncludeDMS: false, IncludeHMS: false, IncludeHourDegLeadingZero: true).Replace(" ", "");
			if (GetStarPosition.GetGaiaPosition(text + text2, 15.5, 1.5 * num, MagLimit, FilterUsingStarMag: false, LimitUsingStarMag: true, out var RA, out var Dec, out var _, out var _, out var MagV, out var MagB, out var MagR, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var StarNumber, out var _))
			{
				string text3 = "";
				if (MagV < 6.0)
				{
					text3 = Utilities.StarIdentifier_ToMag6(num4 * (180.0 / Math.PI), num5 * (180.0 / Math.PI), MagV, WithConstellation: true);
				}
				if (text3.Length > 0)
				{
					text3 = " = " + text3;
				}
				((Control)lblStarID).set_Text(StarNumber + text3 + string.Format(" :  mV={0,4:f1}, mB={1,4:f1}, mR={2,4:f1} :  RA ", MagV, MagB, MagR) + Utilities.DEGtoDMS(RA * (180.0 / Math.PI) / 15.0, 2, 2, MinutesOnly: false, IncludeLeadingZeros: false, IncludePlusSymbol: false, IncludeDMS: false, IncludeHMS: true) + ", Dec " + Utilities.DEGtoDMS(Dec * (180.0 / Math.PI), 3, 1, MinutesOnly: false, IncludeLeadingZeros: false, IncludePlusSymbol: true, IncludeDMS: true, IncludeHMS: false));
				if (StarNumber.Contains("UCAC"))
				{
					((Control)lblStarID).set_BackColor(Color.Yellow);
				}
				else if (StarNumber.Contains("TYC"))
				{
					((Control)lblStarID).set_BackColor(Color.LightGreen);
				}
				else if (StarNumber.Contains("HIP"))
				{
					((Control)lblStarID).set_BackColor(Color.Cyan);
				}
				else
				{
					((Control)lblStarID).set_BackColor(Color.LightSalmon);
				}
			}
			else
			{
				((Control)lblStarID).set_Text("x");
				((Control)lblStarID).set_BackColor(Color.Red);
			}
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Star Chart");
		}

		private void txtRH1_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtRH1).SelectAll();
		}

		private void txtRM1_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtRM1).SelectAll();
		}

		private void txtDD1_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtDD1).SelectAll();
		}

		private void txtDM1_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtDM1).SelectAll();
		}

		private void tychoGaiaToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SetCatalogueChecks(1);
			PlotChart();
		}

		private void GaiaToolStripMenuItem_Click(object sender, EventArgs e)
		{
			StarCatalogIndex = 3;
			SetCatalogueChecks(StarCatalogIndex);
			PlotChart();
		}

		private void uCAC4ToolStripMenuItem_Click(object sender, EventArgs e)
		{
			StarCatalogIndex = 4;
			SetCatalogueChecks(StarCatalogIndex);
			PlotChart();
		}

		private void pPMXLToolStripMenuItem_Click(object sender, EventArgs e)
		{
			StarCatalogIndex = 5;
			SetCatalogueChecks(StarCatalogIndex);
			PlotChart();
		}

		private void nOMADToolStripMenuItem_Click(object sender, EventArgs e)
		{
			StarCatalogIndex = 6;
			SetCatalogueChecks(StarCatalogIndex);
			PlotChart();
		}

		private void SetCatalogueChecks(int CatNum)
		{
			bool use_Gaia;
			GaiaToolStripMenuItem.set_Checked(use_Gaia = CatNum < 4);
			Use_Gaia = use_Gaia;
			uCAC4ToolStripMenuItem.set_Checked(use_Gaia = CatNum == 4);
			Use_UCAC4 = use_Gaia;
			pPMXLToolStripMenuItem.set_Checked(use_Gaia = CatNum == 5);
			Use_PPMXL = use_Gaia;
			nOMADToolStripMenuItem.set_Checked(use_Gaia = CatNum == 6);
			Use_NOMAD = use_Gaia;
			Settings.Default.StarChartUseGaia = CatNum < 4;
			Settings.Default.StarChartUseUCAC4 = CatNum == 4;
			Settings.Default.StarChartUsePPMXL = CatNum == 5;
			Settings.Default.StarChartUseNomad = CatNum == 6;
			ToolStripMenuItem obj = toolStripMenuItemMag6;
			ToolStripMenuItem obj2 = toolStripMenuItemMag7;
			ToolStripMenuItem obj3 = toolStripMenuItemMag8;
			ToolStripMenuItem obj4 = toolStripMenuItemMag9;
			ToolStripMenuItem obj5 = toolStripMenuItemMag10;
			ToolStripMenuItem obj6 = toolStripMenuItemMag11;
			ToolStripMenuItem obj7 = toolStripMenuItemMag12;
			ToolStripMenuItem obj8 = toolStripMenuItemMag13;
			ToolStripMenuItem obj9 = toolStripMenuItemMag14;
			ToolStripMenuItem obj10 = toolStripMenuItemMag15;
			ToolStripMenuItem obj11 = toolStripMenuItemMag16;
			ToolStripMenuItem obj12 = toolStripMenuItemMag17;
			ToolStripMenuItem obj13 = toolStripMenuItemMag18;
			ToolStripMenuItem obj14 = toolStripMenuItemMag19;
			ToolStripMenuItem obj15 = toolStripMenuItemMag20;
			Color control;
			((ToolStripItem)toolStripMenuItemMag21).set_BackColor(control = SystemColors.Control);
			Color color;
			((ToolStripItem)obj15).set_BackColor(color = control);
			Color color2;
			((ToolStripItem)obj14).set_BackColor(color2 = color);
			Color color3;
			((ToolStripItem)obj13).set_BackColor(color3 = color2);
			Color color4;
			((ToolStripItem)obj12).set_BackColor(color4 = color3);
			Color color5;
			((ToolStripItem)obj11).set_BackColor(color5 = color4);
			Color color6;
			((ToolStripItem)obj10).set_BackColor(color6 = color5);
			Color color7;
			((ToolStripItem)obj9).set_BackColor(color7 = color6);
			Color color8;
			((ToolStripItem)obj8).set_BackColor(color8 = color7);
			Color color9;
			((ToolStripItem)obj7).set_BackColor(color9 = color8);
			Color color10;
			((ToolStripItem)obj6).set_BackColor(color10 = color9);
			Color color11;
			((ToolStripItem)obj5).set_BackColor(color11 = color10);
			Color color12;
			((ToolStripItem)obj4).set_BackColor(color12 = color11);
			Color color13;
			((ToolStripItem)obj3).set_BackColor(color13 = color12);
			Color backColor;
			((ToolStripItem)obj2).set_BackColor(backColor = color13);
			((ToolStripItem)obj).set_BackColor(backColor);
			if (CatNum < 4)
			{
				ToolStripMenuItem obj16 = toolStripMenuItemMag18;
				ToolStripMenuItem obj17 = toolStripMenuItemMag19;
				ToolStripMenuItem obj18 = toolStripMenuItemMag20;
				((ToolStripItem)toolStripMenuItemMag21).set_BackColor(color12 = Color.LightBlue);
				((ToolStripItem)obj18).set_BackColor(color13 = color12);
				((ToolStripItem)obj17).set_BackColor(backColor = color13);
				((ToolStripItem)obj16).set_BackColor(backColor);
				return;
			}
			switch (CatNum)
			{
			case 4:
			{
				ToolStripMenuItem obj24 = toolStripMenuItemMag19;
				ToolStripMenuItem obj25 = toolStripMenuItemMag20;
				((ToolStripItem)toolStripMenuItemMag21).set_BackColor(color13 = Color.LightBlue);
				((ToolStripItem)obj25).set_BackColor(backColor = color13);
				((ToolStripItem)obj24).set_BackColor(backColor);
				break;
			}
			case 5:
			{
				ToolStripMenuItem obj20 = toolStripMenuItemMag17;
				ToolStripMenuItem obj21 = toolStripMenuItemMag18;
				ToolStripMenuItem obj22 = toolStripMenuItemMag19;
				ToolStripMenuItem obj23 = toolStripMenuItemMag20;
				((ToolStripItem)toolStripMenuItemMag21).set_BackColor(color11 = Color.LightBlue);
				((ToolStripItem)obj23).set_BackColor(color12 = color11);
				((ToolStripItem)obj22).set_BackColor(color13 = color12);
				((ToolStripItem)obj21).set_BackColor(backColor = color13);
				((ToolStripItem)obj20).set_BackColor(backColor);
				break;
			}
			case 6:
			{
				ToolStripMenuItem obj19 = toolStripMenuItemMag20;
				((ToolStripItem)toolStripMenuItemMag21).set_BackColor(backColor = Color.LightBlue);
				((ToolStripItem)obj19).set_BackColor(backColor);
				break;
			}
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
			//IL_0345: Unknown result type (might be due to invalid IL or missing references)
			//IL_034f: Expected O, but got Unknown
			//IL_0350: Unknown result type (might be due to invalid IL or missing references)
			//IL_035a: Expected O, but got Unknown
			//IL_035b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0365: Expected O, but got Unknown
			//IL_0366: Unknown result type (might be due to invalid IL or missing references)
			//IL_0370: Expected O, but got Unknown
			//IL_0371: Unknown result type (might be due to invalid IL or missing references)
			//IL_037b: Expected O, but got Unknown
			//IL_037c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0386: Expected O, but got Unknown
			//IL_0387: Unknown result type (might be due to invalid IL or missing references)
			//IL_0391: Expected O, but got Unknown
			//IL_0392: Unknown result type (might be due to invalid IL or missing references)
			//IL_039c: Expected O, but got Unknown
			//IL_039d: Unknown result type (might be due to invalid IL or missing references)
			//IL_03a7: Expected O, but got Unknown
			//IL_03a8: Unknown result type (might be due to invalid IL or missing references)
			//IL_03b2: Expected O, but got Unknown
			//IL_03b3: Unknown result type (might be due to invalid IL or missing references)
			//IL_03bd: Expected O, but got Unknown
			//IL_03be: Unknown result type (might be due to invalid IL or missing references)
			//IL_03c8: Expected O, but got Unknown
			//IL_03c9: Unknown result type (might be due to invalid IL or missing references)
			//IL_03d3: Expected O, but got Unknown
			//IL_03d4: Unknown result type (might be due to invalid IL or missing references)
			//IL_03de: Expected O, but got Unknown
			//IL_03df: Unknown result type (might be due to invalid IL or missing references)
			//IL_03e9: Expected O, but got Unknown
			//IL_03ea: Unknown result type (might be due to invalid IL or missing references)
			//IL_03f4: Expected O, but got Unknown
			//IL_03f5: Unknown result type (might be due to invalid IL or missing references)
			//IL_03ff: Expected O, but got Unknown
			//IL_0400: Unknown result type (might be due to invalid IL or missing references)
			//IL_040a: Expected O, but got Unknown
			//IL_040b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0415: Expected O, but got Unknown
			//IL_0416: Unknown result type (might be due to invalid IL or missing references)
			//IL_0420: Expected O, but got Unknown
			//IL_0421: Unknown result type (might be due to invalid IL or missing references)
			//IL_042b: Expected O, but got Unknown
			//IL_042c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0436: Expected O, but got Unknown
			//IL_0437: Unknown result type (might be due to invalid IL or missing references)
			//IL_0441: Expected O, but got Unknown
			//IL_0442: Unknown result type (might be due to invalid IL or missing references)
			//IL_044c: Expected O, but got Unknown
			//IL_044d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0457: Expected O, but got Unknown
			//IL_0458: Unknown result type (might be due to invalid IL or missing references)
			//IL_0462: Expected O, but got Unknown
			//IL_0463: Unknown result type (might be due to invalid IL or missing references)
			//IL_046d: Expected O, but got Unknown
			//IL_046e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0478: Expected O, but got Unknown
			//IL_0479: Unknown result type (might be due to invalid IL or missing references)
			//IL_0483: Expected O, but got Unknown
			//IL_0484: Unknown result type (might be due to invalid IL or missing references)
			//IL_048e: Expected O, but got Unknown
			//IL_048f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0499: Expected O, but got Unknown
			//IL_049a: Unknown result type (might be due to invalid IL or missing references)
			//IL_04a4: Expected O, but got Unknown
			//IL_04a5: Unknown result type (might be due to invalid IL or missing references)
			//IL_04af: Expected O, but got Unknown
			//IL_04b0: Unknown result type (might be due to invalid IL or missing references)
			//IL_04ba: Expected O, but got Unknown
			//IL_04bb: Unknown result type (might be due to invalid IL or missing references)
			//IL_04c5: Expected O, but got Unknown
			//IL_04c6: Unknown result type (might be due to invalid IL or missing references)
			//IL_04d0: Expected O, but got Unknown
			//IL_04d1: Unknown result type (might be due to invalid IL or missing references)
			//IL_04db: Expected O, but got Unknown
			//IL_04dc: Unknown result type (might be due to invalid IL or missing references)
			//IL_04e6: Expected O, but got Unknown
			//IL_04e7: Unknown result type (might be due to invalid IL or missing references)
			//IL_04f1: Expected O, but got Unknown
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
			//IL_0534: Unknown result type (might be due to invalid IL or missing references)
			//IL_053e: Expected O, but got Unknown
			//IL_053f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0549: Expected O, but got Unknown
			//IL_054a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0554: Expected O, but got Unknown
			//IL_2836: Unknown result type (might be due to invalid IL or missing references)
			//IL_2840: Expected O, but got Unknown
			//IL_284d: Unknown result type (might be due to invalid IL or missing references)
			//IL_2857: Expected O, but got Unknown
			//IL_2864: Unknown result type (might be due to invalid IL or missing references)
			//IL_286e: Expected O, but got Unknown
			//IL_2916: Unknown result type (might be due to invalid IL or missing references)
			//IL_2920: Expected O, but got Unknown
			//IL_292d: Unknown result type (might be due to invalid IL or missing references)
			//IL_2937: Expected O, but got Unknown
			//IL_3058: Unknown result type (might be due to invalid IL or missing references)
			//IL_3062: Expected O, but got Unknown
			//IL_35a3: Unknown result type (might be due to invalid IL or missing references)
			//IL_35ad: Expected O, but got Unknown
			//IL_35e8: Unknown result type (might be due to invalid IL or missing references)
			//IL_35f2: Expected O, but got Unknown
			//IL_3af3: Unknown result type (might be due to invalid IL or missing references)
			//IL_3afd: Expected O, but got Unknown
			//IL_3b65: Unknown result type (might be due to invalid IL or missing references)
			//IL_3b6f: Expected O, but got Unknown
			components = new Container();
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(StarChart));
			menuStrip1 = new MenuStrip();
			withPlotToolStripMenuItem = new ToolStripMenuItem();
			showCoordinatesInTooltipToolStripMenuItem = new ToolStripMenuItem();
			flipHorizontallyToolStripMenuItem = new ToolStripMenuItem();
			flipVerticallyToolStripMenuItem = new ToolStripMenuItem();
			recenterToolStripMenuItem = new ToolStripMenuItem();
			blackOnWhiteToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator2 = new ToolStripSeparator();
			applyStoredToolStripMenuItem = new ToolStripMenuItem();
			storeSettingsToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator5 = new ToolStripSeparator();
			toolStripSeparator3 = new ToolStripSeparator();
			displayInGoogleSkyToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator1 = new ToolStripSeparator();
			copyToolStripMenuItem = new ToolStripMenuItem();
			printPreviewToolStripMenuItem = new ToolStripMenuItem();
			printToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			catalogueToolStripMenuItem = new ToolStripMenuItem();
			GaiaToolStripMenuItem = new ToolStripMenuItem();
			uCAC4ToolStripMenuItem = new ToolStripMenuItem();
			pPMXLToolStripMenuItem = new ToolStripMenuItem();
			nOMADToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator4 = new ToolStripSeparator();
			setStarCatalogPathsToolStripMenuItem = new ToolStripMenuItem();
			standardChartsToolStripMenuItem = new ToolStripMenuItem();
			x05DegMag160ToolStripMenuItem = new ToolStripMenuItem();
			x1DegMag140ToolStripMenuItem = new ToolStripMenuItem();
			x3DegMag120ToolStripMenuItem = new ToolStripMenuItem();
			x5DegMag105ToolStripMenuItem = new ToolStripMenuItem();
			x10DegMag85ToolStripMenuItem = new ToolStripMenuItem();
			x15DegMag70ToolStripMenuItem = new ToolStripMenuItem();
			sizeToolStripMenuItem = new ToolStripMenuItem();
			x10mToolStripMenuItem = new ToolStripMenuItem();
			x12mToolStripMenuItem = new ToolStripMenuItem();
			x15mToolStripMenuItem = new ToolStripMenuItem();
			x18mToolStripMenuItem = new ToolStripMenuItem();
			x24mToolStripMenuItem = new ToolStripMenuItem();
			x30mToolStripMenuItem = new ToolStripMenuItem();
			x40mToolStripMenuItem = new ToolStripMenuItem();
			x50mToolStripMenuItem = new ToolStripMenuItem();
			x1DegToolStripMenuItem = new ToolStripMenuItem();
			x2DegToolStripMenuItem = new ToolStripMenuItem();
			x3DegToolStripMenuItem = new ToolStripMenuItem();
			x4DegToolStripMenuItem = new ToolStripMenuItem();
			x5DegToolStripMenuItem = new ToolStripMenuItem();
			x6DegToolStripMenuItem = new ToolStripMenuItem();
			x8DegToolStripMenuItem = new ToolStripMenuItem();
			x10DegToolStripMenuItem = new ToolStripMenuItem();
			x12DegToolStripMenuItem = new ToolStripMenuItem();
			x15DegToolStripMenuItem = new ToolStripMenuItem();
			x20DegToolStripMenuItem = new ToolStripMenuItem();
			magnitudeToolStripMenuItem = new ToolStripMenuItem();
			toolStripMenuItem_AllVisual = new ToolStripMenuItem();
			toolStripMenuItem_All = new ToolStripMenuItem();
			toolStripMenuItemMag21 = new ToolStripMenuItem();
			toolStripMenuItemMag20 = new ToolStripMenuItem();
			toolStripMenuItemMag19 = new ToolStripMenuItem();
			toolStripMenuItemMag18 = new ToolStripMenuItem();
			toolStripMenuItemMag17 = new ToolStripMenuItem();
			toolStripMenuItemMag16 = new ToolStripMenuItem();
			toolStripMenuItemMag15 = new ToolStripMenuItem();
			toolStripMenuItemMag14 = new ToolStripMenuItem();
			toolStripMenuItemMag13 = new ToolStripMenuItem();
			toolStripMenuItemMag12 = new ToolStripMenuItem();
			toolStripMenuItemMag11 = new ToolStripMenuItem();
			toolStripMenuItemMag10 = new ToolStripMenuItem();
			toolStripMenuItemMag9 = new ToolStripMenuItem();
			toolStripMenuItemMag8 = new ToolStripMenuItem();
			toolStripMenuItemMag7 = new ToolStripMenuItem();
			toolStripMenuItemMag6 = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			toolTip1 = new ToolTip(components);
			GoogleSky = new Button();
			PanelCentre = new Panel();
			cmdPlot = new Button();
			label16 = new Label();
			label17 = new Label();
			txtDM1 = new TextBox();
			label18 = new Label();
			txtDD1 = new TextBox();
			label8 = new Label();
			label15 = new Label();
			label13 = new Label();
			txtRM1 = new TextBox();
			txtRH1 = new TextBox();
			lblStarID = new Label();
			lblSelected = new Label();
			trackOpacity = new TrackBar();
			picStars = new PictureBox();
			tBarRotate = new TrackBar();
			lblAngle = new Label();
			label1 = new Label();
			label2 = new Label();
			lblRedraw = new Label();
			label3 = new Label();
			label4 = new Label();
			label5 = new Label();
			label6 = new Label();
			label7 = new Label();
			label9 = new Label();
			label10 = new Label();
			label11 = new Label();
			label12 = new Label();
			cmdPlusHalf = new Button();
			cmdMinusHalf = new Button();
			label14 = new Label();
			label19 = new Label();
			label20 = new Label();
			chkOnTop = new CheckBox();
			panel1 = new Panel();
			panel2 = new Panel();
			comparisonStarDetailsToolStripMenuItem = new ToolStripMenuItem();
			pnlCompStars = new Panel();
			lstStars = new ListBox();
			cmdAddCurrent = new Button();
			button1 = new Button();
			cmdClear = new Button();
			cmdCopy = new Button();
			label22 = new Label();
			((Control)menuStrip1).SuspendLayout();
			((Control)PanelCentre).SuspendLayout();
			((ISupportInitialize)trackOpacity).BeginInit();
			((ISupportInitialize)picStars).BeginInit();
			((ISupportInitialize)tBarRotate).BeginInit();
			((Control)panel1).SuspendLayout();
			((Control)panel2).SuspendLayout();
			((Control)pnlCompStars).SuspendLayout();
			((Control)this).SuspendLayout();
			((ToolStrip)menuStrip1).set_BackColor(Color.LightSeaGreen);
			((Control)menuStrip1).set_Font(new Font("Segoe UI", 9f, FontStyle.Bold));
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[7]
			{
				(ToolStripItem)withPlotToolStripMenuItem,
				(ToolStripItem)catalogueToolStripMenuItem,
				(ToolStripItem)standardChartsToolStripMenuItem,
				(ToolStripItem)sizeToolStripMenuItem,
				(ToolStripItem)magnitudeToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(647, 24));
			((Control)menuStrip1).set_TabIndex(1);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withPlotToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[17]
			{
				(ToolStripItem)blackOnWhiteToolStripMenuItem,
				(ToolStripItem)flipHorizontallyToolStripMenuItem,
				(ToolStripItem)flipVerticallyToolStripMenuItem,
				(ToolStripItem)showCoordinatesInTooltipToolStripMenuItem,
				(ToolStripItem)comparisonStarDetailsToolStripMenuItem,
				(ToolStripItem)recenterToolStripMenuItem,
				(ToolStripItem)toolStripSeparator2,
				(ToolStripItem)applyStoredToolStripMenuItem,
				(ToolStripItem)storeSettingsToolStripMenuItem,
				(ToolStripItem)toolStripSeparator5,
				(ToolStripItem)toolStripSeparator3,
				(ToolStripItem)displayInGoogleSkyToolStripMenuItem,
				(ToolStripItem)toolStripSeparator1,
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)printPreviewToolStripMenuItem,
				(ToolStripItem)printToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem
			});
			((ToolStripItem)withPlotToolStripMenuItem).set_Name("withPlotToolStripMenuItem");
			((ToolStripItem)withPlotToolStripMenuItem).set_Size(new Size(78, 20));
			((ToolStripItem)withPlotToolStripMenuItem).set_Text("with Plot...");
			showCoordinatesInTooltipToolStripMenuItem.set_Checked(true);
			showCoordinatesInTooltipToolStripMenuItem.set_CheckState((CheckState)1);
			((ToolStripItem)showCoordinatesInTooltipToolStripMenuItem).set_Name("showCoordinatesInTooltipToolStripMenuItem");
			((ToolStripItem)showCoordinatesInTooltipToolStripMenuItem).set_Size(new Size(259, 22));
			((ToolStripItem)showCoordinatesInTooltipToolStripMenuItem).set_Text("Show coordinates in tooltip");
			((ToolStripItem)showCoordinatesInTooltipToolStripMenuItem).add_Click((EventHandler)showCoordinatesInTooltipToolStripMenuItem_Click);
			((ToolStripItem)flipHorizontallyToolStripMenuItem).set_Image((Image)Resources.arrow_Sync_16xLG);
			((ToolStripItem)flipHorizontallyToolStripMenuItem).set_Name("flipHorizontallyToolStripMenuItem");
			flipHorizontallyToolStripMenuItem.set_ShortcutKeys((Keys)131149);
			((ToolStripItem)flipHorizontallyToolStripMenuItem).set_Size(new Size(259, 22));
			((ToolStripItem)flipHorizontallyToolStripMenuItem).set_Text("Flip Horizontally");
			((ToolStripItem)flipHorizontallyToolStripMenuItem).add_Click((EventHandler)flipHorizontallyToolStripMenuItem_Click);
			((ToolStripItem)flipVerticallyToolStripMenuItem).set_Image((Image)Resources.arrow_Sync_updn_16xLG);
			((ToolStripItem)flipVerticallyToolStripMenuItem).set_ImageTransparentColor(Color.White);
			((ToolStripItem)flipVerticallyToolStripMenuItem).set_Name("flipVerticallyToolStripMenuItem");
			((ToolStripItem)flipVerticallyToolStripMenuItem).set_Size(new Size(259, 22));
			((ToolStripItem)flipVerticallyToolStripMenuItem).set_Text("Flip vertically");
			((ToolStripItem)flipVerticallyToolStripMenuItem).add_Click((EventHandler)flipVerticallyToolStripMenuItem_Click);
			recenterToolStripMenuItem.set_Checked(Settings.Default.StarMapDoubleClick);
			recenterToolStripMenuItem.set_CheckState((CheckState)1);
			((ToolStripItem)recenterToolStripMenuItem).set_Name("recenterToolStripMenuItem");
			((ToolStripItem)recenterToolStripMenuItem).set_Size(new Size(259, 22));
			((ToolStripItem)recenterToolStripMenuItem).set_Text("Set new center with double-click");
			((ToolStripItem)recenterToolStripMenuItem).add_Click((EventHandler)recenterToolStripMenuItem_Click);
			blackOnWhiteToolStripMenuItem.set_Checked(true);
			blackOnWhiteToolStripMenuItem.set_CheckState((CheckState)1);
			((ToolStripItem)blackOnWhiteToolStripMenuItem).set_Image((Image)Resources.ColorHS);
			((ToolStripItem)blackOnWhiteToolStripMenuItem).set_Name("blackOnWhiteToolStripMenuItem");
			((ToolStripItem)blackOnWhiteToolStripMenuItem).set_Size(new Size(259, 22));
			((ToolStripItem)blackOnWhiteToolStripMenuItem).set_Text("Black stars on White sky");
			((ToolStripItem)blackOnWhiteToolStripMenuItem).add_Click((EventHandler)blackOnWhiteToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator2).set_Name("toolStripSeparator2");
			((ToolStripItem)toolStripSeparator2).set_Size(new Size(256, 6));
			((ToolStripItem)applyStoredToolStripMenuItem).set_Name("applyStoredToolStripMenuItem");
			((ToolStripItem)applyStoredToolStripMenuItem).set_Size(new Size(259, 22));
			((ToolStripItem)applyStoredToolStripMenuItem).set_Text("Apply stored settings");
			((ToolStripItem)applyStoredToolStripMenuItem).add_Click((EventHandler)applyStoredToolStripMenuItem_Click);
			((ToolStripItem)storeSettingsToolStripMenuItem).set_Name("storeSettingsToolStripMenuItem");
			((ToolStripItem)storeSettingsToolStripMenuItem).set_Size(new Size(259, 22));
			((ToolStripItem)storeSettingsToolStripMenuItem).set_Text("Store current settings");
			((ToolStripItem)storeSettingsToolStripMenuItem).add_Click((EventHandler)storeSettingsToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator5).set_Name("toolStripSeparator5");
			((ToolStripItem)toolStripSeparator5).set_Size(new Size(256, 6));
			((ToolStripItem)toolStripSeparator3).set_Name("toolStripSeparator3");
			((ToolStripItem)toolStripSeparator3).set_Size(new Size(256, 6));
			((ToolStripItem)toolStripSeparator3).set_Visible(false);
			((ToolStripItem)displayInGoogleSkyToolStripMenuItem).set_Image((Image)Resources.sky_mo);
			((ToolStripItem)displayInGoogleSkyToolStripMenuItem).set_Name("displayInGoogleSkyToolStripMenuItem");
			displayInGoogleSkyToolStripMenuItem.set_ShortcutKeys((Keys)131143);
			((ToolStripItem)displayInGoogleSkyToolStripMenuItem).set_Size(new Size(259, 22));
			((ToolStripItem)displayInGoogleSkyToolStripMenuItem).set_Text("Display in GoogleSky");
			((ToolStripItem)displayInGoogleSkyToolStripMenuItem).add_Click((EventHandler)displayInGoogleSkyToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator1).set_Name("toolStripSeparator1");
			((ToolStripItem)toolStripSeparator1).set_Size(new Size(256, 6));
			((ToolStripItem)copyToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyToolStripMenuItem).set_Name("copyToolStripMenuItem");
			copyToolStripMenuItem.set_ShortcutKeys((Keys)131139);
			((ToolStripItem)copyToolStripMenuItem).set_Size(new Size(259, 22));
			((ToolStripItem)copyToolStripMenuItem).set_Text("&Copy");
			((ToolStripItem)copyToolStripMenuItem).add_Click((EventHandler)copyToolStripMenuItem_Click);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Image((Image)Resources.PrintPreview);
			((ToolStripItem)printPreviewToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Name("printPreviewToolStripMenuItem");
			((ToolStripItem)printPreviewToolStripMenuItem).set_Size(new Size(259, 22));
			((ToolStripItem)printPreviewToolStripMenuItem).set_Text("Print preview");
			((ToolStripItem)printPreviewToolStripMenuItem).add_Click((EventHandler)printPreviewToolStripMenuItem_Click);
			((ToolStripItem)printToolStripMenuItem).set_Image((Image)Resources.Print);
			((ToolStripItem)printToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printToolStripMenuItem).set_Name("printToolStripMenuItem");
			printToolStripMenuItem.set_ShortcutKeys((Keys)131152);
			((ToolStripItem)printToolStripMenuItem).set_Size(new Size(259, 22));
			((ToolStripItem)printToolStripMenuItem).set_Text("&Print");
			((ToolStripItem)printToolStripMenuItem).add_Click((EventHandler)printToolStripMenuItem_Click);
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			saveToolStripMenuItem.set_ShortcutKeys((Keys)131155);
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(259, 22));
			((ToolStripItem)saveToolStripMenuItem).set_Text("&Save");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
			((ToolStripDropDownItem)catalogueToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[6]
			{
				(ToolStripItem)GaiaToolStripMenuItem,
				(ToolStripItem)uCAC4ToolStripMenuItem,
				(ToolStripItem)pPMXLToolStripMenuItem,
				(ToolStripItem)nOMADToolStripMenuItem,
				(ToolStripItem)toolStripSeparator4,
				(ToolStripItem)setStarCatalogPathsToolStripMenuItem
			});
			((ToolStripItem)catalogueToolStripMenuItem).set_Name("catalogueToolStripMenuItem");
			((ToolStripItem)catalogueToolStripMenuItem).set_Size(new Size(83, 20));
			((ToolStripItem)catalogueToolStripMenuItem).set_Text("Catalogue...");
			((ToolStripItem)GaiaToolStripMenuItem).set_Name("GaiaToolStripMenuItem");
			GaiaToolStripMenuItem.set_ShortcutKeys((Keys)131156);
			((ToolStripItem)GaiaToolStripMenuItem).set_Size(new Size(193, 22));
			((ToolStripItem)GaiaToolStripMenuItem).set_Text("Gaia");
			((ToolStripItem)GaiaToolStripMenuItem).add_Click((EventHandler)GaiaToolStripMenuItem_Click);
			((ToolStripItem)uCAC4ToolStripMenuItem).set_Name("uCAC4ToolStripMenuItem");
			((ToolStripItem)uCAC4ToolStripMenuItem).set_Size(new Size(193, 22));
			((ToolStripItem)uCAC4ToolStripMenuItem).set_Text("UCAC4");
			((ToolStripItem)uCAC4ToolStripMenuItem).add_Click((EventHandler)uCAC4ToolStripMenuItem_Click);
			((ToolStripItem)pPMXLToolStripMenuItem).set_Name("pPMXLToolStripMenuItem");
			((ToolStripItem)pPMXLToolStripMenuItem).set_Size(new Size(193, 22));
			((ToolStripItem)pPMXLToolStripMenuItem).set_Text("PPMXL");
			((ToolStripItem)pPMXLToolStripMenuItem).add_Click((EventHandler)pPMXLToolStripMenuItem_Click);
			((ToolStripItem)nOMADToolStripMenuItem).set_Name("nOMADToolStripMenuItem");
			nOMADToolStripMenuItem.set_ShortcutKeys((Keys)131150);
			((ToolStripItem)nOMADToolStripMenuItem).set_Size(new Size(193, 22));
			((ToolStripItem)nOMADToolStripMenuItem).set_Text("NOMAD");
			((ToolStripItem)nOMADToolStripMenuItem).add_Click((EventHandler)nOMADToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator4).set_Name("toolStripSeparator4");
			((ToolStripItem)toolStripSeparator4).set_Size(new Size(190, 6));
			((ToolStripItem)setStarCatalogPathsToolStripMenuItem).set_Name("setStarCatalogPathsToolStripMenuItem");
			((ToolStripItem)setStarCatalogPathsToolStripMenuItem).set_Size(new Size(193, 22));
			((ToolStripItem)setStarCatalogPathsToolStripMenuItem).set_Text("Set star catalog paths");
			((ToolStripItem)setStarCatalogPathsToolStripMenuItem).add_Click((EventHandler)setStarCatalogPathsToolStripMenuItem_Click);
			((ToolStripDropDownItem)standardChartsToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[6]
			{
				(ToolStripItem)x05DegMag160ToolStripMenuItem,
				(ToolStripItem)x1DegMag140ToolStripMenuItem,
				(ToolStripItem)x3DegMag120ToolStripMenuItem,
				(ToolStripItem)x5DegMag105ToolStripMenuItem,
				(ToolStripItem)x10DegMag85ToolStripMenuItem,
				(ToolStripItem)x15DegMag70ToolStripMenuItem
			});
			((ToolStripItem)standardChartsToolStripMenuItem).set_Name("standardChartsToolStripMenuItem");
			((ToolStripItem)standardChartsToolStripMenuItem).set_Size(new Size(115, 20));
			((ToolStripItem)standardChartsToolStripMenuItem).set_Text("Standard charts...");
			((ToolStripItem)x05DegMag160ToolStripMenuItem).set_Name("x05DegMag160ToolStripMenuItem");
			((ToolStripItem)x05DegMag160ToolStripMenuItem).set_Size(new Size(189, 22));
			((ToolStripItem)x05DegMag160ToolStripMenuItem).set_Text("30'x 30', mag 16.0");
			((ToolStripItem)x05DegMag160ToolStripMenuItem).add_Click((EventHandler)x05DegMag160ToolStripMenuItem_Click);
			((ToolStripItem)x1DegMag140ToolStripMenuItem).set_Name("x1DegMag140ToolStripMenuItem");
			((ToolStripItem)x1DegMag140ToolStripMenuItem).set_Size(new Size(189, 22));
			((ToolStripItem)x1DegMag140ToolStripMenuItem).set_Text("1 x 1 deg, mag 14.0");
			((ToolStripItem)x1DegMag140ToolStripMenuItem).add_Click((EventHandler)x1DegMag140ToolStripMenuItem_Click);
			((ToolStripItem)x3DegMag120ToolStripMenuItem).set_Name("x3DegMag120ToolStripMenuItem");
			((ToolStripItem)x3DegMag120ToolStripMenuItem).set_Size(new Size(189, 22));
			((ToolStripItem)x3DegMag120ToolStripMenuItem).set_Text("3 x 3 deg,  mag 12.0");
			((ToolStripItem)x3DegMag120ToolStripMenuItem).add_Click((EventHandler)x3DegMag120ToolStripMenuItem_Click);
			((ToolStripItem)x5DegMag105ToolStripMenuItem).set_Name("x5DegMag105ToolStripMenuItem");
			((ToolStripItem)x5DegMag105ToolStripMenuItem).set_Size(new Size(189, 22));
			((ToolStripItem)x5DegMag105ToolStripMenuItem).set_Text("5 x 5 deg, mag 10.0");
			((ToolStripItem)x5DegMag105ToolStripMenuItem).add_Click((EventHandler)x5DegMag105ToolStripMenuItem_Click);
			((ToolStripItem)x10DegMag85ToolStripMenuItem).set_Name("x10DegMag85ToolStripMenuItem");
			((ToolStripItem)x10DegMag85ToolStripMenuItem).set_Size(new Size(189, 22));
			((ToolStripItem)x10DegMag85ToolStripMenuItem).set_Text("10 x 10 deg, mag 8.0");
			((ToolStripItem)x10DegMag85ToolStripMenuItem).add_Click((EventHandler)x10DegMag85ToolStripMenuItem_Click);
			((ToolStripItem)x15DegMag70ToolStripMenuItem).set_Name("x15DegMag70ToolStripMenuItem");
			((ToolStripItem)x15DegMag70ToolStripMenuItem).set_Size(new Size(189, 22));
			((ToolStripItem)x15DegMag70ToolStripMenuItem).set_Text("15 x 15 deg, mag 7.0");
			((ToolStripItem)x15DegMag70ToolStripMenuItem).add_Click((EventHandler)x15DegMag70ToolStripMenuItem_Click);
			((ToolStripDropDownItem)sizeToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[19]
			{
				(ToolStripItem)x10mToolStripMenuItem,
				(ToolStripItem)x12mToolStripMenuItem,
				(ToolStripItem)x15mToolStripMenuItem,
				(ToolStripItem)x18mToolStripMenuItem,
				(ToolStripItem)x24mToolStripMenuItem,
				(ToolStripItem)x30mToolStripMenuItem,
				(ToolStripItem)x40mToolStripMenuItem,
				(ToolStripItem)x50mToolStripMenuItem,
				(ToolStripItem)x1DegToolStripMenuItem,
				(ToolStripItem)x2DegToolStripMenuItem,
				(ToolStripItem)x3DegToolStripMenuItem,
				(ToolStripItem)x4DegToolStripMenuItem,
				(ToolStripItem)x5DegToolStripMenuItem,
				(ToolStripItem)x6DegToolStripMenuItem,
				(ToolStripItem)x8DegToolStripMenuItem,
				(ToolStripItem)x10DegToolStripMenuItem,
				(ToolStripItem)x12DegToolStripMenuItem,
				(ToolStripItem)x15DegToolStripMenuItem,
				(ToolStripItem)x20DegToolStripMenuItem
			});
			((ToolStripItem)sizeToolStripMenuItem).set_Name("sizeToolStripMenuItem");
			((ToolStripItem)sizeToolStripMenuItem).set_Size(new Size(51, 20));
			((ToolStripItem)sizeToolStripMenuItem).set_Text("Size...");
			((ToolStripItem)x10mToolStripMenuItem).set_Name("x10mToolStripMenuItem");
			((ToolStripItem)x10mToolStripMenuItem).set_Size(new Size(180, 22));
			((ToolStripItem)x10mToolStripMenuItem).set_Text("10' x 10'");
			((ToolStripItem)x10mToolStripMenuItem).add_Click((EventHandler)x10mToolStripMenuItem_Click);
			((ToolStripItem)x12mToolStripMenuItem).set_Name("x12mToolStripMenuItem");
			((ToolStripItem)x12mToolStripMenuItem).set_Size(new Size(180, 22));
			((ToolStripItem)x12mToolStripMenuItem).set_Text("12' x 12'");
			((ToolStripItem)x12mToolStripMenuItem).add_Click((EventHandler)x12mToolStripMenuItem_Click);
			((ToolStripItem)x15mToolStripMenuItem).set_Name("x15mToolStripMenuItem");
			((ToolStripItem)x15mToolStripMenuItem).set_Size(new Size(180, 22));
			((ToolStripItem)x15mToolStripMenuItem).set_Text("15' x 15'");
			((ToolStripItem)x15mToolStripMenuItem).add_Click((EventHandler)x15mToolStripMenuItem_Click);
			((ToolStripItem)x18mToolStripMenuItem).set_Name("x18mToolStripMenuItem");
			((ToolStripItem)x18mToolStripMenuItem).set_Size(new Size(180, 22));
			((ToolStripItem)x18mToolStripMenuItem).set_Text("18' x 18'");
			((ToolStripItem)x18mToolStripMenuItem).add_Click((EventHandler)x18mToolStripMenuItem_Click);
			((ToolStripItem)x24mToolStripMenuItem).set_Name("x24mToolStripMenuItem");
			((ToolStripItem)x24mToolStripMenuItem).set_Size(new Size(180, 22));
			((ToolStripItem)x24mToolStripMenuItem).set_Text("24' x 24'");
			((ToolStripItem)x24mToolStripMenuItem).add_Click((EventHandler)x24mToolStripMenuItem_Click);
			((ToolStripItem)x30mToolStripMenuItem).set_Name("x30mToolStripMenuItem");
			((ToolStripItem)x30mToolStripMenuItem).set_Size(new Size(180, 22));
			((ToolStripItem)x30mToolStripMenuItem).set_Text("30' x 30'");
			((ToolStripItem)x30mToolStripMenuItem).add_Click((EventHandler)x30mToolStripMenuItem_Click);
			((ToolStripItem)x40mToolStripMenuItem).set_Name("x40mToolStripMenuItem");
			((ToolStripItem)x40mToolStripMenuItem).set_Size(new Size(180, 22));
			((ToolStripItem)x40mToolStripMenuItem).set_Text("40' x 40'");
			((ToolStripItem)x40mToolStripMenuItem).add_Click((EventHandler)x40mToolStripMenuItem_Click);
			((ToolStripItem)x50mToolStripMenuItem).set_Name("x50mToolStripMenuItem");
			((ToolStripItem)x50mToolStripMenuItem).set_Size(new Size(180, 22));
			((ToolStripItem)x50mToolStripMenuItem).set_Text("50' x 50'");
			((ToolStripItem)x50mToolStripMenuItem).add_Click((EventHandler)x50mToolStripMenuItem_Click);
			((ToolStripItem)x1DegToolStripMenuItem).set_Name("x1DegToolStripMenuItem");
			((ToolStripItem)x1DegToolStripMenuItem).set_Size(new Size(180, 22));
			((ToolStripItem)x1DegToolStripMenuItem).set_Text("1° x 1°");
			((ToolStripItem)x1DegToolStripMenuItem).add_Click((EventHandler)x1DegToolStripMenuItem_Click);
			((ToolStripItem)x2DegToolStripMenuItem).set_Name("x2DegToolStripMenuItem");
			((ToolStripItem)x2DegToolStripMenuItem).set_Size(new Size(180, 22));
			((ToolStripItem)x2DegToolStripMenuItem).set_Text("2° x 2°");
			((ToolStripItem)x2DegToolStripMenuItem).add_Click((EventHandler)x2DegToolStripMenuItem_Click);
			((ToolStripItem)x3DegToolStripMenuItem).set_Name("x3DegToolStripMenuItem");
			((ToolStripItem)x3DegToolStripMenuItem).set_Size(new Size(180, 22));
			((ToolStripItem)x3DegToolStripMenuItem).set_Text("3° x 3°");
			((ToolStripItem)x3DegToolStripMenuItem).add_Click((EventHandler)x3DegToolStripMenuItem_Click);
			((ToolStripItem)x4DegToolStripMenuItem).set_Name("x4DegToolStripMenuItem");
			((ToolStripItem)x4DegToolStripMenuItem).set_Size(new Size(180, 22));
			((ToolStripItem)x4DegToolStripMenuItem).set_Text("4° x 4°");
			((ToolStripItem)x4DegToolStripMenuItem).add_Click((EventHandler)toolStripMenuItem11_Click);
			((ToolStripItem)x5DegToolStripMenuItem).set_Name("x5DegToolStripMenuItem");
			((ToolStripItem)x5DegToolStripMenuItem).set_Size(new Size(180, 22));
			((ToolStripItem)x5DegToolStripMenuItem).set_Text("5° x 5°");
			((ToolStripItem)x5DegToolStripMenuItem).add_Click((EventHandler)x5DegToolStripMenuItem_Click);
			((ToolStripItem)x6DegToolStripMenuItem).set_Name("x6DegToolStripMenuItem");
			((ToolStripItem)x6DegToolStripMenuItem).set_Size(new Size(180, 22));
			((ToolStripItem)x6DegToolStripMenuItem).set_Text("6° x 6°");
			((ToolStripItem)x6DegToolStripMenuItem).add_Click((EventHandler)x6DegToolStripMenuItem_Click);
			((ToolStripItem)x8DegToolStripMenuItem).set_Name("x8DegToolStripMenuItem");
			((ToolStripItem)x8DegToolStripMenuItem).set_Size(new Size(180, 22));
			((ToolStripItem)x8DegToolStripMenuItem).set_Text("8° x 8°");
			((ToolStripItem)x8DegToolStripMenuItem).add_Click((EventHandler)x8DegToolStripMenuItem_Click);
			((ToolStripItem)x10DegToolStripMenuItem).set_Name("x10DegToolStripMenuItem");
			((ToolStripItem)x10DegToolStripMenuItem).set_Size(new Size(180, 22));
			((ToolStripItem)x10DegToolStripMenuItem).set_Text("10° x 10°");
			((ToolStripItem)x10DegToolStripMenuItem).add_Click((EventHandler)x10DegToolStripMenuItem_Click);
			((ToolStripItem)x12DegToolStripMenuItem).set_Name("x12DegToolStripMenuItem");
			((ToolStripItem)x12DegToolStripMenuItem).set_Size(new Size(180, 22));
			((ToolStripItem)x12DegToolStripMenuItem).set_Text("12° x 12°");
			((ToolStripItem)x12DegToolStripMenuItem).add_Click((EventHandler)x12DegToolStripMenuItem_Click);
			((ToolStripItem)x15DegToolStripMenuItem).set_Name("x15DegToolStripMenuItem");
			((ToolStripItem)x15DegToolStripMenuItem).set_Size(new Size(180, 22));
			((ToolStripItem)x15DegToolStripMenuItem).set_Text("15° x  15°");
			((ToolStripItem)x15DegToolStripMenuItem).add_Click((EventHandler)x15DegToolStripMenuItem_Click);
			((ToolStripItem)x20DegToolStripMenuItem).set_Name("x20DegToolStripMenuItem");
			((ToolStripItem)x20DegToolStripMenuItem).set_Size(new Size(180, 22));
			((ToolStripItem)x20DegToolStripMenuItem).set_Text("20° x 20°");
			((ToolStripItem)x20DegToolStripMenuItem).add_Click((EventHandler)x20DegToolStripMenuItem_Click);
			((ToolStripDropDownItem)magnitudeToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[18]
			{
				(ToolStripItem)toolStripMenuItem_AllVisual,
				(ToolStripItem)toolStripMenuItem_All,
				(ToolStripItem)toolStripMenuItemMag21,
				(ToolStripItem)toolStripMenuItemMag20,
				(ToolStripItem)toolStripMenuItemMag19,
				(ToolStripItem)toolStripMenuItemMag18,
				(ToolStripItem)toolStripMenuItemMag17,
				(ToolStripItem)toolStripMenuItemMag16,
				(ToolStripItem)toolStripMenuItemMag15,
				(ToolStripItem)toolStripMenuItemMag14,
				(ToolStripItem)toolStripMenuItemMag13,
				(ToolStripItem)toolStripMenuItemMag12,
				(ToolStripItem)toolStripMenuItemMag11,
				(ToolStripItem)toolStripMenuItemMag10,
				(ToolStripItem)toolStripMenuItemMag9,
				(ToolStripItem)toolStripMenuItemMag8,
				(ToolStripItem)toolStripMenuItemMag7,
				(ToolStripItem)toolStripMenuItemMag6
			});
			((ToolStripItem)magnitudeToolStripMenuItem).set_Name("magnitudeToolStripMenuItem");
			((ToolStripItem)magnitudeToolStripMenuItem).set_Size(new Size(88, 20));
			((ToolStripItem)magnitudeToolStripMenuItem).set_Text("Magnitude...");
			toolStripMenuItem_AllVisual.set_Checked(true);
			toolStripMenuItem_AllVisual.set_CheckState((CheckState)1);
			((ToolStripItem)toolStripMenuItem_AllVisual).set_Name("toolStripMenuItem_AllVisual");
			((ToolStripItem)toolStripMenuItem_AllVisual).set_Size(new Size(106, 22));
			((ToolStripItem)toolStripMenuItem_AllVisual).set_Text("Visual");
			((ToolStripItem)toolStripMenuItem_AllVisual).add_Click((EventHandler)visualToolStripMenuItem_Click);
			((ToolStripItem)toolStripMenuItem_All).set_Name("toolStripMenuItem_All");
			((ToolStripItem)toolStripMenuItem_All).set_Size(new Size(106, 22));
			((ToolStripItem)toolStripMenuItem_All).set_Text("All");
			((ToolStripItem)toolStripMenuItem_All).add_Click((EventHandler)toolStripMenuItem_All_Click);
			((ToolStripItem)toolStripMenuItemMag21).set_Name("toolStripMenuItemMag21");
			((ToolStripItem)toolStripMenuItemMag21).set_Size(new Size(106, 22));
			((ToolStripItem)toolStripMenuItemMag21).set_Text("<21.0");
			((ToolStripItem)toolStripMenuItemMag21).add_Click((EventHandler)allToolStripMenuItem1_Click);
			((ToolStripItem)toolStripMenuItemMag20).set_Name("toolStripMenuItemMag20");
			((ToolStripItem)toolStripMenuItemMag20).set_Size(new Size(106, 22));
			((ToolStripItem)toolStripMenuItemMag20).set_Text("<20.0");
			((ToolStripItem)toolStripMenuItemMag20).add_Click((EventHandler)toolStripMenuItem16_Click);
			((ToolStripItem)toolStripMenuItemMag19).set_Name("toolStripMenuItemMag19");
			((ToolStripItem)toolStripMenuItemMag19).set_Size(new Size(106, 22));
			((ToolStripItem)toolStripMenuItemMag19).set_Text("<19.0");
			((ToolStripItem)toolStripMenuItemMag19).add_Click((EventHandler)toolStripMenuItem15_Click);
			((ToolStripItem)toolStripMenuItemMag18).set_Name("toolStripMenuItemMag18");
			((ToolStripItem)toolStripMenuItemMag18).set_Size(new Size(106, 22));
			((ToolStripItem)toolStripMenuItemMag18).set_Text("<18.0");
			((ToolStripItem)toolStripMenuItemMag18).add_Click((EventHandler)toolStripMenuItem14_Click);
			((ToolStripItem)toolStripMenuItemMag17).set_Name("toolStripMenuItemMag17");
			((ToolStripItem)toolStripMenuItemMag17).set_Size(new Size(106, 22));
			((ToolStripItem)toolStripMenuItemMag17).set_Text("<17.0");
			((ToolStripItem)toolStripMenuItemMag17).add_Click((EventHandler)toolStripMenuItem13_Click);
			((ToolStripItem)toolStripMenuItemMag16).set_Name("toolStripMenuItemMag16");
			((ToolStripItem)toolStripMenuItemMag16).set_Size(new Size(106, 22));
			((ToolStripItem)toolStripMenuItemMag16).set_Text("<16.0");
			((ToolStripItem)toolStripMenuItemMag16).add_Click((EventHandler)allToolStripMenuItem_Click);
			((ToolStripItem)toolStripMenuItemMag15).set_Name("toolStripMenuItemMag15");
			((ToolStripItem)toolStripMenuItemMag15).set_Size(new Size(106, 22));
			((ToolStripItem)toolStripMenuItemMag15).set_Text("<15.0");
			((ToolStripItem)toolStripMenuItemMag15).add_Click((EventHandler)toolStripMenuItem12_Click);
			((ToolStripItem)toolStripMenuItemMag14).set_Name("toolStripMenuItemMag14");
			((ToolStripItem)toolStripMenuItemMag14).set_Size(new Size(106, 22));
			((ToolStripItem)toolStripMenuItemMag14).set_Text("<14.0");
			((ToolStripItem)toolStripMenuItemMag14).add_Click((EventHandler)toolStripMenuItem2_Click);
			((ToolStripItem)toolStripMenuItemMag13).set_Name("toolStripMenuItemMag13");
			((ToolStripItem)toolStripMenuItemMag13).set_Size(new Size(106, 22));
			((ToolStripItem)toolStripMenuItemMag13).set_Text("<13.0");
			((ToolStripItem)toolStripMenuItemMag13).add_Click((EventHandler)toolStripMenuItem3_Click);
			((ToolStripItem)toolStripMenuItemMag12).set_Name("toolStripMenuItemMag12");
			((ToolStripItem)toolStripMenuItemMag12).set_Size(new Size(106, 22));
			((ToolStripItem)toolStripMenuItemMag12).set_Text("<12.0");
			((ToolStripItem)toolStripMenuItemMag12).add_Click((EventHandler)toolStripMenuItem4_Click);
			((ToolStripItem)toolStripMenuItemMag11).set_Name("toolStripMenuItemMag11");
			((ToolStripItem)toolStripMenuItemMag11).set_Size(new Size(106, 22));
			((ToolStripItem)toolStripMenuItemMag11).set_Text("<11.0");
			((ToolStripItem)toolStripMenuItemMag11).add_Click((EventHandler)toolStripMenuItem5_Click);
			((ToolStripItem)toolStripMenuItemMag10).set_Name("toolStripMenuItemMag10");
			((ToolStripItem)toolStripMenuItemMag10).set_Size(new Size(106, 22));
			((ToolStripItem)toolStripMenuItemMag10).set_Text("<10.0");
			((ToolStripItem)toolStripMenuItemMag10).add_Click((EventHandler)toolStripMenuItem6_Click);
			((ToolStripItem)toolStripMenuItemMag9).set_Name("toolStripMenuItemMag9");
			((ToolStripItem)toolStripMenuItemMag9).set_Size(new Size(106, 22));
			((ToolStripItem)toolStripMenuItemMag9).set_Text("<9.0");
			((ToolStripItem)toolStripMenuItemMag9).add_Click((EventHandler)toolStripMenuItem7_Click);
			((ToolStripItem)toolStripMenuItemMag8).set_Name("toolStripMenuItemMag8");
			((ToolStripItem)toolStripMenuItemMag8).set_Size(new Size(106, 22));
			((ToolStripItem)toolStripMenuItemMag8).set_Text("<8.0");
			((ToolStripItem)toolStripMenuItemMag8).add_Click((EventHandler)toolStripMenuItem8_Click);
			((ToolStripItem)toolStripMenuItemMag7).set_Name("toolStripMenuItemMag7");
			((ToolStripItem)toolStripMenuItemMag7).set_Size(new Size(106, 22));
			((ToolStripItem)toolStripMenuItemMag7).set_Text("<7.0");
			((ToolStripItem)toolStripMenuItemMag7).add_Click((EventHandler)toolStripMenuItem9_Click);
			((ToolStripItem)toolStripMenuItemMag6).set_Name("toolStripMenuItemMag6");
			((ToolStripItem)toolStripMenuItemMag6).set_Size(new Size(106, 22));
			((ToolStripItem)toolStripMenuItemMag6).set_Text("<6.0");
			((ToolStripItem)toolStripMenuItemMag6).add_Click((EventHandler)toolStripMenuItem10_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(73, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("&Help    ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(56, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("E&xit");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			toolTip1.set_AutoPopDelay(2000);
			toolTip1.set_InitialDelay(100);
			toolTip1.set_ReshowDelay(100);
			((Control)GoogleSky).set_BackgroundImage((Image)Resources.sky_mo);
			((Control)GoogleSky).set_BackgroundImageLayout((ImageLayout)2);
			((Control)GoogleSky).set_Location(new Point(527, 1));
			((Control)GoogleSky).set_Name("GoogleSky");
			((Control)GoogleSky).set_Size(new Size(28, 24));
			((Control)GoogleSky).set_TabIndex(59);
			((Control)GoogleSky).set_Text("button1");
			toolTip1.SetToolTip((Control)(object)GoogleSky, "Plot GoogleSky");
			((ButtonBase)GoogleSky).set_UseVisualStyleBackColor(true);
			((Control)GoogleSky).add_Click((EventHandler)GoogleSky_Click);
			((Control)PanelCentre).set_Anchor((AnchorStyles)1);
			((Control)PanelCentre).get_Controls().Add((Control)(object)GoogleSky);
			((Control)PanelCentre).get_Controls().Add((Control)(object)cmdPlot);
			((Control)PanelCentre).get_Controls().Add((Control)(object)label16);
			((Control)PanelCentre).get_Controls().Add((Control)(object)label17);
			((Control)PanelCentre).get_Controls().Add((Control)(object)txtDM1);
			((Control)PanelCentre).get_Controls().Add((Control)(object)label18);
			((Control)PanelCentre).get_Controls().Add((Control)(object)txtDD1);
			((Control)PanelCentre).get_Controls().Add((Control)(object)label8);
			((Control)PanelCentre).get_Controls().Add((Control)(object)label15);
			((Control)PanelCentre).get_Controls().Add((Control)(object)label13);
			((Control)PanelCentre).get_Controls().Add((Control)(object)txtRM1);
			((Control)PanelCentre).get_Controls().Add((Control)(object)txtRH1);
			((Control)PanelCentre).set_Location(new Point(41, 71));
			((Control)PanelCentre).set_Name("PanelCentre");
			((Control)PanelCentre).set_Size(new Size(565, 26));
			((Control)PanelCentre).set_TabIndex(2);
			((Control)cmdPlot).set_Location(new Point(461, 2));
			((Control)cmdPlot).set_Name("cmdPlot");
			((Control)cmdPlot).set_Size(new Size(36, 22));
			((Control)cmdPlot).set_TabIndex(58);
			((Control)cmdPlot).set_Text("Plot");
			((ButtonBase)cmdPlot).set_UseVisualStyleBackColor(true);
			((Control)cmdPlot).add_Click((EventHandler)cmdPlot_Click);
			((Control)label16).set_AutoSize(true);
			((Control)label16).set_Location(new Point(252, 3));
			((Control)label16).set_Name("label16");
			((Control)label16).set_Size(new Size(15, 13));
			((Control)label16).set_TabIndex(57);
			((Control)label16).set_Text("m");
			((Control)label17).set_AutoSize(true);
			((Control)label17).set_Location(new Point(199, 3));
			((Control)label17).set_Name("label17");
			((Control)label17).set_Size(new Size(13, 13));
			((Control)label17).set_TabIndex(56);
			((Control)label17).set_Text("h");
			((Control)txtDM1).set_Location(new Point(397, 5));
			((Control)txtDM1).set_Name("txtDM1");
			((Control)txtDM1).set_Size(new Size(27, 20));
			((Control)txtDM1).set_TabIndex(52);
			((Control)txtDM1).set_Text("0");
			((Control)txtDM1).add_Enter((EventHandler)txtDM1_Enter);
			((Control)label18).set_AutoSize(true);
			((Control)label18).set_Location(new Point(423, 7));
			((Control)label18).set_Name("label18");
			((Control)label18).set_Size(new Size(9, 13));
			((Control)label18).set_TabIndex(55);
			((Control)label18).set_Text("'");
			((Control)txtDD1).set_Location(new Point(353, 5));
			((Control)txtDD1).set_Name("txtDD1");
			((Control)txtDD1).set_Size(new Size(28, 20));
			((Control)txtDD1).set_TabIndex(51);
			((Control)txtDD1).set_Text("0");
			((Control)txtDD1).add_Enter((EventHandler)txtDD1_Enter);
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Location(new Point(379, 3));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(13, 13));
			((Control)label8).set_TabIndex(54);
			((Control)label8).set_Text("o");
			((Control)label15).set_AutoSize(true);
			((Control)label15).set_Location(new Point(289, 8));
			((Control)label15).set_Name("label15");
			((Control)label15).set_Size(new Size(60, 13));
			((Control)label15).set_TabIndex(53);
			((Control)label15).set_Text("Declination");
			((Control)label13).set_AutoSize(true);
			((Control)label13).set_Location(new Point(23, 8));
			((Control)label13).set_Name("label13");
			((Control)label13).set_Size(new Size(149, 13));
			((Control)label13).set_TabIndex(50);
			((Control)label13).set_Text("Chart centre:   Right Acension");
			((Control)txtRM1).set_Location(new Point(214, 5));
			((Control)txtRM1).set_Name("txtRM1");
			((Control)txtRM1).set_Size(new Size(36, 20));
			((Control)txtRM1).set_TabIndex(49);
			((Control)txtRM1).set_Text("0");
			((Control)txtRM1).add_Enter((EventHandler)txtRM1_Enter);
			((Control)txtRH1).set_Location(new Point(173, 5));
			((Control)txtRH1).set_Name("txtRH1");
			((Control)txtRH1).set_Size(new Size(26, 20));
			((Control)txtRH1).set_TabIndex(48);
			((Control)txtRH1).set_Text("0");
			((Control)txtRH1).add_Enter((EventHandler)txtRH1_Enter);
			((Control)lblStarID).set_AutoSize(true);
			((Control)lblStarID).set_Location(new Point(120, 58));
			((Control)lblStarID).set_Name("lblStarID");
			((Control)lblStarID).set_Size(new Size(12, 13));
			((Control)lblStarID).set_TabIndex(61);
			((Control)lblStarID).set_Text("x");
			((Control)lblSelected).set_AutoSize(true);
			((Control)lblSelected).set_Location(new Point(42, 58));
			((Control)lblSelected).set_Name("lblSelected");
			((Control)lblSelected).set_Size(new Size(81, 13));
			((Control)lblSelected).set_TabIndex(60);
			((Control)lblSelected).set_Text("Selected star = ");
			((Control)trackOpacity).set_AutoSize(false);
			((Control)trackOpacity).set_BackColor(Color.LightYellow);
			((Control)trackOpacity).set_Location(new Point(0, 0));
			trackOpacity.set_Maximum(100);
			trackOpacity.set_Minimum(20);
			((Control)trackOpacity).set_Name("trackOpacity");
			((Control)trackOpacity).set_Size(new Size(149, 23));
			trackOpacity.set_SmallChange(5);
			((Control)trackOpacity).set_TabIndex(39);
			trackOpacity.set_TickFrequency(10);
			trackOpacity.set_Value(100);
			trackOpacity.add_ValueChanged((EventHandler)trackOpacity_ValueChanged);
			((Control)picStars).set_Location(new Point(5, 101));
			((Control)picStars).set_Name("picStars");
			((Control)picStars).set_Size(new Size(636, 636));
			picStars.set_TabIndex(0);
			picStars.set_TabStop(false);
			((Control)picStars).add_MouseDoubleClick(new MouseEventHandler(picStars_MouseDoubleClick));
			((Control)picStars).add_MouseMove(new MouseEventHandler(picStars_MouseMove));
			((Control)picStars).add_MouseUp(new MouseEventHandler(picStars_MouseUp));
			((Control)tBarRotate).set_AutoSize(false);
			((Control)tBarRotate).set_BackColor(Color.LightYellow);
			((Control)tBarRotate).set_Location(new Point(0, 0));
			tBarRotate.set_Maximum(900);
			tBarRotate.set_Minimum(-900);
			((Control)tBarRotate).set_Name("tBarRotate");
			((Control)tBarRotate).set_Size(new Size(361, 23));
			tBarRotate.set_SmallChange(5);
			((Control)tBarRotate).set_TabIndex(40);
			tBarRotate.set_TickFrequency(50);
			((Control)tBarRotate).add_MouseMove(new MouseEventHandler(tBarRotate_MouseMove));
			((Control)tBarRotate).add_MouseUp(new MouseEventHandler(tBarRotate_MouseUp));
			((Control)lblAngle).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblAngle).set_Location(new Point(201, 36));
			((Control)lblAngle).set_Name("lblAngle");
			((Control)lblAngle).set_Size(new Size(46, 13));
			((Control)lblAngle).set_TabIndex(41);
			((Control)lblAngle).set_Text("0°");
			lblAngle.set_TextAlign(ContentAlignment.MiddleRight);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(202, 23));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(45, 13));
			((Control)label1).set_TabIndex(42);
			((Control)label1).set_Text("Rotate");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label2).set_Location(new Point(-2, 25));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(50, 13));
			((Control)label2).set_TabIndex(43);
			((Control)label2).set_Text("Opacity");
			((Control)lblRedraw).set_AutoSize(true);
			((Control)lblRedraw).set_ForeColor(Color.DarkRed);
			((Control)lblRedraw).set_Location(new Point(145, -2));
			((Control)lblRedraw).set_Name("lblRedraw");
			((Control)lblRedraw).set_Size(new Size(73, 13));
			((Control)lblRedraw).set_TabIndex(44);
			((Control)lblRedraw).set_Text("Drawing chart");
			((Control)lblRedraw).set_Visible(false);
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label3).set_Location(new Point(277, 45));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(29, 12));
			((Control)label3).set_TabIndex(45);
			((Control)label3).set_Text("+180");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label4).set_Location(new Point(365, 46));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(23, 12));
			((Control)label4).set_TabIndex(46);
			((Control)label4).set_Text("+90");
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label5).set_Location(new Point(537, 46));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(21, 12));
			((Control)label5).set_TabIndex(47);
			((Control)label5).set_Text("-90");
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label6).set_Location(new Point(619, 46));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(27, 12));
			((Control)label6).set_TabIndex(48);
			((Control)label6).set_Text("-180");
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label7).set_Location(new Point(458, 46));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(11, 12));
			((Control)label7).set_TabIndex(49);
			((Control)label7).set_Text("0");
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label9).set_Location(new Point(79, 45));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(26, 12));
			((Control)label9).set_TabIndex(50);
			((Control)label9).set_Text("40%");
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label10).set_Location(new Point(172, 45));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(32, 12));
			((Control)label10).set_TabIndex(51);
			((Control)label10).set_Text("100%");
			((Control)label11).set_AutoSize(true);
			((Control)label11).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label11).set_Location(new Point(142, 45));
			((Control)label11).set_Name("label11");
			((Control)label11).set_Size(new Size(26, 12));
			((Control)label11).set_TabIndex(52);
			((Control)label11).set_Text("80%");
			((Control)label12).set_AutoSize(true);
			((Control)label12).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label12).set_Location(new Point(111, 45));
			((Control)label12).set_Name("label12");
			((Control)label12).set_Size(new Size(26, 12));
			((Control)label12).set_TabIndex(53);
			((Control)label12).set_Text("60%");
			((Control)cmdPlusHalf).set_BackColor(Color.LawnGreen);
			((ButtonBase)cmdPlusHalf).set_FlatStyle((FlatStyle)0);
			((Control)cmdPlusHalf).set_ForeColor(SystemColors.ControlText);
			((Control)cmdPlusHalf).set_Location(new Point(270, 25));
			((Control)cmdPlusHalf).set_Name("cmdPlusHalf");
			((Control)cmdPlusHalf).set_Size(new Size(9, 10));
			((Control)cmdPlusHalf).set_TabIndex(54);
			((Control)cmdPlusHalf).set_Text("button1");
			((ButtonBase)cmdPlusHalf).set_UseVisualStyleBackColor(false);
			((Control)cmdPlusHalf).add_MouseClick(new MouseEventHandler(cmdPlusHalf_MouseClick));
			((Control)cmdMinusHalf).set_BackColor(Color.Tomato);
			((ButtonBase)cmdMinusHalf).set_FlatStyle((FlatStyle)0);
			((Control)cmdMinusHalf).set_Location(new Point(270, 37));
			((Control)cmdMinusHalf).set_Name("cmdMinusHalf");
			((Control)cmdMinusHalf).set_Size(new Size(9, 10));
			((Control)cmdMinusHalf).set_TabIndex(55);
			((Control)cmdMinusHalf).set_Text("button1");
			((ButtonBase)cmdMinusHalf).set_UseVisualStyleBackColor(false);
			((Control)cmdMinusHalf).add_Click((EventHandler)cmdMinusHalf_Click);
			((Control)label14).set_AutoSize(true);
			((Control)label14).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label14).set_Location(new Point(245, 24));
			((Control)label14).set_Name("label14");
			((Control)label14).set_Size(new Size(27, 12));
			((Control)label14).set_TabIndex(56);
			((Control)label14).set_Text("+0.2°");
			((Control)label19).set_AutoSize(true);
			((Control)label19).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label19).set_Location(new Point(246, 35));
			((Control)label19).set_Name("label19");
			((Control)label19).set_Size(new Size(25, 12));
			((Control)label19).set_TabIndex(57);
			((Control)label19).set_Text("-0.2°");
			((Control)label20).set_AutoSize(true);
			((Control)label20).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label20).set_Location(new Point(47, 45));
			((Control)label20).set_Name("label20");
			((Control)label20).set_Size(new Size(26, 12));
			((Control)label20).set_TabIndex(58);
			((Control)label20).set_Text("20%");
			chkOnTop.set_AutoCheck(false);
			((Control)chkOnTop).set_AutoSize(true);
			chkOnTop.set_CheckAlign(ContentAlignment.BottomCenter);
			((Control)chkOnTop).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkOnTop).set_ForeColor(Color.RoyalBlue);
			((Control)chkOnTop).set_Location(new Point(-1, 39));
			((Control)chkOnTop).set_Name("chkOnTop");
			((Control)chkOnTop).set_Size(new Size(42, 31));
			((Control)chkOnTop).set_TabIndex(59);
			((Control)chkOnTop).set_Text("OnTop");
			((ButtonBase)chkOnTop).set_TextAlign(ContentAlignment.TopCenter);
			((ButtonBase)chkOnTop).set_UseVisualStyleBackColor(true);
			((Control)chkOnTop).add_Click((EventHandler)chkOnTop_Click);
			panel1.set_BorderStyle((BorderStyle)1);
			((Control)panel1).get_Controls().Add((Control)(object)trackOpacity);
			((Control)panel1).set_Location(new Point(45, 24));
			((Control)panel1).set_Name("panel1");
			((Control)panel1).set_Size(new Size(151, 23));
			((Control)panel1).set_TabIndex(60);
			panel2.set_BorderStyle((BorderStyle)1);
			((Control)panel2).get_Controls().Add((Control)(object)lblRedraw);
			((Control)panel2).get_Controls().Add((Control)(object)tBarRotate);
			((Control)panel2).set_Location(new Point(282, 24));
			((Control)panel2).set_Name("panel2");
			((Control)panel2).set_Size(new Size(361, 23));
			((Control)panel2).set_TabIndex(61);
			((ToolStripItem)comparisonStarDetailsToolStripMenuItem).set_Image((Image)Resources.Asterisk_48x);
			((ToolStripItem)comparisonStarDetailsToolStripMenuItem).set_Name("comparisonStarDetailsToolStripMenuItem");
			((ToolStripItem)comparisonStarDetailsToolStripMenuItem).set_Size(new Size(259, 22));
			((ToolStripItem)comparisonStarDetailsToolStripMenuItem).set_Text("Show comparison star details");
			((ToolStripItem)comparisonStarDetailsToolStripMenuItem).add_Click((EventHandler)comparisonStarDetailsToolStripMenuItem_Click);
			((Control)pnlCompStars).set_BackColor(Color.BlanchedAlmond);
			pnlCompStars.set_BorderStyle((BorderStyle)2);
			((Control)pnlCompStars).get_Controls().Add((Control)(object)label22);
			((Control)pnlCompStars).get_Controls().Add((Control)(object)cmdCopy);
			((Control)pnlCompStars).get_Controls().Add((Control)(object)cmdClear);
			((Control)pnlCompStars).get_Controls().Add((Control)(object)button1);
			((Control)pnlCompStars).get_Controls().Add((Control)(object)cmdAddCurrent);
			((Control)pnlCompStars).get_Controls().Add((Control)(object)lstStars);
			((Control)pnlCompStars).set_Location(new Point(60, 408));
			((Control)pnlCompStars).set_Name("pnlCompStars");
			((Control)pnlCompStars).set_Size(new Size(440, 153));
			((Control)pnlCompStars).set_TabIndex(62);
			((Control)pnlCompStars).set_Visible(false);
			((Control)pnlCompStars).add_MouseDown(new MouseEventHandler(pnlCompStars_MouseDown));
			((Control)pnlCompStars).add_MouseEnter((EventHandler)pnlCompStars_MouseEnter);
			((Control)pnlCompStars).add_MouseHover((EventHandler)pnlCompStars_MouseHover);
			((Control)pnlCompStars).add_MouseMove(new MouseEventHandler(pnlCompStars_MouseMove));
			((Control)lstStars).set_BackColor(Color.OldLace);
			((Control)lstStars).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstStars).set_FormattingEnabled(true);
			lstStars.set_HorizontalScrollbar(true);
			lstStars.set_ItemHeight(14);
			((Control)lstStars).set_Location(new Point(69, 16));
			((Control)lstStars).set_Name("lstStars");
			((Control)lstStars).set_Size(new Size(365, 130));
			((Control)lstStars).set_TabIndex(0);
			lstStars.add_SelectedIndexChanged((EventHandler)lstStars_SelectedIndexChanged);
			((Control)cmdAddCurrent).set_Location(new Point(12, 16));
			((Control)cmdAddCurrent).set_Name("cmdAddCurrent");
			((Control)cmdAddCurrent).set_Size(new Size(50, 22));
			((Control)cmdAddCurrent).set_TabIndex(1);
			((Control)cmdAddCurrent).set_Text("Add");
			((ButtonBase)cmdAddCurrent).set_UseVisualStyleBackColor(true);
			((Control)cmdAddCurrent).add_Click((EventHandler)cmdAddCurrent_Click);
			((Control)button1).set_Location(new Point(13, 88));
			((Control)button1).set_Name("button1");
			((Control)button1).set_Size(new Size(50, 22));
			((Control)button1).set_TabIndex(2);
			((Control)button1).set_Text("Delete ");
			((ButtonBase)button1).set_UseVisualStyleBackColor(true);
			((Control)button1).add_Click((EventHandler)button1_Click);
			((Control)cmdClear).set_Location(new Point(13, 124));
			((Control)cmdClear).set_Name("cmdClear");
			((Control)cmdClear).set_Size(new Size(50, 22));
			((Control)cmdClear).set_TabIndex(3);
			((Control)cmdClear).set_Text("Clear");
			((ButtonBase)cmdClear).set_UseVisualStyleBackColor(true);
			((Control)cmdClear).add_Click((EventHandler)cmdClear_Click);
			((Control)cmdCopy).set_Location(new Point(12, 52));
			((Control)cmdCopy).set_Name("cmdCopy");
			((Control)cmdCopy).set_Size(new Size(50, 22));
			((Control)cmdCopy).set_TabIndex(4);
			((Control)cmdCopy).set_Text("Copy");
			((ButtonBase)cmdCopy).set_UseVisualStyleBackColor(true);
			((Control)cmdCopy).add_Click((EventHandler)cmdCopy_Click);
			((Control)label22).set_AutoSize(true);
			((Control)label22).set_Location(new Point(145, 2));
			((Control)label22).set_Name("label22");
			((Control)label22).set_Size(new Size(140, 13));
			((Control)label22).set_TabIndex(5);
			((Control)label22).set_Text("C o m p a r i s o n     S t a r s");
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Control)this).set_BackColor(SystemColors.Control);
			((Form)this).set_ClientSize(new Size(647, 715));
			((Control)this).get_Controls().Add((Control)(object)pnlCompStars);
			((Control)this).get_Controls().Add((Control)(object)lblStarID);
			((Control)this).get_Controls().Add((Control)(object)panel2);
			((Control)this).get_Controls().Add((Control)(object)lblSelected);
			((Control)this).get_Controls().Add((Control)(object)cmdMinusHalf);
			((Control)this).get_Controls().Add((Control)(object)cmdPlusHalf);
			((Control)this).get_Controls().Add((Control)(object)label7);
			((Control)this).get_Controls().Add((Control)(object)label6);
			((Control)this).get_Controls().Add((Control)(object)label5);
			((Control)this).get_Controls().Add((Control)(object)label4);
			((Control)this).get_Controls().Add((Control)(object)label3);
			((Control)this).get_Controls().Add((Control)(object)lblAngle);
			((Control)this).get_Controls().Add((Control)(object)picStars);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)panel1);
			((Control)this).get_Controls().Add((Control)(object)label2);
			((Control)this).get_Controls().Add((Control)(object)chkOnTop);
			((Control)this).get_Controls().Add((Control)(object)label20);
			((Control)this).get_Controls().Add((Control)(object)label12);
			((Control)this).get_Controls().Add((Control)(object)label11);
			((Control)this).get_Controls().Add((Control)(object)label10);
			((Control)this).get_Controls().Add((Control)(object)label9);
			((Control)this).get_Controls().Add((Control)(object)PanelCentre);
			((Control)this).get_Controls().Add((Control)(object)label19);
			((Control)this).get_Controls().Add((Control)(object)label14);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationEphemStarChart", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_Icon((Icon)componentResourceManager.GetObject("$this.Icon"));
			((Form)this).set_Location(Settings.Default.LocationEphemStarChart);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_Name("StarChart");
			((Control)this).set_Text("Star Chart");
			((Form)this).add_FormClosed(new FormClosedEventHandler(StarChart_FormClosed));
			((Form)this).add_Load((EventHandler)StarChart_Load);
			((Control)this).add_Resize((EventHandler)StarChart_Resize);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)PanelCentre).ResumeLayout(false);
			((Control)PanelCentre).PerformLayout();
			((ISupportInitialize)trackOpacity).EndInit();
			((ISupportInitialize)picStars).EndInit();
			((ISupportInitialize)tBarRotate).EndInit();
			((Control)panel1).ResumeLayout(false);
			((Control)panel2).ResumeLayout(false);
			((Control)panel2).PerformLayout();
			((Control)pnlCompStars).ResumeLayout(false);
			((Control)pnlCompStars).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
