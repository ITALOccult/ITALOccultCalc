using System;
using System.ComponentModel;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Drawing.Printing;
using System.IO;
using System.Reflection;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult
{
	public class LunarOccultation_GrazePathMap : Form
	{
		private bool FormCreated;

		private bool ShowToolTip = true;

		private bool GetToolTip = true;

		private readonly string AppPath;

		private const double Radian = 180.0 / Math.PI;

		private int MouseX;

		private int MouseY;

		private int FirstLongitude;

		private int LastLongitude;

		private int NorthLatitude;

		private int SouthLatitude;

		private IContainer components;

		private MenuStrip menuStrip1;

		private PictureBox picPath;

		private Button cmdRePlot;

		private Label label4;

		private Label label3;

		private Label label2;

		private Label label1;

		private NumericUpDown updnSouth;

		private NumericUpDown updnNorth;

		private NumericUpDown updnEast;

		private NumericUpDown updnWest;

		private Label label5;

		internal ComboBox cmbNames;

		internal ComboBox cmbSiteFiles;

		private ToolStripMenuItem withMapToolStripMenuItem;

		private ToolStripMenuItem showCoordinatesInTooltipToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator2;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem printPreviewToolStripMenuItem;

		private ToolStripMenuItem printToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripMenuItem bWToolStripMenuItem;

		private ToolTip toolTip1;

		private Label label7;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripMenuItem usePresetSiteToolStripMenuItem;

		private ToolStripComboBox toolStripComboBox1;

		public LunarOccultation_GrazePathMap()
		{
			InitializeComponent();
			AppPath = Utilities.AppPath;
		}

		private void LunarOccultation_GrazePathMap_Resize(object sender, EventArgs e)
		{
			if (!((((Control)this).get_Width() < 200) | (((Control)this).get_Height() < 300)))
			{
				((Control)picPath).set_Width(((Control)this).get_Width() - 20);
				((Control)picPath).set_Height(((Control)this).get_Height() - 115);
				if (FormCreated)
				{
					DrawPath();
				}
			}
		}

		private void LunarOccultation_GrazePathMap_Load(object sender, EventArgs e)
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
			bWToolStripMenuItem.set_Checked(Settings.Default.BWFlag);
			string[] files = Directory.GetFiles(AppPath + "\\Sites", "*.site");
			foreach (string path in files)
			{
				cmbSiteFiles.get_Items().Add((object)Path.GetFileName(path));
			}
			cmbSiteFiles.get_Items().Add((object)"   don't plot");
			((ListControl)cmbSiteFiles).set_SelectedIndex(0);
			string siteFileForMultiMap = Settings.Default.SiteFileForMultiMap;
			for (int j = 0; j < cmbSiteFiles.get_Items().get_Count(); j++)
			{
				if (siteFileForMultiMap == cmbSiteFiles.get_Items().get_Item(j).ToString())
				{
					((ListControl)cmbSiteFiles).set_SelectedIndex(j);
					break;
				}
			}
			((ListControl)cmbNames).set_SelectedIndex(0);
			toolStripComboBox1.get_Items().Clear();
			toolStripComboBox1.get_Items().Add((object)string.Format("{0,4:F1} to {1,4:F1}; {2,3:F1} to {3,3:F1}", -3, 3, 2, -2));
			toolStripComboBox1.get_Items().Add((object)string.Format("{0,4:F1} to {1,4:F1}; {2,3:F1} to {3,3:F1}", Settings.Default.MapWest1, Settings.Default.MapEast1, Settings.Default.MapNorth1, Settings.Default.MapSouth1));
			toolStripComboBox1.get_Items().Add((object)string.Format("{0,4:F1} to {1,4:F1}; {2,3:F1} to {3,3:F1}", Settings.Default.MapWest2, Settings.Default.MapEast2, Settings.Default.MapNorth2, Settings.Default.MapSouth2));
			toolStripComboBox1.get_Items().Add((object)string.Format("{0,4:F1} to {1,4:F1}; {2,3:F1} to {3,3:F1}", Settings.Default.MapWest3, Settings.Default.MapEast3, Settings.Default.MapNorth3, Settings.Default.MapSouth3));
			toolStripComboBox1.get_Items().Add((object)string.Format("{0,4:F1} to {1,4:F1}; {2,3:F1} to {3,3:F1}", Settings.Default.MapWest4, Settings.Default.MapEast4, Settings.Default.MapNorth4, Settings.Default.MapSouth4));
			toolStripComboBox1.get_Items().Add((object)string.Format("{0,4:F1} to {1,4:F1}; {2,3:F1} to {3,3:F1}", Settings.Default.MapWest5, Settings.Default.MapEast5, Settings.Default.MapNorth5, Settings.Default.MapSouth5));
			toolStripComboBox1.get_Items().Add((object)string.Format("{0,4:F1} to {1,4:F1}; {2,3:F1} to {3,3:F1}", Settings.Default.MapWest6, Settings.Default.MapEast6, Settings.Default.MapNorth6, Settings.Default.MapSouth6));
			toolStripComboBox1.get_Items().Add((object)string.Format("{0,4:F1} to {1,4:F1}; {2,3:F1} to {3,3:F1}", Settings.Default.MapWest7, Settings.Default.MapEast7, Settings.Default.MapNorth7, Settings.Default.MapSouth7));
			toolStripComboBox1.get_Items().Add((object)string.Format("{0,4:F1} to {1,4:F1}; {2,3:F1} to {3,3:F1}", Settings.Default.MapWest8, Settings.Default.MapEast8, Settings.Default.MapNorth8, Settings.Default.MapSouth8));
			if (Settings.Default.MapRegionUseFirst)
			{
				toolStripComboBox1.set_SelectedIndex(1);
			}
			else
			{
				toolStripComboBox1.set_SelectedIndex(0);
			}
			FormCreated = true;
		}

		internal void SetPlotBounds()
		{
			int num = (int)LunarOccultations.LunarPrediction.lstPrediction_Index[((ListControl)LunarOccultations.LunarPrediction.lstPrediction).get_SelectedIndex()];
			if (num >= 0)
			{
				FirstLongitude = (int)LunarOccultations.GrazePredictionLines[0].GrazeLongitude - 5;
				LastLongitude = (int)LunarOccultations.GrazePredictionLines[LunarOccultations.GrazePredictionLines.Count - 1].GrazeLongitude + 5;
				if ((int)LunarOccultations.GrazePredictionLines[num].GrazeLongitude - 3 > FirstLongitude)
				{
					FirstLongitude = (int)LunarOccultations.GrazePredictionLines[num].GrazeLongitude - 3;
				}
				if ((int)LunarOccultations.GrazePredictionLines[num].GrazeLongitude + 3 < LastLongitude)
				{
					LastLongitude = (int)LunarOccultations.GrazePredictionLines[num].GrazeLongitude + 3;
				}
				updnWest.set_Value((decimal)FirstLongitude);
				updnEast.set_Value((decimal)LastLongitude);
				updnNorth.set_Value((decimal)((int)LunarOccultations.GrazePredictionLines[num].GrazeLatitude + 2));
				updnSouth.set_Value((decimal)((int)LunarOccultations.GrazePredictionLines[num].GrazeLatitude - 2));
				NorthLatitude = (int)updnNorth.get_Value();
				SouthLatitude = (int)updnSouth.get_Value();
				toolStripComboBox1.get_Items().RemoveAt(0);
				toolStripComboBox1.get_Items().Insert(0, (object)string.Format("{0,4:F1} to {1,4:F1}; {2,3:F1} to {3,3:F1}", FirstLongitude, LastLongitude, NorthLatitude, SouthLatitude));
				toolStripComboBox1.set_SelectedIndex(0);
				if (Settings.Default.MapRegionUseFirst & (toolStripComboBox1.get_Items().get_Count() > 0))
				{
					toolStripComboBox1.set_SelectedIndex(1);
				}
			}
		}

		private void cmdRePlot_Click(object sender, EventArgs e)
		{
			DrawPath();
		}

		private void PrintGrazePath(object sender, PrintPageEventArgs e)
		{
			Graphics graphics = e.Graphics;
			int width = e.MarginBounds.Width;
			int height = e.MarginBounds.Height;
			Plot(graphics, width, height, Printer: true);
		}

		internal void DrawPath()
		{
			int width = ((Control)picPath).get_Width();
			int height = ((Control)picPath).get_Height();
			Bitmap image = new Bitmap(width, height);
			Graphics graphics = Graphics.FromImage(image);
			Plot(graphics, width, height, Printer: false);
			picPath.set_Image((Image)image);
			graphics.Dispose();
		}

		private void Plot(Graphics formGraphics, int ChartWidth, int ChartHeight, bool Printer)
		{
			float x = 0f;
			float y = 0f;
			Font font = new Font("Courier New", 8f);
			new Font("Courier New", 12f, FontStyle.Bold);
			if (Settings.Default.GraphicsSmoothed)
			{
				formGraphics.SmoothingMode = SmoothingMode.AntiAlias;
			}
			Pen pen;
			Pen pen2;
			Pen pen3;
			Pen pen4;
			Brush brush;
			if (Settings.Default.BWFlag || Printer)
			{
				new Pen(Brushes.Black, 1f);
				pen = new Pen(Brushes.Black, 1f);
				pen2 = new Pen(Brushes.Black, 1f);
				pen3 = new Pen(Brushes.Black, 1f);
				pen4 = new Pen(Brushes.Black, 1f);
				_ = Brushes.Black;
				brush = Brushes.Black;
			}
			else
			{
				new Pen(Brushes.WhiteSmoke, 1f);
				pen = new Pen(Brushes.WhiteSmoke, 1f);
				pen2 = new Pen(Brushes.Magenta, 1f);
				pen3 = new Pen(Brushes.Orange, 1f);
				pen4 = new Pen(Brushes.GreenYellow, 1f);
				_ = Brushes.Yellow;
				brush = Brushes.LightGray;
			}
			if (Settings.Default.BWFlag || Printer)
			{
				formGraphics.Clear(Color.White);
			}
			else
			{
				formGraphics.Clear(Color.Black);
			}
			double num = (double)updnWest.get_Value();
			double num2 = (double)updnEast.get_Value();
			double northLatitude_deg = (double)updnNorth.get_Value();
			double southLatitude_deg = (double)updnSouth.get_Value();
			if (num > num2)
			{
				num2 += 360.0;
			}
			Maps.MapPlot(formGraphics, (float)ChartWidth / 2f, (float)ChartHeight / 2f, num, num2, northLatitude_deg, southLatitude_deg, PlotCities: false, Settings.Default.BWFlag || Printer);
			int num3 = LunarOccultations.GrazePredictionLines[0].GrazeInnerOuterLimit;
			for (int i = 0; i < LunarOccultations.GrazePredictionLines.Count; i++)
			{
				int grazeInnerOuterLimit = LunarOccultations.GrazePredictionLines[i].GrazeInnerOuterLimit;
				Maps.MapProjection(LunarOccultations.GrazePredictionLines[i].GrazeLongitude, LunarOccultations.GrazePredictionLines[i].GrazeLatitude, out var x2, out var y2);
				if (i > 0 && grazeInnerOuterLimit == num3)
				{
					if (grazeInnerOuterLimit >= 0)
					{
						formGraphics.DrawLine(pen, x, y, x2, y2);
					}
					else
					{
						formGraphics.DrawLine(pen2, x, y, x2, y2);
					}
				}
				x = x2;
				y = y2;
				num3 = grazeInnerOuterLimit;
			}
			string path = AppPath + "\\Sites\\" + Settings.Default.SiteFileForMultiMap;
			int selectedIndex = ((ListControl)cmbNames).get_SelectedIndex();
			if (File.Exists(path))
			{
				Sites sites = new Sites();
				float num4 = 0f;
				int num5 = 0;
				StreamReader streamReader = new StreamReader(path);
				do
				{
					sites.Read_SiteFile(streamReader.ReadLine());
					if (sites.PlotOnMap > num5)
					{
						Maps.MapProjection(sites.Longitude, sites.Latitude, out var x3, out var y3);
						if (sites.PlotOnMap <= 3)
						{
							num4 = sites.PlotOnMap switch
							{
								1 => 1f, 
								2 => 2f, 
								3 => 4f, 
								_ => 1f, 
							};
							formGraphics.DrawEllipse(pen3, x3 - num4, y3 - num4, 2f * num4, 2f * num4);
						}
						else if (sites.PlotOnMap > 3)
						{
							num4 = 1.5f;
							formGraphics.DrawRectangle(pen4, x3 - num4, y3 - num4, 2f * num4, 2f * num4);
						}
						if ((sites.PlotOnMap >= selectedIndex || selectedIndex == 1) && selectedIndex > 0)
						{
							formGraphics.DrawString(sites.ShortName, font, brush, x3 + num4, y3 - 5f);
						}
					}
				}
				while (!streamReader.EndOfStream);
				streamReader.Close();
			}
			font = new Font("Times New Roman", 7f, FontStyle.Regular);
			formGraphics.DrawString("Occult " + Assembly.GetExecutingAssembly().GetName(copiedName: false).Version, font, brush, 6f, ChartHeight - 12);
		}

		private void showCoordinatesInTooltipToolStripMenuItem_Click(object sender, EventArgs e)
		{
			showCoordinatesInTooltipToolStripMenuItem.set_Checked(!showCoordinatesInTooltipToolStripMenuItem.get_Checked());
			ShowToolTip = showCoordinatesInTooltipToolStripMenuItem.get_Checked();
		}

		private void copyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetImage(picPath.get_Image());
		}

		private void printPreviewToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0000: Unknown result type (might be due to invalid IL or missing references)
			//IL_0006: Expected O, but got Unknown
			//IL_000c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			//IL_002b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0032: Unknown result type (might be due to invalid IL or missing references)
			//IL_0038: Invalid comparison between Unknown and I4
			//IL_004d: Unknown result type (might be due to invalid IL or missing references)
			PrintPreviewDialog val = new PrintPreviewDialog();
			PrintDocument printDocument = new PrintDocument();
			PrintDialog val2 = new PrintDialog();
			val2.set_UseEXDialog(true);
			printDocument.DefaultPageSettings.Landscape = true;
			val.set_Document(printDocument);
			val2.set_Document(printDocument);
			if ((int)((CommonDialog)val2).ShowDialog() == 1)
			{
				printDocument.PrintPage += PrintGrazePath;
				((Form)val).ShowDialog();
				DrawPath();
			}
		}

		private void printToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0006: Unknown result type (might be due to invalid IL or missing references)
			//IL_000b: Unknown result type (might be due to invalid IL or missing references)
			//IL_001e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0025: Unknown result type (might be due to invalid IL or missing references)
			//IL_002b: Invalid comparison between Unknown and I4
			PrintDocument printDocument = new PrintDocument();
			PrintDialog val = new PrintDialog();
			val.set_UseEXDialog(true);
			printDocument.DefaultPageSettings.Landscape = true;
			val.set_Document(printDocument);
			if ((int)((CommonDialog)val).ShowDialog() == 1)
			{
				printDocument.PrintPage += PrintGrazePath;
				printDocument.Print();
				DrawPath();
			}
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings.Default.Save_LunarPredictions = Output.SaveGraphic(picPath.get_Image(), "Graze path Plot", Settings.Default.Save_LunarPredictions);
		}

		private void picPath_MouseMove(object sender, MouseEventArgs e)
		{
			Maps.InverseMapProjection(out var Longitude_deg, out var Latitude_deg, e.get_X(), e.get_Y());
			MouseX = e.get_X();
			MouseY = e.get_Y();
			if (ShowToolTip)
			{
				GetToolTip = !GetToolTip;
				if (GetToolTip)
				{
					toolTip1.SetToolTip((Control)(object)picPath, Longitude_deg.ToString("+0.00  ;-0.00  ") + Latitude_deg.ToString("+0.00;-0.00"));
				}
			}
			else
			{
				toolTip1.Hide((IWin32Window)(object)picPath);
			}
		}

		private void bWToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings.Default.BWFlag = !Settings.Default.BWFlag;
			bWToolStripMenuItem.set_Checked(Settings.Default.BWFlag);
			DrawPath();
		}

		private void cmbSiteFiles_SelectedValueChanged(object sender, EventArgs e)
		{
			if (FormCreated)
			{
				Settings.Default.SiteFileForMultiMap = cmbSiteFiles.get_Items().get_Item(((ListControl)cmbSiteFiles).get_SelectedIndex()).ToString();
				DrawPath();
			}
		}

		private void cmbNames_SelectedIndexChanged(object sender, EventArgs e)
		{
			if (FormCreated)
			{
				DrawPath();
			}
		}

		private void picPath_DoubleClick(object sender, EventArgs e)
		{
			Maps.InverseMapProjection(out var Longitude_deg, out var Latitude_deg, MouseX, MouseY);
			double num = Longitude_deg - (double)(updnWest.get_Value() + updnEast.get_Value()) / 2.0;
			double num2 = Latitude_deg - (double)(updnNorth.get_Value() + updnSouth.get_Value()) / 2.0;
			updnWest.set_Value(updnWest.get_Value() + (decimal)num);
			updnEast.set_Value(updnEast.get_Value() + (decimal)num);
			updnNorth.set_Value(updnNorth.get_Value() + (decimal)num2);
			updnSouth.set_Value(updnSouth.get_Value() + (decimal)num2);
			DrawPath();
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Lunar Occultations - Graze path map");
		}

		private void toolStripComboBox1_SelectedIndexChanged(object sender, EventArgs e)
		{
			switch (toolStripComboBox1.get_SelectedIndex())
			{
			case 0:
				updnWest.set_Value((decimal)FirstLongitude);
				updnEast.set_Value((decimal)LastLongitude);
				updnNorth.set_Value((decimal)NorthLatitude);
				updnSouth.set_Value((decimal)SouthLatitude);
				break;
			case 1:
				updnWest.set_Value(Settings.Default.MapWest1);
				updnEast.set_Value(Settings.Default.MapEast1);
				updnNorth.set_Value(Settings.Default.MapNorth1);
				updnSouth.set_Value(Settings.Default.MapSouth1);
				break;
			case 2:
				updnWest.set_Value(Settings.Default.MapWest2);
				updnEast.set_Value(Settings.Default.MapEast2);
				updnNorth.set_Value(Settings.Default.MapNorth2);
				updnSouth.set_Value(Settings.Default.MapSouth2);
				break;
			case 3:
				updnWest.set_Value(Settings.Default.MapWest3);
				updnEast.set_Value(Settings.Default.MapEast3);
				updnNorth.set_Value(Settings.Default.MapNorth3);
				updnSouth.set_Value(Settings.Default.MapSouth3);
				break;
			case 4:
				updnWest.set_Value(Settings.Default.MapWest4);
				updnEast.set_Value(Settings.Default.MapEast4);
				updnNorth.set_Value(Settings.Default.MapNorth4);
				updnSouth.set_Value(Settings.Default.MapSouth4);
				break;
			case 5:
				updnWest.set_Value(Settings.Default.MapWest5);
				updnEast.set_Value(Settings.Default.MapEast5);
				updnNorth.set_Value(Settings.Default.MapNorth5);
				updnSouth.set_Value(Settings.Default.MapSouth5);
				break;
			case 6:
				updnWest.set_Value(Settings.Default.MapWest6);
				updnEast.set_Value(Settings.Default.MapEast6);
				updnNorth.set_Value(Settings.Default.MapNorth6);
				updnSouth.set_Value(Settings.Default.MapSouth6);
				break;
			case 7:
				updnWest.set_Value(Settings.Default.MapWest7);
				updnEast.set_Value(Settings.Default.MapEast7);
				updnNorth.set_Value(Settings.Default.MapNorth7);
				updnSouth.set_Value(Settings.Default.MapSouth7);
				break;
			case 8:
				updnWest.set_Value(Settings.Default.MapWest8);
				updnEast.set_Value(Settings.Default.MapEast8);
				updnNorth.set_Value(Settings.Default.MapNorth8);
				updnSouth.set_Value(Settings.Default.MapSouth8);
				break;
			}
			if (FormCreated)
			{
				DrawPath();
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
			//IL_0125: Unknown result type (might be due to invalid IL or missing references)
			//IL_012f: Expected O, but got Unknown
			//IL_0130: Unknown result type (might be due to invalid IL or missing references)
			//IL_013a: Expected O, but got Unknown
			//IL_06ce: Unknown result type (might be due to invalid IL or missing references)
			//IL_06d8: Expected O, but got Unknown
			//IL_0f0f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0f19: Expected O, but got Unknown
			components = new Container();
			menuStrip1 = new MenuStrip();
			withMapToolStripMenuItem = new ToolStripMenuItem();
			bWToolStripMenuItem = new ToolStripMenuItem();
			showCoordinatesInTooltipToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator2 = new ToolStripSeparator();
			copyToolStripMenuItem = new ToolStripMenuItem();
			printPreviewToolStripMenuItem = new ToolStripMenuItem();
			printToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			usePresetSiteToolStripMenuItem = new ToolStripMenuItem();
			toolStripComboBox1 = new ToolStripComboBox();
			helpToolStripMenuItem = new ToolStripMenuItem();
			picPath = new PictureBox();
			cmdRePlot = new Button();
			label4 = new Label();
			label3 = new Label();
			label2 = new Label();
			label1 = new Label();
			updnSouth = new NumericUpDown();
			updnNorth = new NumericUpDown();
			updnEast = new NumericUpDown();
			updnWest = new NumericUpDown();
			label5 = new Label();
			cmbNames = new ComboBox();
			cmbSiteFiles = new ComboBox();
			toolTip1 = new ToolTip(components);
			label7 = new Label();
			((Control)menuStrip1).SuspendLayout();
			((ISupportInitialize)picPath).BeginInit();
			((ISupportInitialize)updnSouth).BeginInit();
			((ISupportInitialize)updnNorth).BeginInit();
			((ISupportInitialize)updnEast).BeginInit();
			((ISupportInitialize)updnWest).BeginInit();
			((Control)this).SuspendLayout();
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[4]
			{
				(ToolStripItem)withMapToolStripMenuItem,
				(ToolStripItem)usePresetSiteToolStripMenuItem,
				(ToolStripItem)toolStripComboBox1,
				(ToolStripItem)helpToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(748, 27));
			((Control)menuStrip1).set_TabIndex(0);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withMapToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[7]
			{
				(ToolStripItem)bWToolStripMenuItem,
				(ToolStripItem)showCoordinatesInTooltipToolStripMenuItem,
				(ToolStripItem)toolStripSeparator2,
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)printPreviewToolStripMenuItem,
				(ToolStripItem)printToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem
			});
			((ToolStripItem)withMapToolStripMenuItem).set_Name("withMapToolStripMenuItem");
			((ToolStripItem)withMapToolStripMenuItem).set_Size(new Size(78, 23));
			((ToolStripItem)withMapToolStripMenuItem).set_Text("with Map...");
			((ToolStripItem)bWToolStripMenuItem).set_Image((Image)Resources.ColorHS);
			((ToolStripItem)bWToolStripMenuItem).set_Name("bWToolStripMenuItem");
			((ToolStripItem)bWToolStripMenuItem).set_Size(new Size(221, 22));
			((ToolStripItem)bWToolStripMenuItem).set_Text("B&&W");
			((ToolStripItem)bWToolStripMenuItem).add_Click((EventHandler)bWToolStripMenuItem_Click);
			showCoordinatesInTooltipToolStripMenuItem.set_Checked(true);
			showCoordinatesInTooltipToolStripMenuItem.set_CheckState((CheckState)1);
			((ToolStripItem)showCoordinatesInTooltipToolStripMenuItem).set_Name("showCoordinatesInTooltipToolStripMenuItem");
			((ToolStripItem)showCoordinatesInTooltipToolStripMenuItem).set_Size(new Size(221, 22));
			((ToolStripItem)showCoordinatesInTooltipToolStripMenuItem).set_Text("Show Coordinates in tooltip");
			((ToolStripItem)showCoordinatesInTooltipToolStripMenuItem).add_Click((EventHandler)showCoordinatesInTooltipToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator2).set_Name("toolStripSeparator2");
			((ToolStripItem)toolStripSeparator2).set_Size(new Size(218, 6));
			((ToolStripItem)copyToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyToolStripMenuItem).set_Name("copyToolStripMenuItem");
			copyToolStripMenuItem.set_ShortcutKeys((Keys)131139);
			((ToolStripItem)copyToolStripMenuItem).set_Size(new Size(221, 22));
			((ToolStripItem)copyToolStripMenuItem).set_Text("&Copy");
			((ToolStripItem)copyToolStripMenuItem).add_Click((EventHandler)copyToolStripMenuItem_Click);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Image((Image)Resources.PrintPreview);
			((ToolStripItem)printPreviewToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Name("printPreviewToolStripMenuItem");
			((ToolStripItem)printPreviewToolStripMenuItem).set_Size(new Size(221, 22));
			((ToolStripItem)printPreviewToolStripMenuItem).set_Text("Print Preview");
			((ToolStripItem)printPreviewToolStripMenuItem).add_Click((EventHandler)printPreviewToolStripMenuItem_Click);
			((ToolStripItem)printToolStripMenuItem).set_Image((Image)Resources.Print);
			((ToolStripItem)printToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printToolStripMenuItem).set_Name("printToolStripMenuItem");
			printToolStripMenuItem.set_ShortcutKeys((Keys)131152);
			((ToolStripItem)printToolStripMenuItem).set_Size(new Size(221, 22));
			((ToolStripItem)printToolStripMenuItem).set_Text("&Print");
			((ToolStripItem)printToolStripMenuItem).add_Click((EventHandler)printToolStripMenuItem_Click);
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			saveToolStripMenuItem.set_ShortcutKeys((Keys)131155);
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(221, 22));
			((ToolStripItem)saveToolStripMenuItem).set_Text("&Save");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
			((ToolStripItem)usePresetSiteToolStripMenuItem).set_Name("usePresetSiteToolStripMenuItem");
			((ToolStripItem)usePresetSiteToolStripMenuItem).set_Size(new Size(133, 23));
			((ToolStripItem)usePresetSiteToolStripMenuItem).set_Text("Use pre-set values =>");
			toolStripComboBox1.set_DropDownStyle((ComboBoxStyle)2);
			toolStripComboBox1.set_MaxDropDownItems(20);
			((ToolStripItem)toolStripComboBox1).set_Name("toolStripComboBox1");
			((ToolStripItem)toolStripComboBox1).set_Size(new Size(181, 23));
			toolStripComboBox1.add_SelectedIndexChanged((EventHandler)toolStripComboBox1_SelectedIndexChanged);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(75, 23));
			((ToolStripItem)helpToolStripMenuItem).set_Text("  &Help   ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((Control)picPath).set_Location(new Point(7, 74));
			((Control)picPath).set_Name("picPath");
			((Control)picPath).set_Size(new Size(736, 456));
			picPath.set_TabIndex(1);
			picPath.set_TabStop(false);
			((Control)picPath).add_DoubleClick((EventHandler)picPath_DoubleClick);
			((Control)picPath).add_MouseMove(new MouseEventHandler(picPath_MouseMove));
			((Control)cmdRePlot).set_Anchor((AnchorStyles)1);
			((Control)cmdRePlot).set_Location(new Point(592, 36));
			((Control)cmdRePlot).set_Name("cmdRePlot");
			((Control)cmdRePlot).set_Size(new Size(64, 20));
			((Control)cmdRePlot).set_TabIndex(18);
			((Control)cmdRePlot).set_Text("Re-draw");
			((ButtonBase)cmdRePlot).set_UseVisualStyleBackColor(true);
			((Control)cmdRePlot).add_Click((EventHandler)cmdRePlot_Click);
			((Control)label4).set_Anchor((AnchorStyles)1);
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Location(new Point(463, 38));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(35, 13));
			((Control)label4).set_TabIndex(16);
			((Control)label4).set_Text("South");
			((Control)label3).set_Anchor((AnchorStyles)1);
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Location(new Point(334, 38));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(33, 13));
			((Control)label3).set_TabIndex(14);
			((Control)label3).set_Text("North");
			((Control)label2).set_Anchor((AnchorStyles)1);
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(199, 39));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(28, 13));
			((Control)label2).set_TabIndex(12);
			((Control)label2).set_Text("East");
			((Control)label1).set_Anchor((AnchorStyles)1);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(56, 38));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(32, 13));
			((Control)label1).set_TabIndex(10);
			((Control)label1).set_Text("West");
			((Control)updnSouth).set_Anchor((AnchorStyles)1);
			updnSouth.set_DecimalPlaces(1);
			((Control)updnSouth).set_Location(new Point(498, 35));
			updnSouth.set_Maximum(new decimal(new int[4] { 90, 0, 0, 0 }));
			updnSouth.set_Minimum(new decimal(new int[4] { 90, 0, 0, -2147483648 }));
			((Control)updnSouth).set_Name("updnSouth");
			((Control)updnSouth).set_Size(new Size(48, 20));
			((Control)updnSouth).set_TabIndex(17);
			((Control)updnNorth).set_Anchor((AnchorStyles)1);
			updnNorth.set_DecimalPlaces(1);
			((Control)updnNorth).set_Location(new Point(367, 35));
			updnNorth.set_Maximum(new decimal(new int[4] { 90, 0, 0, 0 }));
			updnNorth.set_Minimum(new decimal(new int[4] { 90, 0, 0, -2147483648 }));
			((Control)updnNorth).set_Name("updnNorth");
			((Control)updnNorth).set_Size(new Size(52, 20));
			((Control)updnNorth).set_TabIndex(15);
			((Control)updnEast).set_Anchor((AnchorStyles)1);
			updnEast.set_DecimalPlaces(1);
			((Control)updnEast).set_Location(new Point(227, 35));
			updnEast.set_Maximum(new decimal(new int[4] { 180, 0, 0, 0 }));
			updnEast.set_Minimum(new decimal(new int[4] { 180, 0, 0, -2147483648 }));
			((Control)updnEast).set_Name("updnEast");
			((Control)updnEast).set_Size(new Size(65, 20));
			((Control)updnEast).set_TabIndex(13);
			((Control)updnWest).set_Anchor((AnchorStyles)1);
			updnWest.set_DecimalPlaces(1);
			((Control)updnWest).set_Location(new Point(88, 35));
			updnWest.set_Maximum(new decimal(new int[4] { 180, 0, 0, 0 }));
			updnWest.set_Minimum(new decimal(new int[4] { 180, 0, 0, -2147483648 }));
			((Control)updnWest).set_Name("updnWest");
			((Control)updnWest).set_Size(new Size(65, 20));
			((Control)updnWest).set_TabIndex(11);
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Location(new Point(471, 6));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(49, 13));
			((Control)label5).set_TabIndex(34);
			((Control)label5).set_Text("Plot sites");
			cmbNames.set_DropDownStyle((ComboBoxStyle)2);
			((ListControl)cmbNames).set_FormattingEnabled(true);
			cmbNames.get_Items().AddRange(new object[4] { "never", "all", "many", "some" });
			((Control)cmbNames).set_Location(new Point(640, 2));
			cmbNames.set_MaxDropDownItems(20);
			((Control)cmbNames).set_Name("cmbNames");
			((Control)cmbNames).set_Size(new Size(55, 21));
			((Control)cmbNames).set_TabIndex(33);
			cmbNames.add_SelectedIndexChanged((EventHandler)cmbNames_SelectedIndexChanged);
			cmbSiteFiles.set_DropDownStyle((ComboBoxStyle)2);
			((ListControl)cmbSiteFiles).set_FormattingEnabled(true);
			((Control)cmbSiteFiles).set_Location(new Point(520, 2));
			cmbSiteFiles.set_MaxDropDownItems(20);
			((Control)cmbSiteFiles).set_Name("cmbSiteFiles");
			((Control)cmbSiteFiles).set_Size(new Size(114, 21));
			cmbSiteFiles.set_Sorted(true);
			((Control)cmbSiteFiles).set_TabIndex(32);
			((ListControl)cmbSiteFiles).add_SelectedValueChanged((EventHandler)cmbSiteFiles_SelectedValueChanged);
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Font(new Font("Microsoft Sans Serif", 7.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label7).set_Location(new Point(4, 60));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(160, 13));
			((Control)label7).set_TabIndex(36);
			((Control)label7).set_Text("Double-click to re-center the map");
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(748, 537));
			((Control)this).get_Controls().Add((Control)(object)label7);
			((Control)this).get_Controls().Add((Control)(object)label5);
			((Control)this).get_Controls().Add((Control)(object)cmbNames);
			((Control)this).get_Controls().Add((Control)(object)cmbSiteFiles);
			((Control)this).get_Controls().Add((Control)(object)cmdRePlot);
			((Control)this).get_Controls().Add((Control)(object)label4);
			((Control)this).get_Controls().Add((Control)(object)label3);
			((Control)this).get_Controls().Add((Control)(object)label2);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)updnSouth);
			((Control)this).get_Controls().Add((Control)(object)updnNorth);
			((Control)this).get_Controls().Add((Control)(object)updnEast);
			((Control)this).get_Controls().Add((Control)(object)updnWest);
			((Control)this).get_Controls().Add((Control)(object)picPath);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationLunarGrazeMap", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_Location(Settings.Default.LocationLunarGrazeMap);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_Name("LunarOccultation_GrazePathMap");
			((Control)this).set_Text("Lunar occultation - Map of graze path");
			((Form)this).add_Load((EventHandler)LunarOccultation_GrazePathMap_Load);
			((Control)this).add_Resize((EventHandler)LunarOccultation_GrazePathMap_Resize);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((ISupportInitialize)picPath).EndInit();
			((ISupportInitialize)updnSouth).EndInit();
			((ISupportInitialize)updnNorth).EndInit();
			((ISupportInitialize)updnEast).EndInit();
			((ISupportInitialize)updnWest).EndInit();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
