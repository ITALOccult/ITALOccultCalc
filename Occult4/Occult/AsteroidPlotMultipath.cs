using System;
using System.ComponentModel;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Drawing.Printing;
using System.IO;
using System.Reflection;
using System.Windows.Forms;
using Occult.Asteroids;
using Occult.Properties;

namespace Occult
{
	public class AsteroidPlotMultipath : Form
	{
		private readonly string AppPath;

		private const double Radian = 180.0 / Math.PI;

		private static bool IncludeCenterLine = false;

		private static bool ShowToolTip = true;

		private static bool FormCreated = false;

		private static bool ShowPathLabel = true;

		private bool GetToolTip = true;

		private IContainer components;

		private PictureBox picMultipath;

		private MenuStrip menuStrip1;

		private Label label1;

		private Label label2;

		private Label label3;

		private Label label4;

		private Button cmdRePlot;

		private ToolStripMenuItem drawMapToolStripMenuItem;

		private ToolStripMenuItem IncludeCentreLine;

		private ToolStripMenuItem bWToolStripMenuItem;

		private ToolStripMenuItem colourToolStripMenuItem;

		private ToolStripMenuItem withMapToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem printToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator1;

		private ToolTip toolTip1;

		private ToolStripMenuItem showCoordinatesInTooltipToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator2;

		private ToolStripMenuItem printPreviewToolStripMenuItem;

		private ToolStripMenuItem usePresetSiteToolStripMenuItem;

		private ToolStripComboBox toolStripComboBox1;

		internal ComboBox cmbSiteFiles;

		internal ComboBox cmbNames;

		private Label label5;

		private ToolStripMenuItem helpToolStripMenuItem;

		public ToolStripMenuItem createGoogleKMLFileOfEventsToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator3;

		public NumericUpDown updnWest;

		public NumericUpDown updnEast;

		public NumericUpDown updnNorth;

		public NumericUpDown updnSouth;

		private ToolStripMenuItem excludeThePathLabel;

		public AsteroidPlotMultipath()
		{
			InitializeComponent();
			AppPath = Utilities.AppPath;
		}

		private void AsteroidPlotMultipath_Load(object sender, EventArgs e)
		{
			//IL_0492: Unknown result type (might be due to invalid IL or missing references)
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			FormCreated = false;
			if (((Control)this).get_Top() < -30000)
			{
				((Control)this).set_Top(0);
			}
			if ((double)((Control)this).get_Left() + (double)((Control)this).get_Width() / 1.2 < (double)Screen.GetWorkingArea((Control)(object)this).Left)
			{
				((Control)this).set_Left(Screen.GetWorkingArea((Control)(object)this).Left);
			}
			ShowToolTip = showCoordinatesInTooltipToolStripMenuItem.get_Checked();
			toolStripComboBox1.get_Items().Clear();
			toolStripComboBox1.get_Items().Add((object)string.Format("{0,4:F1} to {1,4:F1}; {2,3:F1} to {3,3:F1}", DisplayMPOccultations.SiteLongitude_Rad * (180.0 / Math.PI) - 3.0, DisplayMPOccultations.SiteLongitude_Rad * (180.0 / Math.PI) + 3.0, DisplayMPOccultations.SiteLatitude_Rad * (180.0 / Math.PI) + 2.0, DisplayMPOccultations.SiteLatitude_Rad * (180.0 / Math.PI) - 2.0));
			toolStripComboBox1.get_Items().Add((object)string.Format("{0,4:F1} to {1,4:F1}; {2,3:F1} to {3,3:F1}", Settings.Default.MapWest1, Settings.Default.MapEast1, Settings.Default.MapNorth1, Settings.Default.MapSouth1));
			toolStripComboBox1.get_Items().Add((object)string.Format("{0,4:F1} to {1,4:F1}; {2,3:F1} to {3,3:F1}", Settings.Default.MapWest2, Settings.Default.MapEast2, Settings.Default.MapNorth2, Settings.Default.MapSouth2));
			toolStripComboBox1.get_Items().Add((object)string.Format("{0,4:F1} to {1,4:F1}; {2,3:F1} to {3,3:F1}", Settings.Default.MapWest3, Settings.Default.MapEast3, Settings.Default.MapNorth3, Settings.Default.MapSouth3));
			toolStripComboBox1.get_Items().Add((object)string.Format("{0,4:F1} to {1,4:F1}; {2,3:F1} to {3,3:F1}", Settings.Default.MapWest4, Settings.Default.MapEast4, Settings.Default.MapNorth4, Settings.Default.MapSouth4));
			toolStripComboBox1.get_Items().Add((object)string.Format("{0,4:F1} to {1,4:F1}; {2,3:F1} to {3,3:F1}", Settings.Default.MapWest5, Settings.Default.MapEast5, Settings.Default.MapNorth5, Settings.Default.MapSouth5));
			toolStripComboBox1.get_Items().Add((object)string.Format("{0,4:F1} to {1,4:F1}; {2,3:F1} to {3,3:F1}", Settings.Default.MapWest6, Settings.Default.MapEast6, Settings.Default.MapNorth6, Settings.Default.MapSouth6));
			toolStripComboBox1.get_Items().Add((object)string.Format("{0,4:F1} to {1,4:F1}; {2,3:F1} to {3,3:F1}", Settings.Default.MapWest7, Settings.Default.MapEast7, Settings.Default.MapNorth7, Settings.Default.MapSouth7));
			toolStripComboBox1.get_Items().Add((object)string.Format("{0,4:F1} to {1,4:F1}; {2,3:F1} to {3,3:F1}", Settings.Default.MapWest8, Settings.Default.MapEast8, Settings.Default.MapNorth8, Settings.Default.MapSouth8));
			toolStripComboBox1.set_SelectedIndex(0);
			((Form)this).set_WindowState(Settings.Default.AsteroidPlotMultiPathWindow);
			bWToolStripMenuItem.set_Checked(Settings.Default.BWFlag);
			colourToolStripMenuItem.set_Checked(!bWToolStripMenuItem.get_Checked());
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
			FormCreated = true;
		}

		private void AsteroidPlotMultipath_FormClosing(object sender, FormClosingEventArgs e)
		{
			//IL_0006: Unknown result type (might be due to invalid IL or missing references)
			Settings.Default.AsteroidPlotMultiPathWindow = ((Form)this).get_WindowState();
		}

		private void AsteroidPlotMultipath_Resize(object sender, EventArgs e)
		{
			if (!((((Control)this).get_Width() < 200) | (((Control)this).get_Height() < 200)))
			{
				((Control)picMultipath).set_Width(((Control)this).get_Width() - 32);
				((Control)picMultipath).set_Height(((Control)this).get_Height() - 115);
				if (FormCreated)
				{
					PlotMultiPath();
				}
			}
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
				printDocument.PrintPage += PrintMultiPath;
				((Form)val).ShowDialog();
				PlotMultiPath();
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
				printDocument.PrintPage += PrintMultiPath;
				printDocument.Print();
				PlotMultiPath();
			}
		}

		private void PrintMultiPath(object sender, PrintPageEventArgs e)
		{
			Graphics graphics = e.Graphics;
			int width = e.MarginBounds.Width;
			int height = e.MarginBounds.Height;
			Plot(graphics, width, height, Printer: true);
		}

		public void PlotMultiPath()
		{
			int width = ((Control)picMultipath).get_Width();
			int height = ((Control)picMultipath).get_Height();
			Bitmap image = new Bitmap(width, height);
			Graphics graphics = Graphics.FromImage(image);
			if (Settings.Default.GraphicsSmoothed)
			{
				graphics.SmoothingMode = SmoothingMode.AntiAlias;
			}
			graphics.Clear(Color.White);
			Plot(graphics, width, height, Printer: false);
			picMultipath.set_Image((Image)image);
			graphics.Dispose();
		}

		private void Plot(Graphics formGraphics, int ChartWidth, int ChartHeight, bool Printer)
		{
			//IL_0024: Unknown result type (might be due to invalid IL or missing references)
			if (!File.Exists(AppPath + "\\Generated Files\\multipath.tmp"))
			{
				MessageBox.Show("A summary must be created before the paths can be plotted", "No Summary file", (MessageBoxButtons)0, (MessageBoxIcon)16);
				return;
			}
			if (Settings.Default.GraphicsSmoothed)
			{
				formGraphics.SmoothingMode = SmoothingMode.AntiAlias;
			}
			Font font = new Font("Courier New", 8f);
			new Font("Courier New", 12f, FontStyle.Bold);
			Font font2 = new Font("Times New Roman", 7f, FontStyle.Regular);
			Pen pen;
			Pen pen2;
			Pen pen3;
			Pen pen4;
			Brush brush;
			Brush brush2;
			if (Settings.Default.BWFlag || Printer)
			{
				pen = new Pen(Brushes.Black, 1f);
				pen2 = new Pen(Brushes.Black, 1f);
				pen3 = new Pen(Brushes.Black, 1f);
				pen4 = new Pen(Brushes.Black, 1f);
				brush = Brushes.Black;
				brush2 = Brushes.Black;
			}
			else
			{
				pen = new Pen(Brushes.WhiteSmoke, 1f);
				pen2 = new Pen(Brushes.WhiteSmoke, 1f);
				pen3 = new Pen(Brushes.Orange, 1f);
				pen4 = new Pen(Brushes.GreenYellow, 1f);
				brush = Brushes.Yellow;
				brush2 = Brushes.LightGray;
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
			double num3 = (double)updnNorth.get_Value();
			double num4 = (double)updnSouth.get_Value();
			if (num > num2)
			{
				num2 += 360.0;
			}
			Maps.MapPlot(formGraphics, (float)ChartWidth / 2f, (float)ChartHeight / 2f, num, num2, num3, num4, PlotCities: false, Settings.Default.BWFlag || Printer);
			double num5 = 0.0;
			double num6 = 0.0;
			float x = 0f;
			float y = 0f;
			float num7 = 0f;
			float num8 = 0f;
			float num9 = (float)((double)(5 * ChartHeight) / (num3 - num4));
			float[] array = new float[360];
			float[] array2 = new float[360];
			pen2.DashPattern = new float[2] { 2f, 2f };
			int num10 = -1;
			int num11 = 1;
			string text = "";
			if (new FileInfo(AppPath + "\\Generated Files\\multipath.tmp").Length < 150)
			{
				return;
			}
			for (int i = num10; i <= num11; i++)
			{
				bool flag = false;
				int num12 = 0;
				using StreamReader streamReader = new StreamReader(AppPath + "\\Generated Files\\multipath.tmp");
				do
				{
					string text2 = streamReader.ReadLine()!.PadRight(4);
					if (text2.Substring(0, 3) == "+++")
					{
						text = text2.Substring(5).Trim();
					}
					else if (text2.Substring(0, 3) != "***")
					{
						if (text2.Length <= 10)
						{
							continue;
						}
						string[] array3 = text2.Split(new char[1] { ',' });
						num5 = double.Parse(array3[3 + 2 * i]);
						num6 = double.Parse(array3[4 + 2 * i]);
						bool num13 = num6 < 100.0;
						Maps.MapProjection(num5, num6, out x, out y);
						bool num14 = num13 && x > 0f - num9 && x < (float)ChartWidth + num9 && y > 0f - num9 && y < (float)ChartHeight + num9;
						if (num14 && flag)
						{
							if (i == -1)
							{
								if (IncludeCenterLine)
								{
									formGraphics.DrawLine(pen2, num7, num8, x, y);
								}
							}
							else
							{
								formGraphics.DrawLine(pen, num7, num8, x, y);
							}
							if (x > 0f && x < (float)ChartWidth && y > 0f && y < (float)ChartHeight)
							{
								array[num12] = x;
								array2[num12] = y;
								num12++;
							}
						}
						num7 = x;
						num8 = y;
						flag = num14;
					}
					else if ((text2.Substring(0, 3) == "***") | streamReader.EndOfStream)
					{
						if (ShowPathLabel && i == -1 && num12 > 0)
						{
							int num15 = (int)(formGraphics.MeasureString(text, font).Width / 2f);
							formGraphics.DrawString(text, font, brush, array[num12 / 2] - (float)num15, array2[num12 / 2] - 5f);
						}
						flag = false;
						num12 = 0;
					}
				}
				while (!streamReader.EndOfStream);
			}
			string path = AppPath + "\\Sites\\" + Settings.Default.SiteFileForMultiMap;
			int selectedIndex = ((ListControl)cmbNames).get_SelectedIndex();
			if (File.Exists(path))
			{
				Sites sites = new Sites();
				float num16 = 0f;
				int num17 = 0;
				StreamReader streamReader2 = new StreamReader(path);
				do
				{
					sites.Read_SiteFile(streamReader2.ReadLine());
					if (sites.PlotOnMap > num17)
					{
						Maps.MapProjection(sites.Longitude, sites.Latitude, out x, out y);
						if (sites.PlotOnMap <= 3)
						{
							num16 = sites.PlotOnMap switch
							{
								1 => 1f, 
								2 => 2f, 
								3 => 4f, 
								_ => 1f, 
							};
							formGraphics.DrawEllipse(pen3, x - num16, y - num16, 2f * num16, 2f * num16);
						}
						else if (sites.PlotOnMap > 3)
						{
							num16 = 1.5f;
							formGraphics.DrawRectangle(pen4, x - num16, y - num16, 2f * num16, 2f * num16);
						}
						if ((sites.PlotOnMap >= selectedIndex || selectedIndex == 1) && selectedIndex > 0)
						{
							formGraphics.DrawString(sites.ShortName, font, brush2, x + num16, y - 5f);
						}
					}
				}
				while (!streamReader2.EndOfStream);
				streamReader2.Close();
			}
			formGraphics.DrawString("Occult " + Assembly.GetExecutingAssembly().GetName(copiedName: false).Version, font2, brush2, 6f, ChartHeight - 12);
			((Control)this).Focus();
		}

		private void cmdRePlot_Click(object sender, EventArgs e)
		{
			PlotMultiPath();
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
		}

		private void copyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetImage(picMultipath.get_Image());
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings.Default.Save_AsteroidPredictions = Output.SaveGraphic(picMultipath.get_Image(), "Multipath Plot", Settings.Default.Save_AsteroidPredictions);
		}

		private void bWToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ToolStripMenuItem obj = bWToolStripMenuItem;
			bool @checked = (Settings.Default.BWFlag = true);
			obj.set_Checked(@checked);
			colourToolStripMenuItem.set_Checked(!Settings.Default.BWFlag);
			PlotMultiPath();
		}

		private void colourToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ToolStripMenuItem obj = bWToolStripMenuItem;
			bool @checked = (Settings.Default.BWFlag = false);
			obj.set_Checked(@checked);
			colourToolStripMenuItem.set_Checked(!Settings.Default.BWFlag);
			PlotMultiPath();
		}

		private void centreLinesOnlyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			bool includeCenterLine;
			IncludeCentreLine.set_Checked(includeCenterLine = !IncludeCentreLine.get_Checked());
			IncludeCenterLine = includeCenterLine;
			PlotMultiPath();
		}

		private void excludeThePathLabel_Click(object sender, EventArgs e)
		{
			excludeThePathLabel.set_Checked(!excludeThePathLabel.get_Checked());
			ShowPathLabel = !excludeThePathLabel.get_Checked();
			PlotMultiPath();
		}

		private void picMultipath_MouseMove(object sender, MouseEventArgs e)
		{
			Maps.InverseMapProjection(out var Longitude_deg, out var Latitude_deg, e.get_X(), e.get_Y());
			if (ShowToolTip)
			{
				GetToolTip = !GetToolTip;
				if (GetToolTip)
				{
					toolTip1.SetToolTip((Control)(object)picMultipath, Longitude_deg.ToString("+0.00  ;-0.00  ") + Latitude_deg.ToString("+0.00;-0.00"));
				}
			}
			else
			{
				toolTip1.Hide((IWin32Window)(object)picMultipath);
			}
		}

		private void showCoordinatesInTooltipToolStripMenuItem_Click(object sender, EventArgs e)
		{
			showCoordinatesInTooltipToolStripMenuItem.set_Checked(!showCoordinatesInTooltipToolStripMenuItem.get_Checked());
			ShowToolTip = showCoordinatesInTooltipToolStripMenuItem.get_Checked();
		}

		private void updnWest_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnWest).Select(0, 10);
		}

		private void updnEast_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnEast).Select(0, 10);
		}

		private void updnNorth_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnNorth).Select(0, 10);
		}

		private void updnSouth_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnSouth).Select(0, 10);
		}

		private void toolStripComboBox1_SelectedIndexChanged(object sender, EventArgs e)
		{
			switch (toolStripComboBox1.get_SelectedIndex())
			{
			case 0:
				try
				{
					updnWest.set_Value((decimal)(OccultationElements.SiteLongitudeTest * (180.0 / Math.PI) - 3.0));
					updnEast.set_Value((decimal)(OccultationElements.SiteLongitudeTest * (180.0 / Math.PI) + 3.0));
					updnNorth.set_Value((decimal)(OccultationElements.SiteLatitudeTest * (180.0 / Math.PI) + 2.0));
					updnSouth.set_Value((decimal)(OccultationElements.SiteLatitudeTest * (180.0 / Math.PI) - 2.0));
				}
				catch
				{
					updnWest.set_Value((decimal)(DisplayMPOccultations.SiteLongitude_Rad * (180.0 / Math.PI) - 3.0));
					updnEast.set_Value((decimal)(DisplayMPOccultations.SiteLongitude_Rad * (180.0 / Math.PI) + 3.0));
					updnNorth.set_Value((decimal)(DisplayMPOccultations.SiteLatitude_Rad * (180.0 / Math.PI) + 2.0));
					updnSouth.set_Value((decimal)(DisplayMPOccultations.SiteLatitude_Rad * (180.0 / Math.PI) - 2.0));
				}
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
				PlotMultiPath();
			}
		}

		private void cmbSiteFiles_SelectedValueChanged(object sender, EventArgs e)
		{
			if (FormCreated)
			{
				Settings.Default.SiteFileForMultiMap = cmbSiteFiles.get_Items().get_Item(((ListControl)cmbSiteFiles).get_SelectedIndex()).ToString();
				PlotMultiPath();
			}
		}

		private void cmbNames_SelectedIndexChanged(object sender, EventArgs e)
		{
			if (FormCreated)
			{
				PlotMultiPath();
			}
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Asteroid - plot multipaths");
		}

		private void createGoogleKMLFileOfEventsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			DisplayMPOccultations.MultipathKMZfile(OnlyWhereSunIsDown_StarIsUp: false, Show: false);
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
			//IL_095c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0966: Expected O, but got Unknown
			//IL_11ce: Unknown result type (might be due to invalid IL or missing references)
			//IL_11d8: Expected O, but got Unknown
			//IL_11ef: Unknown result type (might be due to invalid IL or missing references)
			//IL_11f9: Expected O, but got Unknown
			//IL_125f: Unknown result type (might be due to invalid IL or missing references)
			//IL_1271: Unknown result type (might be due to invalid IL or missing references)
			//IL_127b: Expected O, but got Unknown
			components = new Container();
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(AsteroidPlotMultipath));
			menuStrip1 = new MenuStrip();
			withMapToolStripMenuItem = new ToolStripMenuItem();
			showCoordinatesInTooltipToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator2 = new ToolStripSeparator();
			copyToolStripMenuItem = new ToolStripMenuItem();
			printPreviewToolStripMenuItem = new ToolStripMenuItem();
			printToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator3 = new ToolStripSeparator();
			createGoogleKMLFileOfEventsToolStripMenuItem = new ToolStripMenuItem();
			drawMapToolStripMenuItem = new ToolStripMenuItem();
			IncludeCentreLine = new ToolStripMenuItem();
			toolStripSeparator1 = new ToolStripSeparator();
			bWToolStripMenuItem = new ToolStripMenuItem();
			colourToolStripMenuItem = new ToolStripMenuItem();
			usePresetSiteToolStripMenuItem = new ToolStripMenuItem();
			toolStripComboBox1 = new ToolStripComboBox();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			picMultipath = new PictureBox();
			updnWest = new NumericUpDown();
			updnEast = new NumericUpDown();
			updnNorth = new NumericUpDown();
			updnSouth = new NumericUpDown();
			label1 = new Label();
			label2 = new Label();
			label3 = new Label();
			label4 = new Label();
			cmdRePlot = new Button();
			toolTip1 = new ToolTip(components);
			cmbSiteFiles = new ComboBox();
			cmbNames = new ComboBox();
			label5 = new Label();
			excludeThePathLabel = new ToolStripMenuItem();
			((Control)menuStrip1).SuspendLayout();
			((ISupportInitialize)picMultipath).BeginInit();
			((ISupportInitialize)updnWest).BeginInit();
			((ISupportInitialize)updnEast).BeginInit();
			((ISupportInitialize)updnNorth).BeginInit();
			((ISupportInitialize)updnSouth).BeginInit();
			((Control)this).SuspendLayout();
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[6]
			{
				(ToolStripItem)withMapToolStripMenuItem,
				(ToolStripItem)drawMapToolStripMenuItem,
				(ToolStripItem)usePresetSiteToolStripMenuItem,
				(ToolStripItem)toolStripComboBox1,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(841, 27));
			((Control)menuStrip1).set_TabIndex(0);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withMapToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[8]
			{
				(ToolStripItem)showCoordinatesInTooltipToolStripMenuItem,
				(ToolStripItem)toolStripSeparator2,
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)printPreviewToolStripMenuItem,
				(ToolStripItem)printToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem,
				(ToolStripItem)toolStripSeparator3,
				(ToolStripItem)createGoogleKMLFileOfEventsToolStripMenuItem
			});
			((ToolStripItem)withMapToolStripMenuItem).set_Name("withMapToolStripMenuItem");
			((ToolStripItem)withMapToolStripMenuItem).set_Size(new Size(78, 23));
			((ToolStripItem)withMapToolStripMenuItem).set_Text("with Map...");
			showCoordinatesInTooltipToolStripMenuItem.set_Checked(true);
			showCoordinatesInTooltipToolStripMenuItem.set_CheckState((CheckState)1);
			((ToolStripItem)showCoordinatesInTooltipToolStripMenuItem).set_Name("showCoordinatesInTooltipToolStripMenuItem");
			((ToolStripItem)showCoordinatesInTooltipToolStripMenuItem).set_Size(new Size(254, 22));
			((ToolStripItem)showCoordinatesInTooltipToolStripMenuItem).set_Text("Show Coordinates in tooltip");
			((ToolStripItem)showCoordinatesInTooltipToolStripMenuItem).add_Click((EventHandler)showCoordinatesInTooltipToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator2).set_Name("toolStripSeparator2");
			((ToolStripItem)toolStripSeparator2).set_Size(new Size(251, 6));
			((ToolStripItem)copyToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyToolStripMenuItem).set_Name("copyToolStripMenuItem");
			copyToolStripMenuItem.set_ShortcutKeys((Keys)131139);
			((ToolStripItem)copyToolStripMenuItem).set_Size(new Size(254, 22));
			((ToolStripItem)copyToolStripMenuItem).set_Text("&Copy");
			((ToolStripItem)copyToolStripMenuItem).add_Click((EventHandler)copyToolStripMenuItem_Click);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Image((Image)Resources.PrintPreview);
			((ToolStripItem)printPreviewToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Name("printPreviewToolStripMenuItem");
			((ToolStripItem)printPreviewToolStripMenuItem).set_Size(new Size(254, 22));
			((ToolStripItem)printPreviewToolStripMenuItem).set_Text("Print Preview");
			((ToolStripItem)printPreviewToolStripMenuItem).add_Click((EventHandler)printPreviewToolStripMenuItem_Click);
			((ToolStripItem)printToolStripMenuItem).set_Image((Image)Resources.Print);
			((ToolStripItem)printToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printToolStripMenuItem).set_Name("printToolStripMenuItem");
			printToolStripMenuItem.set_ShortcutKeys((Keys)131152);
			((ToolStripItem)printToolStripMenuItem).set_Size(new Size(254, 22));
			((ToolStripItem)printToolStripMenuItem).set_Text("&Print");
			((ToolStripItem)printToolStripMenuItem).add_Click((EventHandler)printToolStripMenuItem_Click);
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			saveToolStripMenuItem.set_ShortcutKeys((Keys)131155);
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(254, 22));
			((ToolStripItem)saveToolStripMenuItem).set_Text("&Save");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator3).set_Name("toolStripSeparator3");
			((ToolStripItem)toolStripSeparator3).set_Size(new Size(251, 6));
			((ToolStripItem)createGoogleKMLFileOfEventsToolStripMenuItem).set_Image((Image)Resources.kml_file);
			((ToolStripItem)createGoogleKMLFileOfEventsToolStripMenuItem).set_Name("createGoogleKMLFileOfEventsToolStripMenuItem");
			createGoogleKMLFileOfEventsToolStripMenuItem.set_ShowShortcutKeys(false);
			((ToolStripItem)createGoogleKMLFileOfEventsToolStripMenuItem).set_Size(new Size(254, 22));
			((ToolStripItem)createGoogleKMLFileOfEventsToolStripMenuItem).set_Text("Save paths as KMZ, for GoogleEarth");
			((ToolStripItem)createGoogleKMLFileOfEventsToolStripMenuItem).add_Click((EventHandler)createGoogleKMLFileOfEventsToolStripMenuItem_Click);
			((ToolStripDropDownItem)drawMapToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[5]
			{
				(ToolStripItem)IncludeCentreLine,
				(ToolStripItem)excludeThePathLabel,
				(ToolStripItem)toolStripSeparator1,
				(ToolStripItem)bWToolStripMenuItem,
				(ToolStripItem)colourToolStripMenuItem
			});
			((ToolStripItem)drawMapToolStripMenuItem).set_Name("drawMapToolStripMenuItem");
			((ToolStripItem)drawMapToolStripMenuItem).set_Size(new Size(82, 23));
			((ToolStripItem)drawMapToolStripMenuItem).set_Text("Draw map...");
			((ToolStripItem)IncludeCentreLine).set_Name("IncludeCentreLine");
			((ToolStripItem)IncludeCentreLine).set_Size(new Size(191, 22));
			((ToolStripItem)IncludeCentreLine).set_Text("Include the center line");
			((ToolStripItem)IncludeCentreLine).add_Click((EventHandler)centreLinesOnlyToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator1).set_Name("toolStripSeparator1");
			((ToolStripItem)toolStripSeparator1).set_Size(new Size(188, 6));
			((ToolStripItem)bWToolStripMenuItem).set_Name("bWToolStripMenuItem");
			((ToolStripItem)bWToolStripMenuItem).set_Size(new Size(191, 22));
			((ToolStripItem)bWToolStripMenuItem).set_Text("B&&W");
			((ToolStripItem)bWToolStripMenuItem).add_Click((EventHandler)bWToolStripMenuItem_Click);
			colourToolStripMenuItem.set_Checked(true);
			colourToolStripMenuItem.set_CheckState((CheckState)1);
			((ToolStripItem)colourToolStripMenuItem).set_Name("colourToolStripMenuItem");
			((ToolStripItem)colourToolStripMenuItem).set_Size(new Size(191, 22));
			((ToolStripItem)colourToolStripMenuItem).set_Text("Colour");
			((ToolStripItem)colourToolStripMenuItem).add_Click((EventHandler)colourToolStripMenuItem_Click);
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
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(60, 23));
			((ToolStripItem)helpToolStripMenuItem).set_Text("&Help");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(54, 23));
			((ToolStripItem)exitToolStripMenuItem).set_Text("E&xit");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((Control)picMultipath).set_Location(new Point(12, 69));
			((Control)picMultipath).set_Name("picMultipath");
			((Control)picMultipath).set_Size(new Size(817, 523));
			picMultipath.set_TabIndex(0);
			picMultipath.set_TabStop(false);
			((Control)picMultipath).add_MouseMove(new MouseEventHandler(picMultipath_MouseMove));
			((Control)updnWest).set_Anchor((AnchorStyles)1);
			updnWest.set_DecimalPlaces(1);
			((Control)updnWest).set_Location(new Point(152, 40));
			updnWest.set_Maximum(new decimal(new int[4] { 180, 0, 0, 0 }));
			updnWest.set_Minimum(new decimal(new int[4] { 180, 0, 0, -2147483648 }));
			((Control)updnWest).set_Name("updnWest");
			((Control)updnWest).set_Size(new Size(65, 20));
			((Control)updnWest).set_TabIndex(2);
			((Control)updnWest).add_Enter((EventHandler)updnWest_Enter);
			((Control)updnEast).set_Anchor((AnchorStyles)1);
			updnEast.set_DecimalPlaces(1);
			((Control)updnEast).set_Location(new Point(291, 40));
			updnEast.set_Maximum(new decimal(new int[4] { 180, 0, 0, 0 }));
			updnEast.set_Minimum(new decimal(new int[4] { 180, 0, 0, -2147483648 }));
			((Control)updnEast).set_Name("updnEast");
			((Control)updnEast).set_Size(new Size(65, 20));
			((Control)updnEast).set_TabIndex(4);
			((Control)updnEast).add_Enter((EventHandler)updnEast_Enter);
			((Control)updnNorth).set_Anchor((AnchorStyles)1);
			updnNorth.set_DecimalPlaces(1);
			((Control)updnNorth).set_Location(new Point(431, 40));
			updnNorth.set_Maximum(new decimal(new int[4] { 90, 0, 0, 0 }));
			updnNorth.set_Minimum(new decimal(new int[4] { 90, 0, 0, -2147483648 }));
			((Control)updnNorth).set_Name("updnNorth");
			((Control)updnNorth).set_Size(new Size(52, 20));
			((Control)updnNorth).set_TabIndex(6);
			((Control)updnNorth).add_Enter((EventHandler)updnNorth_Enter);
			((Control)updnSouth).set_Anchor((AnchorStyles)1);
			updnSouth.set_DecimalPlaces(1);
			((Control)updnSouth).set_Location(new Point(562, 40));
			updnSouth.set_Maximum(new decimal(new int[4] { 90, 0, 0, 0 }));
			updnSouth.set_Minimum(new decimal(new int[4] { 90, 0, 0, -2147483648 }));
			((Control)updnSouth).set_Name("updnSouth");
			((Control)updnSouth).set_Size(new Size(48, 20));
			((Control)updnSouth).set_TabIndex(8);
			((Control)updnSouth).add_Enter((EventHandler)updnSouth_Enter);
			((Control)label1).set_Anchor((AnchorStyles)1);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(120, 43));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(32, 13));
			((Control)label1).set_TabIndex(1);
			((Control)label1).set_Text("West");
			((Control)label2).set_Anchor((AnchorStyles)1);
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(263, 44));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(28, 13));
			((Control)label2).set_TabIndex(3);
			((Control)label2).set_Text("East");
			((Control)label3).set_Anchor((AnchorStyles)1);
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Location(new Point(398, 43));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(33, 13));
			((Control)label3).set_TabIndex(5);
			((Control)label3).set_Text("North");
			((Control)label4).set_Anchor((AnchorStyles)1);
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Location(new Point(527, 43));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(35, 13));
			((Control)label4).set_TabIndex(7);
			((Control)label4).set_Text("South");
			((Control)cmdRePlot).set_Anchor((AnchorStyles)1);
			((Control)cmdRePlot).set_Location(new Point(656, 40));
			((Control)cmdRePlot).set_Name("cmdRePlot");
			((Control)cmdRePlot).set_Size(new Size(64, 22));
			((Control)cmdRePlot).set_TabIndex(9);
			((Control)cmdRePlot).set_Text("&Re-draw");
			((ButtonBase)cmdRePlot).set_UseVisualStyleBackColor(true);
			((Control)cmdRePlot).add_Click((EventHandler)cmdRePlot_Click);
			toolTip1.set_AutoPopDelay(2000);
			toolTip1.set_InitialDelay(100);
			toolTip1.set_ReshowDelay(100);
			cmbSiteFiles.set_DropDownStyle((ComboBoxStyle)2);
			((ListControl)cmbSiteFiles).set_FormattingEnabled(true);
			((Control)cmbSiteFiles).set_Location(new Point(645, 2));
			cmbSiteFiles.set_MaxDropDownItems(20);
			((Control)cmbSiteFiles).set_Name("cmbSiteFiles");
			((Control)cmbSiteFiles).set_Size(new Size(114, 21));
			cmbSiteFiles.set_Sorted(true);
			((Control)cmbSiteFiles).set_TabIndex(28);
			((ListControl)cmbSiteFiles).add_SelectedValueChanged((EventHandler)cmbSiteFiles_SelectedValueChanged);
			cmbNames.set_DropDownStyle((ComboBoxStyle)2);
			((ListControl)cmbNames).set_FormattingEnabled(true);
			cmbNames.get_Items().AddRange(new object[4] { "never", "all", "many", "some" });
			((Control)cmbNames).set_Location(new Point(765, 2));
			((Control)cmbNames).set_Name("cmbNames");
			((Control)cmbNames).set_Size(new Size(55, 21));
			((Control)cmbNames).set_TabIndex(30);
			cmbNames.add_SelectedIndexChanged((EventHandler)cmbNames_SelectedIndexChanged);
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Location(new Point(596, 6));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(49, 13));
			((Control)label5).set_TabIndex(31);
			((Control)label5).set_Text("Plot sites");
			((ToolStripItem)excludeThePathLabel).set_Name("excludeThePathLabel");
			((ToolStripItem)excludeThePathLabel).set_Size(new Size(191, 22));
			((ToolStripItem)excludeThePathLabel).set_Text("Exclude the path label");
			((ToolStripItem)excludeThePathLabel).add_Click((EventHandler)excludeThePathLabel_Click);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(841, 604));
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
			((Control)this).get_Controls().Add((Control)(object)picMultipath);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_DataBindings().Add(new Binding("WindowState", (object)Settings.Default, "AsteroidPlotMultiPathWindow", true, (DataSourceUpdateMode)1));
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationAsterPlotMultipath", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_Icon((Icon)componentResourceManager.GetObject("$this.Icon"));
			((Form)this).set_Location(Settings.Default.LocationAsterPlotMultipath);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_Name("AsteroidPlotMultipath");
			((Control)this).set_Text("Asteroid - Multipath plot");
			((Form)this).set_WindowState(Settings.Default.AsteroidPlotMultiPathWindow);
			((Form)this).add_FormClosing(new FormClosingEventHandler(AsteroidPlotMultipath_FormClosing));
			((Form)this).add_Load((EventHandler)AsteroidPlotMultipath_Load);
			((Control)this).add_Resize((EventHandler)AsteroidPlotMultipath_Resize);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((ISupportInitialize)picMultipath).EndInit();
			((ISupportInitialize)updnWest).EndInit();
			((ISupportInitialize)updnEast).EndInit();
			((ISupportInitialize)updnNorth).EndInit();
			((ISupportInitialize)updnSouth).EndInit();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
