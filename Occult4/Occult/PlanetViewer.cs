using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Drawing.Imaging;
using System.Drawing.Printing;
using System.IO;
using System.Windows.Forms;
using GifCreator;
using Occult.Properties;

namespace Occult
{
	public class PlanetViewer : Form
	{
		private readonly string AppPath;

		private bool breakFlag;

		private bool ShowNames;

		private bool ShowSatNumbers = true;

		private bool BWFlag;

		private bool FormLoaded;

		private bool IsPlotting;

		private bool AnimatedGIF;

		public bool ShowStarPath;

		private float PlotMagnification = 0.25f;

		private double CurrentJD;

		private int Planet = 1;

		private IContainer components;

		private PictureBox picPlanet;

		private GroupBox grpPlanet;

		private Button cmdDraw;

		private NumericUpDown updnDuration;

		private Label label1;

		private TextBox txtInterval;

		private Label label2;

		private Button cmdCancel;

		private GroupBox grpNames;

		private RadioButton optShowNames;

		private RadioButton optShowNumbers;

		private RadioButton optNoLabel;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem1;

		private ToolStripMenuItem fileToolStripMenuItem;

		private ToolStripMenuItem bWToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator1;

		private ToolStripMenuItem copyToolStripMenuItem1;

		private ToolStripMenuItem printToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator;

		private ToolStripMenuItem saveToolStripMenuItem;

		public RadioButton optMercury;

		public RadioButton optVenus;

		public RadioButton optPluto;

		public RadioButton optNeptune;

		public RadioButton optUranus;

		public RadioButton optSaturn;

		public RadioButton optJupiter;

		public RadioButton optMars;

		public NumericUpDown updnYear;

		public NumericUpDown updnMonth;

		public NumericUpDown updnDay;

		public NumericUpDown updnHour;

		public TrackBar trackBar1;

		public CheckBox chkMagnify;

		public CheckBox chkMagnify2;

		public CheckBox chkOrbits;

		public CheckBox chkFaintMoons;

		private ToolStripMenuItem printPreviewToolStripMenuItem;

		private ToolStripMenuItem saveAnimatedGIFToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator2;

		private ToolStripMenuItem animatedGIFSettingsToolStripMenuItem;

		private ToolStripMenuItem plotWithEarthcorrectToolStripMenuItem;

		public CheckBox chkMinorRings;

		public PlanetViewer()
		{
			InitializeComponent();
			AppPath = Utilities.AppPath;
		}

		private void PlanetViewer_Load(object sender, EventArgs e)
		{
			//IL_0107: Unknown result type (might be due to invalid IL or missing references)
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			if (((Control)this).get_Top() < -30000)
			{
				((Control)this).set_Top(0);
			}
			if ((double)((Control)this).get_Left() + (double)((Control)this).get_Width() / 1.2 < (double)Screen.GetWorkingArea((Control)(object)this).Left)
			{
				((Control)this).set_Left(Screen.GetWorkingArea((Control)(object)this).Left);
			}
			updnYear.set_Value((decimal)DateTime.Now.ToUniversalTime().Year);
			updnMonth.set_Value((decimal)DateTime.Now.ToUniversalTime().Month);
			updnDay.set_Value((decimal)DateTime.Now.ToUniversalTime().Day);
			updnHour.set_Value((decimal)DateTime.Now.ToUniversalTime().Hour);
			((Form)this).set_WindowState(Settings.Default.PlanetViewerWindow);
			FormLoaded = true;
			DrawPlanetsAndMoons();
		}

		private void PlanetViewer_FormClosing(object sender, FormClosingEventArgs e)
		{
			//IL_0006: Unknown result type (might be due to invalid IL or missing references)
			Settings.Default.PlanetViewerWindow = ((Form)this).get_WindowState();
		}

		private void PlanetViewer_Resize(object sender, EventArgs e)
		{
			if (!((((Control)this).get_Width() < 100) | (((Control)this).get_Height() < 200)))
			{
				((Control)picPlanet).set_Width(((Control)this).get_Width() - 44);
				((Control)picPlanet).set_Height(((Control)this).get_Height() - 147);
				if (FormLoaded && !(updnDuration.get_Value() > 0m))
				{
					DrawPlanetsAndMoons();
				}
			}
		}

		private void cmdDraw_Click(object sender, EventArgs e)
		{
			DrawPlanetsAndMoons();
		}

		public void DrawPlanetsAndMoons()
		{
			if (IsPlotting)
			{
				return;
			}
			IsPlotting = true;
			List<string> list = new List<string>();
			int num = 10000;
			if (optMercury.get_Checked())
			{
				Planet = 1;
			}
			else if (optVenus.get_Checked())
			{
				Planet = 2;
			}
			else if (optMars.get_Checked())
			{
				Planet = 4;
			}
			else if (optJupiter.get_Checked())
			{
				Planet = 5;
			}
			else if (optSaturn.get_Checked())
			{
				Planet = 6;
			}
			else if (optUranus.get_Checked())
			{
				Planet = 7;
			}
			else if (optNeptune.get_Checked())
			{
				Planet = 8;
			}
			else if (optPluto.get_Checked())
			{
				Planet = 9;
			}
			Satellites.SetPlanetPlusMoonPlotParameters(Planet);
			Image image = new Bitmap(((Control)picPlanet).get_Width(), ((Control)picPlanet).get_Height());
			Graphics graphics = Graphics.FromImage(image);
			if (Settings.Default.GraphicsSmoothed)
			{
				graphics.SmoothingMode = SmoothingMode.AntiAlias;
			}
			graphics.Clear(Color.White);
			Brush brush = Brushes.Black;
			if (BWFlag)
			{
				brush = Brushes.White;
			}
			float num2 = 1f;
			if (!double.TryParse(((Control)txtInterval).get_Text(), out var result))
			{
				return;
			}
			if (result <= 0.0)
			{
				result = 0.1;
				((Control)txtInterval).set_Text(".1");
			}
			double num3 = (double)updnDuration.get_Value();
			double num4 = Utilities.JD_from_Date((int)updnYear.get_Value(), (int)updnMonth.get_Value(), (double)(updnDay.get_Value() + updnHour.get_Value() / 24m));
			((Control)cmdDraw).set_Visible(false);
			((ToolStripItem)fileToolStripMenuItem).set_Enabled(false);
			if (AnimatedGIF)
			{
				string path = AppPath + "\\Predictions\\AnimatedGIF";
				if (!Directory.Exists(path))
				{
					Directory.CreateDirectory(path);
				}
				string[] files = Directory.GetFiles(path, "*.gif");
				foreach (string path2 in files)
				{
					try
					{
						File.Delete(path2);
					}
					catch
					{
					}
				}
			}
			for (double num5 = 0.0; num5 <= num3; num5 += result)
			{
				num2 = 1f;
				if (chkMagnify.get_Checked())
				{
					num2 = 0.1f;
				}
				if (chkMagnify2.get_Checked())
				{
					num2 = 10f;
				}
				CurrentJD = num4 + num5;
				graphics.FillRectangle(brush, 0, 0, ((Control)picPlanet).get_Width(), ((Control)picPlanet).get_Height());
				Satellites.PlotOrbits(graphics, ((Control)picPlanet).get_Width(), ((Control)picPlanet).get_Height(), CurrentJD, Planet, PlotMagnification * num2, chkFaintMoons.get_Checked(), chkOrbits.get_Checked(), chkMinorRings.get_Checked(), ShowMoons: true, ShowNames, ShowSatNumbers, BWFlag, PrintFlag: false, ShowStarPath, plotWithEarthcorrectToolStripMenuItem.get_Checked());
				picPlanet.set_Image(image);
				if (AnimatedGIF)
				{
					string text = Utilities.AppPath + "\\Predictions\\AnimatedGIF\\" + num + ".gif";
					try
					{
						picPlanet.get_Image().Save(text, ImageFormat.Gif);
						list.Add(text);
					}
					catch
					{
					}
					num++;
				}
				Application.DoEvents();
				if (breakFlag)
				{
					break;
				}
			}
			((Control)cmdDraw).set_Visible(true);
			((ToolStripItem)fileToolStripMenuItem).set_Enabled(true);
			picPlanet.set_Image(image);
			graphics.Dispose();
			breakFlag = false;
			IsPlotting = false;
			if (AnimatedGIF)
			{
				global::GifCreator.GifCreator.Create_Animated_Gif(list, Utilities.Planets[Planet] + "_" + updnYear.get_Value() + Utilities.ShortMonths[(int)updnMonth.get_Value()] + updnDay.get_Value().ToString().PadLeft(2, '0') + ".gif");
			}
		}

		private void cmdCancel_Click(object sender, EventArgs e)
		{
			breakFlag = true;
		}

		private void trackBar1_Scroll(object sender, EventArgs e)
		{
			PlotMagnification = (float)trackBar1.get_Value() / 200f;
			DrawPlanetsAndMoons();
		}

		private void optShowNumbers_CheckedChanged(object sender, EventArgs e)
		{
			ShowSatNumbers = optShowNumbers.get_Checked();
		}

		private void optShowNames_CheckedChanged(object sender, EventArgs e)
		{
			ShowNames = optShowNames.get_Checked();
		}

		private void bWToolStripMenuItem_Click(object sender, EventArgs e)
		{
			BWFlag = !BWFlag;
			if (!BWFlag)
			{
				((ToolStripItem)bWToolStripMenuItem).set_Text("Plot in B&&W");
			}
			else
			{
				((ToolStripItem)bWToolStripMenuItem).set_Text("Plot in Colour");
			}
			DrawPlanetsAndMoons();
		}

		private void copyToolStripMenuItem1_Click(object sender, EventArgs e)
		{
			Clipboard.SetImage(picPlanet.get_Image());
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings.Default.Save_EphemerisData = Output.SaveGraphic(picPlanet.get_Image(), Utilities.Date_from_JD(CurrentJD, 0) + Utilities.Planets[Planet] + " image ", Settings.Default.Save_EphemerisData);
		}

		private void exitToolStripMenuItem1_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
		}

		private void chkMagnify2_CheckedChanged(object sender, EventArgs e)
		{
			if (chkMagnify2.get_Checked())
			{
				chkMagnify.set_Checked(false);
			}
			DrawPlanetsAndMoons();
		}

		private void chkMagnify_CheckedChanged(object sender, EventArgs e)
		{
			if (chkMagnify.get_Checked())
			{
				chkMagnify2.set_Checked(false);
			}
			DrawPlanetsAndMoons();
		}

		public void EnablePlanetSelection(bool Enable)
		{
			((Control)optMercury).set_Enabled(Enable);
			((Control)optVenus).set_Enabled(Enable);
			((Control)optMars).set_Enabled(Enable);
			((Control)optJupiter).set_Enabled(Enable);
			((Control)optSaturn).set_Enabled(Enable);
			((Control)optUranus).set_Enabled(Enable);
			((Control)optNeptune).set_Enabled(Enable);
			((Control)optPluto).set_Enabled(Enable);
		}

		private void printPreviewToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0000: Unknown result type (might be due to invalid IL or missing references)
			//IL_0006: Expected O, but got Unknown
			//IL_0013: Unknown result type (might be due to invalid IL or missing references)
			//IL_0018: Unknown result type (might be due to invalid IL or missing references)
			//IL_001f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0026: Unknown result type (might be due to invalid IL or missing references)
			//IL_002c: Invalid comparison between Unknown and I4
			//IL_0041: Unknown result type (might be due to invalid IL or missing references)
			PrintPreviewDialog val = new PrintPreviewDialog();
			PrintDocument printDocument = new PrintDocument();
			val.set_Document(printDocument);
			PrintDialog val2 = new PrintDialog();
			val2.set_UseEXDialog(true);
			val2.set_Document(printDocument);
			if ((int)((CommonDialog)val2).ShowDialog() == 1)
			{
				printDocument.PrintPage += PrintPlanetsAndMoons;
				((Form)val).ShowDialog();
			}
		}

		private void printToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0006: Unknown result type (might be due to invalid IL or missing references)
			//IL_000b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0012: Unknown result type (might be due to invalid IL or missing references)
			//IL_0019: Unknown result type (might be due to invalid IL or missing references)
			//IL_001f: Invalid comparison between Unknown and I4
			PrintDocument printDocument = new PrintDocument();
			PrintDialog val = new PrintDialog();
			val.set_UseEXDialog(true);
			val.set_Document(printDocument);
			if ((int)((CommonDialog)val).ShowDialog() == 1)
			{
				printDocument.PrintPage += PrintPlanetsAndMoons;
				printDocument.Print();
			}
		}

		private void PrintPlanetsAndMoons(object sender, PrintPageEventArgs e)
		{
			Graphics graphics = e.Graphics;
			int width = e.MarginBounds.Width;
			int height = e.MarginBounds.Height;
			if (optMercury.get_Checked())
			{
				Planet = 1;
			}
			if (optVenus.get_Checked())
			{
				Planet = 2;
			}
			if (optMars.get_Checked())
			{
				Planet = 4;
			}
			if (optJupiter.get_Checked())
			{
				Planet = 5;
			}
			if (optSaturn.get_Checked())
			{
				Planet = 6;
			}
			if (optUranus.get_Checked())
			{
				Planet = 7;
			}
			if (optNeptune.get_Checked())
			{
				Planet = 8;
			}
			if (optPluto.get_Checked())
			{
				Planet = 9;
			}
			Satellites.SetPlanetPlusMoonPlotParameters(Planet);
			_ = Brushes.White;
			float num = 1f;
			double jD = Utilities.JD_from_Date((int)updnYear.get_Value(), (int)updnMonth.get_Value(), (double)(updnDay.get_Value() + updnHour.get_Value() / 24m));
			((Control)cmdDraw).set_Visible(false);
			((ToolStripItem)fileToolStripMenuItem).set_Enabled(false);
			num = 1f;
			if (chkMagnify.get_Checked())
			{
				num = 0.1f;
			}
			if (chkMagnify2.get_Checked())
			{
				num = 10f;
			}
			Satellites.PlotOrbits(graphics, width, height, jD, Planet, PlotMagnification * num, chkFaintMoons.get_Checked(), chkOrbits.get_Checked(), chkMinorRings.get_Checked(), ShowMoons: true, ShowNames, ShowSatNumbers, BWflag: true, PrintFlag: true, ShowStarPath, plotWithEarthcorrectToolStripMenuItem.get_Checked());
			((Control)cmdDraw).set_Visible(true);
			((ToolStripItem)fileToolStripMenuItem).set_Enabled(true);
		}

		private void updnYear_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnYear).Select(0, 10);
		}

		private void updnMonth_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnMonth).Select(0, 10);
		}

		private void updnDay_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnDay).Select(0, 10);
		}

		private void updnHour_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnHour).Select(0, 10);
		}

		private void updnDuration_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnDuration).Select(0, 10);
		}

		private void txtInterval_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtInterval).SelectAll();
		}

		private void plotWithEarthcorrectToolStripMenuItem_Click(object sender, EventArgs e)
		{
			DrawPlanetsAndMoons();
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Graphic of planets and their moons");
		}

		internal void SetPlanet(int PlanetNumber)
		{
			RadioButton obj = optMercury;
			RadioButton obj2 = optVenus;
			RadioButton obj3 = optMars;
			RadioButton obj4 = optJupiter;
			RadioButton obj5 = optSaturn;
			RadioButton obj6 = optUranus;
			RadioButton obj7 = optNeptune;
			bool flag;
			optPluto.set_Checked(flag = false);
			bool flag2;
			obj7.set_Checked(flag2 = flag);
			bool flag3;
			obj6.set_Checked(flag3 = flag2);
			bool flag4;
			obj5.set_Checked(flag4 = flag3);
			bool flag5;
			obj4.set_Checked(flag5 = flag4);
			bool flag6;
			obj3.set_Checked(flag6 = flag5);
			bool @checked;
			obj2.set_Checked(@checked = flag6);
			obj.set_Checked(@checked);
			switch (PlanetNumber)
			{
			case 1:
				optMercury.set_Checked(true);
				break;
			case 2:
				optVenus.set_Checked(true);
				break;
			case 4:
				optMars.set_Checked(true);
				break;
			case 5:
				optJupiter.set_Checked(true);
				break;
			case 6:
				optSaturn.set_Checked(true);
				break;
			case 7:
				optUranus.set_Checked(true);
				break;
			case 8:
				optNeptune.set_Checked(true);
				break;
			default:
				optPluto.set_Checked(true);
				break;
			}
			DrawPlanetsAndMoons();
		}

		private void optMercury_Click(object sender, EventArgs e)
		{
			SetPlanet(1);
		}

		private void optVenus_Click(object sender, EventArgs e)
		{
			SetPlanet(2);
		}

		private void optMars_Click(object sender, EventArgs e)
		{
			SetPlanet(4);
		}

		private void optJupiter_Click(object sender, EventArgs e)
		{
			SetPlanet(5);
		}

		private void optSaturn_Click(object sender, EventArgs e)
		{
			SetPlanet(6);
		}

		private void optUranus_Click(object sender, EventArgs e)
		{
			SetPlanet(7);
		}

		private void optNeptune_Click(object sender, EventArgs e)
		{
			SetPlanet(8);
		}

		private void optPluto_Click(object sender, EventArgs e)
		{
			SetPlanet(9);
		}

		private void saveAnimatedGIFToolStripMenuItem_Click(object sender, EventArgs e)
		{
			AnimatedGIF = true;
			DrawPlanetsAndMoons();
			AnimatedGIF = false;
		}

		private void animatedGIFSettingsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0006: Unknown result type (might be due to invalid IL or missing references)
			GIFsettings gIFsettings = new GIFsettings();
			((Form)gIFsettings).ShowDialog();
			((Component)(object)gIFsettings).Dispose();
		}

		private void chkFaintMoons_CheckedChanged(object sender, EventArgs e)
		{
			DrawPlanetsAndMoons();
		}

		private void chkOrbits_CheckedChanged(object sender, EventArgs e)
		{
			DrawPlanetsAndMoons();
		}

		private void chkMinorRings_CheckedChanged(object sender, EventArgs e)
		{
			DrawPlanetsAndMoons();
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
			//IL_110e: Unknown result type (might be due to invalid IL or missing references)
			//IL_1118: Expected O, but got Unknown
			//IL_19a2: Unknown result type (might be due to invalid IL or missing references)
			//IL_19ac: Expected O, but got Unknown
			//IL_1a7c: Unknown result type (might be due to invalid IL or missing references)
			//IL_1a86: Expected O, but got Unknown
			//IL_1c9d: Unknown result type (might be due to invalid IL or missing references)
			//IL_1ca7: Expected O, but got Unknown
			//IL_1d0f: Unknown result type (might be due to invalid IL or missing references)
			//IL_1d19: Expected O, but got Unknown
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(PlanetViewer));
			picPlanet = new PictureBox();
			grpPlanet = new GroupBox();
			optPluto = new RadioButton();
			optNeptune = new RadioButton();
			optUranus = new RadioButton();
			optSaturn = new RadioButton();
			optJupiter = new RadioButton();
			optMars = new RadioButton();
			optVenus = new RadioButton();
			optMercury = new RadioButton();
			updnYear = new NumericUpDown();
			updnMonth = new NumericUpDown();
			updnDay = new NumericUpDown();
			cmdDraw = new Button();
			updnDuration = new NumericUpDown();
			label1 = new Label();
			txtInterval = new TextBox();
			label2 = new Label();
			cmdCancel = new Button();
			trackBar1 = new TrackBar();
			grpNames = new GroupBox();
			optShowNames = new RadioButton();
			optNoLabel = new RadioButton();
			optShowNumbers = new RadioButton();
			chkFaintMoons = new CheckBox();
			chkMagnify = new CheckBox();
			updnHour = new NumericUpDown();
			menuStrip1 = new MenuStrip();
			fileToolStripMenuItem = new ToolStripMenuItem();
			bWToolStripMenuItem = new ToolStripMenuItem();
			plotWithEarthcorrectToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator2 = new ToolStripSeparator();
			animatedGIFSettingsToolStripMenuItem = new ToolStripMenuItem();
			saveAnimatedGIFToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator1 = new ToolStripSeparator();
			copyToolStripMenuItem1 = new ToolStripMenuItem();
			printPreviewToolStripMenuItem = new ToolStripMenuItem();
			printToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator = new ToolStripSeparator();
			saveToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem1 = new ToolStripMenuItem();
			chkMagnify2 = new CheckBox();
			chkOrbits = new CheckBox();
			chkMinorRings = new CheckBox();
			((ISupportInitialize)picPlanet).BeginInit();
			((Control)grpPlanet).SuspendLayout();
			((ISupportInitialize)updnYear).BeginInit();
			((ISupportInitialize)updnMonth).BeginInit();
			((ISupportInitialize)updnDay).BeginInit();
			((ISupportInitialize)updnDuration).BeginInit();
			((ISupportInitialize)trackBar1).BeginInit();
			((Control)grpNames).SuspendLayout();
			((ISupportInitialize)updnHour).BeginInit();
			((Control)menuStrip1).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)picPlanet).set_Location(new Point(22, 99));
			((Control)picPlanet).set_Name("picPlanet");
			((Control)picPlanet).set_Size(new Size(753, 504));
			picPlanet.set_TabIndex(0);
			picPlanet.set_TabStop(false);
			((Control)grpPlanet).set_Anchor((AnchorStyles)1);
			((Control)grpPlanet).get_Controls().Add((Control)(object)optPluto);
			((Control)grpPlanet).get_Controls().Add((Control)(object)optNeptune);
			((Control)grpPlanet).get_Controls().Add((Control)(object)optUranus);
			((Control)grpPlanet).get_Controls().Add((Control)(object)optSaturn);
			((Control)grpPlanet).get_Controls().Add((Control)(object)optJupiter);
			((Control)grpPlanet).get_Controls().Add((Control)(object)optMars);
			((Control)grpPlanet).get_Controls().Add((Control)(object)optVenus);
			((Control)grpPlanet).get_Controls().Add((Control)(object)optMercury);
			((Control)grpPlanet).set_Location(new Point(23, 27));
			((Control)grpPlanet).set_Name("grpPlanet");
			((Control)grpPlanet).set_Size(new Size(208, 65));
			((Control)grpPlanet).set_TabIndex(0);
			grpPlanet.set_TabStop(false);
			((Control)grpPlanet).set_Text("Planet");
			optPluto.set_AutoCheck(false);
			((Control)optPluto).set_AutoSize(true);
			((Control)optPluto).set_Location(new Point(137, 31));
			((Control)optPluto).set_Name("optPluto");
			((Control)optPluto).set_Size(new Size(49, 17));
			((Control)optPluto).set_TabIndex(7);
			((Control)optPluto).set_Text("Pluto");
			((ButtonBase)optPluto).set_UseVisualStyleBackColor(true);
			((Control)optPluto).add_Click((EventHandler)optPluto_Click);
			optNeptune.set_AutoCheck(false);
			((Control)optNeptune).set_AutoSize(true);
			((Control)optNeptune).set_Location(new Point(137, 16));
			((Control)optNeptune).set_Name("optNeptune");
			((Control)optNeptune).set_Size(new Size(66, 17));
			((Control)optNeptune).set_TabIndex(6);
			((Control)optNeptune).set_Text("Neptune");
			((ButtonBase)optNeptune).set_UseVisualStyleBackColor(true);
			((Control)optNeptune).add_Click((EventHandler)optNeptune_Click);
			optUranus.set_AutoCheck(false);
			((Control)optUranus).set_AutoSize(true);
			((Control)optUranus).set_Location(new Point(73, 46));
			((Control)optUranus).set_Name("optUranus");
			((Control)optUranus).set_Size(new Size(59, 17));
			((Control)optUranus).set_TabIndex(5);
			((Control)optUranus).set_Text("Uranus");
			((ButtonBase)optUranus).set_UseVisualStyleBackColor(true);
			((Control)optUranus).add_Click((EventHandler)optUranus_Click);
			optSaturn.set_AutoCheck(false);
			((Control)optSaturn).set_AutoSize(true);
			((Control)optSaturn).set_Location(new Point(73, 31));
			((Control)optSaturn).set_Name("optSaturn");
			((Control)optSaturn).set_Size(new Size(56, 17));
			((Control)optSaturn).set_TabIndex(4);
			((Control)optSaturn).set_Text("Saturn");
			((ButtonBase)optSaturn).set_UseVisualStyleBackColor(true);
			((Control)optSaturn).add_Click((EventHandler)optSaturn_Click);
			optJupiter.set_AutoCheck(false);
			((Control)optJupiter).set_AutoSize(true);
			optJupiter.set_Checked(true);
			((Control)optJupiter).set_Location(new Point(73, 16));
			((Control)optJupiter).set_Name("optJupiter");
			((Control)optJupiter).set_Size(new Size(56, 17));
			((Control)optJupiter).set_TabIndex(3);
			optJupiter.set_TabStop(true);
			((Control)optJupiter).set_Text("Jupiter");
			((ButtonBase)optJupiter).set_UseVisualStyleBackColor(true);
			((Control)optJupiter).add_Click((EventHandler)optJupiter_Click);
			optMars.set_AutoCheck(false);
			((Control)optMars).set_AutoSize(true);
			((Control)optMars).set_Location(new Point(7, 46));
			((Control)optMars).set_Name("optMars");
			((Control)optMars).set_Size(new Size(48, 17));
			((Control)optMars).set_TabIndex(2);
			((Control)optMars).set_Text("Mars");
			((ButtonBase)optMars).set_UseVisualStyleBackColor(true);
			((Control)optMars).add_Click((EventHandler)optMars_Click);
			optVenus.set_AutoCheck(false);
			((Control)optVenus).set_AutoSize(true);
			((Control)optVenus).set_Location(new Point(7, 31));
			((Control)optVenus).set_Name("optVenus");
			((Control)optVenus).set_Size(new Size(55, 17));
			((Control)optVenus).set_TabIndex(1);
			((Control)optVenus).set_Text("Venus");
			((ButtonBase)optVenus).set_UseVisualStyleBackColor(true);
			((Control)optVenus).add_Click((EventHandler)optVenus_Click);
			optMercury.set_AutoCheck(false);
			((Control)optMercury).set_AutoSize(true);
			((Control)optMercury).set_Location(new Point(7, 16));
			((Control)optMercury).set_Name("optMercury");
			((Control)optMercury).set_Size(new Size(63, 17));
			((Control)optMercury).set_TabIndex(0);
			((Control)optMercury).set_Text("Mercury");
			((ButtonBase)optMercury).set_UseVisualStyleBackColor(true);
			((Control)optMercury).add_Click((EventHandler)optMercury_Click);
			((Control)updnYear).set_Anchor((AnchorStyles)1);
			((Control)updnYear).set_Location(new Point(241, 30));
			updnYear.set_Maximum(new decimal(new int[4] { 17000, 0, 0, 0 }));
			updnYear.set_Minimum(new decimal(new int[4] { 13000, 0, 0, -2147483648 }));
			((Control)updnYear).set_Name("updnYear");
			((Control)updnYear).set_Size(new Size(55, 20));
			((Control)updnYear).set_TabIndex(1);
			updnYear.set_Value(new decimal(new int[4] { 2006, 0, 0, 0 }));
			((Control)updnYear).add_Enter((EventHandler)updnYear_Enter);
			((Control)updnMonth).set_Anchor((AnchorStyles)1);
			((Control)updnMonth).set_Location(new Point(301, 30));
			updnMonth.set_Maximum(new decimal(new int[4] { 12, 0, 0, 0 }));
			updnMonth.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnMonth).set_Name("updnMonth");
			((Control)updnMonth).set_Size(new Size(37, 20));
			((Control)updnMonth).set_TabIndex(2);
			updnMonth.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnMonth).add_Enter((EventHandler)updnMonth_Enter);
			((Control)updnDay).set_Anchor((AnchorStyles)1);
			((Control)updnDay).set_Location(new Point(343, 30));
			updnDay.set_Maximum(new decimal(new int[4] { 31, 0, 0, 0 }));
			((Control)updnDay).set_Name("updnDay");
			((Control)updnDay).set_Size(new Size(36, 20));
			((Control)updnDay).set_TabIndex(3);
			updnDay.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnDay).add_Enter((EventHandler)updnDay_Enter);
			((Control)cmdDraw).set_Anchor((AnchorStyles)1);
			((Control)cmdDraw).set_Location(new Point(689, 40));
			((Control)cmdDraw).set_Name("cmdDraw");
			((Control)cmdDraw).set_Size(new Size(70, 25));
			((Control)cmdDraw).set_TabIndex(13);
			((Control)cmdDraw).set_Text("&Draw");
			((ButtonBase)cmdDraw).set_UseVisualStyleBackColor(true);
			((Control)cmdDraw).add_Click((EventHandler)cmdDraw_Click);
			((Control)updnDuration).set_Anchor((AnchorStyles)1);
			updnDuration.set_DecimalPlaces(2);
			updnDuration.set_Increment(new decimal(new int[4] { 1, 0, 0, 65536 }));
			((Control)updnDuration).set_Location(new Point(370, 53));
			updnDuration.set_Maximum(new decimal(new int[4] { 10000, 0, 0, 0 }));
			((Control)updnDuration).set_Name("updnDuration");
			((Control)updnDuration).set_Size(new Size(67, 20));
			((Control)updnDuration).set_TabIndex(6);
			((Control)updnDuration).add_Enter((EventHandler)updnDuration_Enter);
			((Control)label1).set_Anchor((AnchorStyles)1);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(273, 57));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(97, 13));
			((Control)label1).set_TabIndex(5);
			((Control)label1).set_Text("Plot duration (days)");
			((Control)txtInterval).set_Anchor((AnchorStyles)1);
			((Control)txtInterval).set_Location(new Point(391, 75));
			((Control)txtInterval).set_Name("txtInterval");
			((Control)txtInterval).set_Size(new Size(45, 20));
			((Control)txtInterval).set_TabIndex(8);
			((Control)txtInterval).set_Text(".001");
			((Control)txtInterval).add_Enter((EventHandler)txtInterval_Enter);
			((Control)label2).set_Anchor((AnchorStyles)1);
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(288, 79));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(93, 13));
			((Control)label2).set_TabIndex(7);
			((Control)label2).set_Text("Plot interval (days)");
			((Control)cmdCancel).set_Anchor((AnchorStyles)1);
			((Control)cmdCancel).set_Location(new Point(689, 40));
			((Control)cmdCancel).set_Name("cmdCancel");
			((Control)cmdCancel).set_Size(new Size(70, 25));
			((Control)cmdCancel).set_TabIndex(10);
			((Control)cmdCancel).set_Text("Cancel");
			((ButtonBase)cmdCancel).set_UseVisualStyleBackColor(true);
			((Control)cmdCancel).add_Click((EventHandler)cmdCancel_Click);
			((Control)trackBar1).set_Anchor((AnchorStyles)1);
			((Control)trackBar1).set_AutoSize(false);
			trackBar1.set_LargeChange(1);
			((Control)trackBar1).set_Location(new Point(523, 75));
			trackBar1.set_Maximum(110);
			trackBar1.set_Minimum(10);
			((Control)trackBar1).set_Name("trackBar1");
			((Control)trackBar1).set_Size(new Size(171, 21));
			((Control)trackBar1).set_TabIndex(14);
			trackBar1.set_TickFrequency(5);
			trackBar1.set_TickStyle((TickStyle)1);
			trackBar1.set_Value(51);
			trackBar1.add_Scroll((EventHandler)trackBar1_Scroll);
			((Control)grpNames).set_Anchor((AnchorStyles)1);
			((Control)grpNames).get_Controls().Add((Control)(object)optShowNames);
			((Control)grpNames).get_Controls().Add((Control)(object)optNoLabel);
			((Control)grpNames).get_Controls().Add((Control)(object)optShowNumbers);
			((Control)grpNames).set_Location(new Point(446, 31));
			((Control)grpNames).set_Name("grpNames");
			((Control)grpNames).set_Size(new Size(67, 60));
			((Control)grpNames).set_TabIndex(9);
			grpNames.set_TabStop(false);
			((Control)grpNames).set_Text("Labels");
			((Control)optShowNames).set_AutoSize(true);
			((Control)optShowNames).set_Location(new Point(4, 42));
			((Control)optShowNames).set_Name("optShowNames");
			((Control)optShowNames).set_Size(new Size(53, 17));
			((Control)optShowNames).set_TabIndex(2);
			((Control)optShowNames).set_Text("Name");
			((ButtonBase)optShowNames).set_UseVisualStyleBackColor(true);
			optShowNames.add_CheckedChanged((EventHandler)optShowNames_CheckedChanged);
			((Control)optNoLabel).set_AutoSize(true);
			((Control)optNoLabel).set_Location(new Point(4, 12));
			((Control)optNoLabel).set_Name("optNoLabel");
			((Control)optNoLabel).set_Size(new Size(51, 17));
			((Control)optNoLabel).set_TabIndex(0);
			((Control)optNoLabel).set_Text("None");
			((ButtonBase)optNoLabel).set_UseVisualStyleBackColor(true);
			((Control)optShowNumbers).set_AutoSize(true);
			optShowNumbers.set_Checked(true);
			((Control)optShowNumbers).set_Location(new Point(4, 27));
			((Control)optShowNumbers).set_Name("optShowNumbers");
			((Control)optShowNumbers).set_Size(new Size(62, 17));
			((Control)optShowNumbers).set_TabIndex(1);
			optShowNumbers.set_TabStop(true);
			((Control)optShowNumbers).set_Text("Number");
			((ButtonBase)optShowNumbers).set_UseVisualStyleBackColor(true);
			optShowNumbers.add_CheckedChanged((EventHandler)optShowNumbers_CheckedChanged);
			((Control)chkFaintMoons).set_Anchor((AnchorStyles)1);
			((Control)chkFaintMoons).set_AutoSize(true);
			chkFaintMoons.set_Checked(Settings.Default.IncludeSmallMoons);
			chkFaintMoons.set_CheckState((CheckState)1);
			((Control)chkFaintMoons).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "IncludeSmallMoons", true, (DataSourceUpdateMode)1));
			((Control)chkFaintMoons).set_Location(new Point(542, 42));
			((Control)chkFaintMoons).set_Name("chkFaintMoons");
			((Control)chkFaintMoons).set_Size(new Size(121, 17));
			((Control)chkFaintMoons).set_TabIndex(11);
			((Control)chkFaintMoons).set_Text("Include small moons");
			((ButtonBase)chkFaintMoons).set_UseVisualStyleBackColor(true);
			chkFaintMoons.add_CheckedChanged((EventHandler)chkFaintMoons_CheckedChanged);
			((Control)chkMagnify).set_Anchor((AnchorStyles)1);
			((Control)chkMagnify).set_AutoSize(true);
			((Control)chkMagnify).set_Location(new Point(693, 76));
			((Control)chkMagnify).set_Name("chkMagnify");
			((Control)chkMagnify).set_Size(new Size(46, 17));
			((Control)chkMagnify).set_TabIndex(15);
			((Control)chkMagnify).set_Text("x0.1");
			((ButtonBase)chkMagnify).set_UseVisualStyleBackColor(true);
			chkMagnify.add_CheckedChanged((EventHandler)chkMagnify_CheckedChanged);
			((Control)updnHour).set_Anchor((AnchorStyles)1);
			updnHour.set_DecimalPlaces(2);
			updnHour.set_Increment(new decimal(new int[4] { 1, 0, 0, 65536 }));
			((Control)updnHour).set_Location(new Point(384, 30));
			updnHour.set_Maximum(new decimal(new int[4] { 48, 0, 0, 0 }));
			((Control)updnHour).set_Name("updnHour");
			((Control)updnHour).set_Size(new Size(53, 20));
			((Control)updnHour).set_TabIndex(4);
			((Control)updnHour).add_Enter((EventHandler)updnHour_Enter);
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)fileToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem1
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(787, 24));
			((Control)menuStrip1).set_TabIndex(16);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)fileToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[11]
			{
				(ToolStripItem)bWToolStripMenuItem,
				(ToolStripItem)plotWithEarthcorrectToolStripMenuItem,
				(ToolStripItem)toolStripSeparator2,
				(ToolStripItem)animatedGIFSettingsToolStripMenuItem,
				(ToolStripItem)saveAnimatedGIFToolStripMenuItem,
				(ToolStripItem)toolStripSeparator1,
				(ToolStripItem)copyToolStripMenuItem1,
				(ToolStripItem)printPreviewToolStripMenuItem,
				(ToolStripItem)printToolStripMenuItem,
				(ToolStripItem)toolStripSeparator,
				(ToolStripItem)saveToolStripMenuItem
			});
			((ToolStripItem)fileToolStripMenuItem).set_Name("fileToolStripMenuItem");
			((ToolStripItem)fileToolStripMenuItem).set_Size(new Size(108, 20));
			((ToolStripItem)fileToolStripMenuItem).set_Text("with Prediction...");
			((ToolStripItem)bWToolStripMenuItem).set_Image((Image)Resources.ColorHS);
			((ToolStripItem)bWToolStripMenuItem).set_ImageTransparentColor(Color.Magenta);
			((ToolStripItem)bWToolStripMenuItem).set_Name("bWToolStripMenuItem");
			((ToolStripItem)bWToolStripMenuItem).set_Size(new Size(272, 22));
			((ToolStripItem)bWToolStripMenuItem).set_Text("Plot in B&&W");
			((ToolStripItem)bWToolStripMenuItem).add_Click((EventHandler)bWToolStripMenuItem_Click);
			plotWithEarthcorrectToolStripMenuItem.set_CheckOnClick(true);
			((ToolStripItem)plotWithEarthcorrectToolStripMenuItem).set_Name("plotWithEarthcorrectToolStripMenuItem");
			((ToolStripItem)plotWithEarthcorrectToolStripMenuItem).set_Size(new Size(251, 22));
			((ToolStripItem)plotWithEarthcorrectToolStripMenuItem).set_Text("Plot on Earth plane, not Sky plane");
			((ToolStripItem)plotWithEarthcorrectToolStripMenuItem).add_Click((EventHandler)plotWithEarthcorrectToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator2).set_Name("toolStripSeparator2");
			((ToolStripItem)toolStripSeparator2).set_Size(new Size(269, 6));
			((ToolStripItem)animatedGIFSettingsToolStripMenuItem).set_Name("animatedGIFSettingsToolStripMenuItem");
			animatedGIFSettingsToolStripMenuItem.set_ShortcutKeys((Keys)131137);
			((ToolStripItem)animatedGIFSettingsToolStripMenuItem).set_Size(new Size(272, 22));
			((ToolStripItem)animatedGIFSettingsToolStripMenuItem).set_Text("Animated-GIF settings");
			((ToolStripItem)animatedGIFSettingsToolStripMenuItem).add_Click((EventHandler)animatedGIFSettingsToolStripMenuItem_Click);
			((ToolStripItem)saveAnimatedGIFToolStripMenuItem).set_Name("saveAnimatedGIFToolStripMenuItem");
			saveAnimatedGIFToolStripMenuItem.set_ShortcutKeys((Keys)131143);
			((ToolStripItem)saveAnimatedGIFToolStripMenuItem).set_Size(new Size(272, 22));
			((ToolStripItem)saveAnimatedGIFToolStripMenuItem).set_Text("Create animated-GIF");
			((ToolStripItem)saveAnimatedGIFToolStripMenuItem).add_Click((EventHandler)saveAnimatedGIFToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator1).set_Name("toolStripSeparator1");
			((ToolStripItem)toolStripSeparator1).set_Size(new Size(269, 6));
			((ToolStripItem)copyToolStripMenuItem1).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem1).set_ImageTransparentColor(Color.Magenta);
			((ToolStripItem)copyToolStripMenuItem1).set_Name("copyToolStripMenuItem1");
			copyToolStripMenuItem1.set_ShortcutKeys((Keys)131139);
			((ToolStripItem)copyToolStripMenuItem1).set_Size(new Size(272, 22));
			((ToolStripItem)copyToolStripMenuItem1).set_Text("&Copy");
			((ToolStripItem)copyToolStripMenuItem1).add_Click((EventHandler)copyToolStripMenuItem1_Click);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Image((Image)Resources.PrintPreview);
			((ToolStripItem)printPreviewToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Name("printPreviewToolStripMenuItem");
			((ToolStripItem)printPreviewToolStripMenuItem).set_Size(new Size(272, 22));
			((ToolStripItem)printPreviewToolStripMenuItem).set_Text("Print preview");
			((ToolStripItem)printPreviewToolStripMenuItem).add_Click((EventHandler)printPreviewToolStripMenuItem_Click);
			((ToolStripItem)printToolStripMenuItem).set_Image((Image)componentResourceManager.GetObject("printToolStripMenuItem.Image"));
			((ToolStripItem)printToolStripMenuItem).set_ImageTransparentColor(Color.Magenta);
			((ToolStripItem)printToolStripMenuItem).set_Name("printToolStripMenuItem");
			printToolStripMenuItem.set_ShortcutKeys((Keys)131152);
			((ToolStripItem)printToolStripMenuItem).set_Size(new Size(251, 22));
			((ToolStripItem)printToolStripMenuItem).set_Text("&Print");
			((ToolStripItem)printToolStripMenuItem).add_Click((EventHandler)printToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator).set_Name("toolStripSeparator");
			((ToolStripItem)toolStripSeparator).set_Size(new Size(269, 6));
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)componentResourceManager.GetObject("saveToolStripMenuItem.Image"));
			((ToolStripItem)saveToolStripMenuItem).set_ImageTransparentColor(Color.Magenta);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			saveToolStripMenuItem.set_ShortcutKeys((Keys)131155);
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(272, 22));
			((ToolStripItem)saveToolStripMenuItem).set_Text("&Save");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(60, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("&Help");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem1).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem1).set_Name("exitToolStripMenuItem1");
			((ToolStripItem)exitToolStripMenuItem1).set_Size(new Size(54, 20));
			((ToolStripItem)exitToolStripMenuItem1).set_Text("E&xit");
			((ToolStripItem)exitToolStripMenuItem1).add_Click((EventHandler)exitToolStripMenuItem1_Click);
			((Control)chkMagnify2).set_Anchor((AnchorStyles)1);
			((Control)chkMagnify2).set_AutoSize(true);
			((Control)chkMagnify2).set_Location(new Point(737, 76));
			((Control)chkMagnify2).set_Name("chkMagnify2");
			((Control)chkMagnify2).set_Size(new Size(43, 17));
			((Control)chkMagnify2).set_TabIndex(17);
			((Control)chkMagnify2).set_Text("x10");
			((ButtonBase)chkMagnify2).set_UseVisualStyleBackColor(true);
			chkMagnify2.add_CheckedChanged((EventHandler)chkMagnify2_CheckedChanged);
			((Control)chkOrbits).set_Anchor((AnchorStyles)1);
			((Control)chkOrbits).set_AutoSize(true);
			chkOrbits.set_Checked(Settings.Default.DisplayOrbits);
			chkOrbits.set_CheckState((CheckState)1);
			((Control)chkOrbits).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "DisplayOrbits", true, (DataSourceUpdateMode)1));
			((Control)chkOrbits).set_Location(new Point(542, 27));
			((Control)chkOrbits).set_Name("chkOrbits");
			((Control)chkOrbits).set_Size(new Size(88, 17));
			((Control)chkOrbits).set_TabIndex(10);
			((Control)chkOrbits).set_Text("Display orbits");
			((ButtonBase)chkOrbits).set_UseVisualStyleBackColor(true);
			chkOrbits.add_CheckedChanged((EventHandler)chkOrbits_CheckedChanged);
			((Control)chkMinorRings).set_Anchor((AnchorStyles)1);
			((Control)chkMinorRings).set_AutoSize(true);
			chkMinorRings.set_Checked(Settings.Default.IncludeMinorRings);
			chkMinorRings.set_CheckState((CheckState)1);
			((Control)chkMinorRings).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "IncludeMinorRings", true, (DataSourceUpdateMode)1));
			((Control)chkMinorRings).set_Location(new Point(542, 57));
			((Control)chkMinorRings).set_Name("chkMinorRings");
			((Control)chkMinorRings).set_Size(new Size(114, 17));
			((Control)chkMinorRings).set_TabIndex(18);
			((Control)chkMinorRings).set_Text("Include minor rings");
			((ButtonBase)chkMinorRings).set_UseVisualStyleBackColor(true);
			chkMinorRings.add_CheckedChanged((EventHandler)chkMinorRings_CheckedChanged);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(787, 615));
			((Control)this).get_Controls().Add((Control)(object)chkMinorRings);
			((Control)this).get_Controls().Add((Control)(object)chkMagnify2);
			((Control)this).get_Controls().Add((Control)(object)grpNames);
			((Control)this).get_Controls().Add((Control)(object)cmdDraw);
			((Control)this).get_Controls().Add((Control)(object)updnHour);
			((Control)this).get_Controls().Add((Control)(object)chkMagnify);
			((Control)this).get_Controls().Add((Control)(object)chkFaintMoons);
			((Control)this).get_Controls().Add((Control)(object)chkOrbits);
			((Control)this).get_Controls().Add((Control)(object)picPlanet);
			((Control)this).get_Controls().Add((Control)(object)trackBar1);
			((Control)this).get_Controls().Add((Control)(object)cmdCancel);
			((Control)this).get_Controls().Add((Control)(object)label2);
			((Control)this).get_Controls().Add((Control)(object)txtInterval);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)updnDuration);
			((Control)this).get_Controls().Add((Control)(object)updnDay);
			((Control)this).get_Controls().Add((Control)(object)updnMonth);
			((Control)this).get_Controls().Add((Control)(object)updnYear);
			((Control)this).get_Controls().Add((Control)(object)grpPlanet);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationEphemPlanetViewer", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_Icon((Icon)componentResourceManager.GetObject("$this.Icon"));
			((Form)this).set_Location(Settings.Default.LocationEphemPlanetViewer);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_Name("PlanetViewer");
			((Control)this).set_Text("Appearance of the planets and their satellites");
			((Form)this).add_FormClosing(new FormClosingEventHandler(PlanetViewer_FormClosing));
			((Form)this).add_Load((EventHandler)PlanetViewer_Load);
			((Control)this).add_Resize((EventHandler)PlanetViewer_Resize);
			((ISupportInitialize)picPlanet).EndInit();
			((Control)grpPlanet).ResumeLayout(false);
			((Control)grpPlanet).PerformLayout();
			((ISupportInitialize)updnYear).EndInit();
			((ISupportInitialize)updnMonth).EndInit();
			((ISupportInitialize)updnDay).EndInit();
			((ISupportInitialize)updnDuration).EndInit();
			((ISupportInitialize)trackBar1).EndInit();
			((Control)grpNames).ResumeLayout(false);
			((Control)grpNames).PerformLayout();
			((ISupportInitialize)updnHour).EndInit();
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
