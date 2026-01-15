using System;
using System.ComponentModel;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.IO;
using System.Reflection;
using System.Text;
using System.Windows.Forms;
using Occult.Eclipses;
using Occult.Properties;

namespace Occult
{
	public class Transits : Form
	{
		private readonly string AppPath;

		private const double Radian = 180.0 / Math.PI;

		private const double TwoPi = Math.PI * 2.0;

		private static double[] TransitJD_TT = new double[30];

		private static int[] PlanetID = new int[30];

		private double CurrentTransitJD;

		private static int Count;

		private static string Prediction;

		private static string TransitType;

		private static bool BWFlag;

		private IContainer components;

		private ListBox lstTransits;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem fileToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator1;

		private ToolStripSeparator toolStripSeparator2;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem1;

		private TextBox txtTransit;

		private NumericUpDown UpDownYear;

		private Label label1;

		private PictureBox picSun;

		private PictureBox picEarth;

		private TextBox txtLimitLine;

		private Label label2;

		private ToolStripMenuItem saveGraphicsToolStripMenuItem;

		private ToolStripMenuItem copyTextToolStripMenuItem;

		private ToolStripMenuItem bWEarthMapsToolStripMenuItem;

		private ToolStripMenuItem printToolStripMenuItem;

		private Label label3;

		private ComboBox cmbSites;

		private Button cmdMultiSite;

		private Label label4;

		public Transits()
		{
			InitializeComponent();
			AppPath = Utilities.AppPath;
			UpDownYear.set_Value((decimal)(DateTime.Now.Year - DateTime.Now.Year % 100));
			BWFlag = false;
		}

		private void Transits_Load(object sender, EventArgs e)
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
			string[] files = Directory.GetFiles(AppPath + "\\Sites", "*.site");
			foreach (string path in files)
			{
				cmbSites.get_Items().Add((object)Path.GetFileName(path));
			}
			if ((cmbSites.get_Items().get_Count() > 0) & (Settings.Default.MultiLocationSelectedIndex >= 0) & (Settings.Default.MultiLocationSelectedIndex < cmbSites.get_Items().get_Count()))
			{
				((ListControl)cmbSites).set_SelectedIndex(Settings.Default.MultiLocationSelectedIndex);
				((Control)cmbSites).Focus();
			}
		}

		private void exitToolStripMenuItem1_Click(object sender, EventArgs e)
		{
			try
			{
				((Form)TransitMaths.TransitMultiLocation).Close();
				((Component)(object)TransitMaths.TransitMultiLocation).Dispose();
			}
			catch
			{
			}
			((Form)this).Close();
			((Component)this).Dispose();
		}

		public void FindTransits(int Year_TT)
		{
			double num = 0.0;
			double num2 = 0.0;
			double num3 = 0.0;
			int num4 = 3;
			string text = "";
			lstTransits.get_Items().Clear();
			Count = 0;
			Cursor.set_Current(Cursors.get_WaitCursor());
			for (int i = Year_TT; i <= Year_TT + 100; i++)
			{
				for (int j = 1; j <= 4; j++)
				{
					switch (j)
					{
					case 1:
						num = Math.Floor(2451662.13 + (365.25445 + 1.8E-08 * (double)(i - 2000)) * (double)(i - 2000)) + 0.5;
						num2 = num + 20.0;
						num4 = 1;
						break;
					case 2:
						num = Math.Floor(2451697.63 + (365.25162 + 4.29E-08 * (double)(i - 2000)) * (double)(i - 2000)) + 0.5;
						num2 = num + 20.0;
						num4 = 2;
						break;
					case 3:
						num = Math.Floor(2451847.92 + (365.25398 + 1.73E-08 * (double)(i - 2000)) * (double)(i - 2000)) + 0.5;
						num2 = num + 20.0;
						num4 = 1;
						break;
					case 4:
						num = Math.Floor(2451875.97 + (365.25105 + 4.03E-08 * (double)(i - 2000)) * (double)(i - 2000)) + 0.5;
						num2 = num + 20.0;
						num4 = 2;
						break;
					}
					if (Math.Abs(i - 2000) > 2000)
					{
						num -= 4.0;
						num2 += 8.0;
					}
					if (Math.Abs(i - 2000) > 10000)
					{
						num -= 4.0;
						num2 += 8.0;
					}
					TransitMaths.DeltaLB(num, num4, out var deltaL, out var deltaB, out var SunRadius);
					TransitMaths.DeltaLB(num2, num4, out var deltaL2, out var deltaB2, out SunRadius);
					if (!((Math.Abs(deltaL) < 90.0) & (Math.Sign(deltaL) != Math.Sign(deltaL2))))
					{
						continue;
					}
					num3 = Math.Floor((0.0 - deltaL) / (deltaL2 - deltaL) * (num2 - num));
					TransitMaths.DeltaLB(num + num3, num4, out deltaL, out deltaB, out SunRadius);
					TransitMaths.DeltaLB(num + num3 + 1.0, num4, out deltaL2, out deltaB2, out SunRadius);
					double num5 = (0.0 - deltaL) / (deltaL2 - deltaL);
					bool flag = false;
					while (num5 >= 1.0)
					{
						num3 += 1.0;
						num5 -= 1.0;
						flag = true;
					}
					while (num5 < 0.0)
					{
						num3 -= 1.0;
						num5 += 1.0;
						flag = true;
					}
					if (flag)
					{
						TransitMaths.DeltaLB(num + num3, num4, out deltaL, out deltaB, out SunRadius);
						TransitMaths.DeltaLB(num + num3 + 1.0, num4, out deltaL2, out deltaB2, out SunRadius);
						num5 = (0.0 - deltaL) / (deltaL2 - deltaL);
					}
					if (((Math.Abs(deltaL) < 90.0) & (Math.Sign(deltaL) != Math.Sign(deltaL2))) && Math.Abs(60.0 * (num5 * (deltaB2 - deltaB) + deltaB)) < 20.0)
					{
						text = "";
						if (Math.Abs(60.0 * (num5 * (deltaB2 - deltaB) + deltaB)) > SunRadius - 1.0)
						{
							text = "- possible?";
						}
						if (Math.Abs(60.0 * (num5 * (deltaB2 - deltaB) + deltaB)) > SunRadius + 1.0)
						{
							text = "- near-miss";
						}
						StringBuilder stringBuilder = new StringBuilder();
						TransitJD_TT[Count] = Math.Floor(num + num3 - 0.5) + 0.5;
						PlanetID[Count] = num4;
						stringBuilder.AppendFormat(" {0,4:F1}hr  ", num5 * 24.0);
						if (num4 == 1)
						{
							stringBuilder.Append("Mercury ");
						}
						else
						{
							stringBuilder.Append("Venus ");
						}
						stringBuilder.Append(text);
						lstTransits.get_Items().Add((object)(Utilities.Date_from_JD(TransitJD_TT[Count], 0, Use_BC: false) + stringBuilder.ToString()));
						Count++;
					}
				}
			}
			Cursor.set_Current(Cursors.get_Default());
		}

		private void numericUpDown1_ValueChanged(object sender, EventArgs e)
		{
			FindTransits((int)UpDownYear.get_Value());
			((ListControl)lstTransits).set_SelectedIndex(0);
			((Control)lstTransits).Focus();
		}

		public void Transit(double JD, int Planet, out string Prediction)
		{
			TransitMaths.TransitElements(JD, Planet);
			Prediction = TransitMaths.GeocentricContactTime(out TransitType, out var LimitLine);
			((Control)txtLimitLine).set_Text(LimitLine);
			DrawSun(Planet);
			DrawEarth();
			((Control)this).set_Cursor(Cursors.get_Default());
		}

		private void lstTransits_SelectedIndexChanged(object sender, EventArgs e)
		{
			int selectedIndex = ((ListControl)lstTransits).get_SelectedIndex();
			CurrentTransitJD = TransitJD_TT[selectedIndex];
			Transit(CurrentTransitJD, PlanetID[selectedIndex], out Prediction);
			((Control)txtTransit).set_Text(Prediction);
			((Control)cmdMultiSite).set_Enabled(TransitMaths.Use_UT);
		}

		private void DrawSun(int Planet)
		{
			float num = ((Control)picSun).get_Width();
			float num2 = num / 2f;
			float num3 = ((Control)picSun).get_Height();
			float num4 = num3 / 2f;
			float num5 = num / 3f;
			Image image = new Bitmap(((Control)picSun).get_Width(), ((Control)picSun).get_Height());
			Graphics graphics = Graphics.FromImage(image);
			if (Settings.Default.GraphicsSmoothed)
			{
				graphics.SmoothingMode = SmoothingMode.AntiAlias;
			}
			Pen pen = new Pen(Color.Black, 1f);
			if (Planet == 2)
			{
				pen.Width = 4.5f;
			}
			pen.Color = Color.Black;
			graphics.FillRectangle(Brushes.Gray, 0f, 0f, num, num3);
			graphics.FillEllipse(Brushes.Gold, num / 2f - num5, num3 / 2f - num5, 2f * num5, 2f * num5);
			graphics.DrawLine(pen, (float)((double)num2 - (TransitMaths.sunX[0] - 10.0 * TransitMaths.X[1] / TransitMaths.LL1) * (double)num5), (float)((double)num4 - (TransitMaths.sunY[0] - 10.0 * TransitMaths.Y[1] / TransitMaths.LL1) * (double)num5), (float)((double)num2 - (TransitMaths.sunX[1] + 10.0 * TransitMaths.X[1] / TransitMaths.LL1) * (double)num5), (float)((double)num4 - (TransitMaths.sunY[1] + 10.0 * TransitMaths.Y[1] / TransitMaths.LL1) * (double)num5));
			Font font = new Font("Courier New", 10f);
			graphics.DrawString("N", font, Brushes.LightGoldenrodYellow, num2 - 4f, 1f);
			graphics.DrawString("E", font, Brushes.LightGoldenrodYellow, 1f, num4 - 4f);
			picSun.set_Image(image);
			graphics.Dispose();
		}

		private void DrawEarth()
		{
			Image image = new Bitmap(((Control)picEarth).get_Width(), ((Control)picEarth).get_Height());
			Graphics graphics = Graphics.FromImage(image);
			if (Settings.Default.GraphicsSmoothed)
			{
				graphics.SmoothingMode = SmoothingMode.AntiAlias;
			}
			float num = ((Control)picEarth).get_Width();
			float num2 = ((Control)picEarth).get_Height();
			float plotScale = num2 / 2.6f;
			float x = 0f;
			float y = 0f;
			if (BWFlag)
			{
				graphics.FillRectangle(Brushes.White, 0f, 0f, num, num2);
			}
			else
			{
				graphics.FillRectangle(Brushes.Black, 0f, 0f, num, num2);
			}
			double num3 = (double)TransitMaths.deltaTUncertainty / 240.0;
			bool flag = TransitMaths.deltaTUncertainty > 1;
			string text = "Uncertainty in Earth's orientation = ±";
			text = ((num3 < 10.0) ? (text + string.Format("{0,1:f1} degrees", num3)) : ((!(num3 < 180.0)) ? (text + string.Format("{0,1:f1} revolutions", num3 / 360.0)) : (text + string.Format("{0,1:f0} degrees", num3))));
			Maps.EarthGlobe(graphics, plotScale, 0.27f * num, 0.55f * num2, (float)TransitMaths.ZenithL[0], (float)TransitMaths.ZenithD[0], 0.0, 0.0, 0.0, 0.0, ShowSunLit: false, SiteCentered: false, PlotCities: false, FullResolution: false, BWFlag, Mirrored: false);
			float x2;
			float y2;
			float z;
			if (TransitMaths.LimitCount > 0)
			{
				Pen pen = ((!TransitMaths.OuterLimit) ? Pens.Blue : Pens.Red);
				for (int i = 0; i < TransitMaths.LimitCount; i++)
				{
					Maps.GlobeProjection(TransitMaths.LimitLong[i] / (180.0 / Math.PI), TransitMaths.LimitLat[i] / (180.0 / Math.PI), out x2, out y2, out z, Mirrored: false);
					x2 += 0.27f * num;
					y2 = 0f - y2 + 0.55f * num2;
					if (i > 0)
					{
						graphics.DrawLine(pen, x, y, x2, y2);
					}
					x = x2;
					y = y2;
				}
			}
			Maps.EarthGlobe(graphics, plotScale, 0.73f * num, 0.55f * num2, (float)TransitMaths.ZenithL[2], (float)TransitMaths.ZenithD[2], 0.0, 0.0, 0.0, 0.0, ShowSunLit: false, SiteCentered: false, PlotCities: false, FullResolution: false, BWFlag, Mirrored: false);
			if (TransitMaths.LimitCount > 0)
			{
				Pen pen = ((!TransitMaths.OuterLimit) ? Pens.Blue : Pens.Red);
				for (int j = 0; j < TransitMaths.LimitCount; j++)
				{
					Maps.GlobeProjection(TransitMaths.LimitLong[j] / (180.0 / Math.PI), TransitMaths.LimitLat[j] / (180.0 / Math.PI), out x2, out y2, out z, Mirrored: false);
					x2 += 0.73f * num;
					y2 = 0f - y2 + 0.55f * num2;
					if (j > 0)
					{
						graphics.DrawLine(pen, x, y, x2, y2);
					}
					x = x2;
					y = y2;
				}
			}
			Font font = new Font("Times New Roman", 10f);
			Brush brush = (BWFlag ? Brushes.Black : Brushes.LightGoldenrodYellow);
			graphics.DrawString(TransitMaths.TransitLabel, font, brush, (num - graphics.MeasureString(TransitMaths.TransitLabel, font).Width) / 2f, 1f);
			graphics.DrawString(TransitMaths.ContactTimes[0], font, brush, 0.27f * num - graphics.MeasureString(TransitMaths.ContactTimes[0], font).Width / 2f, 18f);
			graphics.DrawString(TransitMaths.ContactTimes[2], font, brush, 0.73f * num - graphics.MeasureString(TransitMaths.ContactTimes[2], font).Width / 2f, 18f);
			font = new Font("Times New Roman", 7f, FontStyle.Regular);
			graphics.DrawString("Occult " + Assembly.GetExecutingAssembly().GetName(copiedName: false).Version, font, brush, 6f, num2 - 12f);
			if (flag)
			{
				float width = graphics.MeasureString(text, font).Width;
				graphics.DrawString(text, font, brush, (num - width) / 2f, num2 - 12f);
			}
			picEarth.set_Image(image);
			graphics.Dispose();
		}

		private void bWEarthMapsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			BWFlag = !BWFlag;
			if (BWFlag)
			{
				((ToolStripItem)bWEarthMapsToolStripMenuItem).set_Text("Colour earth maps");
			}
			else
			{
				((ToolStripItem)bWEarthMapsToolStripMenuItem).set_Text("B&&W earth maps");
			}
			DrawEarth();
		}

		private void copyTextToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(Prediction + "\r\n\r\n" + ((Control)txtLimitLine).get_Text());
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings.Default.Save_Transits = Output.SaveAppendPredictionText(Prediction + "\r\n\r\n" + ((Control)txtLimitLine).get_Text() + "\r\n", Utilities.Date_from_JD(CurrentTransitJD, 0) + "Transit", Settings.Default.Save_Transits);
		}

		private void saveGraphicsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings.Default.Save_Transits = Output.SaveGraphic(picEarth.get_Image(), Utilities.Date_from_JD(CurrentTransitJD, 0) + " Transit Earthmaps", Settings.Default.Save_Transits);
			Settings.Default.Save_Transits = Output.SaveGraphic(picSun.get_Image(), Utilities.Date_from_JD(CurrentTransitJD, 0) + " Transit - Sun", Settings.Default.Save_Transits);
		}

		private void printToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.PrintText(Prediction + "\r\n\r\n" + ((Control)txtLimitLine).get_Text());
		}

		private void UpDownYear_Enter(object sender, EventArgs e)
		{
			((UpDownBase)UpDownYear).Select(0, 10);
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Transits");
		}

		private void cmdMultiSite_Click(object sender, EventArgs e)
		{
			TransitMaths.MultiLocationTransitComputation(cmbSites.get_Items().get_Item(((ListControl)cmbSites).get_SelectedIndex()).ToString(), FullSitePrecisionInOutput: false);
			TransitMaths.Show_TransitMultilocations();
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
			//IL_0d95: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d9f: Expected O, but got Unknown
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(Transits));
			lstTransits = new ListBox();
			menuStrip1 = new MenuStrip();
			fileToolStripMenuItem = new ToolStripMenuItem();
			bWEarthMapsToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator2 = new ToolStripSeparator();
			copyTextToolStripMenuItem = new ToolStripMenuItem();
			printToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator1 = new ToolStripSeparator();
			saveToolStripMenuItem = new ToolStripMenuItem();
			saveGraphicsToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem1 = new ToolStripMenuItem();
			txtTransit = new TextBox();
			UpDownYear = new NumericUpDown();
			label1 = new Label();
			picSun = new PictureBox();
			picEarth = new PictureBox();
			txtLimitLine = new TextBox();
			label2 = new Label();
			label3 = new Label();
			cmbSites = new ComboBox();
			cmdMultiSite = new Button();
			label4 = new Label();
			((Control)menuStrip1).SuspendLayout();
			((ISupportInitialize)UpDownYear).BeginInit();
			((ISupportInitialize)picSun).BeginInit();
			((ISupportInitialize)picEarth).BeginInit();
			((Control)this).SuspendLayout();
			((Control)lstTransits).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstTransits).set_FormattingEnabled(true);
			lstTransits.set_ItemHeight(14);
			((Control)lstTransits).set_Location(new Point(10, 74));
			((Control)lstTransits).set_Name("lstTransits");
			((Control)lstTransits).set_Size(new Size(300, 270));
			((Control)lstTransits).set_TabIndex(0);
			lstTransits.add_SelectedIndexChanged((EventHandler)lstTransits_SelectedIndexChanged);
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)fileToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem1
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(803, 24));
			((Control)menuStrip1).set_TabIndex(6);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)fileToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[7]
			{
				(ToolStripItem)bWEarthMapsToolStripMenuItem,
				(ToolStripItem)toolStripSeparator2,
				(ToolStripItem)copyTextToolStripMenuItem,
				(ToolStripItem)printToolStripMenuItem,
				(ToolStripItem)toolStripSeparator1,
				(ToolStripItem)saveToolStripMenuItem,
				(ToolStripItem)saveGraphicsToolStripMenuItem
			});
			((ToolStripItem)fileToolStripMenuItem).set_Name("fileToolStripMenuItem");
			((ToolStripItem)fileToolStripMenuItem).set_Size(new Size(113, 20));
			((ToolStripItem)fileToolStripMenuItem).set_Text("with Predictions...");
			((ToolStripItem)bWEarthMapsToolStripMenuItem).set_Image((Image)Resources.ColorHS);
			((ToolStripItem)bWEarthMapsToolStripMenuItem).set_Name("bWEarthMapsToolStripMenuItem");
			((ToolStripItem)bWEarthMapsToolStripMenuItem).set_Size(new Size(218, 22));
			((ToolStripItem)bWEarthMapsToolStripMenuItem).set_Text("B&&W Earth Maps");
			((ToolStripItem)bWEarthMapsToolStripMenuItem).add_Click((EventHandler)bWEarthMapsToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator2).set_Name("toolStripSeparator2");
			((ToolStripItem)toolStripSeparator2).set_Size(new Size(215, 6));
			((ToolStripItem)copyTextToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyTextToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyTextToolStripMenuItem).set_Name("copyTextToolStripMenuItem");
			copyTextToolStripMenuItem.set_ShortcutKeys((Keys)131139);
			((ToolStripItem)copyTextToolStripMenuItem).set_Size(new Size(218, 22));
			((ToolStripItem)copyTextToolStripMenuItem).set_Text("Copy text");
			((ToolStripItem)copyTextToolStripMenuItem).add_Click((EventHandler)copyTextToolStripMenuItem_Click);
			((ToolStripItem)printToolStripMenuItem).set_Image((Image)componentResourceManager.GetObject("printToolStripMenuItem.Image"));
			((ToolStripItem)printToolStripMenuItem).set_ImageTransparentColor(Color.Magenta);
			((ToolStripItem)printToolStripMenuItem).set_Name("printToolStripMenuItem");
			printToolStripMenuItem.set_ShortcutKeys((Keys)131152);
			((ToolStripItem)printToolStripMenuItem).set_Size(new Size(218, 22));
			((ToolStripItem)printToolStripMenuItem).set_Text("&Print");
			((ToolStripItem)printToolStripMenuItem).add_Click((EventHandler)printToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator1).set_Name("toolStripSeparator1");
			((ToolStripItem)toolStripSeparator1).set_Size(new Size(215, 6));
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)componentResourceManager.GetObject("saveToolStripMenuItem.Image"));
			((ToolStripItem)saveToolStripMenuItem).set_ImageTransparentColor(Color.Magenta);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			saveToolStripMenuItem.set_ShortcutKeys((Keys)131155);
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(218, 22));
			((ToolStripItem)saveToolStripMenuItem).set_Text("&Save text");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
			((ToolStripItem)saveGraphicsToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveGraphicsToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveGraphicsToolStripMenuItem).set_Name("saveGraphicsToolStripMenuItem");
			saveGraphicsToolStripMenuItem.set_ShortcutKeys((Keys)196691);
			((ToolStripItem)saveGraphicsToolStripMenuItem).set_Size(new Size(218, 22));
			((ToolStripItem)saveGraphicsToolStripMenuItem).set_Text("Save graphics");
			((ToolStripItem)saveGraphicsToolStripMenuItem).add_Click((EventHandler)saveGraphicsToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(60, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("&Help");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem1).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem1).set_Name("exitToolStripMenuItem1");
			((ToolStripItem)exitToolStripMenuItem1).set_Size(new Size(53, 20));
			((ToolStripItem)exitToolStripMenuItem1).set_Text("E&xit");
			((ToolStripItem)exitToolStripMenuItem1).add_Click((EventHandler)exitToolStripMenuItem1_Click);
			((TextBoxBase)txtTransit).set_BorderStyle((BorderStyle)1);
			((Control)txtTransit).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtTransit).set_Location(new Point(363, 166));
			((TextBoxBase)txtTransit).set_Multiline(true);
			((Control)txtTransit).set_Name("txtTransit");
			((TextBoxBase)txtTransit).set_ReadOnly(true);
			((Control)txtTransit).set_Size(new Size(392, 178));
			((Control)txtTransit).set_TabIndex(2);
			((Control)UpDownYear).set_BackColor(SystemColors.Control);
			((UpDownBase)UpDownYear).set_BorderStyle((BorderStyle)1);
			((Control)UpDownYear).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			UpDownYear.set_Increment(new decimal(new int[4] { 100, 0, 0, 0 }));
			((Control)UpDownYear).set_Location(new Point(230, 31));
			UpDownYear.set_Maximum(new decimal(new int[4] { 16000, 0, 0, 0 }));
			UpDownYear.set_Minimum(new decimal(new int[4] { 13000, 0, 0, -2147483648 }));
			((Control)UpDownYear).set_Name("UpDownYear");
			((Control)UpDownYear).set_Size(new Size(76, 26));
			((Control)UpDownYear).set_TabIndex(1);
			((UpDownBase)UpDownYear).set_TextAlign((HorizontalAlignment)1);
			UpDownYear.add_ValueChanged((EventHandler)numericUpDown1_ValueChanged);
			((Control)UpDownYear).add_Enter((EventHandler)UpDownYear_Enter);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(29, 34));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(202, 20));
			((Control)label1).set_TabIndex(7);
			((Control)label1).set_Text("Transits during century :");
			((Control)picSun).set_BackColor(Color.Gray);
			((Control)picSun).set_Location(new Point(416, 34));
			((Control)picSun).set_Name("picSun");
			((Control)picSun).set_Size(new Size(128, 128));
			picSun.set_TabIndex(5);
			picSun.set_TabStop(false);
			((Control)picEarth).set_BackColor(Color.Black);
			((Control)picEarth).set_Location(new Point(325, 352));
			((Control)picEarth).set_Name("picEarth");
			((Control)picEarth).set_Size(new Size(469, 235));
			picEarth.set_TabIndex(6);
			picEarth.set_TabStop(false);
			((TextBoxBase)txtLimitLine).set_BorderStyle((BorderStyle)1);
			((Control)txtLimitLine).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLimitLine).set_Location(new Point(136, 352));
			((TextBoxBase)txtLimitLine).set_Multiline(true);
			((Control)txtLimitLine).set_Name("txtLimitLine");
			((TextBoxBase)txtLimitLine).set_ReadOnly(true);
			txtLimitLine.set_ScrollBars((ScrollBars)2);
			((Control)txtLimitLine).set_Size(new Size(174, 235));
			((Control)txtLimitLine).set_TabIndex(5);
			((TextBoxBase)txtLimitLine).set_WordWrap(false);
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Font(new Font("Microsoft Sans Serif", 9.75f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label2).set_Location(new Point(26, 421));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(106, 112));
			((Control)label2).set_TabIndex(10);
			((Control)label2).set_Text("If the transit is\r\npartial (eg 1999),\r\nor only occurs \r\nover part of the\r\nEarth (eg 1937)\r\nthe limit line is\r\ngiven here       =>");
			label2.set_TextAlign(ContentAlignment.MiddleLeft);
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Location(new Point(13, 61));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(98, 13));
			((Control)label3).set_TabIndex(8);
			((Control)label3).set_Text("Low-precision scan");
			cmbSites.set_DropDownStyle((ComboBoxStyle)2);
			((ListControl)cmbSites).set_FormattingEnabled(true);
			((Control)cmbSites).set_Location(new Point(608, 98));
			cmbSites.set_MaxDropDownItems(20);
			((Control)cmbSites).set_Name("cmbSites");
			((Control)cmbSites).set_Size(new Size(133, 21));
			((Control)cmbSites).set_TabIndex(4);
			((Control)cmdMultiSite).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdMultiSite).set_Location(new Point(618, 54));
			((Control)cmdMultiSite).set_Name("cmdMultiSite");
			((Control)cmdMultiSite).set_Size(new Size(113, 38));
			((Control)cmdMultiSite).set_TabIndex(3);
			((Control)cmdMultiSite).set_Text("Multi-site predictions using:");
			((ButtonBase)cmdMultiSite).set_UseVisualStyleBackColor(true);
			((Control)cmdMultiSite).add_Click((EventHandler)cmdMultiSite_Click);
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label4).set_Location(new Point(221, 59));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(91, 13));
			((Control)label4).set_TabIndex(9);
			((Control)label4).set_Text("(-13000 to 16000)");
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(803, 595));
			((Control)this).get_Controls().Add((Control)(object)label4);
			((Control)this).get_Controls().Add((Control)(object)cmdMultiSite);
			((Control)this).get_Controls().Add((Control)(object)cmbSites);
			((Control)this).get_Controls().Add((Control)(object)label3);
			((Control)this).get_Controls().Add((Control)(object)label2);
			((Control)this).get_Controls().Add((Control)(object)txtLimitLine);
			((Control)this).get_Controls().Add((Control)(object)picEarth);
			((Control)this).get_Controls().Add((Control)(object)UpDownYear);
			((Control)this).get_Controls().Add((Control)(object)picSun);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)txtTransit);
			((Control)this).get_Controls().Add((Control)(object)lstTransits);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationEclipseTransit", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_FormBorderStyle((FormBorderStyle)2);
			((Form)this).set_Icon((Icon)componentResourceManager.GetObject("$this.Icon"));
			((Form)this).set_Location(Settings.Default.LocationEclipseTransit);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Form)this).set_MaximizeBox(false);
			((Control)this).set_Name("Transits");
			((Control)this).set_Text("Transits");
			((Form)this).add_Load((EventHandler)Transits_Load);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((ISupportInitialize)UpDownYear).EndInit();
			((ISupportInitialize)picSun).EndInit();
			((ISupportInitialize)picEarth).EndInit();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
