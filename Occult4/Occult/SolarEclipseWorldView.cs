using System;
using System.ComponentModel;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult
{
	public class SolarEclipseWorldView : Form
	{
		private readonly string AppPath;

		private const double Radian = 180.0 / Math.PI;

		private static double[] EclipseJD_TT = new double[60];

		private static bool FormLoaded = false;

		private static bool ShowToolTip = true;

		private bool GetToolTip = true;

		public bool MouseValid;

		private static int MouseX = 0;

		private static int MouseY = 0;

		internal static bool MouseMoveDisabled = false;

		private float Longitude;

		private float Latitude;

		internal static bool CancelPlanetSearch = false;

		private IContainer components;

		private MenuStrip menuStrip1;

		private NumericUpDown UpDownYear;

		private ListBox lstEclipses;

		private Label label2;

		public PictureBox picEclipse;

		private ToolStripMenuItem withPredictionToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem printPreviewToolStripMenuItem;

		private ToolStripMenuItem printToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripMenuItem multisitePredictionToolStripMenuItem;

		private ToolStripMenuItem pathCoordinatesToolStripMenuItem;

		private ToolStripMenuItem multiLocationPredictionsToolStripMenuItem;

		private ToolStripMenuItem detailedMapToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private ToolTip toolTip1;

		private ToolStripMenuItem showCoordinatesInTooltipToolStripMenuItem;

		private ToolStripMenuItem bWToolStripMenuItem;

		private ToolStripMenuItem colorGraphicToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator1;

		private ToolStripSeparator toolStripSeparator2;

		private ToolStripMenuItem printListOfEclipsesToolStripMenuItem;

		private ToolStripMenuItem saveListOfEclipsesToolStripMenuItem;

		private ToolStripMenuItem copyListOfEclipsesToolStripMenuItem;

		private ToolStripMenuItem printPreviewListOfEclipsesToolStripMenuItem;

		private ToolStripMenuItem saveGoogleEarthKmlFileToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator3;

		private ToolStripMenuItem localPredictionsToolStripMenuItem;

		private Label label1;

		private ToolStripMenuItem saveGoogleMapHtmFileToolStripMenuItem;

		private ToolStripMenuItem savemfiFileForCenterPathToolStripMenuItem;

		private ToolStripMenuItem viewInGoogleEartToolStripMenuItem;

		private Button cmdViewGoogleEarth;

		private Button cmdPath;

		private Button cmdMultiLocation;

		private ToolStripSeparator toolStripSeparator4;

		private Button cmdGoogleMapSave;

		private ContextMenuStrip contextMenuStrip1;

		private ToolStripMenuItem detailedMapToolStripMenuItem1;

		private ToolStripMenuItem localPredictionsToolStripMenuItem1;

		private ToolStripMenuItem multiLocationPredictionsToolStripMenuItem1;

		private ToolStripMenuItem pathCoordinatesToolStripMenuItem1;

		private ToolStripMenuItem viewInGoogleEarthToolStripMenuItem;

		private ToolStripMenuItem saveGoogleEarthToolStripMenuItem;

		private ToolStripMenuItem kMLFileToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator5;

		private Button button1;

		private Label label3;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripMenuItem viewBesselianToolStripMenuItem;

		private Label label4;

		private ToolStripMenuItem findClosePlanetaryConjunctionsToolStripMenuItem;

		private ToolStripMenuItem lunarLimbGenerateSaveToolStripMenuItem;

		public SolarEclipseWorldView()
		{
			InitializeComponent();
			AppPath = Utilities.AppPath;
		}

		private void SolarEclipseWorldView_Load(object sender, EventArgs e)
		{
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			UpDownYear.set_Value((decimal)DateTime.Now.Year);
			bWToolStripMenuItem.set_Checked(Settings.Default.BWFlag);
			colorGraphicToolStripMenuItem.set_Checked(!bWToolStripMenuItem.get_Checked());
			if (((Control)this).get_Top() < -30000)
			{
				((Control)this).set_Top(0);
			}
			if ((double)((Control)this).get_Left() + (double)((Control)this).get_Width() / 1.2 < (double)Screen.GetWorkingArea((Control)(object)this).Left)
			{
				((Control)this).set_Left(Screen.GetWorkingArea((Control)(object)this).Left);
			}
			FormLoaded = true;
			ToolStripMenuItem obj = viewInGoogleEarthToolStripMenuItem;
			ToolStripMenuItem obj2 = viewInGoogleEartToolStripMenuItem;
			bool googleEarthInstalled;
			((Control)cmdViewGoogleEarth).set_Visible(googleEarthInstalled = Settings.Default.GoogleEarthInstalled);
			bool visible;
			((ToolStripItem)obj2).set_Visible(visible = googleEarthInstalled);
			((ToolStripItem)obj).set_Visible(visible);
			FindSolarEclipse((int)UpDownYear.get_Value());
			((ListControl)lstEclipses).set_SelectedIndex(0);
			((Control)lstEclipses).Focus();
		}

		private void SolarEclipseWorldView_FormClosing(object sender, FormClosingEventArgs e)
		{
			//IL_0006: Unknown result type (might be due to invalid IL or missing references)
			Settings.Default.SolarEclipseWorldMapWindow = ((Form)this).get_WindowState();
		}

		private void SolarEclipseWorldView_Resize(object sender, EventArgs e)
		{
			if (!((((Control)this).get_Width() < 100) | (((Control)this).get_Height() < 220)))
			{
				((Control)picEclipse).set_Width(((Control)this).get_Width() - 23);
				((Control)picEclipse).set_Height(((Control)this).get_Height() - 186);
				if (FormLoaded)
				{
					SolarEclipses.SolarEclipseBessellianElements(EclipseJD_TT[((ListControl)lstEclipses).get_SelectedIndex()]);
				}
			}
		}

		private void UpDownYear_ValueChanged(object sender, EventArgs e)
		{
			FindSolarEclipse((int)UpDownYear.get_Value());
			((ListControl)lstEclipses).set_SelectedIndex(0);
		}

		public void FindSolarEclipse(int Year_TT)
		{
			double RA = 0.0;
			double Dec = 0.0;
			double Distance = 0.0;
			double Longitude = 0.0;
			double Latitude = 0.0;
			double PlanetHeliocentricDistance = 0.0;
			int num = 0;
			lstEclipses.get_Items().Clear();
			double num2 = Utilities.JD_from_Date(Year_TT, 1, 1.0);
			double num3 = Utilities.JD_from_Date(Year_TT + 10, 1, 1.0);
			_ = Utilities.delta_T(Year_TT, 7, 1.0) / 3600.0;
			double num4 = num2;
			do
			{
				double num5 = 0.0;
				double Parallax;
				double MoonLatitude;
				double num6;
				do
				{
					Utilities.QuickMoon(num4 + num5, out var _, out var _, out Parallax, out var MoonLongitude, out MoonLatitude);
					Utilities.QuickPlanet(num4 + num5, 3, EquinoxOfDate: true, out RA, out Dec, out Distance, out Longitude, out Latitude, out PlanetHeliocentricDistance);
					for (num6 = (Longitude - MoonLongitude) * (180.0 / Math.PI); num6 < -90.0; num6 += 360.0)
					{
					}
					while (num6 > 270.0)
					{
						num6 -= 360.0;
					}
					num5 += num6 / 12.191;
				}
				while (Math.Abs(num6) > 0.003);
				MoonLatitude *= 180.0 / Math.PI;
				if (Math.Abs(MoonLatitude) < 1.6)
				{
					string text = "annular";
					double num7 = 0.2725076 * Math.Sin(Parallax) * (180.0 / Math.PI) * 3600.0 / (959.63 / Distance);
					text = ((num7 < 0.983) ? "Annular" : ((!(num7 > 1.002)) ? "Annular-Total" : "Total"));
					string text2 = ((Math.Abs(MoonLatitude) > 1.35) ? "Possible partial eclipse" : ((Math.Abs(MoonLatitude) > 1.03) ? "Partial eclipse " : ((!(Math.Abs(MoonLatitude) > 0.9)) ? (text + " eclipse") : ("Possible " + text + " eclipse"))));
					EclipseJD_TT[num] = Math.Floor(num4 + num5 - 0.5) + 0.5;
					StringBuilder stringBuilder = new StringBuilder();
					stringBuilder.Append(Utilities.Date_from_JD(EclipseJD_TT[num], 0, Use_BC: false));
					stringBuilder.AppendFormat(" {0,4:F1}hr   ", (num4 + num5 - EclipseJD_TT[num]) * 24.0);
					lstEclipses.get_Items().Add((object)(stringBuilder.ToString() + text2));
					num++;
				}
				num4 = num4 + num5 + 29.5;
			}
			while (num4 < num3);
			((ListControl)lstEclipses).set_SelectedIndex(0);
			((Control)lstEclipses).Focus();
		}

		private void ComputeElements(object sender, EventArgs e)
		{
			if (FormLoaded)
			{
				SolarEclipses.SolarEclipseBessellianElements(EclipseJD_TT[((ListControl)lstEclipses).get_SelectedIndex()]);
			}
			((ToolStripItem)multiLocationPredictionsToolStripMenuItem1).set_Enabled(SolarEclipses.UseUTC);
		}

		private void copyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetImage(picEclipse.get_Image());
		}

		private void printPreviewToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SolarEclipses.PrintPreviewWorldGraphic();
		}

		private void printToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SolarEclipses.PrintWorldGraphic();
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings.Default.Save_SolarEclipses = Output.SaveGraphic(picEclipse.get_Image(), SolarEclipses.SolarEclipseLabel + " - World map", Settings.Default.Save_SolarEclipses);
		}

		private void picEclipse_MouseMove(object sender, MouseEventArgs e)
		{
			if (MouseMoveDisabled)
			{
				return;
			}
			Maps.InverseMercatorXY(out Longitude, out Latitude, e.get_X(), e.get_Y(), out MouseValid);
			if (MouseValid)
			{
				MouseX = e.get_X();
				MouseY = e.get_Y();
				if (ShowToolTip)
				{
					GetToolTip = !GetToolTip;
					if (GetToolTip)
					{
						toolTip1.SetToolTip((Control)(object)picEclipse, Longitude.ToString("+0.0  ;-0.0  ") + Latitude.ToString("+0.0;-0.0"));
					}
				}
			}
			else
			{
				toolTip1.Hide((IWin32Window)(object)picEclipse);
			}
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SolarEclipses.CloseAllSolarEclipses();
		}

		private void showCoordinatesInTooltipToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ShowToolTip = !ShowToolTip;
			showCoordinatesInTooltipToolStripMenuItem.set_Checked(ShowToolTip);
		}

		private void colorGraphicToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ToolStripMenuItem obj = bWToolStripMenuItem;
			bool @checked = (Settings.Default.BWFlag = false);
			obj.set_Checked(@checked);
			colorGraphicToolStripMenuItem.set_Checked(!bWToolStripMenuItem.get_Checked());
			if (FormLoaded)
			{
				SolarEclipses.SolarEclipseBessellianElements(EclipseJD_TT[((ListControl)lstEclipses).get_SelectedIndex()]);
			}
		}

		private void bWToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ToolStripMenuItem obj = bWToolStripMenuItem;
			bool @checked = (Settings.Default.BWFlag = true);
			obj.set_Checked(@checked);
			colorGraphicToolStripMenuItem.set_Checked(!bWToolStripMenuItem.get_Checked());
			if (FormLoaded)
			{
				SolarEclipses.SolarEclipseBessellianElements(EclipseJD_TT[((ListControl)lstEclipses).get_SelectedIndex()]);
			}
		}

		private string CollectEvents()
		{
			StringBuilder stringBuilder = new StringBuilder();
			int count = lstEclipses.get_Items().get_Count();
			for (int i = 0; i < count; i++)
			{
				stringBuilder.AppendLine(lstEclipses.get_Items().get_Item(i).ToString());
			}
			stringBuilder.AppendLine("");
			return stringBuilder.ToString();
		}

		private void copyListOfEclipsesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(CollectEvents());
		}

		private void printListOfEclipsesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.PrintText(CollectEvents());
		}

		private void printPreviewListOfEclipsesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.PrintPreviewText(CollectEvents());
		}

		private void saveListOfEclipsesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings.Default.Save_SolarEclipses = Output.SaveAppendPredictionText(CollectEvents(), "Summary of Solar eclipses " + UpDownYear.get_Value() + " to " + (UpDownYear.get_Value() + 10m), Settings.Default.Save_SolarEclipses);
		}

		private void UpDownYear_Enter(object sender, EventArgs e)
		{
		}

		private void DrawDetailedMap(object sender, EventArgs e)
		{
			if (MouseValid)
			{
				Maps.InverseMercatorXY(out var Longitude_deg, out var Latitude_deg, MouseX, MouseY, out var Valid);
				if (Valid)
				{
					SolarEclipses.MidLongitude_deg = Longitude_deg;
					SolarEclipses.MidLatitude_deg = Latitude_deg;
					SolarEclipses.Range_deg = 30.0;
					SolarEclipses.ShowLocalMap(UseCursorPosition: true);
				}
			}
		}

		private void detailedMapToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SolarEclipses.ShowLocalMap(UseCursorPosition: false);
		}

		private void viewInGoogleEartToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SolarEclipses.GoogleEarthCurves(View: true);
		}

		private void cmdViewGoogleEarth_Click(object sender, EventArgs e)
		{
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			if (!Utilities.InternetIsAvailable())
			{
				MessageBox.Show("This functionality requires internet access. You are not connected to the internet", "No internet");
			}
			else
			{
				SolarEclipses.GoogleEarthCurves(View: true);
			}
		}

		private void saveGoogleEarthKmlFileToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SolarEclipses.GoogleEarthCurves(View: false);
		}

		private void multiLocationPredictionsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SolarEclipses.Show_Multilocations();
		}

		private void cmdMultiLocation_Click(object sender, EventArgs e)
		{
			SolarEclipses.Show_Multilocations();
		}

		private void localPredictionsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (!double.TryParse(Settings.Default.Site_Longitude_dd_d, out SolarEclipses.LocalLongitude))
			{
				SolarEclipses.LocalLongitude = 0.0;
			}
			if (!double.TryParse(Settings.Default.Site_Latitude_dd_d, out SolarEclipses.LocalLatitude))
			{
				SolarEclipses.LocalLatitude = 0.0;
			}
			if (!double.TryParse(Settings.Default.Site_Altitude, out SolarEclipses.LocalAltitude))
			{
				SolarEclipses.LocalAltitude = 0.0;
			}
			SolarEclipses.LocalName = Settings.Default.Site_Name;
			SolarEclipses.ShowSingleLocation();
			((Control)SolarEclipses.SingleLocation).set_Text(SolarEclipses.SolarEclipseLabel + " - Local predictions");
		}

		private void pathCoordinatesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SolarEclipses.Show_PathCoordinates();
		}

		private void cmdPath_Click(object sender, EventArgs e)
		{
			SolarEclipses.Show_PathCoordinates();
		}

		private void saveGoogleMapHtmFileToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SolarEclipses.GoogleMapCurves();
		}

		private void savemfiFileForCenterPathToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SolarEclipses.mfi_MapCurves();
		}

		private void cmdGoogleMapSave_Click(object sender, EventArgs e)
		{
			SolarEclipses.GoogleMapCurves();
		}

		private void localPredictionsToolStripMenuItem1_Click(object sender, EventArgs e)
		{
			MouseMoveDisabled = true;
			SolarEclipses.LocalLongitude = Longitude;
			SolarEclipses.LocalLatitude = Latitude;
			SolarEclipses.LocalAltitude = 0.0;
			SolarEclipses.LocalName = "Selected location";
			SolarEclipses.ShowSingleLocation();
			((Control)SolarEclipses.SingleLocation).set_Text(SolarEclipses.SolarEclipseLabel + " - Local predictions");
		}

		private void multiLocationPredictionsToolStripMenuItem1_Click(object sender, EventArgs e)
		{
			SolarEclipses.Show_Multilocations();
		}

		private void pathCoordinatesToolStripMenuItem1_Click(object sender, EventArgs e)
		{
			SolarEclipses.Show_PathCoordinates();
		}

		private void detailedMapToolStripMenuItem1_Click(object sender, EventArgs e)
		{
			DrawDetailedMap(sender, e);
		}

		private void viewInGoogleEarthToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			if (!Utilities.InternetIsAvailable())
			{
				MessageBox.Show("This functionality requires internet access. You are not connected to the internet", "No internet");
			}
			else
			{
				SolarEclipses.GoogleEarthCurves(View: true);
			}
		}

		private void saveGoogleEarthToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SolarEclipses.GoogleEarthCurves(View: false);
		}

		private void kMLFileToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SolarEclipses.GoogleMapCurves();
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Solar Eclipses");
		}

		private void viewBesselianToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SolarEclipses.ListBesselianElements();
		}

		internal void FindPlanetConjunctions(int StartYear, int EndYear)
		{
			if (EndYear <= StartYear)
			{
				return;
			}
			int num = 0;
			int num2 = 0;
			SolarEclipses.ClosePlanetaryConjunctions.lstEclipses.get_Items().Clear();
			SolarEclipses.ClosePlanetaryConjunctions.EclipseJD_TT_Planets.Clear();
			SolarEclipses.ClosePlanetaryConjunctions.pBar.set_Minimum(0);
			SolarEclipses.ClosePlanetaryConjunctions.pBar.set_Maximum(EndYear - StartYear);
			((Control)SolarEclipses.ClosePlanetaryConjunctions.pBar).set_Visible(true);
			CancelPlanetSearch = false;
			for (int i = StartYear; i < EndYear; i += 10)
			{
				SolarEclipses.ClosePlanetaryConjunctions.pBar.set_Value(i - StartYear);
				FindSolarEclipse(i);
				for (int j = 0; j < lstEclipses.get_Items().get_Count(); j++)
				{
					SolarEclipses.SolarEclipseBessellianElements(EclipseJD_TT[j], IncludeMaps: false);
					if (SolarEclipses.IsNonEclipse)
					{
						continue;
					}
					string elongationsWorldMap = SolarEclipses.ElongationsWorldMap;
					if (!(elongationsWorldMap.Contains("§") | elongationsWorldMap.Contains("†") | elongationsWorldMap.Contains("‡")))
					{
						continue;
					}
					StringBuilder stringBuilder = new StringBuilder();
					stringBuilder.Append(SolarEclipses.SolarEclipseDateLabel.Replace("  (TT)", "").TrimEnd(Array.Empty<char>()).PadLeft(13));
					num = elongationsWorldMap.IndexOf("§", num + 1);
					if (num > 0)
					{
						num2 = elongationsWorldMap.LastIndexOf("   ", num);
						stringBuilder.Append(" ☺" + elongationsWorldMap.Substring(num2, num - num2).Substring(2));
					}
					num = -1;
					do
					{
						num = elongationsWorldMap.IndexOf("†", num + 1);
						if (num < 0)
						{
							break;
						}
						num2 = elongationsWorldMap.LastIndexOf("   ", num);
						stringBuilder.Append(" ☼" + elongationsWorldMap.Substring(num2, num - num2).Substring(2));
					}
					while (num > 0);
					num = -1;
					do
					{
						num = elongationsWorldMap.IndexOf("‡", num + 1);
						if (num < 0)
						{
							break;
						}
						num2 = elongationsWorldMap.LastIndexOf("   ", num);
						stringBuilder.Append(elongationsWorldMap.Substring(num2, num - num2));
					}
					while (num > 0);
					SolarEclipses.ClosePlanetaryConjunctions.lstEclipses.get_Items().Add((object)stringBuilder.ToString());
					SolarEclipses.ClosePlanetaryConjunctions.EclipseJD_TT_Planets.Add(EclipseJD_TT[j]);
				}
				Application.DoEvents();
				Utilities.PurgeLunarEphemerisCache();
				if (CancelPlanetSearch)
				{
					break;
				}
			}
			((Control)SolarEclipses.ClosePlanetaryConjunctions.pBar).set_Visible(false);
		}

		private void findClosePlanetaryConjunctionsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SolarEclipses.ShowClosePlanets(LunarEclipse: false);
		}

		private void lunarLimbGenerateSaveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			MouseMoveDisabled = true;
			SolarEclipses.ContactTimes[2] = -10.0;
			SolarEclipses.GenerateLunarProfile(Longitude, Latitude, SaveFile: true);
			MouseMoveDisabled = false;
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
			//IL_017d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0187: Expected O, but got Unknown
			//IL_0188: Unknown result type (might be due to invalid IL or missing references)
			//IL_0192: Expected O, but got Unknown
			//IL_0193: Unknown result type (might be due to invalid IL or missing references)
			//IL_019d: Expected O, but got Unknown
			//IL_019e: Unknown result type (might be due to invalid IL or missing references)
			//IL_01a8: Expected O, but got Unknown
			//IL_01a9: Unknown result type (might be due to invalid IL or missing references)
			//IL_01b3: Expected O, but got Unknown
			//IL_01b4: Unknown result type (might be due to invalid IL or missing references)
			//IL_01be: Expected O, but got Unknown
			//IL_01bf: Unknown result type (might be due to invalid IL or missing references)
			//IL_01c9: Expected O, but got Unknown
			//IL_01ca: Unknown result type (might be due to invalid IL or missing references)
			//IL_01d4: Expected O, but got Unknown
			//IL_01d5: Unknown result type (might be due to invalid IL or missing references)
			//IL_01df: Expected O, but got Unknown
			//IL_01e0: Unknown result type (might be due to invalid IL or missing references)
			//IL_01ea: Expected O, but got Unknown
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
			//IL_19f3: Unknown result type (might be due to invalid IL or missing references)
			//IL_19fd: Expected O, but got Unknown
			//IL_1b22: Unknown result type (might be due to invalid IL or missing references)
			//IL_1b2c: Expected O, but got Unknown
			//IL_1ba5: Unknown result type (might be due to invalid IL or missing references)
			//IL_1baf: Expected O, but got Unknown
			components = new Container();
			menuStrip1 = new MenuStrip();
			withPredictionToolStripMenuItem = new ToolStripMenuItem();
			viewBesselianToolStripMenuItem = new ToolStripMenuItem();
			showCoordinatesInTooltipToolStripMenuItem = new ToolStripMenuItem();
			bWToolStripMenuItem = new ToolStripMenuItem();
			colorGraphicToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator1 = new ToolStripSeparator();
			viewInGoogleEartToolStripMenuItem = new ToolStripMenuItem();
			saveGoogleEarthKmlFileToolStripMenuItem = new ToolStripMenuItem();
			saveGoogleMapHtmFileToolStripMenuItem = new ToolStripMenuItem();
			savemfiFileForCenterPathToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator4 = new ToolStripSeparator();
			copyToolStripMenuItem = new ToolStripMenuItem();
			printPreviewToolStripMenuItem = new ToolStripMenuItem();
			printToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator2 = new ToolStripSeparator();
			copyListOfEclipsesToolStripMenuItem = new ToolStripMenuItem();
			printPreviewListOfEclipsesToolStripMenuItem = new ToolStripMenuItem();
			printListOfEclipsesToolStripMenuItem = new ToolStripMenuItem();
			saveListOfEclipsesToolStripMenuItem = new ToolStripMenuItem();
			multisitePredictionToolStripMenuItem = new ToolStripMenuItem();
			detailedMapToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator3 = new ToolStripSeparator();
			localPredictionsToolStripMenuItem = new ToolStripMenuItem();
			multiLocationPredictionsToolStripMenuItem = new ToolStripMenuItem();
			pathCoordinatesToolStripMenuItem = new ToolStripMenuItem();
			findClosePlanetaryConjunctionsToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			UpDownYear = new NumericUpDown();
			lstEclipses = new ListBox();
			label2 = new Label();
			contextMenuStrip1 = new ContextMenuStrip(components);
			localPredictionsToolStripMenuItem1 = new ToolStripMenuItem();
			multiLocationPredictionsToolStripMenuItem1 = new ToolStripMenuItem();
			pathCoordinatesToolStripMenuItem1 = new ToolStripMenuItem();
			lunarLimbGenerateSaveToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator5 = new ToolStripSeparator();
			detailedMapToolStripMenuItem1 = new ToolStripMenuItem();
			viewInGoogleEarthToolStripMenuItem = new ToolStripMenuItem();
			saveGoogleEarthToolStripMenuItem = new ToolStripMenuItem();
			kMLFileToolStripMenuItem = new ToolStripMenuItem();
			toolTip1 = new ToolTip(components);
			button1 = new Button();
			cmdGoogleMapSave = new Button();
			cmdMultiLocation = new Button();
			cmdPath = new Button();
			cmdViewGoogleEarth = new Button();
			label1 = new Label();
			label3 = new Label();
			label4 = new Label();
			picEclipse = new PictureBox();
			((Control)menuStrip1).SuspendLayout();
			((ISupportInitialize)UpDownYear).BeginInit();
			((Control)contextMenuStrip1).SuspendLayout();
			((ISupportInitialize)picEclipse).BeginInit();
			((Control)this).SuspendLayout();
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[5]
			{
				(ToolStripItem)withPredictionToolStripMenuItem,
				(ToolStripItem)multisitePredictionToolStripMenuItem,
				(ToolStripItem)findClosePlanetaryConjunctionsToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(777, 24));
			((Control)menuStrip1).set_TabIndex(8);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withPredictionToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[19]
			{
				(ToolStripItem)viewBesselianToolStripMenuItem,
				(ToolStripItem)showCoordinatesInTooltipToolStripMenuItem,
				(ToolStripItem)bWToolStripMenuItem,
				(ToolStripItem)colorGraphicToolStripMenuItem,
				(ToolStripItem)toolStripSeparator1,
				(ToolStripItem)viewInGoogleEartToolStripMenuItem,
				(ToolStripItem)saveGoogleEarthKmlFileToolStripMenuItem,
				(ToolStripItem)saveGoogleMapHtmFileToolStripMenuItem,
				(ToolStripItem)savemfiFileForCenterPathToolStripMenuItem,
				(ToolStripItem)toolStripSeparator4,
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)printPreviewToolStripMenuItem,
				(ToolStripItem)printToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem,
				(ToolStripItem)toolStripSeparator2,
				(ToolStripItem)copyListOfEclipsesToolStripMenuItem,
				(ToolStripItem)printPreviewListOfEclipsesToolStripMenuItem,
				(ToolStripItem)printListOfEclipsesToolStripMenuItem,
				(ToolStripItem)saveListOfEclipsesToolStripMenuItem
			});
			((ToolStripItem)withPredictionToolStripMenuItem).set_Name("withPredictionToolStripMenuItem");
			((ToolStripItem)withPredictionToolStripMenuItem).set_Size(new Size(108, 20));
			((ToolStripItem)withPredictionToolStripMenuItem).set_Text("with prediction...");
			((ToolStripItem)viewBesselianToolStripMenuItem).set_Image((Image)Resources.ShowGridlines2HS);
			((ToolStripItem)viewBesselianToolStripMenuItem).set_Name("viewBesselianToolStripMenuItem");
			((ToolStripItem)viewBesselianToolStripMenuItem).set_Size(new Size(302, 22));
			((ToolStripItem)viewBesselianToolStripMenuItem).set_Text("List Besselian elements");
			((ToolStripItem)viewBesselianToolStripMenuItem).add_Click((EventHandler)viewBesselianToolStripMenuItem_Click);
			showCoordinatesInTooltipToolStripMenuItem.set_Checked(true);
			showCoordinatesInTooltipToolStripMenuItem.set_CheckState((CheckState)1);
			((ToolStripItem)showCoordinatesInTooltipToolStripMenuItem).set_Name("showCoordinatesInTooltipToolStripMenuItem");
			((ToolStripItem)showCoordinatesInTooltipToolStripMenuItem).set_Size(new Size(302, 22));
			((ToolStripItem)showCoordinatesInTooltipToolStripMenuItem).set_Text("Show coordinates in tooltip");
			((ToolStripItem)showCoordinatesInTooltipToolStripMenuItem).add_Click((EventHandler)showCoordinatesInTooltipToolStripMenuItem_Click);
			((ToolStripItem)bWToolStripMenuItem).set_Image((Image)Resources.ColorHS);
			((ToolStripItem)bWToolStripMenuItem).set_Name("bWToolStripMenuItem");
			((ToolStripItem)bWToolStripMenuItem).set_Size(new Size(302, 22));
			((ToolStripItem)bWToolStripMenuItem).set_Text("B && W graphic");
			((ToolStripItem)bWToolStripMenuItem).add_Click((EventHandler)bWToolStripMenuItem_Click);
			colorGraphicToolStripMenuItem.set_Checked(true);
			colorGraphicToolStripMenuItem.set_CheckState((CheckState)1);
			((ToolStripItem)colorGraphicToolStripMenuItem).set_Image((Image)Resources.ColorHS);
			((ToolStripItem)colorGraphicToolStripMenuItem).set_Name("colorGraphicToolStripMenuItem");
			((ToolStripItem)colorGraphicToolStripMenuItem).set_Size(new Size(302, 22));
			((ToolStripItem)colorGraphicToolStripMenuItem).set_Text("Color graphic");
			((ToolStripItem)colorGraphicToolStripMenuItem).add_Click((EventHandler)colorGraphicToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator1).set_Name("toolStripSeparator1");
			((ToolStripItem)toolStripSeparator1).set_Size(new Size(299, 6));
			((ToolStripItem)viewInGoogleEartToolStripMenuItem).set_Image((Image)Resources.google_earth);
			((ToolStripItem)viewInGoogleEartToolStripMenuItem).set_Name("viewInGoogleEartToolStripMenuItem");
			viewInGoogleEartToolStripMenuItem.set_ShortcutKeys((Keys)131143);
			((ToolStripItem)viewInGoogleEartToolStripMenuItem).set_Size(new Size(302, 22));
			((ToolStripItem)viewInGoogleEartToolStripMenuItem).set_Text("View in GoogleEarth");
			((ToolStripItem)viewInGoogleEartToolStripMenuItem).add_Click((EventHandler)viewInGoogleEartToolStripMenuItem_Click);
			((ToolStripItem)saveGoogleEarthKmlFileToolStripMenuItem).set_Image((Image)Resources.kml_file);
			((ToolStripItem)saveGoogleEarthKmlFileToolStripMenuItem).set_Name("saveGoogleEarthKmlFileToolStripMenuItem");
			((ToolStripItem)saveGoogleEarthKmlFileToolStripMenuItem).set_Size(new Size(302, 22));
			((ToolStripItem)saveGoogleEarthKmlFileToolStripMenuItem).set_Text("Save eclipse region as GoogleEarth KML file");
			((ToolStripItem)saveGoogleEarthKmlFileToolStripMenuItem).add_Click((EventHandler)saveGoogleEarthKmlFileToolStripMenuItem_Click);
			((ToolStripItem)saveGoogleMapHtmFileToolStripMenuItem).set_Image((Image)Resources.mm_20_red);
			((ToolStripItem)saveGoogleMapHtmFileToolStripMenuItem).set_Name("saveGoogleMapHtmFileToolStripMenuItem");
			((ToolStripItem)saveGoogleMapHtmFileToolStripMenuItem).set_Size(new Size(302, 22));
			((ToolStripItem)saveGoogleMapHtmFileToolStripMenuItem).set_Text("Save eclipse region as GoogleMap HTM file");
			((ToolStripItem)saveGoogleMapHtmFileToolStripMenuItem).add_Click((EventHandler)saveGoogleMapHtmFileToolStripMenuItem_Click);
			((ToolStripItem)savemfiFileForCenterPathToolStripMenuItem).set_Enabled(false);
			((ToolStripItem)savemfiFileForCenterPathToolStripMenuItem).set_Name("savemfiFileForCenterPathToolStripMenuItem");
			((ToolStripItem)savemfiFileForCenterPathToolStripMenuItem).set_Size(new Size(302, 22));
			((ToolStripItem)savemfiFileForCenterPathToolStripMenuItem).set_Text("Save path as .mif file");
			((ToolStripItem)savemfiFileForCenterPathToolStripMenuItem).add_Click((EventHandler)savemfiFileForCenterPathToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator4).set_Name("toolStripSeparator4");
			((ToolStripItem)toolStripSeparator4).set_Size(new Size(299, 6));
			((ToolStripItem)copyToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyToolStripMenuItem).set_Name("copyToolStripMenuItem");
			copyToolStripMenuItem.set_ShortcutKeys((Keys)131139);
			((ToolStripItem)copyToolStripMenuItem).set_Size(new Size(302, 22));
			((ToolStripItem)copyToolStripMenuItem).set_Text("&Copy");
			((ToolStripItem)copyToolStripMenuItem).add_Click((EventHandler)copyToolStripMenuItem_Click);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Image((Image)Resources.PrintPreview);
			((ToolStripItem)printPreviewToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Name("printPreviewToolStripMenuItem");
			((ToolStripItem)printPreviewToolStripMenuItem).set_Size(new Size(302, 22));
			((ToolStripItem)printPreviewToolStripMenuItem).set_Text("Print preview [graphic]");
			((ToolStripItem)printPreviewToolStripMenuItem).add_Click((EventHandler)printPreviewToolStripMenuItem_Click);
			((ToolStripItem)printToolStripMenuItem).set_Image((Image)Resources.Print);
			((ToolStripItem)printToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printToolStripMenuItem).set_Name("printToolStripMenuItem");
			printToolStripMenuItem.set_ShortcutKeys((Keys)131152);
			((ToolStripItem)printToolStripMenuItem).set_Size(new Size(302, 22));
			((ToolStripItem)printToolStripMenuItem).set_Text("&Print [graphic]");
			((ToolStripItem)printToolStripMenuItem).add_Click((EventHandler)printToolStripMenuItem_Click);
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			saveToolStripMenuItem.set_ShortcutKeys((Keys)131155);
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(302, 22));
			((ToolStripItem)saveToolStripMenuItem).set_Text("&Save");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator2).set_Name("toolStripSeparator2");
			((ToolStripItem)toolStripSeparator2).set_Size(new Size(299, 6));
			((ToolStripItem)copyListOfEclipsesToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyListOfEclipsesToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyListOfEclipsesToolStripMenuItem).set_Name("copyListOfEclipsesToolStripMenuItem");
			copyListOfEclipsesToolStripMenuItem.set_ShortcutKeys((Keys)196675);
			((ToolStripItem)copyListOfEclipsesToolStripMenuItem).set_Size(new Size(302, 22));
			((ToolStripItem)copyListOfEclipsesToolStripMenuItem).set_Text("Copy list of eclipses");
			((ToolStripItem)copyListOfEclipsesToolStripMenuItem).add_Click((EventHandler)copyListOfEclipsesToolStripMenuItem_Click);
			((ToolStripItem)printPreviewListOfEclipsesToolStripMenuItem).set_Image((Image)Resources.PrintPreview);
			((ToolStripItem)printPreviewListOfEclipsesToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printPreviewListOfEclipsesToolStripMenuItem).set_Name("printPreviewListOfEclipsesToolStripMenuItem");
			((ToolStripItem)printPreviewListOfEclipsesToolStripMenuItem).set_Size(new Size(302, 22));
			((ToolStripItem)printPreviewListOfEclipsesToolStripMenuItem).set_Text("Print preview list of eclipses");
			((ToolStripItem)printPreviewListOfEclipsesToolStripMenuItem).add_Click((EventHandler)printPreviewListOfEclipsesToolStripMenuItem_Click);
			((ToolStripItem)printListOfEclipsesToolStripMenuItem).set_Image((Image)Resources.Print);
			((ToolStripItem)printListOfEclipsesToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printListOfEclipsesToolStripMenuItem).set_Name("printListOfEclipsesToolStripMenuItem");
			printListOfEclipsesToolStripMenuItem.set_ShortcutKeys((Keys)196688);
			((ToolStripItem)printListOfEclipsesToolStripMenuItem).set_Size(new Size(302, 22));
			((ToolStripItem)printListOfEclipsesToolStripMenuItem).set_Text("Print list of eclipses");
			((ToolStripItem)printListOfEclipsesToolStripMenuItem).add_Click((EventHandler)printListOfEclipsesToolStripMenuItem_Click);
			((ToolStripItem)saveListOfEclipsesToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveListOfEclipsesToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveListOfEclipsesToolStripMenuItem).set_Name("saveListOfEclipsesToolStripMenuItem");
			saveListOfEclipsesToolStripMenuItem.set_ShortcutKeys((Keys)196691);
			((ToolStripItem)saveListOfEclipsesToolStripMenuItem).set_Size(new Size(302, 22));
			((ToolStripItem)saveListOfEclipsesToolStripMenuItem).set_Text("Save list of eclipses");
			((ToolStripItem)saveListOfEclipsesToolStripMenuItem).add_Click((EventHandler)saveListOfEclipsesToolStripMenuItem_Click);
			((ToolStripDropDownItem)multisitePredictionToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[5]
			{
				(ToolStripItem)detailedMapToolStripMenuItem,
				(ToolStripItem)toolStripSeparator3,
				(ToolStripItem)localPredictionsToolStripMenuItem,
				(ToolStripItem)multiLocationPredictionsToolStripMenuItem,
				(ToolStripItem)pathCoordinatesToolStripMenuItem
			});
			((ToolStripItem)multisitePredictionToolStripMenuItem).set_Name("multisitePredictionToolStripMenuItem");
			((ToolStripItem)multisitePredictionToolStripMenuItem).set_Size(new Size(133, 20));
			((ToolStripItem)multisitePredictionToolStripMenuItem).set_Text("Detailed predictions...");
			((ToolStripItem)detailedMapToolStripMenuItem).set_Name("detailedMapToolStripMenuItem");
			((ToolStripItem)detailedMapToolStripMenuItem).set_Size(new Size(210, 22));
			((ToolStripItem)detailedMapToolStripMenuItem).set_Text("Detailed map");
			((ToolStripItem)detailedMapToolStripMenuItem).add_Click((EventHandler)detailedMapToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator3).set_Name("toolStripSeparator3");
			((ToolStripItem)toolStripSeparator3).set_Size(new Size(207, 6));
			((ToolStripItem)localPredictionsToolStripMenuItem).set_Name("localPredictionsToolStripMenuItem");
			((ToolStripItem)localPredictionsToolStripMenuItem).set_Size(new Size(210, 22));
			((ToolStripItem)localPredictionsToolStripMenuItem).set_Text("Local predictions");
			((ToolStripItem)localPredictionsToolStripMenuItem).add_Click((EventHandler)localPredictionsToolStripMenuItem_Click);
			((ToolStripItem)multiLocationPredictionsToolStripMenuItem).set_Name("multiLocationPredictionsToolStripMenuItem");
			((ToolStripItem)multiLocationPredictionsToolStripMenuItem).set_Size(new Size(210, 22));
			((ToolStripItem)multiLocationPredictionsToolStripMenuItem).set_Text("MultiLocation predictions");
			((ToolStripItem)multiLocationPredictionsToolStripMenuItem).add_Click((EventHandler)multiLocationPredictionsToolStripMenuItem_Click);
			((ToolStripItem)pathCoordinatesToolStripMenuItem).set_Name("pathCoordinatesToolStripMenuItem");
			((ToolStripItem)pathCoordinatesToolStripMenuItem).set_Size(new Size(210, 22));
			((ToolStripItem)pathCoordinatesToolStripMenuItem).set_Text("Path coordinates");
			((ToolStripItem)pathCoordinatesToolStripMenuItem).add_Click((EventHandler)pathCoordinatesToolStripMenuItem_Click);
			((ToolStripItem)findClosePlanetaryConjunctionsToolStripMenuItem).set_Image((Image)Resources.Stars3);
			((ToolStripItem)findClosePlanetaryConjunctionsToolStripMenuItem).set_Name("findClosePlanetaryConjunctionsToolStripMenuItem");
			((ToolStripItem)findClosePlanetaryConjunctionsToolStripMenuItem).set_Size(new Size(224, 20));
			((ToolStripItem)findClosePlanetaryConjunctionsToolStripMenuItem).set_Text("Find close planetary conjunctions    ");
			((ToolStripItem)findClosePlanetaryConjunctionsToolStripMenuItem).add_Click((EventHandler)findClosePlanetaryConjunctionsToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(69, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("&Help   ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(53, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("E&xit");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((Control)UpDownYear).set_Anchor((AnchorStyles)1);
			((Control)UpDownYear).set_BackColor(SystemColors.Control);
			((UpDownBase)UpDownYear).set_BorderStyle((BorderStyle)1);
			((Control)UpDownYear).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			UpDownYear.set_Increment(new decimal(new int[4] { 10, 0, 0, 0 }));
			((Control)UpDownYear).set_Location(new Point(200, 78));
			UpDownYear.set_Maximum(new decimal(new int[4] { 16990, 0, 0, 0 }));
			UpDownYear.set_Minimum(new decimal(new int[4] { 13000, 0, 0, -2147483648 }));
			((Control)UpDownYear).set_Name("UpDownYear");
			((Control)UpDownYear).set_Size(new Size(76, 26));
			((Control)UpDownYear).set_TabIndex(1);
			((UpDownBase)UpDownYear).set_TextAlign((HorizontalAlignment)1);
			UpDownYear.add_ValueChanged((EventHandler)UpDownYear_ValueChanged);
			((Control)UpDownYear).add_Enter((EventHandler)UpDownYear_Enter);
			((Control)lstEclipses).set_Anchor((AnchorStyles)1);
			((Control)lstEclipses).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstEclipses).set_FormattingEnabled(true);
			lstEclipses.set_ItemHeight(14);
			((Control)lstEclipses).set_Location(new Point(286, 29));
			((Control)lstEclipses).set_Name("lstEclipses");
			((Control)lstEclipses).set_Size(new Size(387, 102));
			((Control)lstEclipses).set_TabIndex(0);
			lstEclipses.add_SelectedIndexChanged((EventHandler)ComputeElements);
			((Control)lstEclipses).add_DoubleClick((EventHandler)ComputeElements);
			((Control)label2).set_Anchor((AnchorStyles)1);
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label2).set_Location(new Point(8, 40));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(221, 60));
			((Control)label2).set_TabIndex(9);
			((Control)label2).set_Text("Select eclipse from the list\r\nof eclipses over the period\r\nof 10 years, starting at:\r\n");
			((ToolStrip)contextMenuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[9]
			{
				(ToolStripItem)localPredictionsToolStripMenuItem1,
				(ToolStripItem)multiLocationPredictionsToolStripMenuItem1,
				(ToolStripItem)pathCoordinatesToolStripMenuItem1,
				(ToolStripItem)lunarLimbGenerateSaveToolStripMenuItem,
				(ToolStripItem)toolStripSeparator5,
				(ToolStripItem)detailedMapToolStripMenuItem1,
				(ToolStripItem)viewInGoogleEarthToolStripMenuItem,
				(ToolStripItem)saveGoogleEarthToolStripMenuItem,
				(ToolStripItem)kMLFileToolStripMenuItem
			});
			((Control)contextMenuStrip1).set_Name("contextMenuStrip1");
			((Control)contextMenuStrip1).set_Size(new Size(228, 186));
			((ToolStripItem)localPredictionsToolStripMenuItem1).set_Name("localPredictionsToolStripMenuItem1");
			((ToolStripItem)localPredictionsToolStripMenuItem1).set_Size(new Size(227, 22));
			((ToolStripItem)localPredictionsToolStripMenuItem1).set_Text("Local predictions");
			((ToolStripItem)localPredictionsToolStripMenuItem1).add_Click((EventHandler)localPredictionsToolStripMenuItem1_Click);
			((ToolStripItem)multiLocationPredictionsToolStripMenuItem1).set_Image((Image)Resources.MultiLocation);
			((ToolStripItem)multiLocationPredictionsToolStripMenuItem1).set_Name("multiLocationPredictionsToolStripMenuItem1");
			((ToolStripItem)multiLocationPredictionsToolStripMenuItem1).set_Size(new Size(227, 22));
			((ToolStripItem)multiLocationPredictionsToolStripMenuItem1).set_Text("MultiLocation predictions");
			((ToolStripItem)multiLocationPredictionsToolStripMenuItem1).add_Click((EventHandler)multiLocationPredictionsToolStripMenuItem1_Click);
			((ToolStripItem)pathCoordinatesToolStripMenuItem1).set_Image((Image)Resources.Paths);
			((ToolStripItem)pathCoordinatesToolStripMenuItem1).set_Name("pathCoordinatesToolStripMenuItem1");
			((ToolStripItem)pathCoordinatesToolStripMenuItem1).set_Size(new Size(227, 22));
			((ToolStripItem)pathCoordinatesToolStripMenuItem1).set_Text("Path coordinates");
			((ToolStripItem)pathCoordinatesToolStripMenuItem1).add_Click((EventHandler)pathCoordinatesToolStripMenuItem1_Click);
			((ToolStripItem)lunarLimbGenerateSaveToolStripMenuItem).set_Image((Image)Resources.Fresnel);
			((ToolStripItem)lunarLimbGenerateSaveToolStripMenuItem).set_Name("lunarLimbGenerateSaveToolStripMenuItem");
			((ToolStripItem)lunarLimbGenerateSaveToolStripMenuItem).set_Size(new Size(227, 22));
			((ToolStripItem)lunarLimbGenerateSaveToolStripMenuItem).set_Text("Lunar limb - generate && save");
			((ToolStripItem)lunarLimbGenerateSaveToolStripMenuItem).add_Click((EventHandler)lunarLimbGenerateSaveToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator5).set_Name("toolStripSeparator5");
			((ToolStripItem)toolStripSeparator5).set_Size(new Size(224, 6));
			((ToolStripItem)detailedMapToolStripMenuItem1).set_Name("detailedMapToolStripMenuItem1");
			((ToolStripItem)detailedMapToolStripMenuItem1).set_Size(new Size(227, 22));
			((ToolStripItem)detailedMapToolStripMenuItem1).set_Text("Detailed map");
			((ToolStripItem)detailedMapToolStripMenuItem1).add_Click((EventHandler)detailedMapToolStripMenuItem1_Click);
			((ToolStripItem)viewInGoogleEarthToolStripMenuItem).set_Image((Image)Resources.google_earth);
			((ToolStripItem)viewInGoogleEarthToolStripMenuItem).set_Name("viewInGoogleEarthToolStripMenuItem");
			((ToolStripItem)viewInGoogleEarthToolStripMenuItem).set_Size(new Size(227, 22));
			((ToolStripItem)viewInGoogleEarthToolStripMenuItem).set_Text("View in GoogleEarth");
			((ToolStripItem)viewInGoogleEarthToolStripMenuItem).add_Click((EventHandler)viewInGoogleEarthToolStripMenuItem_Click);
			((ToolStripItem)saveGoogleEarthToolStripMenuItem).set_Image((Image)Resources.kml_file);
			((ToolStripItem)saveGoogleEarthToolStripMenuItem).set_Name("saveGoogleEarthToolStripMenuItem");
			((ToolStripItem)saveGoogleEarthToolStripMenuItem).set_Size(new Size(227, 22));
			((ToolStripItem)saveGoogleEarthToolStripMenuItem).set_Text("Save GoogleEarth KML file");
			((ToolStripItem)saveGoogleEarthToolStripMenuItem).add_Click((EventHandler)saveGoogleEarthToolStripMenuItem_Click);
			((ToolStripItem)kMLFileToolStripMenuItem).set_Image((Image)Resources.mm_20_red);
			((ToolStripItem)kMLFileToolStripMenuItem).set_Name("kMLFileToolStripMenuItem");
			((ToolStripItem)kMLFileToolStripMenuItem).set_Size(new Size(227, 22));
			((ToolStripItem)kMLFileToolStripMenuItem).set_Text("Save GoogleMaps HTM file");
			((ToolStripItem)kMLFileToolStripMenuItem).add_Click((EventHandler)kMLFileToolStripMenuItem_Click);
			toolTip1.set_AutoPopDelay(2000);
			toolTip1.set_InitialDelay(100);
			toolTip1.set_ReshowDelay(100);
			((Control)button1).set_Anchor((AnchorStyles)1);
			((Control)button1).set_BackgroundImage((Image)Resources.kml_file);
			((Control)button1).set_BackgroundImageLayout((ImageLayout)4);
			((Control)button1).set_Location(new Point(697, 100));
			((Control)button1).set_Name("button1");
			((Control)button1).set_Size(new Size(26, 23));
			((Control)button1).set_TabIndex(6);
			toolTip1.SetToolTip((Control)(object)button1, "Save eclipse as GoogleEarth KML file");
			((ButtonBase)button1).set_UseVisualStyleBackColor(true);
			((Control)cmdGoogleMapSave).set_Anchor((AnchorStyles)1);
			((Control)cmdGoogleMapSave).set_BackgroundImage((Image)Resources.mm_20_red);
			((Control)cmdGoogleMapSave).set_BackgroundImageLayout((ImageLayout)4);
			((Control)cmdGoogleMapSave).set_Location(new Point(729, 100));
			((Control)cmdGoogleMapSave).set_Name("cmdGoogleMapSave");
			((Control)cmdGoogleMapSave).set_Size(new Size(26, 23));
			((Control)cmdGoogleMapSave).set_TabIndex(7);
			toolTip1.SetToolTip((Control)(object)cmdGoogleMapSave, "Save eclipse as GoogleMaps HTM");
			((ButtonBase)cmdGoogleMapSave).set_UseVisualStyleBackColor(true);
			((Control)cmdGoogleMapSave).add_Click((EventHandler)cmdGoogleMapSave_Click);
			((Control)cmdMultiLocation).set_Anchor((AnchorStyles)1);
			((Control)cmdMultiLocation).set_BackgroundImage((Image)Resources.MultiLocation);
			((Control)cmdMultiLocation).set_BackgroundImageLayout((ImageLayout)3);
			((Control)cmdMultiLocation).set_Location(new Point(729, 40));
			((Control)cmdMultiLocation).set_Name("cmdMultiLocation");
			((Control)cmdMultiLocation).set_Size(new Size(26, 23));
			((Control)cmdMultiLocation).set_TabIndex(4);
			toolTip1.SetToolTip((Control)(object)cmdMultiLocation, "MultiLocation prediction");
			((ButtonBase)cmdMultiLocation).set_UseVisualStyleBackColor(true);
			((Control)cmdMultiLocation).add_Click((EventHandler)cmdMultiLocation_Click);
			((Control)cmdPath).set_Anchor((AnchorStyles)1);
			((Control)cmdPath).set_BackgroundImage((Image)Resources.Paths);
			((Control)cmdPath).set_BackgroundImageLayout((ImageLayout)4);
			((Control)cmdPath).set_Location(new Point(697, 40));
			((Control)cmdPath).set_Name("cmdPath");
			((Control)cmdPath).set_Size(new Size(26, 23));
			((Control)cmdPath).set_TabIndex(3);
			toolTip1.SetToolTip((Control)(object)cmdPath, "Path coordinates");
			((ButtonBase)cmdPath).set_UseVisualStyleBackColor(true);
			((Control)cmdPath).add_Click((EventHandler)cmdPath_Click);
			((Control)cmdViewGoogleEarth).set_Anchor((AnchorStyles)1);
			((Control)cmdViewGoogleEarth).set_BackgroundImage((Image)Resources.google_earth);
			((Control)cmdViewGoogleEarth).set_BackgroundImageLayout((ImageLayout)4);
			((Control)cmdViewGoogleEarth).set_Location(new Point(697, 70));
			((Control)cmdViewGoogleEarth).set_Name("cmdViewGoogleEarth");
			((Control)cmdViewGoogleEarth).set_Size(new Size(26, 23));
			((Control)cmdViewGoogleEarth).set_TabIndex(5);
			toolTip1.SetToolTip((Control)(object)cmdViewGoogleEarth, "Display eclipse in GoogleEarth");
			((ButtonBase)cmdViewGoogleEarth).set_UseVisualStyleBackColor(true);
			((Control)cmdViewGoogleEarth).add_Click((EventHandler)cmdViewGoogleEarth_Click);
			((Control)label1).set_Anchor((AnchorStyles)1);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(305, 132));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(167, 13));
			((Control)label1).set_TabIndex(9);
			((Control)label1).set_Text("Double-click for more detailed map");
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label3).set_Location(new Point(4, 132));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(162, 13));
			((Control)label3).set_TabIndex(2);
			((Control)label3).set_Text("Right click for menu options");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label4).set_Location(new Point(192, 107));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(91, 13));
			((Control)label4).set_TabIndex(10);
			((Control)label4).set_Text("(-13000 to 16990)");
			((Control)picEclipse).set_BackColor(Color.Black);
			((Control)picEclipse).set_ContextMenuStrip(contextMenuStrip1);
			((Control)picEclipse).set_Location(new Point(7, 145));
			((Control)picEclipse).set_Name("picEclipse");
			((Control)picEclipse).set_Size(new Size(762, 467));
			picEclipse.set_TabIndex(8);
			picEclipse.set_TabStop(false);
			((Control)picEclipse).add_DoubleClick((EventHandler)DrawDetailedMap);
			((Control)picEclipse).add_MouseMove(new MouseEventHandler(picEclipse_MouseMove));
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(777, 619));
			((Control)this).get_Controls().Add((Control)(object)label4);
			((Control)this).get_Controls().Add((Control)(object)label3);
			((Control)this).get_Controls().Add((Control)(object)button1);
			((Control)this).get_Controls().Add((Control)(object)cmdGoogleMapSave);
			((Control)this).get_Controls().Add((Control)(object)cmdMultiLocation);
			((Control)this).get_Controls().Add((Control)(object)cmdPath);
			((Control)this).get_Controls().Add((Control)(object)cmdViewGoogleEarth);
			((Control)this).get_Controls().Add((Control)(object)picEclipse);
			((Control)this).get_Controls().Add((Control)(object)UpDownYear);
			((Control)this).get_Controls().Add((Control)(object)lstEclipses);
			((Control)this).get_Controls().Add((Control)(object)label2);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationEclipseSunWorld", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_Location(Settings.Default.LocationEclipseSunWorld);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_MinimumSize(new Size(770, 650));
			((Control)this).set_Name("SolarEclipseWorldView");
			((Control)this).set_Text("Solar Eclipse - world view");
			((Form)this).add_Load((EventHandler)SolarEclipseWorldView_Load);
			((Form)this).add_FormClosing(new FormClosingEventHandler(SolarEclipseWorldView_FormClosing));
			((Control)this).add_Resize((EventHandler)SolarEclipseWorldView_Resize);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((ISupportInitialize)UpDownYear).EndInit();
			((Control)contextMenuStrip1).ResumeLayout(false);
			((ISupportInitialize)picEclipse).EndInit();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
