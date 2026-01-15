using System;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.IO.Compression;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using Occult.Asteroid_Observations;
using Occult.Mapping;
using Occult.Properties;

namespace Occult
{
	public class AsteroidPath : Form
	{
		private readonly string AppPath;

		private const double Radian = 180.0 / Math.PI;

		private int FileCount;

		private bool ListingIsTopo;

		private bool ShowTopoWarning = true;

		private IContainer components;

		private MenuStrip menuStrip1;

		public ListBox lstPathCoords;

		private CheckBox chkSunUp;

		private NumericUpDown updnStep;

		private ToolStripMenuItem exitToolStripMenuItem;

		private ToolStripMenuItem withPredictionToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem printToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private Label label2;

		private ToolStripMenuItem saveOtherMapFormatsToolStripMenuItem;

		private ToolStripMenuItem cmxPrecisionMappingStreetsToolStripMenuItem;

		private ToolStripMenuItem gpxEasyGPSToolStripMenuItem;

		private ToolStripMenuItem htmforGoogleMapsToolStripMenuItem;

		private ToolStripMenuItem mifMSMapPointToolStripMenuItem;

		private ToolStripMenuItem pltOziExplorerToolStripMenuItem;

		private ToolStripMenuItem genforMakingESRIShapefilesToolStripMenuItem;

		private ContextMenuStrip contextMenuStrip1;

		private ToolStripMenuItem cmxPrecisionMappingStreetsToolStripMenuItem1;

		private ToolStripMenuItem genToolStripMenuItem;

		private ToolStripMenuItem gpxEasyGPSToolStripMenuItem1;

		private ToolStripMenuItem htmGoogleMapsHtmlFileToolStripMenuItem;

		private ToolStripMenuItem kmlGoogleEarthKmlFileToolStripMenuItem;

		private ToolStripMenuItem mifMSMapPointToolStripMenuItem1;

		private ToolStripMenuItem pltOziExplorerToolStripMenuItem1;

		private ToolStripMenuItem kmlGoogleEarthKmlToolStripMenuItem;

		private ToolStripMenuItem txtforToolStripMenuItem;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripMenuItem createPredictedObservationReportToolStripMenuItem;

		private ToolStripMenuItem eurasterToolStripMenuItem;

		private ToolStripMenuItem japanToolStripMenuItem;

		private ToolStripMenuItem occultToolStripMenuItem;

		private ToolStripMenuItem copyPredictionLineOccultFormatToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator1;

		private ToolStripMenuItem copyPredictionLinePasteIntoReportToolStripMenuItem;

		private ToolStripMenuItem copyPredictionLinePasteIntoReportToolStripMenuItem1;

		private ToolStripSeparator toolStripSeparator2;

		private CheckBox chktimePrecision;

		private Label label9;

		private Label label8;

		private NumericUpDown updnFenceNumber_Left;

		private CheckBox chkIncludeFences;

		private Panel panel4;

		private Label label20;

		private Label label19;

		private Label label5;

		private Label label10;

		private Label label11;

		private Label label12;

		private Panel panel5;

		private NumericUpDown updnFenceNumberRight;

		private Label label13;

		internal NumericUpDown updnLatN;

		internal NumericUpDown updnLongE;

		internal NumericUpDown updnLatS;

		internal NumericUpDown updnLongW;

		internal CheckBox chkLimit;

		private NumericUpDown updnSpacing;

		internal Panel pnlTopography;

		internal Label lblWarning;

		internal Panel panelWarn;

		internal Button cmdHelp;

		private Panel panel2;

		internal Button cmdPlotInGE;

		internal Button cmdCompute;

		private Label label1;

		private ToolTip toolTip1;

		internal CheckBox chkHeights;

		internal Button cmdComputeTopo;

		private Label lblGEwarning;

		private CheckBox chkTime;

		public AsteroidPath()
		{
			InitializeComponent();
			AppPath = Utilities.AppPath;
		}

		private void AsteroidPathCoordinates_Load(object sender, EventArgs e)
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
			((Control)panelWarn).set_Top(((Control)pnlTopography).get_Top() + 2);
			((Control)panelWarn).set_Left(((Control)pnlTopography).get_Left() + 140);
			PathWithin10DegNS();
			SetWarning();
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
		}

		private void cmdCompute_Click(object sender, EventArgs e)
		{
			lstPathCoords.get_Items().Clear();
			PathWithin10DegNS();
			ComputePath(TopoPlot: false, 0, 0, 0, chkTime.get_Checked());
		}

		private void cmdComputeTopo_Click(object sender, EventArgs e)
		{
			//IL_0049: Unknown result type (might be due to invalid IL or missing references)
			//IL_004f: Invalid comparison between Unknown and I4
			try
			{
				int.TryParse(DisplayMPOccultations.PredictionDate.Substring(0, 4), out var result);
				if (result > 0 && result < 2023 && DisplayMPOccultations.AsteroidDiameter < 15.0)
				{
					((Control)this).set_Text("Generating topographic coordinates is only reliable\r\nif the most accurate prediction has been generated.\r\n\r\nPast events that have been computed and saved using\r\nOccult do not include a flag to indicate their level\r\nof reliabilitythat have been Only use if the event has been downloaded from\r\nOccultWatcher or OWC, or has been specially generated\r\nin Occult, with the List & Display list of events\r\nbeing on a BLUE BACKGROUND.");
					if ((int)MessageBox.Show(((Control)this).get_Text(), "Warning", (MessageBoxButtons)1, (MessageBoxIcon)48) == 2)
					{
						return;
					}
				}
			}
			catch
			{
			}
			int numberOfFences_Left = 0;
			int numberOfFences_Right = 0;
			int fenceSpacing_meters = 50;
			if (chkIncludeFences.get_Checked())
			{
				numberOfFences_Left = (int)updnFenceNumber_Left.get_Value();
				numberOfFences_Right = (int)updnFenceNumberRight.get_Value();
				fenceSpacing_meters = (int)updnSpacing.get_Value();
			}
			PathWithin10DegNS();
			ComputePath(TopoPlot: true, numberOfFences_Left, numberOfFences_Right, fenceSpacing_meters, chkTime.get_Checked());
		}

		private void ComputePath(bool TopoPlot, int NumberOfFences_Left, int NumberOfFences_Right, int FenceSpacing_meters, bool IterateByTimeOnly)
		{
			//IL_0026: Unknown result type (might be due to invalid IL or missing references)
			//IL_002c: Invalid comparison between Unknown and I4
			//IL_008f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0095: Invalid comparison between Unknown and I4
			ListingIsTopo = TopoPlot;
			if (TopoPlot)
			{
				if (!File.Exists(EarthTopography.FileName))
				{
					if ((int)MessageBox.Show("To generate topographic coordinates you need to\r\nhave a file containing the required elevation data.\r\nIt is a large file - 0.45GB\r\n\r\nWould you like to download the file now?", "Download Earth2014", (MessageBoxButtons)1, (MessageBoxIcon)32) == 2)
					{
						return;
					}
					((Control)this).set_Cursor(Cursors.get_WaitCursor());
					http.GetEarth2014();
					((Control)this).set_Cursor(Cursors.get_Default());
				}
				if (DisplayMPOccultations.PredictionMJD < 59945.0 && DisplayMPOccultations.PredictionMJD > 50000.0 && ShowTopoWarning)
				{
					if (DisplayMPOccultations.AsteroidDiameter < 15.0 && (int)MessageBox.Show("Generating topographic coordinates is only reliable\r\nif the most accurate prediction has been generated.\r\n\r\nPast events that have been computed and saved using\r\nOccult do not include a flag to indicate whether\r\nthey have the required level of accuracy.\r\n\r\nOnly procede with generating topographic coordinates if\r\nthe event has come from OccultWatcher or OWC, or\r\nhas been generated by Occult version 4.2022.12.12\r\nor later.", "Warning", (MessageBoxButtons)1, (MessageBoxIcon)48) == 2)
					{
						return;
					}
					ShowTopoWarning = false;
				}
				((Control)cmdPlotInGE).set_Text("Draw listed TOPO path in\r\nGoogle Earth");
				((Control)cmdPlotInGE).set_BackColor(((Control)cmdComputeTopo).get_BackColor());
			}
			else
			{
				((Control)cmdPlotInGE).set_Text("Draw listed MSL path in\r\nGoogle Earth");
				((Control)cmdPlotInGE).set_BackColor(((Control)cmdCompute).get_BackColor());
			}
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			lstPathCoords.get_Items().Clear();
			lstPathCoords.get_Items().Add((object)"Please wait while path is being generated. It may take several seconds to display the path after this message disappears");
			Application.DoEvents();
			DisplayMPOccultations.AsteroidPath((double)updnStep.get_Value(), TopoPlot, NumberOfFences_Left, NumberOfFences_Right, FenceSpacing_meters, chkTime.get_Checked());
			DisplayPathCoords();
			((Control)this).set_Cursor(Cursors.get_Default());
		}

		private string CollectEvents()
		{
			if (lstPathCoords.get_Items().get_Count() < 0)
			{
				return "";
			}
			StringBuilder stringBuilder = new StringBuilder();
			int count = lstPathCoords.get_Items().get_Count();
			for (int i = 0; i < count; i++)
			{
				stringBuilder.AppendLine(lstPathCoords.get_Items().get_Item(i).ToString());
			}
			stringBuilder.AppendLine("");
			return stringBuilder.ToString();
		}

		private void copyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (lstPathCoords.get_Items().get_Count() >= 0)
			{
				Clipboard.SetText(CollectEvents());
			}
		}

		private void printToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (lstPathCoords.get_Items().get_Count() >= 0)
			{
				Output.PrintText(CollectEvents());
			}
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (lstPathCoords.get_Items().get_Count() >= 0)
			{
				Settings.Default.Save_AsteroidPredictions = Output.SaveAppendPredictionText(CollectEvents(), lstPathCoords.get_Items().get_Item(0).ToString(), Settings.Default.Save_AsteroidPredictions);
			}
		}

		public void DisplayPathCoords()
		{
			int num = 0;
			bool flag = false;
			bool @checked = chkSunUp.get_Checked();
			AsteroidPathCoords.ExtraPrecisionOnTime = chktimePrecision.get_Checked();
			if (DisplayMPOccultations.PathCoordinates.Count < 1)
			{
				return;
			}
			lstPathCoords.get_Items().Clear();
			if (ListingIsTopo)
			{
				lstPathCoords.get_Items().Add((object)(DisplayMPOccultations.EventHeader + ", at topographic elevations"));
			}
			else
			{
				lstPathCoords.get_Items().Add((object)(DisplayMPOccultations.EventHeader + ", at MSL"));
			}
			lstPathCoords.get_Items().Add((object)"");
			string[] array = new string[4]
			{
				"                   Centre                Star  Star  Sun",
				"  E. Longitude    Latitude      U.T.      Alt   Az   Alt",
				"      o  '  \"     o  '  \"    h  m  s       o     o     o",
				"".PadRight(57)
			};
			string[] array2 = new string[4] { "|      Path Limit 1   |      Path Limit 2   |      Error Limit 1  |      Error Limit 2  ", "| Longitude   Latitude| Longitude   Latitude| Longitude   Latitude| Longitude   Latitude", "|   o  '  \"    o  '  \"|   o  '  \"    o  '  \"|   o  '  \"    o  '  \"|   o  '  \"    o  '  \"", "" };
			string[] array3 = new string[4] { "|      Path Limits    |     Error Limits    ", "|  Latitude   Latitude|  Latitude   Latitude", "|   o  '  \"    o  '  \"|   o  '  \"    o  '  \"", "" };
			string[] array4 = new string[4] { "|      Path Limits    |     Error Limits    ", "| Longitude  Longitude| Longitude  Longitude", "|   o  '  \"    o  '  \"|   o  '  \"    o  '  \"", "" };
			string[] array5 = new string[4] { "|     Umbra Limit 1  |      Umbra Limit 2  ", "| Longitude  Latitude|  Longitude  Latitude", "|   o  '  \"    o  '  \"    o  '  \"    o  '  \"", "" };
			string[] array6 = new string[4] { "|     Umbra Limits    ", "|  Latitude   Latitude", "|   o  '  \"    o  '  \"", "" };
			string[] array7 = new string[4]
			{
				array6[0],
				"| Longitude  Longitude",
				"|   o  '  \"    o  '  \"",
				""
			};
			string[] array8 = new string[4] { "|       Ring Limit 1  |       Ring Limit 2  ", "|  Longitude  Latitude|  Longitude  Latitude", "|   o  '  \"    o  '  \"    o  '  \"    o  '  \"", "" };
			string[] array9 = new string[4] { "|      Ring Limits    ", "|  Latitude   Latitude", "|   o  '  \"    o  '  \"", "" };
			string[] array10 = new string[4] { "|      Ring Limits    ", "| Longitude  Longitude", "|   o  '  \"    o  '  \"", "" };
			string[] array11 = new string[4] { "|   Alt", "|   Crn", "|", "" };
			string[] array12 = new string[4];
			if (DisplayMPOccultations.PathCoordinates[0].IterateOnT)
			{
				for (int i = 0; i < 3; i++)
				{
					array12[i] = array[i] + array2[i];
				}
				if (DisplayMPOccultations.PathCoordinates[0].ShowUmbralPath)
				{
					for (int j = 0; j < 3; j++)
					{
						array12[j] += array5[j];
					}
				}
				if (DisplayMPOccultations.PathCoordinates[0].PathHasRings)
				{
					for (int k = 0; k < 3; k++)
					{
						array12[k] += array8[k];
					}
				}
			}
			else if (DisplayMPOccultations.PathCoordinates[0].IterateOnLatitude)
			{
				for (int l = 0; l < 3; l++)
				{
					array12[l] = array[l] + array4[l];
				}
				if (DisplayMPOccultations.PathCoordinates[0].ShowUmbralPath)
				{
					for (int m = 0; m < 3; m++)
					{
						array12[m] += array7[m];
					}
				}
				if (DisplayMPOccultations.PathCoordinates[0].PathHasRings)
				{
					for (int n = 0; n < 3; n++)
					{
						array12[n] += array10[n];
					}
				}
			}
			else
			{
				for (int num2 = 0; num2 < 3; num2++)
				{
					array12[num2] = array[num2] + array3[num2];
				}
				if (DisplayMPOccultations.PathCoordinates[0].ShowUmbralPath)
				{
					for (int num3 = 0; num3 < 3; num3++)
					{
						array12[num3] += array6[num3];
					}
				}
				if (DisplayMPOccultations.PathCoordinates[0].PathHasRings)
				{
					for (int num4 = 0; num4 < 3; num4++)
					{
						array12[num4] += array9[num4];
					}
				}
			}
			for (int num5 = 0; num5 < 3; num5++)
			{
				array12[num5] += array11[num5];
			}
			for (int num6 = 0; num6 < 3; num6++)
			{
				lstPathCoords.get_Items().Add((object)array12[num6]);
			}
			lstPathCoords.get_Items().Add((object)"");
			flag = DisplayMPOccultations.PathCoordinates[0].IterateOnLatitude;
			for (int num7 = 0; num7 < DisplayMPOccultations.PathCoordinates.Count; num7++)
			{
				if (!DisplayMPOccultations.PathCoordinates[num7].IterateOnLatitude && flag && (DisplayMPOccultations.PathCoordinates[num7].SunAlt < 0.0 || @checked))
				{
					lstPathCoords.get_Items().Add((object)"");
					num = 0;
					if (!DisplayMPOccultations.AllowAsteroidRings)
					{
						lstPathCoords.get_Items().Add((object)("".PadRight(59) + "Latitude   Latitude   Latitude   Latitude"));
					}
					else
					{
						lstPathCoords.get_Items().Add((object)("".PadRight(57) + "Latitude   Latitude   Latitude   Latitude   Latitude   Latitude"));
					}
					flag = false;
				}
				if (DisplayMPOccultations.PathCoordinates[num7].SunAlt < 0.0 || @checked)
				{
					lstPathCoords.get_Items().Add((object)DisplayMPOccultations.PathCoordinates[num7].ToString());
					num++;
					if ((num % 5 == 0) & Settings.Default.Skip5thLine)
					{
						lstPathCoords.get_Items().Add((object)"");
					}
				}
			}
			lstPathCoords.get_Items().Add((object)"");
			lstPathCoords.get_Items().Add((object)("Uncertainty in time = +/- " + string.Format("{0,1:F0} secs", DisplayMPOccultations.ErrorInTime)));
			lstPathCoords.get_Items().Add((object)"");
			lstPathCoords.get_Items().Add((object)("[which results in a separate uncertainty of +/- " + string.Format("{0,1:F2} degrees in the path longitudes]", DisplayMPOccultations.ErrorInTime / 240.0)));
			lstPathCoords.get_Items().Add((object)"");
			if (DisplayMPOccultations.StellarDiameter_mas > 0.0)
			{
				lstPathCoords.get_Items().Add((object)"The width of occultation path includes the penumbral region resulting from the star");
				lstPathCoords.get_Items().Add((object)("  having a diameter of " + string.Format("{0,1:f3}", DisplayMPOccultations.StellarDiameter_mas / 1000.0) + "\""));
				lstPathCoords.get_Items().Add((object)"");
			}
			lstPathCoords.get_Items().Add((object)DisplayMPOccultations.PredictionDate);
			((Control)this).Focus();
		}

		private void cmxPrecisionMappingStreetsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SaveCMXPath();
		}

		private void genforMakingESRIShapefilesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SaveGENPath();
		}

		private void gpxEasyGPSToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SaveGPXPath();
		}

		private void htmforGoogleMapsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SaveHTMPath();
		}

		private void kmlGoogleEarthKmlToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SaveKMLPath();
		}

		private void mifMSMapPointToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SaveMIFPath();
		}

		private void pltOziExplorerToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SavePLTPath();
		}

		private void SaveCMXPath()
		{
			double num = 0.0;
			double num2 = 0.0;
			bool flag = false;
			bool @checked = chkSunUp.get_Checked();
			int num3 = 1;
			if (lstPathCoords.get_Items().get_Count() < 4 || !GoogleEarth.Create_New_CMX_File(lstPathCoords.get_Items().get_Item(0).ToString()!.Substring(15).Trim().Replace(" ", "_"), out var SavedFileName))
			{
				return;
			}
			for (num3 = -1; num3 <= 3; num3++)
			{
				for (int i = 0; i < DisplayMPOccultations.PathCoordinates.Count; i++)
				{
					switch (num3)
					{
					case -1:
						num = DisplayMPOccultations.PathCoordinates[i].LongitudeCentre;
						num2 = DisplayMPOccultations.PathCoordinates[i].LatitudeCentre;
						flag = DisplayMPOccultations.PathCoordinates[i].CentreLineValid;
						break;
					case 0:
						num = DisplayMPOccultations.PathCoordinates[i].LongitudeLeft;
						num2 = DisplayMPOccultations.PathCoordinates[i].LatitudeLeft;
						flag = DisplayMPOccultations.PathCoordinates[i].LatitudeLeft < 100.0;
						break;
					case 1:
						num = DisplayMPOccultations.PathCoordinates[i].LongitudeRight;
						num2 = DisplayMPOccultations.PathCoordinates[i].LatitudeRight;
						flag = DisplayMPOccultations.PathCoordinates[i].LatitudeRight < 100.0;
						break;
					case 2:
						num = DisplayMPOccultations.PathCoordinates[i].LongitudeLeftSigma;
						num2 = DisplayMPOccultations.PathCoordinates[i].LatitudeLeftSigma;
						flag = DisplayMPOccultations.PathCoordinates[i].LatitudeLeftSigma < 100.0;
						break;
					case 3:
						num = DisplayMPOccultations.PathCoordinates[i].LongitudeRightSigma;
						num2 = DisplayMPOccultations.PathCoordinates[i].LatitudeRightSigma;
						flag = DisplayMPOccultations.PathCoordinates[i].LatitudeRightSigma < 100.0;
						break;
					}
					flag &= DisplayMPOccultations.PathCoordinates[i].SunAlt < 0.0 || @checked;
					flag = flag && num > -165.0 && num < -65.0 && num2 > 15.0;
					if (flag)
					{
						GoogleEarth.Add_point_to_CMX_Line(num, num2);
					}
				}
				GoogleEarth.Write_CMX_Line_Block(num3, num3 == 3);
			}
			GoogleEarth.Close_CMX_File();
			string text = Path.GetDirectoryName(SavedFileName) + "\\" + Path.GetFileNameWithoutExtension(SavedFileName) + "_cmx.zip";
			if (File.Exists(text))
			{
				File.Delete(text);
			}
			using ZipArchive destination = ZipFile.Open(text, ZipArchiveMode.Create);
			destination.CreateEntryFromFile(SavedFileName, new FileInfo(SavedFileName).Name);
		}

		private void SaveGENPath()
		{
			double longitude = 0.0;
			double latitude = 0.0;
			bool flag = false;
			bool @checked = chkSunUp.get_Checked();
			int num = 1;
			int num2 = 1;
			if (lstPathCoords.get_Items().get_Count() < 4 || !GoogleEarth.Create_New_GEN_File(lstPathCoords.get_Items().get_Item(0).ToString()!.Substring(15).Trim().Replace(" ", "_")))
			{
				return;
			}
			num2 = 1;
			for (num = -1; num <= 3; num++)
			{
				for (int i = 0; i < DisplayMPOccultations.PathCoordinates.Count; i++)
				{
					switch (num)
					{
					case -1:
						longitude = DisplayMPOccultations.PathCoordinates[i].LongitudeCentre;
						latitude = DisplayMPOccultations.PathCoordinates[i].LatitudeCentre;
						flag = DisplayMPOccultations.PathCoordinates[i].CentreLineValid;
						break;
					case 0:
						longitude = DisplayMPOccultations.PathCoordinates[i].LongitudeLeft;
						latitude = DisplayMPOccultations.PathCoordinates[i].LatitudeLeft;
						flag = DisplayMPOccultations.PathCoordinates[i].LatitudeLeft < 100.0;
						break;
					case 1:
						longitude = DisplayMPOccultations.PathCoordinates[i].LongitudeRight;
						latitude = DisplayMPOccultations.PathCoordinates[i].LatitudeRight;
						flag = DisplayMPOccultations.PathCoordinates[i].LatitudeRight < 100.0;
						break;
					case 2:
						longitude = DisplayMPOccultations.PathCoordinates[i].LongitudeLeftSigma;
						latitude = DisplayMPOccultations.PathCoordinates[i].LatitudeLeftSigma;
						flag = DisplayMPOccultations.PathCoordinates[i].LatitudeLeftSigma < 100.0;
						break;
					case 3:
						longitude = DisplayMPOccultations.PathCoordinates[i].LongitudeRightSigma;
						latitude = DisplayMPOccultations.PathCoordinates[i].LatitudeRightSigma;
						flag = DisplayMPOccultations.PathCoordinates[i].LatitudeRightSigma < 100.0;
						break;
					}
					flag &= DisplayMPOccultations.PathCoordinates[i].SunAlt < 0.0 || @checked;
					if (flag)
					{
						GoogleEarth.Add_point_to_GEN_Line(longitude, latitude);
					}
				}
				GoogleEarth.Write_GEN_Line_Block(num2);
				num2++;
			}
			GoogleEarth.Close_GEN_File();
		}

		private void SaveGPXPath()
		{
			double longitude = 0.0;
			double latitude = 0.0;
			bool flag = false;
			bool @checked = chkSunUp.get_Checked();
			int num = 1;
			if (lstPathCoords.get_Items().get_Count() < 4 || !GoogleEarth.Create_New_GPX_File(lstPathCoords.get_Items().get_Item(0).ToString()!.Substring(15).Trim().Replace(" ", "_")))
			{
				return;
			}
			for (num = -1; num <= 3; num++)
			{
				for (int i = 0; i < DisplayMPOccultations.PathCoordinates.Count; i++)
				{
					switch (num)
					{
					case -1:
						longitude = DisplayMPOccultations.PathCoordinates[i].LongitudeCentre;
						latitude = DisplayMPOccultations.PathCoordinates[i].LatitudeCentre;
						flag = DisplayMPOccultations.PathCoordinates[i].CentreLineValid;
						break;
					case 0:
						longitude = DisplayMPOccultations.PathCoordinates[i].LongitudeLeft;
						latitude = DisplayMPOccultations.PathCoordinates[i].LatitudeLeft;
						flag = DisplayMPOccultations.PathCoordinates[i].LatitudeLeft < 100.0;
						break;
					case 1:
						longitude = DisplayMPOccultations.PathCoordinates[i].LongitudeRight;
						latitude = DisplayMPOccultations.PathCoordinates[i].LatitudeRight;
						flag = DisplayMPOccultations.PathCoordinates[i].LatitudeRight < 100.0;
						break;
					case 2:
						longitude = DisplayMPOccultations.PathCoordinates[i].LongitudeLeftSigma;
						latitude = DisplayMPOccultations.PathCoordinates[i].LatitudeLeftSigma;
						flag = DisplayMPOccultations.PathCoordinates[i].LatitudeLeftSigma < 100.0;
						break;
					case 3:
						longitude = DisplayMPOccultations.PathCoordinates[i].LongitudeRightSigma;
						latitude = DisplayMPOccultations.PathCoordinates[i].LatitudeRightSigma;
						flag = DisplayMPOccultations.PathCoordinates[i].LatitudeRightSigma < 100.0;
						break;
					}
					flag &= DisplayMPOccultations.PathCoordinates[i].SunAlt < 0.0 || @checked;
					if (flag)
					{
						GoogleEarth.Add_point_to_GPX_Line(longitude, latitude);
					}
				}
				GoogleEarth.Write_GPX_Line_Block();
			}
			GoogleEarth.Close_GPX_File();
		}

		private void SaveHTMPath()
		{
			DisplayMPOccultations.CreateGoogleMapHTMFile(Utilities.AppPath + "\\Generated Files\\GoogleEarth\\" + lstPathCoords.get_Items().get_Item(0).ToString()!.Substring(15).Trim().Replace(" ", "_"), DisplayMPOccultations.EventHeader, Auto: false);
		}

		private void SaveKMLPath()
		{
			DisplayMPOccultations.CreateGoogleEarthKMLFile(Utilities.AppPath + "\\Generated Files\\GoogleEarth\\" + lstPathCoords.get_Items().get_Item(0).ToString()!.Substring(15).Trim(), lstPathCoords.get_Items().get_Item(0).ToString()!.Substring(15), Auto: false, View: false, "");
		}

		private void SaveMIFPath()
		{
			double longitude = 0.0;
			double latitude = 0.0;
			bool flag = false;
			bool @checked = chkSunUp.get_Checked();
			int num = 1;
			if (lstPathCoords.get_Items().get_Count() < 4 || !GoogleEarth.Create_New_MIF_File(lstPathCoords.get_Items().get_Item(0).ToString()!.Substring(15).Trim().Replace(" ", "_")))
			{
				return;
			}
			for (num = -1; num <= 3; num++)
			{
				for (int i = 0; i < DisplayMPOccultations.PathCoordinates.Count; i++)
				{
					switch (num)
					{
					case -1:
						longitude = DisplayMPOccultations.PathCoordinates[i].LongitudeCentre;
						latitude = DisplayMPOccultations.PathCoordinates[i].LatitudeCentre;
						flag = DisplayMPOccultations.PathCoordinates[i].CentreLineValid;
						break;
					case 0:
						longitude = DisplayMPOccultations.PathCoordinates[i].LongitudeLeft;
						latitude = DisplayMPOccultations.PathCoordinates[i].LatitudeLeft;
						flag = DisplayMPOccultations.PathCoordinates[i].LatitudeLeft < 100.0;
						break;
					case 1:
						longitude = DisplayMPOccultations.PathCoordinates[i].LongitudeRight;
						latitude = DisplayMPOccultations.PathCoordinates[i].LatitudeRight;
						flag = DisplayMPOccultations.PathCoordinates[i].LatitudeRight < 100.0;
						break;
					case 2:
						longitude = DisplayMPOccultations.PathCoordinates[i].LongitudeLeftSigma;
						latitude = DisplayMPOccultations.PathCoordinates[i].LatitudeLeftSigma;
						flag = DisplayMPOccultations.PathCoordinates[i].LatitudeLeftSigma < 100.0;
						break;
					case 3:
						longitude = DisplayMPOccultations.PathCoordinates[i].LongitudeRightSigma;
						latitude = DisplayMPOccultations.PathCoordinates[i].LatitudeRightSigma;
						flag = DisplayMPOccultations.PathCoordinates[i].LatitudeRightSigma < 100.0;
						break;
					}
					flag &= DisplayMPOccultations.PathCoordinates[i].SunAlt < 0.0 || @checked;
					if (flag)
					{
						GoogleEarth.Add_point_to_MIF_Line(longitude, latitude);
					}
				}
				GoogleEarth.Write_MIF_Line_Block(num);
			}
			GoogleEarth.Close_MIF_File();
		}

		private void SavePLTPath()
		{
			double longitude = 0.0;
			double latitude = 0.0;
			bool flag = false;
			bool @checked = chkSunUp.get_Checked();
			int num = 1;
			int num2 = 1;
			if (lstPathCoords.get_Items().get_Count() < 4 || !GoogleEarth.Create_New_PLT_File(lstPathCoords.get_Items().get_Item(0).ToString()!.Substring(15).Trim().Replace(" ", "_")))
			{
				return;
			}
			for (num = -1; num <= 3; num++)
			{
				num2 = 1;
				for (int i = 0; i < DisplayMPOccultations.PathCoordinates.Count; i++)
				{
					switch (num)
					{
					case -1:
						longitude = DisplayMPOccultations.PathCoordinates[i].LongitudeCentre;
						latitude = DisplayMPOccultations.PathCoordinates[i].LatitudeCentre;
						flag = DisplayMPOccultations.PathCoordinates[i].CentreLineValid;
						break;
					case 0:
						longitude = DisplayMPOccultations.PathCoordinates[i].LongitudeLeft;
						latitude = DisplayMPOccultations.PathCoordinates[i].LatitudeLeft;
						flag = DisplayMPOccultations.PathCoordinates[i].LatitudeLeft < 100.0;
						break;
					case 1:
						longitude = DisplayMPOccultations.PathCoordinates[i].LongitudeRight;
						latitude = DisplayMPOccultations.PathCoordinates[i].LatitudeRight;
						flag = DisplayMPOccultations.PathCoordinates[i].LatitudeRight < 100.0;
						break;
					case 2:
						longitude = DisplayMPOccultations.PathCoordinates[i].LongitudeLeftSigma;
						latitude = DisplayMPOccultations.PathCoordinates[i].LatitudeLeftSigma;
						flag = DisplayMPOccultations.PathCoordinates[i].LatitudeLeftSigma < 100.0;
						break;
					case 3:
						longitude = DisplayMPOccultations.PathCoordinates[i].LongitudeRightSigma;
						latitude = DisplayMPOccultations.PathCoordinates[i].LatitudeRightSigma;
						flag = DisplayMPOccultations.PathCoordinates[i].LatitudeRightSigma < 100.0;
						break;
					}
					flag &= DisplayMPOccultations.PathCoordinates[i].SunAlt < 0.0 || @checked;
					if (flag)
					{
						GoogleEarth.Add_point_to_PLT_Line(longitude, latitude, num2);
						num2 = 0;
					}
				}
			}
			GoogleEarth.Write_PLT_Line_Block();
			GoogleEarth.Close_PLT_File();
		}

		private void SaveDeLormePath()
		{
			double longitude = 0.0;
			double latitude = 0.0;
			bool flag = false;
			bool @checked = chkSunUp.get_Checked();
			int num = 1;
			if (lstPathCoords.get_Items().get_Count() < 4 || !GoogleEarth.Create_New_DeLorme_File(lstPathCoords.get_Items().get_Item(0).ToString()!.Substring(15).Trim().Replace(" ", "_")))
			{
				return;
			}
			for (num = -1; num <= 3; num++)
			{
				for (int i = 0; i < DisplayMPOccultations.PathCoordinates.Count; i++)
				{
					switch (num)
					{
					case -1:
						longitude = DisplayMPOccultations.PathCoordinates[i].LongitudeCentre;
						latitude = DisplayMPOccultations.PathCoordinates[i].LatitudeCentre;
						flag = DisplayMPOccultations.PathCoordinates[i].CentreLineValid;
						break;
					case 0:
						longitude = DisplayMPOccultations.PathCoordinates[i].LongitudeLeft;
						latitude = DisplayMPOccultations.PathCoordinates[i].LatitudeLeft;
						flag = DisplayMPOccultations.PathCoordinates[i].LatitudeLeft < 100.0;
						break;
					case 1:
						longitude = DisplayMPOccultations.PathCoordinates[i].LongitudeRight;
						latitude = DisplayMPOccultations.PathCoordinates[i].LatitudeRight;
						flag = DisplayMPOccultations.PathCoordinates[i].LatitudeRight < 100.0;
						break;
					case 2:
						longitude = DisplayMPOccultations.PathCoordinates[i].LongitudeLeftSigma;
						latitude = DisplayMPOccultations.PathCoordinates[i].LatitudeLeftSigma;
						flag = DisplayMPOccultations.PathCoordinates[i].LatitudeLeftSigma < 100.0;
						break;
					case 3:
						longitude = DisplayMPOccultations.PathCoordinates[i].LongitudeRightSigma;
						latitude = DisplayMPOccultations.PathCoordinates[i].LatitudeRightSigma;
						flag = DisplayMPOccultations.PathCoordinates[i].LatitudeRightSigma < 100.0;
						break;
					}
					flag &= DisplayMPOccultations.PathCoordinates[i].SunAlt < 0.0 || @checked;
					if (flag)
					{
						GoogleEarth.Add_point_to_DeLorme_Line(longitude, latitude);
					}
				}
				GoogleEarth.Write_DeLorme_Line_Block();
			}
			GoogleEarth.Close_DeLorme_File();
		}

		private void cmxPrecisionMappingStreetsToolStripMenuItem1_Click(object sender, EventArgs e)
		{
			SaveCMXPath();
		}

		private void genToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SaveGENPath();
		}

		private void gpxEasyGPSToolStripMenuItem1_Click(object sender, EventArgs e)
		{
			SaveGPXPath();
		}

		private void htmGoogleMapsHtmlFileToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SaveHTMPath();
		}

		private void kmlGoogleEarthKmlFileToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SaveKMLPath();
		}

		private void mifMSMapPointToolStripMenuItem1_Click(object sender, EventArgs e)
		{
			SaveMIFPath();
		}

		private void pltOziExplorerToolStripMenuItem1_Click(object sender, EventArgs e)
		{
			SavePLTPath();
		}

		private void txtforToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SaveDeLormePath();
		}

		private void AsteroidPathCoordinates_Resize(object sender, EventArgs e)
		{
			if (!((((Control)this).get_Width() < 100) | (((Control)this).get_Height() < 230)))
			{
				((Control)lstPathCoords).set_Width(((Control)this).get_Width() - 28);
				((Control)lstPathCoords).set_Height(((Control)this).get_Height() - 152);
			}
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Asteroid - path coordinates");
		}

		private void eurasterToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0019: Unknown result type (might be due to invalid IL or missing references)
			if (((ListControl)lstPathCoords).get_SelectedIndex() < 0)
			{
				MessageBox.Show("You must select a line of the list of path coordinates", "No point selected", (MessageBoxButtons)0);
				return;
			}
			string text = lstPathCoords.get_Items().get_Item(((ListControl)lstPathCoords).get_SelectedIndex()).ToString();
			string text2 = "E ";
			if (text.Substring(3, 1) == "-")
			{
				text2 = "W ";
			}
			string text3 = text.Substring(4, 1) + text.Substring(5, 2).Replace(" ", "0") + " " + text.Substring(8, 2).Replace(" ", "0") + " " + text.Substring(11, 2).Replace(" ", "0");
			string text4 = "N ";
			if (text.Substring(16, 1) == "-")
			{
				text4 = "S ";
			}
			string text5 = text.Substring(17, 2).Replace(" ", "0") + " " + text.Substring(20, 2).Replace(" ", "0") + " " + text.Substring(23, 2).Replace(" ", "0");
			string text6 = text.Substring(28, 2).Replace(" ", "0") + ":" + text.Substring(31, 2).Replace(" ", "0") + ":" + text.Substring(34, 2).Replace(" ", "0");
			string text7 = DisplayMPOccultations.OrbitSourceDate.Replace("INTG:", "").PadRight(20).Substring(0, 20);
			Clipboard.SetText("P+ | " + text7 + " | " + text6 + " | " + text6 + " |       |     |    | " + text2 + text3 + "   | " + text4 + text5 + "   |    0 | WS |;");
		}

		private void japanToolStripMenuItem_Click(object sender, EventArgs e)
		{
		}

		private void occultToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(lstPathCoords.get_Items().get_Item(((ListControl)lstPathCoords).get_SelectedIndex()).ToString() + "^" + DisplayMPOccultations.OrbitSourceDate.Replace("INTG:", ""));
		}

		private void cmdPlotInGE_Click(object sender, EventArgs e)
		{
			if (lstPathCoords.get_Items().get_Count() > 0)
			{
				DisplayMPOccultations.CreateGoogleEarthKMLFile(Utilities.AppPath + "\\Generated Files\\GoogleEarth\\" + lstPathCoords.get_Items().get_Item(0).ToString()!.Substring(15).Trim() + "abcdefghijk".Substring(FileCount, 1), lstPathCoords.get_Items().get_Item(0).ToString()!.Substring(15), Auto: true, View: true, "");
			}
			FileCount++;
			if (FileCount > 8)
			{
				FileCount = 0;
			}
			SetWarning();
		}

		private void copyPredictionLineOccultFormatToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_001b: Unknown result type (might be due to invalid IL or missing references)
			if (((ListControl)lstPathCoords).get_SelectedIndex() < 0)
			{
				MessageBox.Show("A line to copy has not been selected. Please select a line and try again", "Prediction line not selected", (MessageBoxButtons)0, (MessageBoxIcon)16);
			}
			else
			{
				Clipboard.SetText(lstPathCoords.get_Items().get_Item(((ListControl)lstPathCoords).get_SelectedIndex()).ToString() + "^" + DisplayMPOccultations.OrbitSourceDate.Replace("INTG:", ""));
			}
		}

		private void copyPredictionLinePasteIntoReportToolStripMenuItem_Click(object sender, EventArgs e)
		{
			CopyAndPaste();
		}

		private void CopyAndPaste()
		{
			//IL_0014: Unknown result type (might be due to invalid IL or missing references)
			//IL_0036: Unknown result type (might be due to invalid IL or missing references)
			if (Data_and_Plots.Observations_Editor == null)
			{
				MessageBox.Show("Cannot copy prediction and Paste into report, as the observations editor is not open", "Observation editor not open", (MessageBoxButtons)0, (MessageBoxIcon)16);
				return;
			}
			if (((ListControl)lstPathCoords).get_SelectedIndex() < 0)
			{
				MessageBox.Show("A line to copy has not been selected. Please select a line and try again", "Prediction line not selected", (MessageBoxButtons)0, (MessageBoxIcon)16);
				return;
			}
			Clipboard.SetText(lstPathCoords.get_Items().get_Item(((ListControl)lstPathCoords).get_SelectedIndex()).ToString() + "^" + DisplayMPOccultations.OrbitSourceDate.Replace("INTG:", ""));
			Data_and_Plots.Observations_Editor.PastePredictionLine();
		}

		private void copyPredictionLinePasteIntoReportToolStripMenuItem1_Click(object sender, EventArgs e)
		{
			CopyAndPaste();
		}

		private void ChktimePrecision_CheckedChanged(object sender, EventArgs e)
		{
			ComputePath(TopoPlot: false, 0, 0, 0, chkTime.get_Checked());
		}

		private void updnStep_MouseUp(object sender, MouseEventArgs e)
		{
			if (updnStep.get_Value() > 1.01m)
			{
				updnStep.set_Increment(0.5m);
			}
			else
			{
				updnStep.set_Increment(0.1m);
			}
			if (updnStep.get_Value() > 1.1m)
			{
				updnStep.set_Value((decimal)(int)(2m * updnStep.get_Value()) / 2m);
			}
			else if (updnStep.get_Value() == 1.1m)
			{
				updnStep.set_Value(1.5m);
			}
			else
			{
				updnStep.set_Value((decimal)(int)(10m * updnStep.get_Value()) / 10m);
			}
		}

		private void updnFenceNumberLeft_ValueChanged(object sender, EventArgs e)
		{
			updnFenceNumber_Left.set_Value((decimal)Convert.ToInt16(updnFenceNumber_Left.get_Value()));
		}

		private void txtFenceSeparation_KeyPress(object sender, KeyPressEventArgs e)
		{
			e.set_Handled(!char.IsNumber(e.get_KeyChar()) && e.get_KeyChar() != '\b');
		}

		private void updnFenceNumberRight_ValueChanged(object sender, EventArgs e)
		{
			updnFenceNumberRight.set_Value((decimal)Convert.ToInt16(updnFenceNumberRight.get_Value()));
		}

		private void cmdHelp_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Asteroid - No TopoPath");
		}

		private void updnLongW_ValueChanged(object sender, EventArgs e)
		{
			PathWithin10DegNS();
		}

		private void updnLongE_ValueChanged(object sender, EventArgs e)
		{
			PathWithin10DegNS();
		}

		private void updnLatN_ValueChanged(object sender, EventArgs e)
		{
			PathWithin10DegNS();
		}

		private void updnLatS_ValueChanged(object sender, EventArgs e)
		{
			PathWithin10DegNS();
		}

		private void PathWithin10DegNS()
		{
			int num = (int)(updnLongE.get_Value() - updnLongW.get_Value());
			if (num > 360)
			{
				num -= 360;
			}
			if (num < 0)
			{
				num += 360;
			}
			int num2 = (int)(updnLatN.get_Value() - updnLatS.get_Value());
			bool enabled = (num <= 10 && num2 <= 10) & ListingIsTopo & chkLimit.get_Checked();
			((Control)chkHeights).set_Enabled(enabled);
		}

		private void chkLimit_CheckedChanged(object sender, EventArgs e)
		{
			PathWithin10DegNS();
		}

		private void SetWarning()
		{
			string[] files = Directory.GetFiles(Utilities.AppPath + "\\Generated Files\\GoogleEarth", "*.kmz");
			long num = files.Sum((string x) => new FileInfo(x).Length);
			((Control)lblGEwarning).set_Text("There are " + files.Length + " .kmz files occupying" + string.Format(" {0,1:f0}MB ", num / 1048576) + "\r\n  in the Occult 4 subdirectory:  Generated Files/GoogleEarth/  ");
			((Control)lblGEwarning).set_Visible(files.Length > 10);
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
			//IL_0104: Unknown result type (might be due to invalid IL or missing references)
			//IL_010e: Expected O, but got Unknown
			//IL_010f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0119: Expected O, but got Unknown
			//IL_011a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0124: Expected O, but got Unknown
			//IL_0125: Unknown result type (might be due to invalid IL or missing references)
			//IL_012f: Expected O, but got Unknown
			//IL_0130: Unknown result type (might be due to invalid IL or missing references)
			//IL_013a: Expected O, but got Unknown
			//IL_013b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0145: Expected O, but got Unknown
			//IL_0146: Unknown result type (might be due to invalid IL or missing references)
			//IL_0150: Expected O, but got Unknown
			//IL_0151: Unknown result type (might be due to invalid IL or missing references)
			//IL_015b: Expected O, but got Unknown
			//IL_015c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0166: Expected O, but got Unknown
			//IL_0167: Unknown result type (might be due to invalid IL or missing references)
			//IL_0171: Expected O, but got Unknown
			//IL_0172: Unknown result type (might be due to invalid IL or missing references)
			//IL_017c: Expected O, but got Unknown
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
			//IL_01eb: Unknown result type (might be due to invalid IL or missing references)
			//IL_01f5: Expected O, but got Unknown
			//IL_01f6: Unknown result type (might be due to invalid IL or missing references)
			//IL_0200: Expected O, but got Unknown
			//IL_0201: Unknown result type (might be due to invalid IL or missing references)
			//IL_020b: Expected O, but got Unknown
			//IL_020c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0216: Expected O, but got Unknown
			//IL_0217: Unknown result type (might be due to invalid IL or missing references)
			//IL_0221: Expected O, but got Unknown
			//IL_0222: Unknown result type (might be due to invalid IL or missing references)
			//IL_022c: Expected O, but got Unknown
			//IL_022d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0237: Expected O, but got Unknown
			//IL_0238: Unknown result type (might be due to invalid IL or missing references)
			//IL_0242: Expected O, but got Unknown
			//IL_0243: Unknown result type (might be due to invalid IL or missing references)
			//IL_024d: Expected O, but got Unknown
			//IL_024e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0258: Expected O, but got Unknown
			//IL_0259: Unknown result type (might be due to invalid IL or missing references)
			//IL_0263: Expected O, but got Unknown
			//IL_0264: Unknown result type (might be due to invalid IL or missing references)
			//IL_026e: Expected O, but got Unknown
			//IL_026f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0279: Expected O, but got Unknown
			//IL_027a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0284: Expected O, but got Unknown
			//IL_0285: Unknown result type (might be due to invalid IL or missing references)
			//IL_028f: Expected O, but got Unknown
			//IL_0290: Unknown result type (might be due to invalid IL or missing references)
			//IL_029a: Expected O, but got Unknown
			//IL_029b: Unknown result type (might be due to invalid IL or missing references)
			//IL_02a5: Expected O, but got Unknown
			//IL_02a6: Unknown result type (might be due to invalid IL or missing references)
			//IL_02b0: Expected O, but got Unknown
			//IL_02b1: Unknown result type (might be due to invalid IL or missing references)
			//IL_02bb: Expected O, but got Unknown
			//IL_02bc: Unknown result type (might be due to invalid IL or missing references)
			//IL_02c6: Expected O, but got Unknown
			//IL_02c7: Unknown result type (might be due to invalid IL or missing references)
			//IL_02d1: Expected O, but got Unknown
			//IL_02d2: Unknown result type (might be due to invalid IL or missing references)
			//IL_02dc: Expected O, but got Unknown
			//IL_02dd: Unknown result type (might be due to invalid IL or missing references)
			//IL_02e7: Expected O, but got Unknown
			//IL_02e8: Unknown result type (might be due to invalid IL or missing references)
			//IL_02f2: Expected O, but got Unknown
			//IL_02f3: Unknown result type (might be due to invalid IL or missing references)
			//IL_02fd: Expected O, but got Unknown
			//IL_0304: Unknown result type (might be due to invalid IL or missing references)
			//IL_030e: Expected O, but got Unknown
			//IL_030f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0319: Expected O, but got Unknown
			//IL_031a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0324: Expected O, but got Unknown
			//IL_1290: Unknown result type (might be due to invalid IL or missing references)
			//IL_129a: Expected O, but got Unknown
			//IL_1332: Unknown result type (might be due to invalid IL or missing references)
			//IL_133c: Expected O, but got Unknown
			//IL_13eb: Unknown result type (might be due to invalid IL or missing references)
			//IL_13f5: Expected O, but got Unknown
			//IL_293c: Unknown result type (might be due to invalid IL or missing references)
			//IL_2946: Expected O, but got Unknown
			components = new Container();
			menuStrip1 = new MenuStrip();
			withPredictionToolStripMenuItem = new ToolStripMenuItem();
			copyPredictionLinePasteIntoReportToolStripMenuItem1 = new ToolStripMenuItem();
			createPredictedObservationReportToolStripMenuItem = new ToolStripMenuItem();
			occultToolStripMenuItem = new ToolStripMenuItem();
			eurasterToolStripMenuItem = new ToolStripMenuItem();
			japanToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator2 = new ToolStripSeparator();
			copyToolStripMenuItem = new ToolStripMenuItem();
			printToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			saveOtherMapFormatsToolStripMenuItem = new ToolStripMenuItem();
			cmxPrecisionMappingStreetsToolStripMenuItem = new ToolStripMenuItem();
			genforMakingESRIShapefilesToolStripMenuItem = new ToolStripMenuItem();
			gpxEasyGPSToolStripMenuItem = new ToolStripMenuItem();
			htmforGoogleMapsToolStripMenuItem = new ToolStripMenuItem();
			kmlGoogleEarthKmlToolStripMenuItem = new ToolStripMenuItem();
			mifMSMapPointToolStripMenuItem = new ToolStripMenuItem();
			pltOziExplorerToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			lstPathCoords = new ListBox();
			contextMenuStrip1 = new ContextMenuStrip(components);
			copyPredictionLinePasteIntoReportToolStripMenuItem = new ToolStripMenuItem();
			copyPredictionLineOccultFormatToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator1 = new ToolStripSeparator();
			cmxPrecisionMappingStreetsToolStripMenuItem1 = new ToolStripMenuItem();
			genToolStripMenuItem = new ToolStripMenuItem();
			gpxEasyGPSToolStripMenuItem1 = new ToolStripMenuItem();
			htmGoogleMapsHtmlFileToolStripMenuItem = new ToolStripMenuItem();
			kmlGoogleEarthKmlFileToolStripMenuItem = new ToolStripMenuItem();
			mifMSMapPointToolStripMenuItem1 = new ToolStripMenuItem();
			pltOziExplorerToolStripMenuItem1 = new ToolStripMenuItem();
			txtforToolStripMenuItem = new ToolStripMenuItem();
			label2 = new Label();
			cmdCompute = new Button();
			chkSunUp = new CheckBox();
			updnStep = new NumericUpDown();
			chktimePrecision = new CheckBox();
			pnlTopography = new Panel();
			updnSpacing = new NumericUpDown();
			cmdComputeTopo = new Button();
			updnFenceNumberRight = new NumericUpDown();
			chkIncludeFences = new CheckBox();
			label13 = new Label();
			updnFenceNumber_Left = new NumericUpDown();
			label9 = new Label();
			label8 = new Label();
			panelWarn = new Panel();
			cmdHelp = new Button();
			lblWarning = new Label();
			panel4 = new Panel();
			cmdPlotInGE = new Button();
			label20 = new Label();
			label19 = new Label();
			updnLatN = new NumericUpDown();
			updnLongE = new NumericUpDown();
			updnLatS = new NumericUpDown();
			updnLongW = new NumericUpDown();
			chkLimit = new CheckBox();
			label5 = new Label();
			label10 = new Label();
			label11 = new Label();
			label12 = new Label();
			panel5 = new Panel();
			label1 = new Label();
			chkHeights = new CheckBox();
			panel2 = new Panel();
			toolTip1 = new ToolTip(components);
			lblGEwarning = new Label();
			chkTime = new CheckBox();
			((Control)menuStrip1).SuspendLayout();
			((Control)contextMenuStrip1).SuspendLayout();
			((ISupportInitialize)updnStep).BeginInit();
			((Control)pnlTopography).SuspendLayout();
			((ISupportInitialize)updnSpacing).BeginInit();
			((ISupportInitialize)updnFenceNumberRight).BeginInit();
			((ISupportInitialize)updnFenceNumber_Left).BeginInit();
			((Control)panelWarn).SuspendLayout();
			((Control)panel4).SuspendLayout();
			((ISupportInitialize)updnLatN).BeginInit();
			((ISupportInitialize)updnLongE).BeginInit();
			((ISupportInitialize)updnLatS).BeginInit();
			((ISupportInitialize)updnLongW).BeginInit();
			((Control)panel5).SuspendLayout();
			((Control)panel2).SuspendLayout();
			((Control)this).SuspendLayout();
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)withPredictionToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(1095, 24));
			((Control)menuStrip1).set_TabIndex(0);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withPredictionToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[7]
			{
				(ToolStripItem)copyPredictionLinePasteIntoReportToolStripMenuItem1,
				(ToolStripItem)createPredictedObservationReportToolStripMenuItem,
				(ToolStripItem)toolStripSeparator2,
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)printToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem,
				(ToolStripItem)saveOtherMapFormatsToolStripMenuItem
			});
			((ToolStripItem)withPredictionToolStripMenuItem).set_Name("withPredictionToolStripMenuItem");
			((ToolStripItem)withPredictionToolStripMenuItem).set_Size(new Size(108, 20));
			((ToolStripItem)withPredictionToolStripMenuItem).set_Text("with Prediction...");
			((ToolStripItem)copyPredictionLinePasteIntoReportToolStripMenuItem1).set_Font(new Font("Segoe UI", 9f, FontStyle.Bold));
			((ToolStripItem)copyPredictionLinePasteIntoReportToolStripMenuItem1).set_Name("copyPredictionLinePasteIntoReportToolStripMenuItem1");
			((ToolStripItem)copyPredictionLinePasteIntoReportToolStripMenuItem1).set_Size(new Size(307, 22));
			((ToolStripItem)copyPredictionLinePasteIntoReportToolStripMenuItem1).set_Text("Copy prediction line && Paste into report");
			((ToolStripItem)copyPredictionLinePasteIntoReportToolStripMenuItem1).add_Click((EventHandler)copyPredictionLinePasteIntoReportToolStripMenuItem1_Click);
			((ToolStripDropDownItem)createPredictedObservationReportToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)occultToolStripMenuItem,
				(ToolStripItem)eurasterToolStripMenuItem,
				(ToolStripItem)japanToolStripMenuItem
			});
			((ToolStripItem)createPredictedObservationReportToolStripMenuItem).set_Name("createPredictedObservationReportToolStripMenuItem");
			((ToolStripItem)createPredictedObservationReportToolStripMenuItem).set_Size(new Size(307, 22));
			((ToolStripItem)createPredictedObservationReportToolStripMenuItem).set_Text("Create Prediction line for Observation report");
			((ToolStripItem)occultToolStripMenuItem).set_Name("occultToolStripMenuItem");
			((ToolStripItem)occultToolStripMenuItem).set_Size(new Size(156, 22));
			((ToolStripItem)occultToolStripMenuItem).set_Text("Occult");
			((ToolStripItem)occultToolStripMenuItem).add_Click((EventHandler)occultToolStripMenuItem_Click);
			((ToolStripItem)eurasterToolStripMenuItem).set_Name("eurasterToolStripMenuItem");
			eurasterToolStripMenuItem.set_ShortcutKeys((Keys)131141);
			((ToolStripItem)eurasterToolStripMenuItem).set_Size(new Size(156, 22));
			((ToolStripItem)eurasterToolStripMenuItem).set_Text("Euraster");
			((ToolStripItem)eurasterToolStripMenuItem).add_Click((EventHandler)eurasterToolStripMenuItem_Click);
			((ToolStripItem)japanToolStripMenuItem).set_Enabled(false);
			((ToolStripItem)japanToolStripMenuItem).set_Name("japanToolStripMenuItem");
			japanToolStripMenuItem.set_ShortcutKeys((Keys)131146);
			((ToolStripItem)japanToolStripMenuItem).set_Size(new Size(156, 22));
			((ToolStripItem)japanToolStripMenuItem).set_Text("Japan");
			((ToolStripItem)japanToolStripMenuItem).add_Click((EventHandler)japanToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator2).set_Name("toolStripSeparator2");
			((ToolStripItem)toolStripSeparator2).set_Size(new Size(304, 6));
			((ToolStripItem)copyToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyToolStripMenuItem).set_Name("copyToolStripMenuItem");
			copyToolStripMenuItem.set_ShortcutKeys((Keys)131139);
			((ToolStripItem)copyToolStripMenuItem).set_Size(new Size(307, 22));
			((ToolStripItem)copyToolStripMenuItem).set_Text("&Copy path listing");
			((ToolStripItem)copyToolStripMenuItem).add_Click((EventHandler)copyToolStripMenuItem_Click);
			((ToolStripItem)printToolStripMenuItem).set_Image((Image)Resources.Print);
			((ToolStripItem)printToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printToolStripMenuItem).set_Name("printToolStripMenuItem");
			printToolStripMenuItem.set_ShortcutKeys((Keys)131152);
			((ToolStripItem)printToolStripMenuItem).set_Size(new Size(307, 22));
			((ToolStripItem)printToolStripMenuItem).set_Text("&Print path listing");
			((ToolStripItem)printToolStripMenuItem).add_Click((EventHandler)printToolStripMenuItem_Click);
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			saveToolStripMenuItem.set_ShortcutKeys((Keys)131155);
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(307, 22));
			((ToolStripItem)saveToolStripMenuItem).set_Text("&Save path listing");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
			((ToolStripDropDownItem)saveOtherMapFormatsToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[7]
			{
				(ToolStripItem)cmxPrecisionMappingStreetsToolStripMenuItem,
				(ToolStripItem)genforMakingESRIShapefilesToolStripMenuItem,
				(ToolStripItem)gpxEasyGPSToolStripMenuItem,
				(ToolStripItem)htmforGoogleMapsToolStripMenuItem,
				(ToolStripItem)kmlGoogleEarthKmlToolStripMenuItem,
				(ToolStripItem)mifMSMapPointToolStripMenuItem,
				(ToolStripItem)pltOziExplorerToolStripMenuItem
			});
			((ToolStripItem)saveOtherMapFormatsToolStripMenuItem).set_Enabled(false);
			((ToolStripItem)saveOtherMapFormatsToolStripMenuItem).set_Name("saveOtherMapFormatsToolStripMenuItem");
			((ToolStripItem)saveOtherMapFormatsToolStripMenuItem).set_Size(new Size(307, 22));
			((ToolStripItem)saveOtherMapFormatsToolStripMenuItem).set_Text("Save in map formats");
			((ToolStripItem)cmxPrecisionMappingStreetsToolStripMenuItem).set_Name("cmxPrecisionMappingStreetsToolStripMenuItem");
			((ToolStripItem)cmxPrecisionMappingStreetsToolStripMenuItem).set_Size(new Size(251, 22));
			((ToolStripItem)cmxPrecisionMappingStreetsToolStripMenuItem).set_Text(".cmx  [Precision Mapping Streets]");
			((ToolStripItem)cmxPrecisionMappingStreetsToolStripMenuItem).add_Click((EventHandler)cmxPrecisionMappingStreetsToolStripMenuItem_Click);
			((ToolStripItem)genforMakingESRIShapefilesToolStripMenuItem).set_Name("genforMakingESRIShapefilesToolStripMenuItem");
			((ToolStripItem)genforMakingESRIShapefilesToolStripMenuItem).set_Size(new Size(251, 22));
			((ToolStripItem)genforMakingESRIShapefilesToolStripMenuItem).set_Text(".gen  [for making ESRI shapefiles]");
			((ToolStripItem)genforMakingESRIShapefilesToolStripMenuItem).add_Click((EventHandler)genforMakingESRIShapefilesToolStripMenuItem_Click);
			((ToolStripItem)gpxEasyGPSToolStripMenuItem).set_Image((Image)Resources.easygps);
			((ToolStripItem)gpxEasyGPSToolStripMenuItem).set_Name("gpxEasyGPSToolStripMenuItem");
			((ToolStripItem)gpxEasyGPSToolStripMenuItem).set_Size(new Size(251, 22));
			((ToolStripItem)gpxEasyGPSToolStripMenuItem).set_Text(".gpx  [Easy GPS]");
			((ToolStripItem)gpxEasyGPSToolStripMenuItem).add_Click((EventHandler)gpxEasyGPSToolStripMenuItem_Click);
			((ToolStripItem)htmforGoogleMapsToolStripMenuItem).set_Image((Image)Resources.mm_20_red);
			((ToolStripItem)htmforGoogleMapsToolStripMenuItem).set_Name("htmforGoogleMapsToolStripMenuItem");
			((ToolStripItem)htmforGoogleMapsToolStripMenuItem).set_Size(new Size(251, 22));
			((ToolStripItem)htmforGoogleMapsToolStripMenuItem).set_Text(".htm  [for GoogleMaps]");
			((ToolStripItem)htmforGoogleMapsToolStripMenuItem).add_Click((EventHandler)htmforGoogleMapsToolStripMenuItem_Click);
			((ToolStripItem)kmlGoogleEarthKmlToolStripMenuItem).set_Image((Image)Resources.google_earth);
			((ToolStripItem)kmlGoogleEarthKmlToolStripMenuItem).set_Name("kmlGoogleEarthKmlToolStripMenuItem");
			((ToolStripItem)kmlGoogleEarthKmlToolStripMenuItem).set_Size(new Size(251, 22));
			((ToolStripItem)kmlGoogleEarthKmlToolStripMenuItem).set_Text(".kmz   [Google Earth kmz]");
			((ToolStripItem)kmlGoogleEarthKmlToolStripMenuItem).add_Click((EventHandler)kmlGoogleEarthKmlToolStripMenuItem_Click);
			((ToolStripItem)mifMSMapPointToolStripMenuItem).set_Image((Image)Resources.web_article_icons_mp_cd);
			((ToolStripItem)mifMSMapPointToolStripMenuItem).set_Name("mifMSMapPointToolStripMenuItem");
			((ToolStripItem)mifMSMapPointToolStripMenuItem).set_Size(new Size(251, 22));
			((ToolStripItem)mifMSMapPointToolStripMenuItem).set_Text(".mif   [MS MapPoint]");
			((ToolStripItem)mifMSMapPointToolStripMenuItem).add_Click((EventHandler)mifMSMapPointToolStripMenuItem_Click);
			((ToolStripItem)pltOziExplorerToolStripMenuItem).set_Name("pltOziExplorerToolStripMenuItem");
			((ToolStripItem)pltOziExplorerToolStripMenuItem).set_Size(new Size(251, 22));
			((ToolStripItem)pltOziExplorerToolStripMenuItem).set_Text(".plt    [OziExplorer]");
			((ToolStripItem)pltOziExplorerToolStripMenuItem).add_Click((EventHandler)pltOziExplorerToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(72, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("&Help    ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(54, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("E&xit");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((Control)lstPathCoords).set_ContextMenuStrip(contextMenuStrip1);
			((Control)lstPathCoords).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstPathCoords).set_FormattingEnabled(true);
			lstPathCoords.set_HorizontalExtent(1060);
			lstPathCoords.set_HorizontalScrollbar(true);
			lstPathCoords.set_ItemHeight(14);
			((Control)lstPathCoords).set_Location(new Point(6, 106));
			((Control)lstPathCoords).set_Name("lstPathCoords");
			((Control)lstPathCoords).set_Size(new Size(1082, 382));
			((Control)lstPathCoords).set_TabIndex(1);
			((ToolStrip)contextMenuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[11]
			{
				(ToolStripItem)copyPredictionLinePasteIntoReportToolStripMenuItem,
				(ToolStripItem)copyPredictionLineOccultFormatToolStripMenuItem,
				(ToolStripItem)toolStripSeparator1,
				(ToolStripItem)cmxPrecisionMappingStreetsToolStripMenuItem1,
				(ToolStripItem)genToolStripMenuItem,
				(ToolStripItem)gpxEasyGPSToolStripMenuItem1,
				(ToolStripItem)htmGoogleMapsHtmlFileToolStripMenuItem,
				(ToolStripItem)kmlGoogleEarthKmlFileToolStripMenuItem,
				(ToolStripItem)mifMSMapPointToolStripMenuItem1,
				(ToolStripItem)pltOziExplorerToolStripMenuItem1,
				(ToolStripItem)txtforToolStripMenuItem
			});
			((Control)contextMenuStrip1).set_Name("contextMenuStrip1");
			((Control)contextMenuStrip1).set_Size(new Size(401, 390));
			((ToolStripItem)copyPredictionLinePasteIntoReportToolStripMenuItem).set_Font(new Font("Segoe UI", 9f, FontStyle.Bold));
			((ToolStripItem)copyPredictionLinePasteIntoReportToolStripMenuItem).set_Name("copyPredictionLinePasteIntoReportToolStripMenuItem");
			((ToolStripItem)copyPredictionLinePasteIntoReportToolStripMenuItem).set_Size(new Size(400, 38));
			((ToolStripItem)copyPredictionLinePasteIntoReportToolStripMenuItem).set_Text("Copy prediction line && Paste into report");
			((ToolStripItem)copyPredictionLinePasteIntoReportToolStripMenuItem).add_Click((EventHandler)copyPredictionLinePasteIntoReportToolStripMenuItem_Click);
			((ToolStripItem)copyPredictionLineOccultFormatToolStripMenuItem).set_Name("copyPredictionLineOccultFormatToolStripMenuItem");
			((ToolStripItem)copyPredictionLineOccultFormatToolStripMenuItem).set_Size(new Size(400, 38));
			((ToolStripItem)copyPredictionLineOccultFormatToolStripMenuItem).set_Text("Copy prediction line for observation report - Occult format");
			((ToolStripItem)copyPredictionLineOccultFormatToolStripMenuItem).add_Click((EventHandler)copyPredictionLineOccultFormatToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator1).set_Name("toolStripSeparator1");
			((ToolStripItem)toolStripSeparator1).set_Size(new Size(397, 6));
			((ToolStripItem)cmxPrecisionMappingStreetsToolStripMenuItem1).set_Name("cmxPrecisionMappingStreetsToolStripMenuItem1");
			((ToolStripItem)cmxPrecisionMappingStreetsToolStripMenuItem1).set_Size(new Size(400, 38));
			((ToolStripItem)cmxPrecisionMappingStreetsToolStripMenuItem1).set_Text(".cmx   [Precision Mapping Streets (zip to email)]");
			((ToolStripItem)cmxPrecisionMappingStreetsToolStripMenuItem1).add_Click((EventHandler)cmxPrecisionMappingStreetsToolStripMenuItem1_Click);
			((ToolStripItem)genToolStripMenuItem).set_Name("genToolStripMenuItem");
			((ToolStripItem)genToolStripMenuItem).set_Size(new Size(400, 38));
			((ToolStripItem)genToolStripMenuItem).set_Text(".gen   [for making ESRI shapefiles]");
			((ToolStripItem)genToolStripMenuItem).add_Click((EventHandler)genToolStripMenuItem_Click);
			((ToolStripItem)gpxEasyGPSToolStripMenuItem1).set_Image((Image)Resources.easygps);
			((ToolStripItem)gpxEasyGPSToolStripMenuItem1).set_ImageScaling((ToolStripItemImageScaling)0);
			((ToolStripItem)gpxEasyGPSToolStripMenuItem1).set_Name("gpxEasyGPSToolStripMenuItem1");
			((ToolStripItem)gpxEasyGPSToolStripMenuItem1).set_Size(new Size(400, 38));
			((ToolStripItem)gpxEasyGPSToolStripMenuItem1).set_Text(".gpx   [Easy GPS]");
			((ToolStripItem)gpxEasyGPSToolStripMenuItem1).add_Click((EventHandler)gpxEasyGPSToolStripMenuItem1_Click);
			((ToolStripItem)htmGoogleMapsHtmlFileToolStripMenuItem).set_Image((Image)Resources.mm_20_red);
			((ToolStripItem)htmGoogleMapsHtmlFileToolStripMenuItem).set_ImageAlign(ContentAlignment.MiddleRight);
			((ToolStripItem)htmGoogleMapsHtmlFileToolStripMenuItem).set_ImageScaling((ToolStripItemImageScaling)0);
			((ToolStripItem)htmGoogleMapsHtmlFileToolStripMenuItem).set_Name("htmGoogleMapsHtmlFileToolStripMenuItem");
			((ToolStripItem)htmGoogleMapsHtmlFileToolStripMenuItem).set_Size(new Size(400, 38));
			((ToolStripItem)htmGoogleMapsHtmlFileToolStripMenuItem).set_Text(".htm   [GoogleMaps html file]");
			((ToolStripItem)htmGoogleMapsHtmlFileToolStripMenuItem).add_Click((EventHandler)htmGoogleMapsHtmlFileToolStripMenuItem_Click);
			((ToolStripItem)kmlGoogleEarthKmlFileToolStripMenuItem).set_Image((Image)Resources.kml_file);
			((ToolStripItem)kmlGoogleEarthKmlFileToolStripMenuItem).set_ImageScaling((ToolStripItemImageScaling)0);
			((ToolStripItem)kmlGoogleEarthKmlFileToolStripMenuItem).set_Name("kmlGoogleEarthKmlFileToolStripMenuItem");
			((ToolStripItem)kmlGoogleEarthKmlFileToolStripMenuItem).set_Size(new Size(400, 38));
			((ToolStripItem)kmlGoogleEarthKmlFileToolStripMenuItem).set_Text(".kml    [Google Earth kml file]");
			((ToolStripItem)kmlGoogleEarthKmlFileToolStripMenuItem).add_Click((EventHandler)kmlGoogleEarthKmlFileToolStripMenuItem_Click);
			((ToolStripItem)mifMSMapPointToolStripMenuItem1).set_Image((Image)Resources.web_article_icons_mp_cd);
			((ToolStripItem)mifMSMapPointToolStripMenuItem1).set_Name("mifMSMapPointToolStripMenuItem1");
			((ToolStripItem)mifMSMapPointToolStripMenuItem1).set_Size(new Size(400, 38));
			((ToolStripItem)mifMSMapPointToolStripMenuItem1).set_Text(".mif    [MS MapPoint]");
			((ToolStripItem)mifMSMapPointToolStripMenuItem1).add_Click((EventHandler)mifMSMapPointToolStripMenuItem1_Click);
			((ToolStripItem)pltOziExplorerToolStripMenuItem1).set_Name("pltOziExplorerToolStripMenuItem1");
			((ToolStripItem)pltOziExplorerToolStripMenuItem1).set_Size(new Size(400, 38));
			((ToolStripItem)pltOziExplorerToolStripMenuItem1).set_Text(".plt     [OziExplorer]");
			((ToolStripItem)pltOziExplorerToolStripMenuItem1).add_Click((EventHandler)pltOziExplorerToolStripMenuItem1_Click);
			((ToolStripItem)txtforToolStripMenuItem).set_Name("txtforToolStripMenuItem");
			((ToolStripItem)txtforToolStripMenuItem).set_Size(new Size(400, 38));
			((ToolStripItem)txtforToolStripMenuItem).set_Text(".txt     [for Delorme Street Atlas]");
			((ToolStripItem)txtforToolStripMenuItem).add_Click((EventHandler)txtforToolStripMenuItem_Click);
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label2).set_Location(new Point(104, 8));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(71, 26));
			((Control)label2).set_TabIndex(5);
			((Control)label2).set_Text("Coordinate \r\ninterval - deg.");
			((Control)cmdCompute).set_BackColor(Color.FromArgb(255, 255, 128));
			((Control)cmdCompute).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdCompute).set_Location(new Point(8, 5));
			((Control)cmdCompute).set_Name("cmdCompute");
			((Control)cmdCompute).set_Size(new Size(91, 56));
			((Control)cmdCompute).set_TabIndex(9);
			((Control)cmdCompute).set_Text("Compute\r\npath at\r\nMSL");
			((ButtonBase)cmdCompute).set_UseVisualStyleBackColor(false);
			((Control)cmdCompute).add_Click((EventHandler)cmdCompute_Click);
			((Control)chkSunUp).set_AutoSize(true);
			chkSunUp.set_Checked(Settings.Default.AsteroidPathSunUp);
			((Control)chkSunUp).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "AsteroidPathSunUp", true, (DataSourceUpdateMode)1));
			((Control)chkSunUp).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkSunUp).set_Location(new Point(4, -3));
			((Control)chkSunUp).set_Name("chkSunUp");
			((Control)chkSunUp).set_Size(new Size(99, 30));
			((Control)chkSunUp).set_TabIndex(2);
			((Control)chkSunUp).set_Text("Include path \r\nwhen Sun is up");
			((ButtonBase)chkSunUp).set_UseVisualStyleBackColor(true);
			((Control)updnStep).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "AsteroidPathStepSize", true, (DataSourceUpdateMode)1));
			updnStep.set_DecimalPlaces(1);
			((Control)updnStep).set_Location(new Point(114, 38));
			updnStep.set_Maximum(new decimal(new int[4] { 20, 0, 0, 0 }));
			updnStep.set_Minimum(new decimal(new int[4] { 1, 0, 0, 65536 }));
			((Control)updnStep).set_Name("updnStep");
			((Control)updnStep).set_Size(new Size(45, 20));
			((Control)updnStep).set_TabIndex(3);
			updnStep.set_Value(Settings.Default.AsteroidPathStepSize);
			((Control)updnStep).add_MouseUp(new MouseEventHandler(updnStep_MouseUp));
			((Control)chktimePrecision).set_AutoSize(true);
			chktimePrecision.set_Checked(true);
			chktimePrecision.set_CheckState((CheckState)1);
			((Control)chktimePrecision).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chktimePrecision).set_Location(new Point(4, 23));
			((Control)chktimePrecision).set_Name("chktimePrecision");
			((Control)chktimePrecision).set_Size(new Size(102, 30));
			((Control)chktimePrecision).set_TabIndex(15);
			((Control)chktimePrecision).set_Text("Display extra \r\nprecision in Time");
			((ButtonBase)chktimePrecision).set_UseVisualStyleBackColor(true);
			chktimePrecision.add_CheckedChanged((EventHandler)ChktimePrecision_CheckedChanged);
			((Control)pnlTopography).set_BackColor(Color.LightYellow);
			pnlTopography.set_BorderStyle((BorderStyle)2);
			((Control)pnlTopography).get_Controls().Add((Control)(object)updnSpacing);
			((Control)pnlTopography).get_Controls().Add((Control)(object)cmdComputeTopo);
			((Control)pnlTopography).get_Controls().Add((Control)(object)updnFenceNumberRight);
			((Control)pnlTopography).get_Controls().Add((Control)(object)chkIncludeFences);
			((Control)pnlTopography).get_Controls().Add((Control)(object)label13);
			((Control)pnlTopography).get_Controls().Add((Control)(object)updnFenceNumber_Left);
			((Control)pnlTopography).get_Controls().Add((Control)(object)label9);
			((Control)pnlTopography).get_Controls().Add((Control)(object)label8);
			((Control)pnlTopography).set_Location(new Point(328, 32));
			((Control)pnlTopography).set_Name("pnlTopography");
			((Control)pnlTopography).set_Size(new Size(370, 70));
			((Control)pnlTopography).set_TabIndex(20);
			updnSpacing.set_Increment(new decimal(new int[4] { 10, 0, 0, 0 }));
			((Control)updnSpacing).set_Location(new Point(170, 40));
			updnSpacing.set_Maximum(new decimal(new int[4] { 50000, 0, 0, 0 }));
			updnSpacing.set_Minimum(new decimal(new int[4] { 10, 0, 0, 0 }));
			((Control)updnSpacing).set_Name("updnSpacing");
			((Control)updnSpacing).set_Size(new Size(49, 20));
			((Control)updnSpacing).set_TabIndex(27);
			updnSpacing.set_Value(new decimal(new int[4] { 100, 0, 0, 0 }));
			((Control)cmdComputeTopo).set_BackColor(Color.FromArgb(128, 255, 128));
			((ButtonBase)cmdComputeTopo).get_FlatAppearance().set_BorderColor(Color.White);
			((Control)cmdComputeTopo).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdComputeTopo).set_Location(new Point(5, 5));
			((Control)cmdComputeTopo).set_Name("cmdComputeTopo");
			((Control)cmdComputeTopo).set_Size(new Size(130, 56));
			((Control)cmdComputeTopo).set_TabIndex(21);
			((Control)cmdComputeTopo).set_Text("Compute using\r\nlocal topography\r\nat 0.75 arcmin steps");
			((ButtonBase)cmdComputeTopo).set_UseVisualStyleBackColor(false);
			((Control)cmdComputeTopo).add_Click((EventHandler)cmdComputeTopo_Click);
			((Control)updnFenceNumberRight).set_Location(new Point(316, 40));
			updnFenceNumberRight.set_Maximum(new decimal(new int[4] { 20, 0, 0, 0 }));
			updnFenceNumberRight.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnFenceNumberRight).set_Name("updnFenceNumberRight");
			((Control)updnFenceNumberRight).set_Size(new Size(35, 20));
			((Control)updnFenceNumberRight).set_TabIndex(26);
			updnFenceNumberRight.set_Value(new decimal(new int[4] { 4, 0, 0, 0 }));
			updnFenceNumberRight.add_ValueChanged((EventHandler)updnFenceNumberRight_ValueChanged);
			((Control)chkIncludeFences).set_AutoSize(true);
			((Control)chkIncludeFences).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)chkIncludeFences).set_Location(new Point(178, 5));
			((Control)chkIncludeFences).set_Name("chkIncludeFences");
			((Control)chkIncludeFences).set_Size(new Size(137, 17));
			((Control)chkIncludeFences).set_TabIndex(20);
			((Control)chkIncludeFences).set_Text("Include Fence lines");
			((ButtonBase)chkIncludeFences).set_UseVisualStyleBackColor(true);
			((Control)label13).set_AutoSize(true);
			((Control)label13).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label13).set_Location(new Point(307, 25));
			((Control)label13).set_Name("label13");
			((Control)label13).set_Size(new Size(52, 13));
			((Control)label13).set_TabIndex(25);
			((Control)label13).set_Text("# on right");
			((Control)updnFenceNumber_Left).set_Location(new Point(257, 40));
			updnFenceNumber_Left.set_Maximum(new decimal(new int[4] { 20, 0, 0, 0 }));
			updnFenceNumber_Left.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnFenceNumber_Left).set_Name("updnFenceNumber_Left");
			((Control)updnFenceNumber_Left).set_Size(new Size(35, 20));
			((Control)updnFenceNumber_Left).set_TabIndex(21);
			updnFenceNumber_Left.set_Value(new decimal(new int[4] { 4, 0, 0, 0 }));
			updnFenceNumber_Left.add_ValueChanged((EventHandler)updnFenceNumberLeft_ValueChanged);
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label9).set_Location(new Point(142, 25));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(98, 13));
			((Control)label9).set_TabIndex(23);
			((Control)label9).set_Text("Separation (meters)");
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label8).set_Location(new Point(251, 25));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(46, 13));
			((Control)label8).set_TabIndex(22);
			((Control)label8).set_Text("# on left");
			((Control)panelWarn).set_BackColor(Color.LightYellow);
			((Control)panelWarn).get_Controls().Add((Control)(object)cmdHelp);
			((Control)panelWarn).get_Controls().Add((Control)(object)lblWarning);
			((Control)panelWarn).set_Location(new Point(522, 120));
			((Control)panelWarn).set_Name("panelWarn");
			((Control)panelWarn).set_Size(new Size(229, 66));
			((Control)panelWarn).set_TabIndex(43);
			((Control)cmdHelp).set_BackColor(Color.LightSkyBlue);
			((Control)cmdHelp).set_Location(new Point(171, 39));
			((Control)cmdHelp).set_Name("cmdHelp");
			((Control)cmdHelp).set_Size(new Size(47, 24));
			((Control)cmdHelp).set_TabIndex(27);
			((Control)cmdHelp).set_Text("Help");
			((ButtonBase)cmdHelp).set_UseVisualStyleBackColor(false);
			((Control)cmdHelp).set_Visible(false);
			((Control)cmdHelp).add_Click((EventHandler)cmdHelp_Click);
			((Control)lblWarning).set_AutoSize(true);
			((Control)lblWarning).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblWarning).set_ForeColor(Color.Red);
			((Control)lblWarning).set_Location(new Point(5, 5));
			((Control)lblWarning).set_Name("lblWarning");
			((Control)lblWarning).set_Size(new Size(219, 51));
			((Control)lblWarning).set_TabIndex(26);
			((Control)lblWarning).set_Text("This option is only available when\r\nthe orbital elements are for the \r\ndate && time of the event");
			((Control)panel4).set_BackColor(Color.Honeydew);
			panel4.set_BorderStyle((BorderStyle)2);
			((Control)panel4).get_Controls().Add((Control)(object)label2);
			((Control)panel4).get_Controls().Add((Control)(object)cmdCompute);
			((Control)panel4).get_Controls().Add((Control)(object)updnStep);
			((Control)panel4).set_Location(new Point(131, 32));
			((Control)panel4).set_Name("panel4");
			((Control)panel4).set_Size(new Size(182, 70));
			((Control)panel4).set_TabIndex(21);
			((Control)cmdPlotInGE).set_BackColor(Color.Aqua);
			((Control)cmdPlotInGE).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdPlotInGE).set_ForeColor(Color.Black);
			((Control)cmdPlotInGE).set_Location(new Point(8, 5));
			((Control)cmdPlotInGE).set_Name("cmdPlotInGE");
			((Control)cmdPlotInGE).set_Size(new Size(109, 56));
			((Control)cmdPlotInGE).set_TabIndex(22);
			((Control)cmdPlotInGE).set_Text("Draw listed MSL path in \r\nGoogle Earth");
			((ButtonBase)cmdPlotInGE).set_UseVisualStyleBackColor(false);
			((Control)cmdPlotInGE).add_Click((EventHandler)cmdPlotInGE_Click);
			((Control)label20).set_AutoSize(true);
			((Control)label20).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label20).set_Location(new Point(128, 22));
			((Control)label20).set_Name("label20");
			((Control)label20).set_Size(new Size(34, 13));
			((Control)label20).set_TabIndex(25);
			((Control)label20).set_Text("Long.");
			((Control)label19).set_AutoSize(true);
			((Control)label19).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label19).set_Location(new Point(134, 46));
			((Control)label19).set_Name("label19");
			((Control)label19).set_Size(new Size(25, 13));
			((Control)label19).set_TabIndex(26);
			((Control)label19).set_Text("Lat.");
			((Control)updnLatN).set_Location(new Point(171, 43));
			updnLatN.set_Maximum(new decimal(new int[4] { 90, 0, 0, 0 }));
			updnLatN.set_Minimum(new decimal(new int[4] { 90, 0, 0, -2147483648 }));
			((Control)updnLatN).set_Name("updnLatN");
			((Control)updnLatN).set_Size(new Size(35, 20));
			((Control)updnLatN).set_TabIndex(32);
			updnLatN.set_Value(new decimal(new int[4] { 50, 0, 0, 0 }));
			updnLatN.add_ValueChanged((EventHandler)updnLatN_ValueChanged);
			((Control)updnLongE).set_Location(new Point(229, 19));
			updnLongE.set_Maximum(new decimal(new int[4] { 180, 0, 0, 0 }));
			updnLongE.set_Minimum(new decimal(new int[4] { 180, 0, 0, -2147483648 }));
			((Control)updnLongE).set_Name("updnLongE");
			((Control)updnLongE).set_Size(new Size(42, 20));
			((Control)updnLongE).set_TabIndex(31);
			updnLongE.set_Value(new decimal(new int[4] { 60, 0, 0, -2147483648 }));
			updnLongE.add_ValueChanged((EventHandler)updnLongE_ValueChanged);
			((Control)updnLatS).set_Location(new Point(236, 44));
			updnLatS.set_Maximum(new decimal(new int[4] { 90, 0, 0, 0 }));
			updnLatS.set_Minimum(new decimal(new int[4] { 90, 0, 0, -2147483648 }));
			((Control)updnLatS).set_Name("updnLatS");
			((Control)updnLatS).set_Size(new Size(35, 20));
			((Control)updnLatS).set_TabIndex(30);
			updnLatS.set_Value(new decimal(new int[4] { 20, 0, 0, 0 }));
			updnLatS.add_ValueChanged((EventHandler)updnLatS_ValueChanged);
			((Control)updnLongW).set_Location(new Point(164, 19));
			updnLongW.set_Maximum(new decimal(new int[4] { 180, 0, 0, 0 }));
			updnLongW.set_Minimum(new decimal(new int[4] { 180, 0, 0, -2147483648 }));
			((Control)updnLongW).set_Name("updnLongW");
			((Control)updnLongW).set_Size(new Size(42, 20));
			((Control)updnLongW).set_TabIndex(29);
			updnLongW.set_Value(new decimal(new int[4] { 120, 0, 0, -2147483648 }));
			updnLongW.add_ValueChanged((EventHandler)updnLongW_ValueChanged);
			((Control)chkLimit).set_AutoSize(true);
			((Control)chkLimit).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)chkLimit).set_Location(new Point(128, 2));
			((Control)chkLimit).set_Name("chkLimit");
			((Control)chkLimit).set_Size(new Size(168, 17));
			((Control)chkLimit).set_TabIndex(37);
			((Control)chkLimit).set_Text("Limit path to being within");
			((ButtonBase)chkLimit).set_UseVisualStyleBackColor(true);
			chkLimit.add_CheckedChanged((EventHandler)chkLimit_CheckedChanged);
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Location(new Point(206, 47));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(24, 13));
			((Control)label5).set_TabIndex(38);
			((Control)label5).set_Text("Nth");
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Location(new Point(271, 48));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(23, 13));
			((Control)label10).set_TabIndex(39);
			((Control)label10).set_Text("Sth");
			((Control)label11).set_AutoSize(true);
			((Control)label11).set_Location(new Point(207, 23));
			((Control)label11).set_Name("label11");
			((Control)label11).set_Size(new Size(18, 13));
			((Control)label11).set_TabIndex(40);
			((Control)label11).set_Text("W");
			((Control)label12).set_AutoSize(true);
			((Control)label12).set_Location(new Point(273, 23));
			((Control)label12).set_Name("label12");
			((Control)label12).set_Size(new Size(14, 13));
			((Control)label12).set_TabIndex(41);
			((Control)label12).set_Text("E");
			((Control)panel5).set_BackColor(Color.Lavender);
			panel5.set_BorderStyle((BorderStyle)2);
			((Control)panel5).get_Controls().Add((Control)(object)label1);
			((Control)panel5).get_Controls().Add((Control)(object)chkHeights);
			((Control)panel5).get_Controls().Add((Control)(object)label12);
			((Control)panel5).get_Controls().Add((Control)(object)label11);
			((Control)panel5).get_Controls().Add((Control)(object)label10);
			((Control)panel5).get_Controls().Add((Control)(object)label5);
			((Control)panel5).get_Controls().Add((Control)(object)chkLimit);
			((Control)panel5).get_Controls().Add((Control)(object)label20);
			((Control)panel5).get_Controls().Add((Control)(object)label19);
			((Control)panel5).get_Controls().Add((Control)(object)updnLatN);
			((Control)panel5).get_Controls().Add((Control)(object)updnLongE);
			((Control)panel5).get_Controls().Add((Control)(object)updnLatS);
			((Control)panel5).get_Controls().Add((Control)(object)updnLongW);
			((Control)panel5).get_Controls().Add((Control)(object)cmdPlotInGE);
			((Control)panel5).set_Location(new Point(713, 32));
			((Control)panel5).set_Name("panel5");
			((Control)panel5).set_Size(new Size(370, 70));
			((Control)panel5).set_TabIndex(42);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label1).set_ForeColor(Color.MediumBlue);
			((Control)label1).set_Location(new Point(296, 7));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(64, 13));
			((Control)label1).set_TabIndex(43);
			((Control)label1).set_Text("10°x10° limit");
			((Control)chkHeights).set_AutoSize(true);
			chkHeights.set_CheckAlign(ContentAlignment.BottomCenter);
			((Control)chkHeights).set_Location(new Point(292, 19));
			((Control)chkHeights).set_Name("chkHeights");
			((Control)chkHeights).set_Size(new Size(72, 44));
			((Control)chkHeights).set_TabIndex(42);
			((Control)chkHeights).set_Text("Include\r\n topo heights");
			((ButtonBase)chkHeights).set_TextAlign(ContentAlignment.MiddleCenter);
			toolTip1.SetToolTip((Control)(object)chkHeights, "This is only available if  the path is limited to  no more than 10° in both longitude and latitude");
			((ButtonBase)chkHeights).set_UseVisualStyleBackColor(true);
			((Control)panel2).set_BackColor(Color.LightCyan);
			panel2.set_BorderStyle((BorderStyle)2);
			((Control)panel2).get_Controls().Add((Control)(object)chkTime);
			((Control)panel2).get_Controls().Add((Control)(object)chkSunUp);
			((Control)panel2).get_Controls().Add((Control)(object)chktimePrecision);
			((Control)panel2).set_Location(new Point(6, 32));
			((Control)panel2).set_Name("panel2");
			((Control)panel2).set_Size(new Size(110, 70));
			((Control)panel2).set_TabIndex(43);
			((Control)lblGEwarning).set_AutoSize(true);
			((Control)lblGEwarning).set_ForeColor(Color.Maroon);
			((Control)lblGEwarning).set_Location(new Point(778, 4));
			((Control)lblGEwarning).set_Name("lblGEwarning");
			((Control)lblGEwarning).set_Size(new Size(300, 26));
			((Control)lblGEwarning).set_TabIndex(44);
			((Control)lblGEwarning).set_Text("There are 50 .kmz files occupying 56MB\r\n  in the Occult 4 subdirectory:  Generated Files/GoogleEarth/  ");
			lblGEwarning.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)chkTime).set_AutoSize(true);
			((Control)chkTime).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkTime).set_Location(new Point(4, 51));
			((Control)chkTime).set_Name("chkTime");
			((Control)chkTime).set_Size(new Size(105, 17));
			((Control)chkTime).set_TabIndex(16);
			((Control)chkTime).set_Text("path by time only");
			((ButtonBase)chkTime).set_UseVisualStyleBackColor(true);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(1095, 495));
			((Control)this).get_Controls().Add((Control)(object)lblGEwarning);
			((Control)this).get_Controls().Add((Control)(object)panelWarn);
			((Control)this).get_Controls().Add((Control)(object)panel2);
			((Control)this).get_Controls().Add((Control)(object)panel5);
			((Control)this).get_Controls().Add((Control)(object)panel4);
			((Control)this).get_Controls().Add((Control)(object)pnlTopography);
			((Control)this).get_Controls().Add((Control)(object)lstPathCoords);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationAsterPath", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_Location(Settings.Default.LocationAsterPath);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_Name("AsteroidPath");
			((Control)this).set_Text("Asteroid - Path coordinates");
			((Form)this).add_Load((EventHandler)AsteroidPathCoordinates_Load);
			((Control)this).add_Resize((EventHandler)AsteroidPathCoordinates_Resize);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)contextMenuStrip1).ResumeLayout(false);
			((ISupportInitialize)updnStep).EndInit();
			((Control)pnlTopography).ResumeLayout(false);
			((Control)pnlTopography).PerformLayout();
			((ISupportInitialize)updnSpacing).EndInit();
			((ISupportInitialize)updnFenceNumberRight).EndInit();
			((ISupportInitialize)updnFenceNumber_Left).EndInit();
			((Control)panelWarn).ResumeLayout(false);
			((Control)panelWarn).PerformLayout();
			((Control)panel4).ResumeLayout(false);
			((Control)panel4).PerformLayout();
			((ISupportInitialize)updnLatN).EndInit();
			((ISupportInitialize)updnLongE).EndInit();
			((ISupportInitialize)updnLatS).EndInit();
			((ISupportInitialize)updnLongW).EndInit();
			((Control)panel5).ResumeLayout(false);
			((Control)panel5).PerformLayout();
			((Control)panel2).ResumeLayout(false);
			((Control)panel2).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
