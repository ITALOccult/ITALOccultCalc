using System;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Text;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Asteroid_Observations
{
	public class DisplayPositions : Form
	{
		internal static string HeaderLineEventLabels;

		internal static string HeaderLineEventTime;

		internal static string HeaderLineXYZLabels;

		internal static string HeaderLineXYZ;

		internal static string HeaderLineConjunctionLabels;

		internal static string HeaderLineConjunctionTime;

		internal static string HeaderLineEventLabelsCSV;

		internal static string HeaderLineEventTimeCSV;

		internal static string HeaderLineXYZLabelsCSV;

		internal static string HeaderLineXYZCSV;

		internal static string HeaderLineConjunctionLabelsCSV;

		internal static string HeaderLineConjunctionTimeCSV;

		private bool IsReading;

		private IContainer components;

		private MenuStrip menuStrip1;

		private ListBox lstPositions;

		private ToolStripMenuItem withPositionsToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem printPreviewToolStripMenuItem;

		private ToolStripMenuItem printToolStripMenuItem;

		private ToolStripMenuItem saveAllAsTextToolStripMenuItem;

		private ToolStripMenuItem sortToolStripMenuItem;

		private ToolStripMenuItem byAsteroidNumberToolStripMenuItem;

		private ToolStripMenuItem byAsteroidNameToolStripMenuItem;

		private ToolStripMenuItem byDateToolStripMenuItem;

		private ToolStripMenuItem bySeparationToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private ToolStripMenuItem byUncertaintyInSeparationToolStripMenuItem;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripMenuItem byPositionSourceToolStripMenuItem;

		private Label lblPosition;

		private ToolStripMenuItem byRAOffsetToolStripMenuItem;

		private ToolStripMenuItem byDecOffsetToolStripMenuItem;

		private RadioButton optAtEvent;

		private RadioButton optConjunction;

		private Panel panel1;

		private Button cmdRegenerate;

		private ToolStripMenuItem bySeparationAtConjunctionToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator1;

		private ToolStripSeparator toolStripSeparator2;

		private ToolStripMenuItem byDateOfConjunctionToolStripMenuItem;

		private Label lblCoord;

		private ToolStripMenuItem copySelectedToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator4;

		private ToolStripSeparator toolStripSeparator3;

		private ToolStripMenuItem clearAllSelectionsToolStripMenuItem;

		private Label lblMainHeader;

		private ToolStripMenuItem saveAllAsCSVToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator5;

		private CheckBox chkByYear;

		private ComboBox cmbYear;

		private Label label1;

		private CheckBox chkPlanetsOnly;

		private RadioButton optXYZ;

		private CheckBox chkExcludeStarsNoPM;

		private Label lblCount;

		public DisplayPositions()
		{
			InitializeComponent();
		}

		private void DisplayPositions_Load(object sender, EventArgs e)
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
			for (int i = 2000; i <= DateTime.Now.Year; i++)
			{
				cmbYear.get_Items().Add((object)i.ToString());
			}
			((ListControl)cmbYear).set_SelectedIndex(cmbYear.get_Items().get_Count() - 1);
			HeaderLineEventLabels = "    Object                 Date      Time         Right Ascens.   Declination     Right Ascens.   Declination    Src           dRA      dDec  |Track PA| Event T|  T   track Xtrack   dRA   dDec  corln|    Star   |Dup  RU shape";
			HeaderLineEventTime = "      # Asteroid       Year  m  d  h  m   s       h  m   s        o  '   \"        h  m   s        o  '   \"       Cat     N     asec     asec  |   o    |   s    |  s     mas   mas    mas    mas  coeff| mas   mas |Src  WE cntrd";
			HeaderLineXYZLabels = "    Object                 Date      Time          Geocentric coordinates (km)     Right Ascens.   Declination    Src       |Track PA| Event T|  T   track Xtrack   dRA   dDec  corln|    Star   |Dup  RU shape";
			HeaderLineXYZ = "      # Asteroid       Year  m  d  h  m   s          X          Y          Z       h  m   s        o  '   \"       Cat     N |   o    |   s    |  s     mas   mas    mas    mas  coeff| mas   mas |Src  WE cntrd";
			HeaderLineConjunctionLabels = "    Object                 Date      Time        Right Ascens.   Declination     Right Ascens.   Declination    Src           Sepn    P.A.  |Track PA| Event T|  Fit to object   |    Star   |Dup  RU shape";
			HeaderLineConjunctionTime = "      # Asteroid       Year  m  d  h  m   s      h  m   s        o  '   \"        h  m   s        o  '   \"       Cat     N     \"        o    |   o    |   s    |  s     mas   mas | mas   mas |Src  WE cntrd";
			HeaderLineEventLabelsCSV = ",Object,Y,M,D,Time,Right Ascens.,Declination,Right Ascens.,Declination,Src,Chords,dRA,dDec,Track PA,Event T,T,track,Xtrack,dRA,dDec,corln,Star,Star,Dup,RU,shape";
			HeaderLineEventTimeCSV = "#,Asteroid,Year,m,d,h  m   s,h  m   s,o  '   \",h  m   s,o  '   \",Cat,N,asec,asec  ,o,s,s,mas,mas,mas,mas,coeff,mas,mas,Src,WE,cntrd";
			HeaderLineXYZLabelsCSV = ",Object,Y,M,D,Time,Geo X, GeoY, GeoZ,Right Ascens.,Declination,Src, Chords,Track PA,Event T,T,track,Xtrack,dRA,dDec,corln,Star,Star,Dup,RU,shape";
			HeaderLineXYZCSV = "#,Asteroid,Year,m,d,h  m   s,X,Y,Z,h  m   s,o  '   \",Cat,N,o,s,s,mas,mas,mas,mas,coeff,mas,mas,Src,WE,cntrd";
			HeaderLineConjunctionLabelsCSV = ",Object,Y,M,D,Time,Right Ascens.,Declination,Right Ascens.,Declination,Src, Chords,Sepn,P.A.,Track PA,Event T,FitT,Along,Across,Star,Star,Dup,RU,shape";
			HeaderLineConjunctionTimeCSV = "#,Asteroid,Year,m,d,h  m   s,h  m   s,o  '   \",h  m   s,o  '   \",Cat,N, \",o,o,s,s,mas,mas,mas,mas,Src,WE,cntrd";
		}

		private void copySelectedToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(CollectEvents(OnlySelected: true));
		}

		private void clearAllSelectionsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			for (int i = 0; i < lstPositions.get_Items().get_Count(); i++)
			{
				lstPositions.SetSelected(i, false);
			}
		}

		private void copyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(CollectEvents(OnlySelected: false));
		}

		private void printPreviewToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.PrintPreviewText(CollectEvents(OnlySelected: false));
		}

		private void printToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0061: Unknown result type (might be due to invalid IL or missing references)
			//IL_0067: Invalid comparison between Unknown and I4
			int count = lstPositions.get_Items().get_Count();
			int num = count / 65 + 1;
			if ((int)MessageBox.Show("The number of lines to be printed is " + count + "\r\n\r\nThis will output about " + num + " pages.\r\n\r\nAre you sure you want to continue?", "Print length warning", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 7)
			{
				Output.PrintText_Landscape(CollectEvents(OnlySelected: false), 6);
			}
		}

		private void saveAllAsTextToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Save(AsCSV: false);
		}

		private void saveAllAsCSVToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Save(AsCSV: true);
		}

		private void Save(bool AsCSV)
		{
			string text = "";
			string text2 = "";
			if (chkByYear.get_Checked())
			{
				text2 = ((((ListControl)cmbYear).get_SelectedIndex() != 0) ? ("_" + (1999 + ((ListControl)cmbYear).get_SelectedIndex()) + "_") : "_Pre2000_");
			}
			string text3 = Utilities.AppPath + "/Asteroid/Results/AstrometryXYZ";
			if (optAtEvent.get_Checked())
			{
				text3 = Utilities.AppPath + "/Asteroid/Results/AstrometryAtEvent";
			}
			if (optConjunction.get_Checked())
			{
				text3 = Utilities.AppPath + "/Asteroid/Results/AstrometryAtConjunction";
			}
			text3 = text3 + text2 + DateTime.UtcNow.Year + Utilities.ShortMonths[DateTime.UtcNow.Month] + DateTime.UtcNow.Day;
			text3 = ((!AsCSV) ? (text3 + ".txt") : (text3 + ".csv"));
			using StreamWriter streamWriter = new StreamWriter(text3);
			text = (AsCSV ? (optAtEvent.get_Checked() ? (HeaderLineEventLabelsCSV + "\r\n" + HeaderLineEventTimeCSV) : ((!optXYZ.get_Checked()) ? (HeaderLineConjunctionLabelsCSV + "\r\n" + HeaderLineConjunctionTimeCSV) : (HeaderLineXYZLabelsCSV + "\r\n" + HeaderLineXYZCSV))) : (optAtEvent.get_Checked() ? (HeaderLineEventLabels + "\r\n" + HeaderLineEventTime) : ((!optXYZ.get_Checked()) ? (HeaderLineConjunctionLabels + "\r\n" + HeaderLineConjunctionTime) : (HeaderLineXYZLabels + "\r\n" + HeaderLineXYZ))));
			streamWriter.WriteLine(text);
			for (int i = 0; i < Asteroid_Observations_Reports.PositionsList.Count; i++)
			{
				if (chkByYear.get_Checked())
				{
					if (((ListControl)cmbYear).get_SelectedIndex() == 0)
					{
						if (Asteroid_Observations_Reports.PositionsList[i].RefYear > 1999)
						{
							continue;
						}
					}
					else if (Asteroid_Observations_Reports.PositionsList[i].RefYear != 1999 + ((ListControl)cmbYear).get_SelectedIndex())
					{
						continue;
					}
				}
				text = (optAtEvent.get_Checked() ? Asteroid_Observations_Reports.PositionsList[i].MakeEventString(Satellite: false, 0, XYZcoords: false, out var CSVline) : ((!optXYZ.get_Checked()) ? Asteroid_Observations_Reports.PositionsList[i].MakeConjunctionString(Satellite: false, 0, out CSVline) : Asteroid_Observations_Reports.PositionsList[i].MakeEventString(Satellite: false, 0, XYZcoords: true, out CSVline)));
				if (CSVline.Length > 0)
				{
					if (AsCSV)
					{
						streamWriter.WriteLine(CSVline);
					}
					else
					{
						streamWriter.WriteLine(text);
					}
				}
				int numSatellites = Asteroid_Observations_Reports.PositionsList[i].NumSatellites;
				if (numSatellites <= 0)
				{
					continue;
				}
				for (int j = 0; j < numSatellites; j++)
				{
					text = (optAtEvent.get_Checked() ? Asteroid_Observations_Reports.PositionsList[i].MakeEventString(Satellite: true, j, XYZcoords: false, out CSVline) : ((!optXYZ.get_Checked()) ? Asteroid_Observations_Reports.PositionsList[i].MakeConjunctionString(Satellite: true, j, out CSVline) : Asteroid_Observations_Reports.PositionsList[i].MakeEventString(Satellite: true, j, XYZcoords: true, out CSVline)));
					if (CSVline.Length > 0)
					{
						if (AsCSV)
						{
							streamWriter.WriteLine(CSVline);
						}
						else
						{
							streamWriter.WriteLine(text);
						}
					}
				}
			}
		}

		private string CollectEvents(bool OnlySelected)
		{
			StringBuilder stringBuilder = new StringBuilder();
			int count = lstPositions.get_Items().get_Count();
			if (OnlySelected)
			{
				int num = 0;
				for (int i = 0; i < count; i++)
				{
					if (lstPositions.GetSelected(i) & (lstPositions.get_Items().get_Item(i).ToString()!.Trim() != ""))
					{
						num++;
					}
				}
				for (int j = 0; j < 8; j++)
				{
					string text = lstPositions.get_Items().get_Item(j).ToString();
					if (j == 0)
					{
						text = num + text.Substring(text.IndexOf(" "));
					}
					stringBuilder.AppendLine(text);
					if (text.Contains("#"))
					{
						break;
					}
				}
			}
			for (int k = 0; k < count; k++)
			{
				if (!(OnlySelected & (!lstPositions.GetSelected(k) | (lstPositions.get_Items().get_Item(k).ToString()!.Trim() == ""))))
				{
					string value = lstPositions.get_Items().get_Item(k).ToString();
					stringBuilder.AppendLine(value);
				}
			}
			stringBuilder.AppendLine("");
			return stringBuilder.ToString();
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Component)this).Dispose();
		}

		public void Display()
		{
			string text = "offsets";
			string text2 = "";
			int num = -1;
			if (optConjunction.get_Checked())
			{
				text = "positions";
			}
			lstPositions.get_Items().Clear();
			lstPositions.get_Items().Add((object)(Asteroid_Observations_Reports.PositionsList.Count + " astrometric positions of asteroids derived from occultations"));
			if (optXYZ.get_Checked())
			{
				lstPositions.get_Items().Add((object)"Shadow axis coordinates are the J2000 Geocentric coordinates of the intersection of the shadow axis with the fundamental plane. At this location the asteroid appears to be exactly at the star position.");
			}
			else
			{
				lstPositions.get_Items().Add((object)("All " + text + " corrected for the effects of precession, nutation, aberration and light-time across the geoid, but not deflection"));
			}
			lstPositions.get_Items().Add((object)"Star position has had proper motion, foreshortening and parallax applied, but not deflection");
			lstPositions.get_Items().Add((object)"");
			if (optAtEvent.get_Checked())
			{
				((Control)lblMainHeader).set_Text("                                                       O B J E C T                        S T A R                             O F F S E T S  |.......................U N C E R T A I N T I E S....................|  F L A G S");
				((Control)lblCoord).set_Text(HeaderLineEventLabels);
				((Control)lblPosition).set_Text(HeaderLineEventTime);
				lstPositions.get_Items().Add((object)((Control)lblMainHeader).get_Text());
				lstPositions.get_Items().Add((object)HeaderLineEventLabels);
				lstPositions.get_Items().Add((object)HeaderLineEventTime);
			}
			else if (optXYZ.get_Checked())
			{
				((Control)lblMainHeader).set_Text("                                                     S H A D O W  A X I S                 S T A R                          |...................... U N C E R T A I N T I E S ...................|  F L A G S");
				((Control)lblCoord).set_Text(HeaderLineXYZLabels);
				((Control)lblPosition).set_Text(HeaderLineXYZ);
				lstPositions.get_Items().Add((object)((Control)lblMainHeader).get_Text());
				lstPositions.get_Items().Add((object)HeaderLineXYZLabels);
				lstPositions.get_Items().Add((object)HeaderLineXYZ);
			}
			else
			{
				((Control)lblMainHeader).set_Text("                                                       O B J E C T                        S T A R                            O F F S E T S |..........U N C E R T A I N T I E S.............|  F L A G S");
				((Control)lblCoord).set_Text(HeaderLineConjunctionLabels);
				((Control)lblPosition).set_Text(HeaderLineConjunctionTime);
				lstPositions.get_Items().Add((object)((Control)lblMainHeader).get_Text());
				lstPositions.get_Items().Add((object)HeaderLineConjunctionLabels);
				lstPositions.get_Items().Add((object)HeaderLineConjunctionTime);
			}
			Asteroid_Observations_Reports.PositionsList.Sort();
			for (int i = 0; i < Asteroid_Observations_Reports.PositionsList.Count; i++)
			{
				if (chkPlanetsOnly.get_Checked() && !Asteroid_Observations_Reports.PositionsList[i].AsteroidNumber.Contains("P"))
				{
					continue;
				}
				text2 = (optAtEvent.get_Checked() ? Asteroid_Observations_Reports.PositionsList[i].MakeEventString(Satellite: false, 0, XYZcoords: false, out var CSVline) : ((!optXYZ.get_Checked()) ? Asteroid_Observations_Reports.PositionsList[i].MakeConjunctionString(Satellite: false, 0, out CSVline) : Asteroid_Observations_Reports.PositionsList[i].MakeEventString(Satellite: false, 0, XYZcoords: true, out CSVline)));
				if (text2.Length > 0)
				{
					lstPositions.get_Items().Add((object)text2);
					num++;
					if (num % 5 == 4)
					{
						lstPositions.get_Items().Add((object)"");
					}
				}
				int numSatellites = Asteroid_Observations_Reports.PositionsList[i].NumSatellites;
				if (numSatellites <= 0)
				{
					continue;
				}
				for (int j = 0; j < numSatellites; j++)
				{
					text2 = (optAtEvent.get_Checked() ? Asteroid_Observations_Reports.PositionsList[i].MakeEventString(Satellite: true, j, XYZcoords: false, out CSVline) : ((!optXYZ.get_Checked()) ? Asteroid_Observations_Reports.PositionsList[i].MakeConjunctionString(Satellite: true, j, out CSVline) : Asteroid_Observations_Reports.PositionsList[i].MakeEventString(Satellite: true, j, XYZcoords: true, out CSVline)));
					if (text2.Length > 0)
					{
						lstPositions.get_Items().Add((object)text2);
						num++;
						if (num % 5 == 4)
						{
							lstPositions.get_Items().Add((object)"");
						}
					}
				}
			}
			((Control)lblCount).set_Text(string.Format("{0,1} astrometric positions", num));
		}

		private void byAsteroidNumberToolStripMenuItem_Click(object sender, EventArgs e)
		{
			AsteroidPositions.SortField = 0;
			Display();
		}

		private void byAsteroidNameToolStripMenuItem_Click(object sender, EventArgs e)
		{
			AsteroidPositions.SortField = 1;
			Display();
		}

		private void byDateToolStripMenuItem_Click(object sender, EventArgs e)
		{
			AsteroidPositions.SortField = 2;
			Display();
		}

		private void byRAOffsetToolStripMenuItem_Click(object sender, EventArgs e)
		{
			AsteroidPositions.SortField = 3;
			Display();
		}

		private void byDecOffsetToolStripMenuItem_Click(object sender, EventArgs e)
		{
			AsteroidPositions.SortField = 4;
			Display();
		}

		private void bySeparationToolStripMenuItem_Click(object sender, EventArgs e)
		{
			AsteroidPositions.SortField = 5;
			Display();
		}

		private void byUncertaintyInSeparationToolStripMenuItem_Click(object sender, EventArgs e)
		{
			AsteroidPositions.SortField = 6;
			Display();
		}

		private void chkPlanetsOnly_MouseClick(object sender, MouseEventArgs e)
		{
			chkPlanetsOnly.set_Checked(!chkPlanetsOnly.get_Checked());
			ReadPositions(chkByYear.get_Checked());
		}

		private void chkExcludeStarsNoPM_MouseClick(object sender, MouseEventArgs e)
		{
			chkExcludeStarsNoPM.set_Checked(!chkExcludeStarsNoPM.get_Checked());
			ReadPositions(chkByYear.get_Checked());
		}

		private void chkByYear_MouseClick(object sender, MouseEventArgs e)
		{
			chkByYear.set_Checked(!chkByYear.get_Checked());
			ReadPositions(chkByYear.get_Checked());
		}

		private void cmbYear_SelectedIndexChanged(object sender, EventArgs e)
		{
			if (chkByYear.get_Checked())
			{
				ReadPositions(chkByYear.get_Checked());
			}
		}

		private void byPositionSourceToolStripMenuItem_Click(object sender, EventArgs e)
		{
			AsteroidPositions.SortField = 7;
			Display();
		}

		private void bySeparationAtConjunctionToolStripMenuItem_Click(object sender, EventArgs e)
		{
			AsteroidPositions.SortField = 8;
			Display();
		}

		private void byDateOfConjunctionToolStripMenuItem_Click(object sender, EventArgs e)
		{
			AsteroidPositions.SortField = 9;
			Display();
		}

		private void DisplayPositions_Resize_1(object sender, EventArgs e)
		{
			if (((Control)this).get_Height() < 400)
			{
				((Control)this).set_Height(400);
			}
			if (((Control)this).get_Width() < 300)
			{
				((Control)this).set_Width(300);
			}
			((Control)lstPositions).set_Height(((Control)this).get_Height() - 180);
			((Control)lstPositions).set_Width(((Control)this).get_Width() - 22);
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Astrometric positions - Display");
		}

		private void optConjunction_CheckedChanged(object sender, EventArgs e)
		{
			if (optConjunction.get_Checked())
			{
				Display();
			}
		}

		private void optAtEvent_CheckedChanged(object sender, EventArgs e)
		{
			if (optAtEvent.get_Checked())
			{
				Display();
			}
		}

		private void optXYZ_CheckedChanged(object sender, EventArgs e)
		{
			if (optXYZ.get_Checked())
			{
				Display();
			}
		}

		private void cmdRegenerate_Click(object sender, EventArgs e)
		{
			ReadPositions(chkByYear.get_Checked());
		}

		internal void ReadPositions(bool LimitToYear)
		{
			int yearStart = 0;
			int yearEnd = 2200;
			if (IsReading)
			{
				return;
			}
			IsReading = true;
			if (LimitToYear)
			{
				if (((ListControl)cmbYear).get_SelectedIndex() > 0)
				{
					yearStart = (yearEnd = ((ListControl)cmbYear).get_SelectedIndex() + 1999);
				}
				else
				{
					yearStart = 0;
					yearEnd = 1999;
				}
			}
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			lstPositions.get_Items().Clear();
			CheckBox obj = chkByYear;
			CheckBox obj2 = chkExcludeStarsNoPM;
			CheckBox obj3 = chkPlanetsOnly;
			Panel obj4 = panel1;
			Button obj5 = cmdRegenerate;
			bool flag;
			((Control)cmbYear).set_Enabled(flag = false);
			bool flag2;
			((Control)obj5).set_Enabled(flag2 = flag);
			bool flag3;
			((Control)obj4).set_Enabled(flag3 = flag2);
			bool flag4;
			((Control)obj3).set_Enabled(flag4 = flag3);
			bool enabled;
			((Control)obj2).set_Enabled(enabled = flag4);
			((Control)obj).set_Enabled(enabled);
			Asteroid_Observations_Reports.CreatePositionList(WithNames: false, Gaia_DR2_Updates: false, Asteroids: true, Planets: true, AsteroidSatellites: true, ListAll: true, yearStart, yearEnd, 3000000.0, 0, "", ObserverOnly: false, chkExcludeStarsNoPM.get_Checked(), IncludeExtraData: false);
			Display();
			CheckBox obj6 = chkByYear;
			CheckBox obj7 = chkExcludeStarsNoPM;
			CheckBox obj8 = chkPlanetsOnly;
			Panel obj9 = panel1;
			Button obj10 = cmdRegenerate;
			((Control)cmbYear).set_Enabled(flag = true);
			((Control)obj10).set_Enabled(flag2 = flag);
			((Control)obj9).set_Enabled(flag3 = flag2);
			((Control)obj8).set_Enabled(flag4 = flag3);
			((Control)obj7).set_Enabled(enabled = flag4);
			((Control)obj6).set_Enabled(enabled);
			((Control)this).set_Cursor(Cursors.get_Default());
			IsReading = false;
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
			//IL_10fe: Unknown result type (might be due to invalid IL or missing references)
			//IL_1108: Expected O, but got Unknown
			//IL_127e: Unknown result type (might be due to invalid IL or missing references)
			//IL_1288: Expected O, but got Unknown
			//IL_1311: Unknown result type (might be due to invalid IL or missing references)
			//IL_131b: Expected O, but got Unknown
			//IL_14b1: Unknown result type (might be due to invalid IL or missing references)
			//IL_14bb: Expected O, but got Unknown
			menuStrip1 = new MenuStrip();
			withPositionsToolStripMenuItem = new ToolStripMenuItem();
			copySelectedToolStripMenuItem = new ToolStripMenuItem();
			clearAllSelectionsToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator4 = new ToolStripSeparator();
			copyToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator3 = new ToolStripSeparator();
			printPreviewToolStripMenuItem = new ToolStripMenuItem();
			printToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator5 = new ToolStripSeparator();
			saveAllAsTextToolStripMenuItem = new ToolStripMenuItem();
			saveAllAsCSVToolStripMenuItem = new ToolStripMenuItem();
			sortToolStripMenuItem = new ToolStripMenuItem();
			byAsteroidNameToolStripMenuItem = new ToolStripMenuItem();
			byAsteroidNumberToolStripMenuItem = new ToolStripMenuItem();
			byDateToolStripMenuItem = new ToolStripMenuItem();
			byDateOfConjunctionToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator1 = new ToolStripSeparator();
			byRAOffsetToolStripMenuItem = new ToolStripMenuItem();
			byDecOffsetToolStripMenuItem = new ToolStripMenuItem();
			bySeparationToolStripMenuItem = new ToolStripMenuItem();
			bySeparationAtConjunctionToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator2 = new ToolStripSeparator();
			byUncertaintyInSeparationToolStripMenuItem = new ToolStripMenuItem();
			byPositionSourceToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			lstPositions = new ListBox();
			lblPosition = new Label();
			optAtEvent = new RadioButton();
			optConjunction = new RadioButton();
			panel1 = new Panel();
			optXYZ = new RadioButton();
			cmdRegenerate = new Button();
			lblCoord = new Label();
			lblMainHeader = new Label();
			chkByYear = new CheckBox();
			cmbYear = new ComboBox();
			label1 = new Label();
			chkPlanetsOnly = new CheckBox();
			chkExcludeStarsNoPM = new CheckBox();
			lblCount = new Label();
			((Control)menuStrip1).SuspendLayout();
			((Control)panel1).SuspendLayout();
			((Control)this).SuspendLayout();
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[4]
			{
				(ToolStripItem)withPositionsToolStripMenuItem,
				(ToolStripItem)sortToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(940, 24));
			((Control)menuStrip1).set_TabIndex(0);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withPositionsToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[10]
			{
				(ToolStripItem)copySelectedToolStripMenuItem,
				(ToolStripItem)clearAllSelectionsToolStripMenuItem,
				(ToolStripItem)toolStripSeparator4,
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)toolStripSeparator3,
				(ToolStripItem)printPreviewToolStripMenuItem,
				(ToolStripItem)printToolStripMenuItem,
				(ToolStripItem)toolStripSeparator5,
				(ToolStripItem)saveAllAsTextToolStripMenuItem,
				(ToolStripItem)saveAllAsCSVToolStripMenuItem
			});
			((ToolStripItem)withPositionsToolStripMenuItem).set_Name("withPositionsToolStripMenuItem");
			((ToolStripItem)withPositionsToolStripMenuItem).set_Size(new Size(114, 20));
			((ToolStripItem)withPositionsToolStripMenuItem).set_Text("with Positions...    ");
			((ToolStripItem)copySelectedToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copySelectedToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copySelectedToolStripMenuItem).set_Name("copySelectedToolStripMenuItem");
			((ToolStripItem)copySelectedToolStripMenuItem).set_Size(new Size(193, 22));
			((ToolStripItem)copySelectedToolStripMenuItem).set_Text("Copy selected lines");
			((ToolStripItem)copySelectedToolStripMenuItem).add_Click((EventHandler)copySelectedToolStripMenuItem_Click);
			((ToolStripItem)clearAllSelectionsToolStripMenuItem).set_Image((Image)Resources.clear_all_black_144x144);
			((ToolStripItem)clearAllSelectionsToolStripMenuItem).set_Name("clearAllSelectionsToolStripMenuItem");
			((ToolStripItem)clearAllSelectionsToolStripMenuItem).set_Size(new Size(193, 22));
			((ToolStripItem)clearAllSelectionsToolStripMenuItem).set_Text("Clear all selections");
			((ToolStripItem)clearAllSelectionsToolStripMenuItem).add_Click((EventHandler)clearAllSelectionsToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator4).set_Name("toolStripSeparator4");
			((ToolStripItem)toolStripSeparator4).set_Size(new Size(190, 6));
			((ToolStripItem)copyToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyToolStripMenuItem).set_Name("copyToolStripMenuItem");
			copyToolStripMenuItem.set_ShortcutKeys((Keys)131139);
			((ToolStripItem)copyToolStripMenuItem).set_Size(new Size(193, 22));
			((ToolStripItem)copyToolStripMenuItem).set_Text("&Copy list");
			((ToolStripItem)copyToolStripMenuItem).add_Click((EventHandler)copyToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator3).set_Name("toolStripSeparator3");
			((ToolStripItem)toolStripSeparator3).set_Size(new Size(190, 6));
			((ToolStripItem)printPreviewToolStripMenuItem).set_Image((Image)Resources.PrintPreview);
			((ToolStripItem)printPreviewToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Name("printPreviewToolStripMenuItem");
			((ToolStripItem)printPreviewToolStripMenuItem).set_Size(new Size(193, 22));
			((ToolStripItem)printPreviewToolStripMenuItem).set_Text("Print preview");
			((ToolStripItem)printPreviewToolStripMenuItem).add_Click((EventHandler)printPreviewToolStripMenuItem_Click);
			((ToolStripItem)printToolStripMenuItem).set_Image((Image)Resources.Print);
			((ToolStripItem)printToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printToolStripMenuItem).set_Name("printToolStripMenuItem");
			printToolStripMenuItem.set_ShortcutKeys((Keys)131152);
			((ToolStripItem)printToolStripMenuItem).set_Size(new Size(193, 22));
			((ToolStripItem)printToolStripMenuItem).set_Text("&Print list");
			((ToolStripItem)printToolStripMenuItem).add_Click((EventHandler)printToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator5).set_Name("toolStripSeparator5");
			((ToolStripItem)toolStripSeparator5).set_Size(new Size(190, 6));
			((ToolStripItem)saveAllAsTextToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveAllAsTextToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveAllAsTextToolStripMenuItem).set_Name("saveAllAsTextToolStripMenuItem");
			saveAllAsTextToolStripMenuItem.set_ShortcutKeys((Keys)131155);
			((ToolStripItem)saveAllAsTextToolStripMenuItem).set_Size(new Size(193, 22));
			((ToolStripItem)saveAllAsTextToolStripMenuItem).set_Text("&Save list as text");
			((ToolStripItem)saveAllAsTextToolStripMenuItem).set_ToolTipText("File saved at \r\nAsteroid\\results\\Astrometry\r\nxyz....txt\r\nAtEvent....txt\r\nAtConjunction....txt\r\n");
			((ToolStripItem)saveAllAsTextToolStripMenuItem).add_Click((EventHandler)saveAllAsTextToolStripMenuItem_Click);
			((ToolStripItem)saveAllAsCSVToolStripMenuItem).set_Image((Image)Resources.CSV);
			((ToolStripItem)saveAllAsCSVToolStripMenuItem).set_Name("saveAllAsCSVToolStripMenuItem");
			((ToolStripItem)saveAllAsCSVToolStripMenuItem).set_Size(new Size(193, 22));
			((ToolStripItem)saveAllAsCSVToolStripMenuItem).set_Text("Save list as CSV");
			((ToolStripItem)saveAllAsCSVToolStripMenuItem).set_ToolTipText("File saved at\r\n/Asteroid/Results/Astrometry\r\nxyz....csv\r\nAtEvent....csv\r\nAtConjunction....csv");
			((ToolStripItem)saveAllAsCSVToolStripMenuItem).add_Click((EventHandler)saveAllAsCSVToolStripMenuItem_Click);
			((ToolStripDropDownItem)sortToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[12]
			{
				(ToolStripItem)byAsteroidNameToolStripMenuItem,
				(ToolStripItem)byAsteroidNumberToolStripMenuItem,
				(ToolStripItem)byDateToolStripMenuItem,
				(ToolStripItem)byDateOfConjunctionToolStripMenuItem,
				(ToolStripItem)toolStripSeparator1,
				(ToolStripItem)byRAOffsetToolStripMenuItem,
				(ToolStripItem)byDecOffsetToolStripMenuItem,
				(ToolStripItem)bySeparationToolStripMenuItem,
				(ToolStripItem)bySeparationAtConjunctionToolStripMenuItem,
				(ToolStripItem)toolStripSeparator2,
				(ToolStripItem)byUncertaintyInSeparationToolStripMenuItem,
				(ToolStripItem)byPositionSourceToolStripMenuItem
			});
			((ToolStripItem)sortToolStripMenuItem).set_Image((Image)Resources.Sort);
			((ToolStripItem)sortToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)sortToolStripMenuItem).set_Name("sortToolStripMenuItem");
			((ToolStripItem)sortToolStripMenuItem).set_Size(new Size(77, 20));
			((ToolStripItem)sortToolStripMenuItem).set_Text("Sort...    ");
			((ToolStripItem)byAsteroidNameToolStripMenuItem).set_Name("byAsteroidNameToolStripMenuItem");
			((ToolStripItem)byAsteroidNameToolStripMenuItem).set_Size(new Size(228, 22));
			((ToolStripItem)byAsteroidNameToolStripMenuItem).set_Text("by Asteroid number");
			((ToolStripItem)byAsteroidNameToolStripMenuItem).add_Click((EventHandler)byAsteroidNameToolStripMenuItem_Click);
			((ToolStripItem)byAsteroidNumberToolStripMenuItem).set_Name("byAsteroidNumberToolStripMenuItem");
			((ToolStripItem)byAsteroidNumberToolStripMenuItem).set_Size(new Size(228, 22));
			((ToolStripItem)byAsteroidNumberToolStripMenuItem).set_Text("by Asteroid name");
			((ToolStripItem)byAsteroidNumberToolStripMenuItem).add_Click((EventHandler)byAsteroidNumberToolStripMenuItem_Click);
			((ToolStripItem)byDateToolStripMenuItem).set_Name("byDateToolStripMenuItem");
			((ToolStripItem)byDateToolStripMenuItem).set_Size(new Size(228, 22));
			((ToolStripItem)byDateToolStripMenuItem).set_Text("by Date of Event");
			((ToolStripItem)byDateToolStripMenuItem).add_Click((EventHandler)byDateToolStripMenuItem_Click);
			((ToolStripItem)byDateOfConjunctionToolStripMenuItem).set_Name("byDateOfConjunctionToolStripMenuItem");
			((ToolStripItem)byDateOfConjunctionToolStripMenuItem).set_Size(new Size(228, 22));
			((ToolStripItem)byDateOfConjunctionToolStripMenuItem).set_Text("by Date of Conjunction");
			((ToolStripItem)byDateOfConjunctionToolStripMenuItem).add_Click((EventHandler)byDateOfConjunctionToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator1).set_Name("toolStripSeparator1");
			((ToolStripItem)toolStripSeparator1).set_Size(new Size(225, 6));
			((ToolStripItem)byRAOffsetToolStripMenuItem).set_Name("byRAOffsetToolStripMenuItem");
			((ToolStripItem)byRAOffsetToolStripMenuItem).set_Size(new Size(228, 22));
			((ToolStripItem)byRAOffsetToolStripMenuItem).set_Text("by RA offset");
			((ToolStripItem)byRAOffsetToolStripMenuItem).add_Click((EventHandler)byRAOffsetToolStripMenuItem_Click);
			((ToolStripItem)byDecOffsetToolStripMenuItem).set_Name("byDecOffsetToolStripMenuItem");
			((ToolStripItem)byDecOffsetToolStripMenuItem).set_Size(new Size(228, 22));
			((ToolStripItem)byDecOffsetToolStripMenuItem).set_Text("by Dec offset");
			((ToolStripItem)byDecOffsetToolStripMenuItem).add_Click((EventHandler)byDecOffsetToolStripMenuItem_Click);
			((ToolStripItem)bySeparationToolStripMenuItem).set_Name("bySeparationToolStripMenuItem");
			((ToolStripItem)bySeparationToolStripMenuItem).set_Size(new Size(228, 22));
			((ToolStripItem)bySeparationToolStripMenuItem).set_Text("by Separation at event");
			((ToolStripItem)bySeparationToolStripMenuItem).add_Click((EventHandler)bySeparationToolStripMenuItem_Click);
			((ToolStripItem)bySeparationAtConjunctionToolStripMenuItem).set_Name("bySeparationAtConjunctionToolStripMenuItem");
			((ToolStripItem)bySeparationAtConjunctionToolStripMenuItem).set_Size(new Size(228, 22));
			((ToolStripItem)bySeparationAtConjunctionToolStripMenuItem).set_Text("by Separation at Conjunction");
			((ToolStripItem)bySeparationAtConjunctionToolStripMenuItem).add_Click((EventHandler)bySeparationAtConjunctionToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator2).set_Name("toolStripSeparator2");
			((ToolStripItem)toolStripSeparator2).set_Size(new Size(225, 6));
			((ToolStripItem)byUncertaintyInSeparationToolStripMenuItem).set_Name("byUncertaintyInSeparationToolStripMenuItem");
			((ToolStripItem)byUncertaintyInSeparationToolStripMenuItem).set_Size(new Size(228, 22));
			((ToolStripItem)byUncertaintyInSeparationToolStripMenuItem).set_Text("by Uncertainty in Separation");
			((ToolStripItem)byUncertaintyInSeparationToolStripMenuItem).add_Click((EventHandler)byUncertaintyInSeparationToolStripMenuItem_Click);
			((ToolStripItem)byPositionSourceToolStripMenuItem).set_Name("byPositionSourceToolStripMenuItem");
			((ToolStripItem)byPositionSourceToolStripMenuItem).set_Size(new Size(228, 22));
			((ToolStripItem)byPositionSourceToolStripMenuItem).set_Text("by Source of star positiuon");
			((ToolStripItem)byPositionSourceToolStripMenuItem).add_Click((EventHandler)byPositionSourceToolStripMenuItem_Click);
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
			((Control)lstPositions).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstPositions).set_FormattingEnabled(true);
			lstPositions.set_ItemHeight(14);
			((Control)lstPositions).set_Location(new Point(6, 138));
			((Control)lstPositions).set_Name("lstPositions");
			lstPositions.set_SelectionMode((SelectionMode)3);
			((Control)lstPositions).set_Size(new Size(929, 340));
			((Control)lstPositions).set_TabIndex(1);
			((Control)lblPosition).set_AutoSize(true);
			((Control)lblPosition).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblPosition).set_Location(new Point(7, 112));
			((Control)lblPosition).set_Name("lblPosition");
			((Control)lblPosition).set_Size(new Size(819, 14));
			((Control)lblPosition).set_TabIndex(2);
			((Control)lblPosition).set_Text("     # Asteroid       Year  M   Day         RA offset (\")     Dec offset (\")    h  m   s        o  '   \"     Cat   N");
			((Control)optAtEvent).set_AutoSize(true);
			((Control)optAtEvent).set_Location(new Point(128, 11));
			((Control)optAtEvent).set_Name("optAtEvent");
			((Control)optAtEvent).set_Size(new Size(126, 17));
			((Control)optAtEvent).set_TabIndex(3);
			((Control)optAtEvent).set_Text("Position at event time");
			((ButtonBase)optAtEvent).set_UseVisualStyleBackColor(true);
			optAtEvent.add_CheckedChanged((EventHandler)optAtEvent_CheckedChanged);
			((Control)optConjunction).set_AutoSize(true);
			((Control)optConjunction).set_Location(new Point(258, 11));
			((Control)optConjunction).set_Name("optConjunction");
			((Control)optConjunction).set_Size(new Size(132, 17));
			((Control)optConjunction).set_TabIndex(4);
			((Control)optConjunction).set_Text("Position at conjunction");
			((ButtonBase)optConjunction).set_UseVisualStyleBackColor(true);
			optConjunction.add_CheckedChanged((EventHandler)optConjunction_CheckedChanged);
			panel1.set_BorderStyle((BorderStyle)1);
			((Control)panel1).get_Controls().Add((Control)(object)optXYZ);
			((Control)panel1).get_Controls().Add((Control)(object)optConjunction);
			((Control)panel1).get_Controls().Add((Control)(object)optAtEvent);
			((Control)panel1).set_Location(new Point(20, 30));
			((Control)panel1).set_Name("panel1");
			((Control)panel1).set_Size(new Size(394, 40));
			((Control)panel1).set_TabIndex(5);
			((Control)optXYZ).set_AutoSize(true);
			optXYZ.set_Checked(true);
			((Control)optXYZ).set_Location(new Point(8, 11));
			((Control)optXYZ).set_Name("optXYZ");
			((Control)optXYZ).set_Size(new Size(116, 17));
			((Control)optXYZ).set_TabIndex(5);
			optXYZ.set_TabStop(true);
			((Control)optXYZ).set_Text("(x,y,z) at event time");
			((ButtonBase)optXYZ).set_UseVisualStyleBackColor(true);
			optXYZ.add_CheckedChanged((EventHandler)optXYZ_CheckedChanged);
			((Control)cmdRegenerate).set_Location(new Point(858, 32));
			((Control)cmdRegenerate).set_Name("cmdRegenerate");
			((Control)cmdRegenerate).set_Size(new Size(65, 36));
			((Control)cmdRegenerate).set_TabIndex(7);
			((Control)cmdRegenerate).set_Text("Read\r\nastrometry");
			((ButtonBase)cmdRegenerate).set_UseVisualStyleBackColor(true);
			((Control)cmdRegenerate).set_Visible(false);
			((Control)cmdRegenerate).add_Click((EventHandler)cmdRegenerate_Click);
			((Control)lblCoord).set_AutoSize(true);
			((Control)lblCoord).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblCoord).set_Location(new Point(7, 98));
			((Control)lblCoord).set_Name("lblCoord");
			((Control)lblCoord).set_Size(new Size(14, 14));
			((Control)lblCoord).set_TabIndex(8);
			((Control)lblCoord).set_Text("-");
			((Control)lblMainHeader).set_AutoSize(true);
			((Control)lblMainHeader).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblMainHeader).set_Location(new Point(7, 84));
			((Control)lblMainHeader).set_Name("lblMainHeader");
			((Control)lblMainHeader).set_Size(new Size(14, 14));
			((Control)lblMainHeader).set_TabIndex(9);
			((Control)lblMainHeader).set_Text("-");
			chkByYear.set_AutoCheck(false);
			((Control)chkByYear).set_AutoSize(true);
			((Control)chkByYear).set_Location(new Point(676, 35));
			((Control)chkByYear).set_Name("chkByYear");
			((Control)chkByYear).set_Size(new Size(80, 30));
			((Control)chkByYear).set_TabIndex(10);
			((Control)chkByYear).set_Text("List events \r\nfor a year");
			((ButtonBase)chkByYear).set_UseVisualStyleBackColor(true);
			((Control)chkByYear).add_MouseClick(new MouseEventHandler(chkByYear_MouseClick));
			((ListControl)cmbYear).set_FormattingEnabled(true);
			cmbYear.get_Items().AddRange(new object[1] { "Before 2000" });
			((Control)cmbYear).set_Location(new Point(754, 40));
			((Control)cmbYear).set_Name("cmbYear");
			((Control)cmbYear).set_Size(new Size(82, 21));
			((Control)cmbYear).set_TabIndex(11);
			cmbYear.add_SelectedIndexChanged((EventHandler)cmbYear_SelectedIndexChanged);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(769, 26));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(29, 13));
			((Control)label1).set_TabIndex(12);
			((Control)label1).set_Text("Year");
			chkPlanetsOnly.set_AutoCheck(false);
			((Control)chkPlanetsOnly).set_AutoSize(true);
			((Control)chkPlanetsOnly).set_Location(new Point(568, 35));
			((Control)chkPlanetsOnly).set_Name("chkPlanetsOnly");
			((Control)chkPlanetsOnly).set_Size(new Size(91, 30));
			((Control)chkPlanetsOnly).set_TabIndex(13);
			((Control)chkPlanetsOnly).set_Text("Only list \r\nPlanet events");
			((ButtonBase)chkPlanetsOnly).set_UseVisualStyleBackColor(true);
			((Control)chkPlanetsOnly).add_MouseClick(new MouseEventHandler(chkPlanetsOnly_MouseClick));
			chkExcludeStarsNoPM.set_AutoCheck(false);
			((Control)chkExcludeStarsNoPM).set_AutoSize(true);
			((Control)chkExcludeStarsNoPM).set_Location(new Point(437, 35));
			((Control)chkExcludeStarsNoPM).set_Name("chkExcludeStarsNoPM");
			((Control)chkExcludeStarsNoPM).set_Size(new Size(114, 30));
			((Control)chkExcludeStarsNoPM).set_TabIndex(14);
			((Control)chkExcludeStarsNoPM).set_Text("Exclude stars with \r\nno Proper Motion");
			((ButtonBase)chkExcludeStarsNoPM).set_UseVisualStyleBackColor(true);
			((Control)chkExcludeStarsNoPM).add_MouseClick(new MouseEventHandler(chkExcludeStarsNoPM_MouseClick));
			((Control)lblCount).set_AutoSize(true);
			((Control)lblCount).set_ForeColor(Color.Maroon);
			((Control)lblCount).set_Location(new Point(17, 74));
			((Control)lblCount).set_Name("lblCount");
			((Control)lblCount).set_Size(new Size(111, 13));
			((Control)lblCount).set_TabIndex(15);
			((Control)lblCount).set_Text("0 astrometric positions");
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(940, 482));
			((Control)this).get_Controls().Add((Control)(object)lblCount);
			((Control)this).get_Controls().Add((Control)(object)chkExcludeStarsNoPM);
			((Control)this).get_Controls().Add((Control)(object)chkPlanetsOnly);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)cmbYear);
			((Control)this).get_Controls().Add((Control)(object)chkByYear);
			((Control)this).get_Controls().Add((Control)(object)lblMainHeader);
			((Control)this).get_Controls().Add((Control)(object)cmdRegenerate);
			((Control)this).get_Controls().Add((Control)(object)panel1);
			((Control)this).get_Controls().Add((Control)(object)lblPosition);
			((Control)this).get_Controls().Add((Control)(object)lstPositions);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_Controls().Add((Control)(object)lblCoord);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationAsterShowPositions", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_Location(Settings.Default.LocationAsterShowPositions);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_Name("DisplayPositions");
			((Control)this).set_Text("Display astrometric positions");
			((Form)this).add_Load((EventHandler)DisplayPositions_Load);
			((Control)this).add_Resize((EventHandler)DisplayPositions_Resize_1);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)panel1).ResumeLayout(false);
			((Control)panel1).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
