using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Asteroid_Observations
{
	public class DisplayEventsByAnObserver : Form
	{
		private readonly string AppPath;

		internal static List<SiteLocations> Locations;

		internal static List<SiteLocations> UniqueLocations;

		private SiteLocations Locn;

		private IContainer components;

		private ListBox lstEvents;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem withListToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem printPreviewToolStripMenuItem;

		private ToolStripMenuItem printToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private Button cmdSearch;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator1;

		private ToolStripMenuItem plotSitesInGoogleEarthToolStripMenuItem;

		private ToolStripMenuItem copyWithNoToolStripMenuItem;

		private ToolStripMenuItem saveWithNoSymbolsToolStripMenuItem;

		private Label label2;

		private Label label3;

		private RadioButton optNTP;

		private RadioButton optPhone;

		private RadioButton optElectronic;

		private Label label1;

		private Label label4;

		private Panel panel1;

		private Button cmdPlotInGE;

		private ToolStripMenuItem copySelectedToolStripMenuItem;

		private ToolStripMenuItem copySelectedNoSymbolsToolStripMenuItem;

		internal RadioButton optName;

		internal TextBox txtSearch;

		private Label label5;

		public DisplayEventsByAnObserver()
		{
			InitializeComponent();
			AppPath = Utilities.AppPath;
		}

		private void copyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(CollectEvents(NoPlusMinus: false, SelectedLinesOnly: false));
		}

		private void copySelectedToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(CollectEvents(NoPlusMinus: false, SelectedLinesOnly: true));
		}

		private void copySelectedNoSymbolsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(CollectEvents(NoPlusMinus: true, SelectedLinesOnly: true));
		}

		private void copyWithNoToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(CollectEvents(NoPlusMinus: true, SelectedLinesOnly: false));
		}

		private void printPreviewToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.PrintPreviewText(CollectEvents(NoPlusMinus: false, SelectedLinesOnly: false));
		}

		private void printToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.PrintText(CollectEvents(NoPlusMinus: false, SelectedLinesOnly: false));
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings.Default.Save_AsteroidResults = Output.SavePredictionText(CollectEvents(NoPlusMinus: false, SelectedLinesOnly: false), "Events by " + ((Control)txtSearch).get_Text().Trim(), Settings.Default.Save_AsteroidResults);
		}

		private void saveWithNoSymbolsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings.Default.Save_AsteroidResults = Output.SavePredictionText(CollectEvents(NoPlusMinus: true, SelectedLinesOnly: false), "Events by " + ((Control)txtSearch).get_Text().Trim(), Settings.Default.Save_AsteroidResults);
		}

		private string CollectEvents(bool NoPlusMinus, bool SelectedLinesOnly)
		{
			StringBuilder stringBuilder = new StringBuilder();
			int count = lstEvents.get_Items().get_Count();
			if (SelectedLinesOnly)
			{
				for (int i = 0; i < lstEvents.get_SelectedItems().get_Count(); i++)
				{
					string value = lstEvents.get_SelectedItems().get_Item(i).ToString()!.Replace("±", " ");
					stringBuilder.AppendLine(value);
				}
			}
			else
			{
				for (int j = 0; j < count; j++)
				{
					string value2 = lstEvents.get_Items().get_Item(j).ToString()!.Replace("±", " ");
					stringBuilder.AppendLine(value2);
				}
			}
			return stringBuilder.ToString();
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private void TxtSearch_KeyDown(object sender, KeyEventArgs e)
		{
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_0008: Invalid comparison between Unknown and I4
			if ((int)e.get_KeyCode() == 13)
			{
				FindEvents();
			}
		}

		private void cmdSearch_Click(object sender, EventArgs e)
		{
			FindEvents();
		}

		private void FindEvents()
		{
			//IL_0033: Unknown result type (might be due to invalid IL or missing references)
			if (optName.get_Checked() & (((Control)txtSearch).get_Text().Trim().Length == 0))
			{
				MessageBox.Show("For a Name search, a name must be specified", "No search name", (MessageBoxButtons)0, (MessageBoxIcon)16);
				return;
			}
			Locations = new List<SiteLocations>();
			List<UnistellarObservers> list = new List<UnistellarObservers>();
			List<UnistellarObservers> list2 = new List<UnistellarObservers>();
			string value = ((Control)txtSearch).get_Text().ToUpper();
			int num = 0;
			int num2 = 0;
			string text = "";
			string text2 = "";
			bool flag = true;
			bool flag2 = true;
			bool flag3 = false;
			Cursor.set_Current(Cursors.get_WaitCursor());
			lstEvents.get_Items().Clear();
			lstEvents.get_Items().Add((object)" Counts         Asteroid             Date             Star");
			lstEvents.get_Items().Add((object)" Pos  Stn ref  Name and location                               Longitude    Latitude    Alt.");
			lstEvents.get_Items().Add((object)"============================================================================================");
			if (Data_and_Plots.Historical_AllEvents.OccEvents.Count == 0)
			{
				Data_and_Plots.Historical_AllEvents.ReadObservationsFile(HistoricalFile: true, "");
			}
			for (int i = 0; i < Data_and_Plots.Historical_AllEvents.OccEvents.Count; i++)
			{
				for (int j = 0; j < Data_and_Plots.Historical_AllEvents.OccEvents[i].Lines.Count; j++)
				{
					if (!Data_and_Plots.Historical_AllEvents.OccEvents[i].Lines[j].Contains(Tags.TagEnd[21]))
					{
						continue;
					}
					string[] array = Data_and_Plots.Historical_AllEvents.OccEvents[i].Lines[j].Trim().Replace(Tags.TagStart[21], "").Replace(Tags.TagEnd[21], "")
						.Replace("&amp;", " ")
						.Split(new char[1] { '|' });
					string[] array2 = Data_and_Plots.Historical_AllEvents.OccEvents[i].Lines[j + 2].Trim().Replace(Tags.TagStart[23], "").Replace(Tags.TagEnd[23], "")
						.Replace("&amp;", " ")
						.Split(new char[1] { '|' });
					string[] array3 = Data_and_Plots.Historical_AllEvents.OccEvents[i].Lines[j + 3].Trim().Replace(Tags.TagStart[24], "").Replace(Tags.TagEnd[24], "")
						.Replace("&amp;", " ")
						.Split(new char[1] { '|' });
					flag3 = false;
					if (optNTP.get_Checked() & ((array[13] == "b") & (array[11] != "8")))
					{
						flag3 = true;
					}
					else if (optPhone.get_Checked() & ((array[13] == "c") & (array[11] != "8")))
					{
						flag3 = true;
					}
					else if (optElectronic.get_Checked() & (array[11] == "8"))
					{
						flag3 = true;
					}
					else if (optName.get_Checked() && (array[1].ToUpper().Contains(value) | array[2].ToUpper().Contains(value)))
					{
						flag3 = true;
					}
					if (!flag3)
					{
						continue;
					}
					num++;
					if ("DdGg".Contains(array2[1]) | "RrBb".Contains(array3[1]))
					{
						if (flag2 | !optName.get_Checked())
						{
							num2++;
							text = string.Format("{0,4:F0} ", num2);
							flag2 = false;
						}
						else
						{
							text = "   + ";
						}
					}
					else
					{
						text = "   - ";
					}
					if (flag)
					{
						lstEvents.get_Items().Add((object)("".PadRight(13) + Data_and_Plots.Historical_AllEvents.GetEventID(i, IncludeMagnitude: false, Kepler2: false).Replace("\r\n", "")));
					}
					text2 = text + string.Format("{0,4:F0} ", num) + array[0].PadLeft(3) + "  ";
					string text3;
					if (optName.get_Checked())
					{
						if (array[1].ToUpper().Contains(value))
						{
							text3 = array[1];
							if (array[2].ToUpper().Contains(value))
							{
								text3 = text3 + " + " + array[2];
							}
						}
						else
						{
							text3 = array[2];
						}
					}
					else
					{
						text3 = array[1];
						if (array[2].ToUpper().Trim().Length > 0)
						{
							text3 = text3 + " + " + array[2].Trim();
						}
					}
					if (array[4].Trim().Length > 0)
					{
						text3 = text3 + ", at " + array[4].Trim();
					}
					if (array[5].Trim().Length > 0)
					{
						text3 = text3 + ", in " + array[5].Trim();
					}
					text2 += text3.PadRight(46).Substring(0, 45);
					text2 = text2.PadRight(60) + array[6] + "  " + array[7] + array[8].PadLeft(6);
					lstEvents.get_Items().Add((object)text2);
					Locn = new SiteLocations();
					Locn.Site = num.ToString();
					int.TryParse(array[6].Substring(0, 4).Replace("-", "").Replace("+", ""), out var result);
					int.TryParse(array[6].Substring(5, 2), out var result2);
					double.TryParse(array[6].Substring(8), out var result3);
					Locn.Longitude = (double)result + (double)result2 / 60.0 + result3 / 3600.0;
					if (array[6].Substring(0, 4).Contains("-"))
					{
						Locn.Longitude = 0.0 - Locn.Longitude;
					}
					int.TryParse(array[7].Substring(0, 3).Replace("-", "").Replace("+", ""), out var result4);
					int.TryParse(array[7].Substring(4, 2), out var result5);
					double.TryParse(array[7].Substring(7), out var result6);
					Locn.Latitude = (double)result4 + (double)result5 / 60.0 + result6 / 3600.0;
					if (array[7].Substring(0, 3).Contains("-"))
					{
						Locn.Latitude = 0.0 - Locn.Latitude;
					}
					Locations.Add(Locn);
					if (optElectronic.get_Checked())
					{
						UnistellarObservers unistellarObservers = new UnistellarObservers();
						unistellarObservers.Positive = "DdGg".Contains(array2[1]) || "RrBb".Contains(array3[1]);
						unistellarObservers.Name = array[1];
						list.Add(unistellarObservers);
					}
					flag = false;
				}
				if (!flag)
				{
					lstEvents.get_Items().Add((object)"");
				}
				flag = true;
				flag2 = true;
			}
			Locations.Sort();
			UniqueLocations = new List<SiteLocations>();
			double num3 = 180.0 / Math.PI;
			double Distance = 0.0;
			double num4 = 0.00044991541590181045 / num3;
			SiteLocations siteLocations = new SiteLocations();
			for (int k = 0; k < Locations.Count; k++)
			{
				if (UniqueLocations.Count == 0)
				{
					siteLocations = new SiteLocations();
					siteLocations.Longitude = Locations[k].Longitude;
					siteLocations.Latitude = Locations[k].Latitude;
					siteLocations.Site = Locations[k].Site + ", ";
					int num7 = (siteLocations.SiteCount = (Locations[k].SiteCount = 1));
					UniqueLocations.Add(siteLocations);
					continue;
				}
				double longitude = Locations[k].Longitude;
				double latitude = Locations[k].Latitude;
				for (int l = 0; l < UniqueLocations.Count; l++)
				{
					Utilities.Distance(longitude / num3, latitude / num3, UniqueLocations[l].Longitude / num3, UniqueLocations[l].Latitude / num3, out Distance, out var _);
					if (Distance < num4)
					{
						SiteLocations siteLocations2 = UniqueLocations[l];
						siteLocations2.Site = siteLocations2.Site + Locations[k].Site + ", ";
						UniqueLocations[l].SiteCount = UniqueLocations[l].SiteCount + 1;
						break;
					}
				}
				if (!(Distance < num4))
				{
					siteLocations = new SiteLocations();
					siteLocations.Longitude = Locations[k].Longitude;
					siteLocations.Latitude = Locations[k].Latitude;
					siteLocations.Site = Locations[k].Site + ", ";
					int num7 = (siteLocations.SiteCount = (Locations[k].SiteCount = 1));
					UniqueLocations.Add(siteLocations);
				}
			}
			lstEvents.get_Items().Add((object)"".PadRight(40, '='));
			lstEvents.get_Items().Add((object)"");
			lstEvents.get_Items().Add((object)(UniqueLocations.Count + " Unique locations  (Separation greater than 50 meters)"));
			lstEvents.get_Items().Add((object)"");
			lstEvents.get_Items().Add((object)"  Longitude    Latitude");
			for (int m = 0; m < UniqueLocations.Count; m++)
			{
				lstEvents.get_Items().Add((object)(Utilities.DEGtoDMS(UniqueLocations[m].Longitude, 4, 1, MinutesOnly: false, IncludeLeadingZeros: false, IncludePlusSymbol: true) + "  " + Utilities.DEGtoDMS(UniqueLocations[m].Latitude, 3, 1, MinutesOnly: false, IncludeLeadingZeros: false, IncludePlusSymbol: true) + "   Sites [" + UniqueLocations[m].SiteCount.ToString().PadLeft(3) + "] " + UniqueLocations[m].Site));
			}
			if (optElectronic.get_Checked())
			{
				list2 = new List<UnistellarObservers>();
				UnistellarObservers.SortField = 0;
				list.Sort();
				int num10 = 0;
				int num11 = 0;
				int num12 = 0;
				int num13 = 0;
				int num14 = 0;
				UnistellarObservers unistellarObservers;
				for (int n = 0; n < list.Count; n++)
				{
					if (n == 0)
					{
						num10 = 1;
						num11 = 0;
						if (list[n].Positive)
						{
							num11 = 1;
						}
						num14 = 1;
						continue;
					}
					if (list[n].Name == list[n - 1].Name)
					{
						num10++;
						if (list[n].Positive)
						{
							num11++;
						}
						continue;
					}
					unistellarObservers = new UnistellarObservers();
					unistellarObservers.StationCount = num10;
					unistellarObservers.PositiveCount = num11;
					unistellarObservers.Name = list[n - 1].Name;
					list2.Add(unistellarObservers);
					num12 += num10;
					num13 += num11;
					num10 = 1;
					num11 = 0;
					num14++;
					if (list[n].Positive)
					{
						num11 = 1;
					}
				}
				unistellarObservers = new UnistellarObservers();
				unistellarObservers.StationCount = num10;
				unistellarObservers.PositiveCount = num11;
				unistellarObservers.Name = list[list.Count - 1].Name;
				list2.Add(unistellarObservers);
				lstEvents.get_Items().Add((object)"");
				lstEvents.get_Items().Add((object)"".PadRight(40, '='));
				lstEvents.get_Items().Add((object)"");
				lstEvents.get_Items().Add((object)"Summaries of Electronic telescope observations");
				lstEvents.get_Items().Add((object)"   Observations from an event are only included if someone (including IOTA observers)");
				lstEvents.get_Items().Add((object)"   observed an occultation in that event");
				lstEvents.get_Items().Add((object)"");
				lstEvents.get_Items().Add((object)"PART 1   observations by Observer name");
				lstEvents.get_Items().Add((object)"");
				lstEvents.get_Items().Add((object)"Events  +'ve  Observer");
				UnistellarObservers.SortField = 0;
				list2.Sort();
				for (int num15 = 0; num15 < list2.Count; num15++)
				{
					lstEvents.get_Items().Add((object)list2[num15].ToString());
				}
				lstEvents.get_Items().Add((object)"");
				lstEvents.get_Items().Add((object)"".PadRight(40, '-'));
				lstEvents.get_Items().Add((object)"PART 2   observers with 2 or more POSITIVE observations");
				lstEvents.get_Items().Add((object)"");
				lstEvents.get_Items().Add((object)"Events  +'ve  Observer");
				UnistellarObservers.SortField = 2;
				list2.Sort();
				for (int num16 = 0; num16 < list2.Count; num16++)
				{
					if (list2[num16].PositiveCount > 1)
					{
						lstEvents.get_Items().Add((object)list2[num16].ToString());
					}
				}
				lstEvents.get_Items().Add((object)"");
				lstEvents.get_Items().Add((object)"".PadRight(40, '-'));
				lstEvents.get_Items().Add((object)"PART 3   observers with 1 POSITIVE observation");
				lstEvents.get_Items().Add((object)"");
				lstEvents.get_Items().Add((object)"Events  +'ve  Observer");
				UnistellarObservers.SortField = 2;
				list2.Sort();
				for (int num17 = 0; num17 < list2.Count; num17++)
				{
					if (list2[num17].PositiveCount == 1)
					{
						lstEvents.get_Items().Add((object)list2[num17].ToString());
					}
				}
				lstEvents.get_Items().Add((object)"");
				lstEvents.get_Items().Add((object)"".PadRight(40, '-'));
				lstEvents.get_Items().Add((object)"PART 4   observers with more than 1 observation - positive or negative");
				lstEvents.get_Items().Add((object)"");
				lstEvents.get_Items().Add((object)"Events  +'ve  Observer");
				UnistellarObservers.SortField = 1;
				list2.Sort();
				for (int num18 = 0; num18 < list2.Count; num18++)
				{
					if (list2[num18].StationCount > 1)
					{
						lstEvents.get_Items().Add((object)list2[num18].ToString());
					}
				}
				lstEvents.get_Items().Add((object)"");
				lstEvents.get_Items().Add((object)"".PadRight(40, '-'));
				lstEvents.get_Items().Add((object)"SUMMARY");
				lstEvents.get_Items().Add((object)"");
				lstEvents.get_Items().Add((object)string.Format("Total number of observers = {0,1}", num14));
				lstEvents.get_Items().Add((object)string.Format("Total number of events observed = {0,1}", num12));
				lstEvents.get_Items().Add((object)string.Format("Total number of positive observations = {0,1}", num13));
			}
			Cursor.set_Current(((Control)this).get_DefaultCursor());
		}

		private void DisplayEventsByAnObserver_Resize(object sender, EventArgs e)
		{
			if (((Control)this).get_Height() > 150)
			{
				((Control)lstEvents).set_Height(((Control)this).get_Height() - 146);
			}
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Asteroid observations by Observer");
		}

		private void DisplayEventsByAnObserver_Load(object sender, EventArgs e)
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
			optName.set_Checked(true);
			((Control)txtSearch).Focus();
			((TextBoxBase)txtSearch).set_SelectionStart(0);
		}

		private void PlotSitesInGoogleEarthToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0032: Unknown result type (might be due to invalid IL or missing references)
			string value = ((Control)txtSearch).get_Text().ToUpper();
			int num = 0;
			if (EventDetails.Observers.Count < 0)
			{
				return;
			}
			if (!Utilities.InternetIsAvailable())
			{
				MessageBox.Show("This functionality requires internet access. You are not connected to the internet", "No internet");
			}
			else
			{
				if (!GoogleEarth.Create_New_GoogleEarthKMZ_File(Utilities.AppPath + "\\Asteroid\\ObserverSites.KML", "Sites", AutoOpenFile: true, out var CreatedFile))
				{
					return;
				}
				for (int i = 0; i < Data_and_Plots.Historical_AllEvents.OccEvents.Count; i++)
				{
					for (int j = 0; j < Data_and_Plots.Historical_AllEvents.OccEvents[i].Lines.Count; j++)
					{
						if (Data_and_Plots.Historical_AllEvents.OccEvents[i].Lines[j].Contains(Tags.TagEnd[21]))
						{
							string[] array = Data_and_Plots.Historical_AllEvents.OccEvents[i].Lines[j].Trim().Replace(Tags.TagStart[21], "").Replace(Tags.TagEnd[21], "")
								.Split(new char[1] { '|' });
							if (array[1].ToUpper().Contains(value) | array[2].ToUpper().Contains(value))
							{
								num++;
								string name = num + "/" + array[0];
								double longitude = Utilities.DMStoDeg(array[6]);
								double latitude = Utilities.DMStoDeg(" " + array[7]);
								GoogleEarth.Write_PlaceMark_PinPlusName_GoogleEarthKML(name, longitude, latitude);
							}
						}
					}
				}
				CreatedFile = GoogleEarth.Close_GoogleEarthKMZ_File(CreatedFile, ConvertToKMZ: true);
				GoogleEarth.DisplayGoogleMap(CreatedFile);
			}
		}

		private void optName_MouseClick(object sender, MouseEventArgs e)
		{
			((Control)txtSearch).Focus();
		}

		private void txtSearch_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtSearch).SelectAll();
		}

		private void optNTP_MouseClick(object sender, MouseEventArgs e)
		{
			FindEvents();
		}

		private void optPhone_MouseClick(object sender, MouseEventArgs e)
		{
			FindEvents();
		}

		private void optElectronic_MouseClick(object sender, MouseEventArgs e)
		{
			FindEvents();
		}

		private void cmdPlotInGE_Click(object sender, EventArgs e)
		{
			//IL_001f: Unknown result type (might be due to invalid IL or missing references)
			if (EventDetails.Observers.Count < 0)
			{
				return;
			}
			if (!Utilities.InternetIsAvailable())
			{
				MessageBox.Show("This functionality requires internet access. You are not connected to the internet", "No internet");
				return;
			}
			string text = Utilities.ProperCase(((Control)txtSearch).get_Text().Trim().Replace(".", "")
				.Replace(" ", "_"));
			if (GoogleEarth.Create_New_GoogleEarthKMZ_File(Utilities.AppPath + "\\Asteroid\\" + UniqueLocations.Count + " sites of " + text + ".KML", "Sites for " + text, AutoOpenFile: true, out var CreatedFile))
			{
				for (int i = 0; i < UniqueLocations.Count; i++)
				{
					GoogleEarth.Write_PlaceMark_PinPlusName_GoogleEarthKML("#" + (i + 1), UniqueLocations[i].Longitude, UniqueLocations[i].Latitude, 4);
				}
				CreatedFile = GoogleEarth.Close_GoogleEarthKMZ_File(CreatedFile, ConvertToKMZ: true);
				GoogleEarth.DisplayGoogleMap(CreatedFile);
			}
		}

		private void lstEvents_MouseDown(object sender, MouseEventArgs e)
		{
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_000b: Invalid comparison between Unknown and I4
			if ((int)e.get_Button() != 2097152)
			{
				return;
			}
			try
			{
				string text = lstEvents.get_SelectedItem().ToString();
				if (text.Trim().Length == 0)
				{
					ErrorMessage(((Control)this).get_Left(), ((Control)this).get_Top());
					return;
				}
				if (text.Substring(0, 10).Trim().Length > 0)
				{
					ErrorMessage(((Control)this).get_Left(), ((Control)this).get_Top());
					return;
				}
				string asteroidNumber = text.Substring(13, 7);
				string asteroidName = text.Substring(21, 16).Trim();
				int.TryParse(text.Substring(37, 4), out var result);
				int month = 1;
				string text2 = text.Substring(42, 3);
				for (int i = 1; i < 13; i++)
				{
					if (Utilities.ShortMonths[i] == text2)
					{
						month = i;
					}
				}
				int.TryParse(text.Substring(46, 2), out var result2);
				Data_and_Plots.ShowEditor();
				Data_and_Plots.Observations_Editor.DisplayEvent_ByDate_Number(result, month, result2, asteroidNumber, asteroidName);
			}
			catch
			{
			}
		}

		private void ErrorMessage(int left, int top)
		{
			TimedMessageDisplay timedMessageDisplay = new TimedMessageDisplay();
			((Control)timedMessageDisplay).set_Text("Wrong line selected");
			((Control)timedMessageDisplay.txtMessage).set_Text("You must select the line that identifies the event");
			((Control)timedMessageDisplay).Show();
			((Control)timedMessageDisplay).set_Left(left + 10);
			((Control)timedMessageDisplay).set_Top(top + 10);
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
			//IL_0200: Unknown result type (might be due to invalid IL or missing references)
			//IL_020a: Expected O, but got Unknown
			//IL_0812: Unknown result type (might be due to invalid IL or missing references)
			//IL_081c: Expected O, but got Unknown
			//IL_0a4d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a57: Expected O, but got Unknown
			//IL_0ae3: Unknown result type (might be due to invalid IL or missing references)
			//IL_0aed: Expected O, but got Unknown
			//IL_0b7a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b84: Expected O, but got Unknown
			//IL_0c25: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c2f: Expected O, but got Unknown
			//IL_1023: Unknown result type (might be due to invalid IL or missing references)
			//IL_102d: Expected O, but got Unknown
			lstEvents = new ListBox();
			menuStrip1 = new MenuStrip();
			withListToolStripMenuItem = new ToolStripMenuItem();
			copyToolStripMenuItem = new ToolStripMenuItem();
			copySelectedToolStripMenuItem = new ToolStripMenuItem();
			copyWithNoToolStripMenuItem = new ToolStripMenuItem();
			copySelectedNoSymbolsToolStripMenuItem = new ToolStripMenuItem();
			printPreviewToolStripMenuItem = new ToolStripMenuItem();
			printToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			saveWithNoSymbolsToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator1 = new ToolStripSeparator();
			plotSitesInGoogleEarthToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			txtSearch = new TextBox();
			cmdSearch = new Button();
			label2 = new Label();
			label3 = new Label();
			optNTP = new RadioButton();
			optPhone = new RadioButton();
			optElectronic = new RadioButton();
			optName = new RadioButton();
			label1 = new Label();
			label4 = new Label();
			panel1 = new Panel();
			cmdPlotInGE = new Button();
			label5 = new Label();
			((Control)menuStrip1).SuspendLayout();
			((Control)panel1).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)lstEvents).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstEvents).set_FormattingEnabled(true);
			lstEvents.set_HorizontalScrollbar(true);
			lstEvents.set_ItemHeight(14);
			((Control)lstEvents).set_Location(new Point(5, 93));
			((Control)lstEvents).set_Name("lstEvents");
			lstEvents.set_ScrollAlwaysVisible(true);
			lstEvents.set_SelectionMode((SelectionMode)3);
			((Control)lstEvents).set_Size(new Size(694, 494));
			((Control)lstEvents).set_TabIndex(2);
			((Control)lstEvents).add_MouseDown(new MouseEventHandler(lstEvents_MouseDown));
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)withListToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(704, 24));
			((Control)menuStrip1).set_TabIndex(4);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withListToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[10]
			{
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)copySelectedToolStripMenuItem,
				(ToolStripItem)copyWithNoToolStripMenuItem,
				(ToolStripItem)copySelectedNoSymbolsToolStripMenuItem,
				(ToolStripItem)printPreviewToolStripMenuItem,
				(ToolStripItem)printToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem,
				(ToolStripItem)saveWithNoSymbolsToolStripMenuItem,
				(ToolStripItem)toolStripSeparator1,
				(ToolStripItem)plotSitesInGoogleEarthToolStripMenuItem
			});
			((ToolStripItem)withListToolStripMenuItem).set_Name("withListToolStripMenuItem");
			((ToolStripItem)withListToolStripMenuItem).set_Size(new Size(72, 20));
			((ToolStripItem)withListToolStripMenuItem).set_Text("with List...");
			((ToolStripItem)copyToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyToolStripMenuItem).set_Name("copyToolStripMenuItem");
			copyToolStripMenuItem.set_ShortcutKeys((Keys)131139);
			((ToolStripItem)copyToolStripMenuItem).set_Size(new Size(229, 22));
			((ToolStripItem)copyToolStripMenuItem).set_Text("&Copy ALL");
			((ToolStripItem)copyToolStripMenuItem).add_Click((EventHandler)copyToolStripMenuItem_Click);
			((ToolStripItem)copySelectedToolStripMenuItem).set_Name("copySelectedToolStripMenuItem");
			((ToolStripItem)copySelectedToolStripMenuItem).set_Size(new Size(229, 22));
			((ToolStripItem)copySelectedToolStripMenuItem).set_Text("Copy - Selected only");
			((ToolStripItem)copySelectedToolStripMenuItem).add_Click((EventHandler)copySelectedToolStripMenuItem_Click);
			((ToolStripItem)copyWithNoToolStripMenuItem).set_Name("copyWithNoToolStripMenuItem");
			((ToolStripItem)copyWithNoToolStripMenuItem).set_Size(new Size(229, 22));
			((ToolStripItem)copyWithNoToolStripMenuItem).set_Text("Copy ALL, with no ± symbols");
			((ToolStripItem)copyWithNoToolStripMenuItem).set_Visible(false);
			((ToolStripItem)copyWithNoToolStripMenuItem).add_Click((EventHandler)copyWithNoToolStripMenuItem_Click);
			((ToolStripItem)copySelectedNoSymbolsToolStripMenuItem).set_Name("copySelectedNoSymbolsToolStripMenuItem");
			((ToolStripItem)copySelectedNoSymbolsToolStripMenuItem).set_Size(new Size(229, 22));
			((ToolStripItem)copySelectedNoSymbolsToolStripMenuItem).set_Text("Copy Selected, no ± symbols");
			((ToolStripItem)copySelectedNoSymbolsToolStripMenuItem).set_Visible(false);
			((ToolStripItem)copySelectedNoSymbolsToolStripMenuItem).add_Click((EventHandler)copySelectedNoSymbolsToolStripMenuItem_Click);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Image((Image)Resources.PrintPreview);
			((ToolStripItem)printPreviewToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Name("printPreviewToolStripMenuItem");
			((ToolStripItem)printPreviewToolStripMenuItem).set_Size(new Size(229, 22));
			((ToolStripItem)printPreviewToolStripMenuItem).set_Text("Print preview");
			((ToolStripItem)printPreviewToolStripMenuItem).add_Click((EventHandler)printPreviewToolStripMenuItem_Click);
			((ToolStripItem)printToolStripMenuItem).set_Image((Image)Resources.Print);
			((ToolStripItem)printToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printToolStripMenuItem).set_Name("printToolStripMenuItem");
			printToolStripMenuItem.set_ShortcutKeys((Keys)131152);
			((ToolStripItem)printToolStripMenuItem).set_Size(new Size(229, 22));
			((ToolStripItem)printToolStripMenuItem).set_Text("&Print");
			((ToolStripItem)printToolStripMenuItem).add_Click((EventHandler)printToolStripMenuItem_Click);
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			saveToolStripMenuItem.set_ShortcutKeys((Keys)131155);
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(229, 22));
			((ToolStripItem)saveToolStripMenuItem).set_Text("&Save");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
			((ToolStripItem)saveWithNoSymbolsToolStripMenuItem).set_Name("saveWithNoSymbolsToolStripMenuItem");
			((ToolStripItem)saveWithNoSymbolsToolStripMenuItem).set_Size(new Size(229, 22));
			((ToolStripItem)saveWithNoSymbolsToolStripMenuItem).set_Text("Save, with no ± symbols");
			((ToolStripItem)saveWithNoSymbolsToolStripMenuItem).add_Click((EventHandler)saveWithNoSymbolsToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator1).set_Name("toolStripSeparator1");
			((ToolStripItem)toolStripSeparator1).set_Size(new Size(226, 6));
			((ToolStripItem)plotSitesInGoogleEarthToolStripMenuItem).set_Image((Image)Resources.google_earth);
			((ToolStripItem)plotSitesInGoogleEarthToolStripMenuItem).set_Name("plotSitesInGoogleEarthToolStripMenuItem");
			((ToolStripItem)plotSitesInGoogleEarthToolStripMenuItem).set_Size(new Size(229, 22));
			((ToolStripItem)plotSitesInGoogleEarthToolStripMenuItem).set_Text("Plot sites in GoogleEarth");
			((ToolStripItem)plotSitesInGoogleEarthToolStripMenuItem).add_Click((EventHandler)PlotSitesInGoogleEarthToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(69, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("&Help   ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(54, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("E&xit");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((Control)txtSearch).set_Location(new Point(132, 1));
			((Control)txtSearch).set_Name("txtSearch");
			((Control)txtSearch).set_Size(new Size(106, 20));
			((Control)txtSearch).set_TabIndex(0);
			((Control)txtSearch).add_Enter((EventHandler)txtSearch_Enter);
			((Control)txtSearch).add_KeyDown(new KeyEventHandler(TxtSearch_KeyDown));
			((Control)cmdSearch).set_BackColor(Color.Aquamarine);
			((Control)cmdSearch).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdSearch).set_Location(new Point(446, 22));
			((Control)cmdSearch).set_Name("cmdSearch");
			((Control)cmdSearch).set_Size(new Size(85, 35));
			((Control)cmdSearch).set_TabIndex(1);
			((Control)cmdSearch).set_Text("List events");
			((ButtonBase)cmdSearch).set_UseVisualStyleBackColor(false);
			((Control)cmdSearch).add_Click((EventHandler)cmdSearch_Click);
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label2).set_Location(new Point(7, 65));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(413, 14));
			((Control)label2).set_TabIndex(5);
			((Control)label2).set_Text(" Counts         Asteroid             Date             Star");
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label3).set_Location(new Point(6, 77));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(651, 14));
			((Control)label3).set_TabIndex(6);
			((Control)label3).set_Text(" Pos  Stn ref  Name and location                               Longitude    Latitude    Alt.");
			((Control)optNTP).set_AutoSize(true);
			((Control)optNTP).set_ForeColor(Color.DarkBlue);
			((Control)optNTP).set_Location(new Point(132, 22));
			((Control)optNTP).set_Name("optNTP");
			((Control)optNTP).set_Size(new Size(47, 17));
			((Control)optNTP).set_TabIndex(7);
			((Control)optNTP).set_Text("NTP");
			((ButtonBase)optNTP).set_UseVisualStyleBackColor(true);
			((Control)optNTP).add_MouseClick(new MouseEventHandler(optNTP_MouseClick));
			((Control)optPhone).set_AutoSize(true);
			((Control)optPhone).set_ForeColor(Color.DarkBlue);
			((Control)optPhone).set_Location(new Point(190, 22));
			((Control)optPhone).set_Name("optPhone");
			((Control)optPhone).set_Size(new Size(56, 17));
			((Control)optPhone).set_TabIndex(8);
			((Control)optPhone).set_Text("Phone");
			((ButtonBase)optPhone).set_UseVisualStyleBackColor(true);
			((Control)optPhone).add_MouseClick(new MouseEventHandler(optPhone_MouseClick));
			((Control)optElectronic).set_AutoSize(true);
			((Control)optElectronic).set_ForeColor(Color.DarkBlue);
			((Control)optElectronic).set_Location(new Point(257, 22));
			((Control)optElectronic).set_Name("optElectronic");
			((Control)optElectronic).set_Size(new Size(72, 17));
			((Control)optElectronic).set_TabIndex(9);
			((Control)optElectronic).set_Text("Electronic");
			((ButtonBase)optElectronic).set_UseVisualStyleBackColor(true);
			((Control)optElectronic).add_MouseClick(new MouseEventHandler(optElectronic_MouseClick));
			((Control)optName).set_AutoSize(true);
			optName.set_Checked(true);
			((Control)optName).set_ForeColor(Color.Maroon);
			((Control)optName).set_Location(new Point(26, 3));
			((Control)optName).set_Name("optName");
			((Control)optName).set_Size(new Size(104, 17));
			((Control)optName).set_TabIndex(10);
			optName.set_TabStop(true);
			((Control)optName).set_Text("Observer's name");
			((ButtonBase)optName).set_UseVisualStyleBackColor(true);
			((Control)optName).add_MouseClick(new MouseEventHandler(optName_MouseClick));
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_ForeColor(Color.DarkBlue);
			((Control)label1).set_Location(new Point(30, 24));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(100, 13));
			((Control)label1).set_TabIndex(11);
			((Control)label1).set_Text("Specific parameters");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label4).set_ForeColor(Color.Green);
			((Control)label4).set_Location(new Point(20, 31));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(85, 26));
			((Control)label4).set_TabIndex(12);
			((Control)label4).set_Text("Select search\r\nparameters");
			label4.set_TextAlign(ContentAlignment.MiddleCenter);
			panel1.set_BorderStyle((BorderStyle)2);
			((Control)panel1).get_Controls().Add((Control)(object)label1);
			((Control)panel1).get_Controls().Add((Control)(object)optName);
			((Control)panel1).get_Controls().Add((Control)(object)optElectronic);
			((Control)panel1).get_Controls().Add((Control)(object)optPhone);
			((Control)panel1).get_Controls().Add((Control)(object)optNTP);
			((Control)panel1).get_Controls().Add((Control)(object)txtSearch);
			((Control)panel1).set_Location(new Point(107, 22));
			((Control)panel1).set_Name("panel1");
			((Control)panel1).set_Size(new Size(333, 44));
			((Control)panel1).set_TabIndex(13);
			((Control)cmdPlotInGE).set_BackColor(Color.LightBlue);
			((Control)cmdPlotInGE).set_Location(new Point(556, 22));
			((Control)cmdPlotInGE).set_Name("cmdPlotInGE");
			((Control)cmdPlotInGE).set_Size(new Size(115, 35));
			((Control)cmdPlotInGE).set_TabIndex(14);
			((Control)cmdPlotInGE).set_Text("Plot unique locations in GoogleEarth");
			((ButtonBase)cmdPlotInGE).set_UseVisualStyleBackColor(false);
			((Control)cmdPlotInGE).add_Click((EventHandler)cmdPlotInGE_Click);
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_BackColor(Color.MistyRose);
			((Control)label5).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label5).set_ForeColor(Color.DarkBlue);
			((Control)label5).set_Location(new Point(442, 61));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(258, 13));
			((Control)label5).set_TabIndex(15);
			((Control)label5).set_Text("Right-click on event line to display the event");
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(704, 597));
			((Control)this).get_Controls().Add((Control)(object)label5);
			((Control)this).get_Controls().Add((Control)(object)cmdPlotInGE);
			((Control)this).get_Controls().Add((Control)(object)panel1);
			((Control)this).get_Controls().Add((Control)(object)label4);
			((Control)this).get_Controls().Add((Control)(object)label3);
			((Control)this).get_Controls().Add((Control)(object)label2);
			((Control)this).get_Controls().Add((Control)(object)cmdSearch);
			((Control)this).get_Controls().Add((Control)(object)lstEvents);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationAsterShowObservers", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_Location(Settings.Default.LocationAsterShowObservers);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_MaximumSize(new Size(720, 4000));
			((Control)this).set_MinimumSize(new Size(720, 600));
			((Control)this).set_Name("DisplayEventsByAnObserver");
			((Control)this).set_Text("Display events observed by an observer");
			((Form)this).add_Load((EventHandler)DisplayEventsByAnObserver_Load);
			((Control)this).add_Resize((EventHandler)DisplayEventsByAnObserver_Resize);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)panel1).ResumeLayout(false);
			((Control)panel1).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
