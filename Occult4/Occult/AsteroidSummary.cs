using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Text;
using System.Windows.Forms;
using Occult.Asteroids;
using Occult.Properties;

namespace Occult
{
	public class AsteroidSummary : Form
	{
		private readonly string AppPath;

		public static string SummaryHeader;

		public static bool Global;

		public static bool Abbreviated = false;

		public static bool DistInArcSec = true;

		internal bool PlanetTimes;

		private bool AutoGenerateCancelFlag;

		private bool CheckBoxList;

		private bool UseCombinedMag;

		private bool UpdatingChecks;

		internal static List<Future_dat> Future = new List<Future_dat>();

		internal Future_dat FutureID;

		private static bool[] MoreThanOnce;

		private static int Max;

		private static int LastListItem;

		private static bool ListStarsMoreThanOnce = false;

		private const string Header1a = "     Date       U.T.    Diameter   Durn  ";

		private const string Header1b = "  Mag-Drop Elon  %      Star         d     Planet             ";

		private const string Header1Extra = "Alt  Dist Sun Proba-   Moon   § ◉    R.A. (J2000)  Dec.    ";

		private const string Header2 = "   y   m  d    h   m     km   \"   sec/m   mag   V    R     o Ill     No.               No Name             ";

		private const string Header2ExtraKM = "  o   km  Alt bility Elon ill  ☾    h  m   s      o  '   \"  ";

		private const string Header2Sec = "  o    \"  Alt bility Elon ill  ☾    h  m   s      o  '   \"  ";

		private const string GHeader1Extra = "Min          Moon   § ◉    R.A. (J2000)  Dec.     ";

		private const string GHeader2Extra = " D   Error Elon ill  ☾    h  m   s      o  '   \"  ";

		private IContainer components;

		public ListBox lstSummary;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem byDateToolStripMenuItem;

		private ToolStripMenuItem byDiameterkmToolStripMenuItem;

		private ToolStripMenuItem byDiameterapparentToolStripMenuItem;

		private ToolStripMenuItem byDurationToolStripMenuItem;

		private ToolStripMenuItem byStarMagToolStripMenuItem;

		private ToolStripMenuItem byMagnitudeDropToolStripMenuItem;

		private ToolStripMenuItem byElongationToolStripMenuItem;

		private ToolStripMenuItem byNameToolStripMenuItem;

		private ToolStripMenuItem byNumberToolStripMenuItem;

		private ToolStripMenuItem byStarNumberToolStripMenuItem;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private ToolStripMenuItem withPredictionsToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem printToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripMenuItem stepthroughGraphicsToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator1;

		public ToolStripMenuItem byDistanceToolStripMenuItem;

		public ToolStripMenuItem byProbabilityToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator2;

		private ToolStripMenuItem printPreviewToolStripMenuItem;

		public ToolStripMenuItem sortToolStripMenuItem;

		public ToolStripMenuItem abbreviatedOutputToolStripMenuItem;

		public ToolStripMenuItem plotPathsOnAMapToolStripMenuItem;

		public ToolStripMenuItem createGoogleKMLFileOfEventsToolStripMenuItem;

		private ToolStripMenuItem listStarsOccultToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator3;

		private ToolStripMenuItem autoOutToolStripMenuItem;

		private Button cmdAutoOutput;

		internal ProgressBar PBarSummary;

		private CheckBox chkNewMultiFile;

		private Button cmdAutoCancel;

		internal ToolStripComboBox cmbSites;

		private ToolStripMenuItem byMagnitudeDropredToolStripMenuItem;

		public CheckedListBox lstChkSummary;

		private ToolStripMenuItem checkAll;

		private ToolStripMenuItem uncheckAll;

		private ToolStripSeparator toolStripSeparator5;

		private ToolStripMenuItem CombinedMagnitude;

		private ToolStripMenuItem byCombinedMagnitudeToolStripMenuItem;

		private ToolStripMenuItem excludeEventIfInFUTUREDATToolStripMenuItem;

		public AsteroidSummary()
		{
			InitializeComponent();
			AppPath = Utilities.AppPath;
		}

		private void AsteroidSummary_Load(object sender, EventArgs e)
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
			cmbSites.get_Items().Clear();
			string[] files = Directory.GetFiles(AppPath + "\\Sites", "*.site");
			foreach (string path in files)
			{
				cmbSites.get_Items().Add((object)Path.GetFileName(path));
			}
			string siteFileForAutoMap = Settings.Default.SiteFileForAutoMap;
			cmbSites.set_SelectedIndex(0);
			for (int j = 0; j < cmbSites.get_Items().get_Count(); j++)
			{
				if (siteFileForAutoMap == cmbSites.get_Items().get_Item(j).ToString())
				{
					cmbSites.set_SelectedIndex(j);
					break;
				}
			}
			CheckedListBox obj = lstChkSummary;
			ToolStripComboBox obj2 = cmbSites;
			Button obj3 = cmdAutoOutput;
			Button obj4 = cmdAutoCancel;
			bool asteroidalsAutoPredictFutureOutputs;
			((Control)chkNewMultiFile).set_Visible(asteroidalsAutoPredictFutureOutputs = Settings.Default.AsteroidalsAutoPredictFutureOutputs);
			bool flag;
			((Control)obj4).set_Visible(flag = asteroidalsAutoPredictFutureOutputs);
			bool flag2;
			((Control)obj3).set_Visible(flag2 = flag);
			bool flag3;
			((ToolStripItem)obj2).set_Visible(flag3 = flag2);
			bool checkBoxList;
			((Control)obj).set_Visible(checkBoxList = flag3);
			CheckBoxList = checkBoxList;
			((Control)lstSummary).set_Visible(!Settings.Default.AsteroidalsAutoPredictFutureOutputs);
			((Control)cmdAutoCancel).set_Top(((Control)cmdAutoOutput).get_Top());
			((Control)lstChkSummary).set_Top(((Control)lstSummary).get_Top());
			((Control)lstChkSummary).set_Left(((Control)lstSummary).get_Left());
			AsteroidSummary_Resize(sender, e);
			ToolStripMenuItem obj5 = checkAll;
			((ToolStripItem)uncheckAll).set_Visible(checkBoxList = CheckBoxList);
			((ToolStripItem)obj5).set_Visible(checkBoxList);
			CombinedMagnitude.set_Checked(Settings.Default.AsteroidUseCombinedMagnitudes);
			UseCombinedMag = CombinedMagnitude.get_Checked();
		}

		private void AsteroidSummary_Resize(object sender, EventArgs e)
		{
			if (!((((Control)this).get_Width() < 100) | (((Control)this).get_Height() < 150)))
			{
				((Control)lstSummary).set_Width(((Control)this).get_Width() - 32);
				((Control)lstSummary).set_Height(((Control)this).get_Height() - 74);
				((Control)lstChkSummary).set_Width(((Control)this).get_Width() - 32);
				((Control)lstChkSummary).set_Height(((Control)this).get_Height() - 74);
			}
		}

		private void byDateToolStripMenuItem_Click(object sender, EventArgs e)
		{
			AsteroidSummaryLine.SortField = 0;
			Display();
		}

		internal void DefaultSort()
		{
			AsteroidSummaryLine.SortField = 0;
			Display();
		}

		private void byDiameterkmToolStripMenuItem_Click(object sender, EventArgs e)
		{
			AsteroidSummaryLine.SortField = 1;
			Display();
		}

		private void byDiameterapparentToolStripMenuItem_Click(object sender, EventArgs e)
		{
			AsteroidSummaryLine.SortField = 2;
			Display();
		}

		private void byDurationToolStripMenuItem_Click(object sender, EventArgs e)
		{
			AsteroidSummaryLine.SortField = 3;
			Display();
		}

		private void byStarMagToolStripMenuItem_Click(object sender, EventArgs e)
		{
			AsteroidSummaryLine.SortField = 4;
			Display();
		}

		private void byCombinedMagnitudeToolStripMenuItem_Click(object sender, EventArgs e)
		{
			AsteroidSummaryLine.SortField = 13;
			Display();
		}

		private void byMagnitudeDropToolStripMenuItem_Click(object sender, EventArgs e)
		{
			AsteroidSummaryLine.SortField = 5;
			Display();
		}

		private void byMagnitudeDropredToolStripMenuItem_Click(object sender, EventArgs e)
		{
			AsteroidSummaryLine.SortField = 12;
			Display();
		}

		private void byElongationToolStripMenuItem_Click(object sender, EventArgs e)
		{
			AsteroidSummaryLine.SortField = 6;
			Display();
		}

		private void byDistanceToolStripMenuItem_Click(object sender, EventArgs e)
		{
			AsteroidSummaryLine.SortField = 11;
			Display();
		}

		private void byProbabilityToolStripMenuItem_Click(object sender, EventArgs e)
		{
			AsteroidSummaryLine.SortField = 10;
			Display();
		}

		private void byNameToolStripMenuItem_Click(object sender, EventArgs e)
		{
			AsteroidSummaryLine.SortField = 9;
			Display();
		}

		private void byNumberToolStripMenuItem_Click(object sender, EventArgs e)
		{
			AsteroidSummaryLine.SortField = 8;
			Display();
		}

		private void byStarNumberToolStripMenuItem_Click(object sender, EventArgs e)
		{
			AsteroidSummaryLine.SortField = 7;
			Display();
		}

		private void listStarsOccultToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ListStarsMoreThanOnce = true;
			AsteroidSummaryLine.SortField = 7;
			Display();
		}

		private void Display()
		{
			int num = 0;
			int num2 = 0;
			bool flag = true;
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			DisplayMPOccultations.EventOut.Sort();
			string text = "Star";
			if (!CheckBoxList)
			{
				lstSummary.BeginUpdate();
				lstSummary.get_Items().Clear();
				if (!PlanetTimes)
				{
					if (Global)
					{
						lstSummary.get_Items().Add((object)"Global summary of events");
					}
					else
					{
						lstSummary.get_Items().Add((object)SummaryHeader);
					}
					lstSummary.get_Items().Add((object)("".PadRight(100) + "'§' = shape model, '☾' = moon, '◉' = rings "));
					if (!Abbreviated)
					{
						if (!Global)
						{
							lstSummary.get_Items().Add((object)("     Date       U.T.    Diameter   Durn  " + text + "  Mag-Drop Elon  %      Star         d     Planet             Alt  Dist Sun Proba-   Moon   § ◉    R.A. (J2000)  Dec.    "));
							if (!DistInArcSec)
							{
								lstSummary.get_Items().Add((object)"   y   m  d    h   m     km   \"   sec/m   mag   V    R     o Ill     No.               No Name               o   km  Alt bility Elon ill  ☾    h  m   s      o  '   \"  ");
							}
							else
							{
								lstSummary.get_Items().Add((object)"   y   m  d    h   m     km   \"   sec/m   mag   V    R     o Ill     No.               No Name               o    \"  Alt bility Elon ill  ☾    h  m   s      o  '   \"  ");
							}
						}
						else
						{
							lstSummary.get_Items().Add((object)("     Date       U.T.    Diameter   Durn  " + text + "  Mag-Drop Elon  %      Star         d     Planet             Min          Moon   § ◉    R.A. (J2000)  Dec.     "));
							lstSummary.get_Items().Add((object)"   y   m  d    h   m     km   \"   sec/m   mag   V    R     o Ill     No.               No Name              D   Error Elon ill  ☾    h  m   s      o  '   \"  ");
						}
					}
					else
					{
						lstSummary.get_Items().Add((object)("     Date       U.T.    Diameter   Durn  " + text + "  Mag-Drop Elon  %      Star         d     Planet             "));
						lstSummary.get_Items().Add((object)"   y   m  d    h   m     km   \"   sec/m   mag   V    R     o Ill     No.               No Name             ");
					}
					Max = DisplayMPOccultations.EventOut.Count - 1;
					MoreThanOnce = new bool[Max + 1];
					for (int i = 0; i <= Max; i++)
					{
						MoreThanOnce[i] = false;
					}
					if (ListStarsMoreThanOnce)
					{
						for (int j = 0; j < Max; j++)
						{
							if ((DisplayMPOccultations.EventOut[j].StarNo == DisplayMPOccultations.EventOut[j + 1].StarNo) & (DisplayMPOccultations.EventOut[j].AsteroidName != DisplayMPOccultations.EventOut[j + 1].AsteroidName))
							{
								MoreThanOnce[j] = (MoreThanOnce[j + 1] = true);
							}
						}
					}
					for (int k = 0; k <= Max; k++)
					{
						if (!(!ListStarsMoreThanOnce | MoreThanOnce[k]))
						{
							continue;
						}
						flag = true;
						if (excludeEventIfInFUTUREDATToolStripMenuItem.get_Checked())
						{
							flag = !IsInFuture_Dat(DisplayMPOccultations.EventOut[k].AsteroidNumber, DisplayMPOccultations.EventOut[k].UTDate.Replace(" 0", "  ").Trim(), DisplayMPOccultations.EventOut[k].StarNo.Replace("u", " ").Trim());
						}
						if (flag)
						{
							lstSummary.get_Items().Add((object)DisplayMPOccultations.EventOut[k].ToString(Abbreviated, UseCombinedMag));
							num++;
							if (num % 5 == 0)
							{
								lstSummary.get_Items().Add((object)"");
							}
						}
					}
					ListStarsMoreThanOnce = false;
				}
				else
				{
					lstSummary.get_Items().Add((object)SummaryHeader);
					lstSummary.get_Items().Add((object)"");
					lstSummary.get_Items().Add((object)"     Date    Planet        Star          Star Elon  %       D      PA alt      R      PA alt       Rec");
					lstSummary.get_Items().Add((object)"   y   m  d  Name          No.            mag    o ill    h   m     o   o    h   m     o   o         #");
					Max = DisplayMPOccultations.EventOut.Count - 1;
					for (int l = 0; l <= Max; l++)
					{
						lstSummary.get_Items().Add((object)DisplayMPOccultations.EventOut[l].ToString(Abbreviated: false, UseCombinedMag));
						if ((l + 1) % 5 == 0)
						{
							lstSummary.get_Items().Add((object)"");
						}
					}
				}
				lstSummary.EndUpdate();
			}
			else
			{
				((ListBox)lstChkSummary).BeginUpdate();
				((ObjectCollection)lstChkSummary.get_Items()).Clear();
				UpdatingChecks = true;
				if (!PlanetTimes)
				{
					if (Global)
					{
						((ObjectCollection)lstChkSummary.get_Items()).Add((object)"Global summary of events");
					}
					else
					{
						((ObjectCollection)lstChkSummary.get_Items()).Add((object)SummaryHeader);
					}
					((ObjectCollection)lstChkSummary.get_Items()).Add((object)"");
					if (!Abbreviated)
					{
						if (!Global)
						{
							((ObjectCollection)lstChkSummary.get_Items()).Add((object)("     Date       U.T.    Diameter   Durn  " + text + "  Mag-Drop Elon  %      Star         d     Planet             Alt  Dist Sun Proba-   Moon   § ◉    R.A. (J2000)  Dec.    "));
							if (DistInArcSec)
							{
								((ObjectCollection)lstChkSummary.get_Items()).Add((object)"   y   m  d    h   m     km   \"   sec/m   mag   V    R     o Ill     No.               No Name               o   km  Alt bility Elon ill  ☾    h  m   s      o  '   \"  ");
							}
							else
							{
								((ObjectCollection)lstChkSummary.get_Items()).Add((object)"   y   m  d    h   m     km   \"   sec/m   mag   V    R     o Ill     No.               No Name               o    \"  Alt bility Elon ill  ☾    h  m   s      o  '   \"  ");
							}
						}
						else
						{
							((ObjectCollection)lstChkSummary.get_Items()).Add((object)("     Date       U.T.    Diameter   Durn  " + text + "  Mag-Drop Elon  %      Star         d     Planet             Min          Moon   § ◉    R.A. (J2000)  Dec.     "));
							((ObjectCollection)lstChkSummary.get_Items()).Add((object)"   y   m  d    h   m     km   \"   sec/m   mag   V    R     o Ill     No.               No Name              D   Error Elon ill  ☾    h  m   s      o  '   \"  ");
						}
					}
					else
					{
						((ObjectCollection)lstChkSummary.get_Items()).Add((object)("     Date       U.T.    Diameter   Durn  " + text + "  Mag-Drop Elon  %      Star         d     Planet             "));
						((ObjectCollection)lstChkSummary.get_Items()).Add((object)"   y   m  d    h   m     km   \"   sec/m   mag   V    R     o Ill     No.               No Name             ");
					}
					Max = DisplayMPOccultations.EventOut.Count - 1;
					MoreThanOnce = new bool[Max + 1];
					for (int m = 0; m <= Max; m++)
					{
						MoreThanOnce[m] = false;
					}
					if (ListStarsMoreThanOnce)
					{
						for (int n = 0; n < Max; n++)
						{
							if ((DisplayMPOccultations.EventOut[n].StarNo == DisplayMPOccultations.EventOut[n + 1].StarNo) & (DisplayMPOccultations.EventOut[n].AsteroidName != DisplayMPOccultations.EventOut[n + 1].AsteroidName))
							{
								MoreThanOnce[n] = (MoreThanOnce[n + 1] = true);
							}
						}
					}
					for (int num3 = 0; num3 <= Max; num3++)
					{
						if (!ListStarsMoreThanOnce | MoreThanOnce[num3])
						{
							if (excludeEventIfInFUTUREDATToolStripMenuItem.get_Checked())
							{
								DisplayMPOccultations.EventOut[num3].IsChecked &= !IsInFuture_Dat(DisplayMPOccultations.EventOut[num3].AsteroidNumber, DisplayMPOccultations.EventOut[num3].UTDate.Replace(" 0", "  ").Trim(), DisplayMPOccultations.EventOut[num3].StarNo.Replace("u", " ").Trim());
							}
							num2 = ((ObjectCollection)lstChkSummary.get_Items()).Add((object)DisplayMPOccultations.EventOut[num3].ToString(Abbreviated, UseCombinedMag));
							lstChkSummary.SetItemChecked(num2, DisplayMPOccultations.EventOut[num3].IsChecked);
							num++;
							if (num % 5 == 0)
							{
								((ObjectCollection)lstChkSummary.get_Items()).Add((object)"");
							}
						}
					}
					ListStarsMoreThanOnce = false;
				}
				else
				{
					((ObjectCollection)lstChkSummary.get_Items()).Add((object)SummaryHeader);
					((ObjectCollection)lstChkSummary.get_Items()).Add((object)"");
					((ObjectCollection)lstChkSummary.get_Items()).Add((object)"     Date    Planet        Star          Star Elon  %       D      PA alt      R      PA alt       Rec");
					((ObjectCollection)lstChkSummary.get_Items()).Add((object)"   y   m  d  Name          No.            mag    o ill    h   m     o   o    h   m     o   o         #");
					Max = DisplayMPOccultations.EventOut.Count - 1;
					for (int num4 = 0; num4 <= Max; num4++)
					{
						num2 = ((ObjectCollection)lstChkSummary.get_Items()).Add((object)DisplayMPOccultations.EventOut[num4].ToString(Abbreviated: false, UseCombinedMag));
						lstChkSummary.SetItemChecked(num2, DisplayMPOccultations.EventOut[num4].IsChecked);
						if ((num4 + 1) % 5 == 0)
						{
							((ObjectCollection)lstChkSummary.get_Items()).Add((object)"");
						}
					}
				}
				UpdatingChecks = false;
				((ListBox)lstChkSummary).EndUpdate();
			}
			((Control)this).Focus();
			((Control)this).set_Cursor(Cursors.get_Default());
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
		}

		private void abbreviatedOutputToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Abbreviated = !Abbreviated;
			abbreviatedOutputToolStripMenuItem.set_Checked(Abbreviated);
			((ToolStripItem)byDistanceToolStripMenuItem).set_Enabled(!Abbreviated);
			((ToolStripItem)byProbabilityToolStripMenuItem).set_Enabled(!Abbreviated);
			((ToolStripItem)stepthroughGraphicsToolStripMenuItem).set_Enabled(!Abbreviated);
			Display();
		}

		private void copyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(CollectEvents());
		}

		private void printPreviewToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.PrintPreviewText(CollectEvents());
		}

		private void printToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.PrintText(CollectEvents());
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings.Default.Save_AsteroidResults = Output.SaveAppendPredictionText(CollectEvents(), "Summary of Asteroidal Occultations", Settings.Default.Save_AsteroidResults);
		}

		private string CollectEvents()
		{
			StringBuilder stringBuilder = new StringBuilder();
			if (CheckBoxList)
			{
				int count = ((ObjectCollection)lstChkSummary.get_Items()).get_Count();
				for (int i = 0; i <= 3; i++)
				{
					stringBuilder.AppendLine(((ObjectCollection)lstChkSummary.get_Items()).get_Item(i).ToString());
				}
				for (int j = 4; j < count; j++)
				{
					if (lstChkSummary.GetItemChecked(j))
					{
						string text = ((ObjectCollection)lstChkSummary.get_Items()).get_Item(j).ToString();
						if (text.Length > 1)
						{
							stringBuilder.AppendLine(text);
						}
					}
				}
			}
			else
			{
				int count = lstSummary.get_Items().get_Count();
				for (int k = 0; k < count; k++)
				{
					stringBuilder.AppendLine(lstSummary.get_Items().get_Item(k).ToString());
				}
			}
			stringBuilder.AppendLine("");
			return stringBuilder.ToString();
		}

		private void stepthroughGraphicsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			stepthroughGraphicsToolStripMenuItem.set_Checked(!stepthroughGraphicsToolStripMenuItem.get_Checked());
		}

		private void lstSummary_DoubleClick(object sender, EventArgs e)
		{
			if (((ListControl)lstSummary).get_SelectedIndex() < 0)
			{
				return;
			}
			string text = lstSummary.get_Items().get_Item(((ListControl)lstSummary).get_SelectedIndex()).ToString();
			if (text.Trim().Length >= 20)
			{
				int result = -1;
				if (int.TryParse(text.Substring(text.Length - 10), out result) && result >= 0)
				{
					DisplayMPOccultations.DisplayEventFromSummary(result);
					LastListItem = ((ListControl)lstSummary).get_SelectedIndex();
				}
			}
		}

		private void lstSummary_SelectedIndexChanged(object sender, EventArgs e)
		{
			if (((ListControl)lstSummary).get_SelectedIndex() < 0)
			{
				return;
			}
			if (lstSummary.get_Items().get_Item(((ListControl)lstSummary).get_SelectedIndex()).ToString()!.Trim() == "")
			{
				if ((LastListItem < ((ListControl)lstSummary).get_SelectedIndex()) & (((ListControl)lstSummary).get_SelectedIndex() < lstSummary.get_Items().get_Count() - 1))
				{
					ListBox obj = lstSummary;
					int selectedIndex = ((ListControl)obj).get_SelectedIndex();
					((ListControl)obj).set_SelectedIndex(selectedIndex + 1);
				}
				else if (((ListControl)lstSummary).get_SelectedIndex() > 6)
				{
					ListBox obj2 = lstSummary;
					int selectedIndex = ((ListControl)obj2).get_SelectedIndex();
					((ListControl)obj2).set_SelectedIndex(selectedIndex - 1);
				}
			}
			else if (stepthroughGraphicsToolStripMenuItem.get_Checked())
			{
				lstSummary_DoubleClick(sender, e);
			}
		}

		private void plotPathsOnAMapToolStripMenuItem_Click(object sender, EventArgs e)
		{
			DisplayMPOccultations.Plot_MultiPaths(FromListAndDisplay: false);
		}

		private void createGoogleKMLFileOfEventsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			double num = 0.0;
			double num2 = 0.0;
			int num3 = 0;
			int num4 = 1;
			if (!GoogleEarth.Create_New_GoogleEarthKMZ_File("Multipath", "Multipath", AutoOpenFile: false, out var CreatedFile))
			{
				return;
			}
			for (int i = num3; i <= num4; i++)
			{
				int num5 = 0;
				StreamReader streamReader = new StreamReader(AppPath + "\\Generated Files\\multipath.tmp");
				GoogleEarth.Write_Tags_For_New_GoogleEarthKMZ_Path(num5 % 16);
				if (streamReader.BaseStream.Length > 150)
				{
					while (!streamReader.EndOfStream)
					{
						string text = streamReader.ReadLine()!.PadRight(4);
						if (text.Substring(0, 3) != "***")
						{
							if (text.Length > 10)
							{
								num = double.Parse(text.Substring(27 + 18 * i, 9));
								num2 = double.Parse(text.Substring(36 + 18 * i, 9));
								if (num2 < 100.0)
								{
									GoogleEarth.Write_Path_Coordinate_GoogleEarthKMZ(num, num2, 0.0);
								}
							}
						}
						else
						{
							GoogleEarth.Write_Tags_At_End_of_Path_GoogleEarthKMZ();
							GoogleEarth.Write_Tags_For_New_GoogleEarthKMZ_Path(num5 % 16);
							num5++;
						}
					}
				}
				streamReader.Close();
				GoogleEarth.Write_Tags_At_End_of_Path_GoogleEarthKMZ();
				num5 = 0;
			}
			CreatedFile = GoogleEarth.Close_GoogleEarthKMZ_File(CreatedFile, ConvertToKMZ: true);
		}

		private void autoOutToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Control)cmdAutoOutput).set_Enabled(autoOutToolStripMenuItem.get_Checked());
			((ToolStripItem)cmbSites).set_Enabled(autoOutToolStripMenuItem.get_Checked());
		}

		private void cmdAutoOutput_Click(object sender, EventArgs e)
		{
			StreamWriter streamWriter = new StreamWriter(AppPath + "\\AutoGenerated Asteroids\\FileIndex.txt", append: false);
			PBarSummary.set_Minimum(3);
			PBarSummary.set_Value(3);
			if (CheckBoxList)
			{
				PBarSummary.set_Maximum(((ObjectCollection)lstChkSummary.get_Items()).get_Count());
			}
			else
			{
				PBarSummary.set_Maximum(lstSummary.get_Items().get_Count());
			}
			((Control)PBarSummary).set_Visible(true);
			AutoGenerateCancelFlag = false;
			((Control)cmdAutoOutput).set_Visible(false);
			int num = ((!CheckBoxList) ? lstSummary.get_Items().get_Count() : ((ObjectCollection)lstChkSummary.get_Items()).get_Count());
			for (int i = 0; i < num; i++)
			{
				string text = ((!CheckBoxList) ? lstSummary.get_Items().get_Item(i).ToString() : ((ObjectCollection)lstChkSummary.get_Items()).get_Item(i).ToString());
				if ((text.Trim().Length > 20) & lstChkSummary.GetItemChecked(i))
				{
					int result = -1;
					if (int.TryParse(text.Substring(text.Length - 10), out result))
					{
						PBarSummary.set_Value(i);
						if (result >= 0)
						{
							DisplayMPOccultations.GeneratePredictionOutputFilesFromSummaryPage(FromListAndDisplay: false, DisplayMPOccultations.FileNameIn, result, cmbSites.get_Items().get_Item(cmbSites.get_SelectedIndex()).ToString(), 1.0, chkNewMultiFile.get_Checked(), streamWriter);
						}
					}
				}
				Application.DoEvents();
				if (AutoGenerateCancelFlag)
				{
					break;
				}
			}
			((Control)cmdAutoOutput).set_Visible(true);
			streamWriter.Close();
			((Control)PBarSummary).set_Visible(false);
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Asteroid - Summary");
		}

		private void cmdAutoCancel_Click(object sender, EventArgs e)
		{
			AutoGenerateCancelFlag = true;
		}

		private void excludeEventIfInFUTUREDATToolStripMenuItem_Click(object sender, EventArgs e)
		{
		}

		private bool IsInFuture_Dat(string AsterNum, string Date, string Star)
		{
			if (Future.Count < 0)
			{
				return false;
			}
			if (!int.TryParse(AsterNum, out var result))
			{
				return false;
			}
			int num = 0;
			int num2 = Future.Count - 1;
			int num3 = result - 1;
			int num4;
			if (num3 < 1)
			{
				num4 = 0;
			}
			else
			{
				do
				{
					num4 = (num + num2) / 2;
					if (num3 == Future[num4].AsteroidNumber)
					{
						break;
					}
					if (num3 > Future[num4].AsteroidNumber)
					{
						num = num4 + 1;
					}
					else
					{
						num2 = num4 - 1;
					}
				}
				while (num2 >= num);
			}
			Star.Contains("TYC");
			do
			{
				if (result == Future[num4].AsteroidNumber)
				{
					if ((Date == Future[num4].Date) & (Star == Future[num4].Star))
					{
						return true;
					}
				}
				else if (result < Future[num4].AsteroidNumber)
				{
					return false;
				}
				num4++;
			}
			while (num4 < Future.Count);
			return false;
		}

		private void lstChkSummary_SelectedIndexChanged(object sender, EventArgs e)
		{
			if ((((ListControl)lstChkSummary).get_SelectedIndex() < 0) | CheckBoxList)
			{
				return;
			}
			if (((ObjectCollection)lstChkSummary.get_Items()).get_Item(((ListControl)lstChkSummary).get_SelectedIndex()).ToString()!.Trim() == "")
			{
				if ((LastListItem < ((ListControl)lstChkSummary).get_SelectedIndex()) & (((ListControl)lstChkSummary).get_SelectedIndex() < ((ObjectCollection)lstChkSummary.get_Items()).get_Count() - 1))
				{
					CheckedListBox obj = lstChkSummary;
					int selectedIndex = ((ListControl)obj).get_SelectedIndex();
					((ListControl)obj).set_SelectedIndex(selectedIndex + 1);
				}
				else if (((ListControl)lstChkSummary).get_SelectedIndex() > 6)
				{
					CheckedListBox obj2 = lstChkSummary;
					int selectedIndex = ((ListControl)obj2).get_SelectedIndex();
					((ListControl)obj2).set_SelectedIndex(selectedIndex - 1);
				}
			}
			else if (stepthroughGraphicsToolStripMenuItem.get_Checked())
			{
				lstChkSummary_DoubleClick(sender, e);
			}
		}

		private void lstChkSummary_DoubleClick(object sender, EventArgs e)
		{
			if (((ListControl)lstChkSummary).get_SelectedIndex() < 0)
			{
				return;
			}
			string text = ((ObjectCollection)lstChkSummary.get_Items()).get_Item(((ListControl)lstChkSummary).get_SelectedIndex()).ToString();
			if (text.Trim().Length >= 20)
			{
				int result = -1;
				if (int.TryParse(text.Substring(text.Length - 10), out result) && result >= 0)
				{
					DisplayMPOccultations.DisplayEventFromSummary(result);
					LastListItem = ((ListControl)lstChkSummary).get_SelectedIndex();
				}
			}
		}

		private void checkAll_Click(object sender, EventArgs e)
		{
			CheckAllValid();
		}

		private void CheckAllValid()
		{
			for (int i = 0; i < DisplayMPOccultations.EventOut.Count; i++)
			{
				DisplayMPOccultations.EventOut[i].IsChecked = true;
			}
			Display();
		}

		private void uncheckAll_Click(object sender, EventArgs e)
		{
			for (int i = 0; i < DisplayMPOccultations.EventOut.Count; i++)
			{
				DisplayMPOccultations.EventOut[i].IsChecked = false;
			}
			Display();
		}

		private void CombinedMagnitude_Click(object sender, EventArgs e)
		{
			CombinedMagnitude.set_Checked(!CombinedMagnitude.get_Checked());
			Settings.Default.AsteroidUseCombinedMagnitudes = CombinedMagnitude.get_Checked();
			UseCombinedMag = CombinedMagnitude.get_Checked();
			Display();
		}

		private void lstChkSummary_ItemCheck(object sender, ItemCheckEventArgs e)
		{
			if (!UpdatingChecks)
			{
				int num = ((ListControl)lstChkSummary).get_SelectedIndex() - 4;
				if (num >= 0)
				{
					int index = num - (int)Math.Floor((double)num / 6.0);
					DisplayMPOccultations.EventOut[index].IsChecked = !DisplayMPOccultations.EventOut[index].IsChecked;
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
			//IL_1319: Unknown result type (might be due to invalid IL or missing references)
			//IL_1323: Expected O, but got Unknown
			//IL_1410: Unknown result type (might be due to invalid IL or missing references)
			//IL_141a: Expected O, but got Unknown
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(AsteroidSummary));
			lstSummary = new ListBox();
			menuStrip1 = new MenuStrip();
			withPredictionsToolStripMenuItem = new ToolStripMenuItem();
			plotPathsOnAMapToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator2 = new ToolStripSeparator();
			abbreviatedOutputToolStripMenuItem = new ToolStripMenuItem();
			stepthroughGraphicsToolStripMenuItem = new ToolStripMenuItem();
			CombinedMagnitude = new ToolStripMenuItem();
			toolStripSeparator5 = new ToolStripSeparator();
			checkAll = new ToolStripMenuItem();
			uncheckAll = new ToolStripMenuItem();
			excludeEventIfInFUTUREDATToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator1 = new ToolStripSeparator();
			copyToolStripMenuItem = new ToolStripMenuItem();
			printPreviewToolStripMenuItem = new ToolStripMenuItem();
			printToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			createGoogleKMLFileOfEventsToolStripMenuItem = new ToolStripMenuItem();
			autoOutToolStripMenuItem = new ToolStripMenuItem();
			sortToolStripMenuItem = new ToolStripMenuItem();
			byDateToolStripMenuItem = new ToolStripMenuItem();
			byDiameterkmToolStripMenuItem = new ToolStripMenuItem();
			byDiameterapparentToolStripMenuItem = new ToolStripMenuItem();
			byDurationToolStripMenuItem = new ToolStripMenuItem();
			byStarMagToolStripMenuItem = new ToolStripMenuItem();
			byCombinedMagnitudeToolStripMenuItem = new ToolStripMenuItem();
			byMagnitudeDropToolStripMenuItem = new ToolStripMenuItem();
			byMagnitudeDropredToolStripMenuItem = new ToolStripMenuItem();
			byElongationToolStripMenuItem = new ToolStripMenuItem();
			byDistanceToolStripMenuItem = new ToolStripMenuItem();
			byProbabilityToolStripMenuItem = new ToolStripMenuItem();
			byNameToolStripMenuItem = new ToolStripMenuItem();
			byNumberToolStripMenuItem = new ToolStripMenuItem();
			byStarNumberToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator3 = new ToolStripSeparator();
			listStarsOccultToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			cmbSites = new ToolStripComboBox();
			PBarSummary = new ProgressBar();
			cmdAutoOutput = new Button();
			chkNewMultiFile = new CheckBox();
			cmdAutoCancel = new Button();
			lstChkSummary = new CheckedListBox();
			((Control)menuStrip1).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)lstSummary).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstSummary).set_FormattingEnabled(true);
			lstSummary.set_HorizontalExtent(1204);
			lstSummary.set_HorizontalScrollbar(true);
			lstSummary.set_ItemHeight(14);
			((Control)lstSummary).set_Location(new Point(10, 27));
			((Control)lstSummary).set_Name("lstSummary");
			((Control)lstSummary).set_Size(new Size(1215, 466));
			((Control)lstSummary).set_TabIndex(7);
			lstSummary.add_SelectedIndexChanged((EventHandler)lstSummary_SelectedIndexChanged);
			((Control)lstSummary).add_DoubleClick((EventHandler)lstSummary_DoubleClick);
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[5]
			{
				(ToolStripItem)withPredictionsToolStripMenuItem,
				(ToolStripItem)sortToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem,
				(ToolStripItem)cmbSites
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(1094, 27));
			((Control)menuStrip1).set_TabIndex(1);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withPredictionsToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[16]
			{
				(ToolStripItem)plotPathsOnAMapToolStripMenuItem,
				(ToolStripItem)toolStripSeparator2,
				(ToolStripItem)abbreviatedOutputToolStripMenuItem,
				(ToolStripItem)stepthroughGraphicsToolStripMenuItem,
				(ToolStripItem)CombinedMagnitude,
				(ToolStripItem)toolStripSeparator5,
				(ToolStripItem)checkAll,
				(ToolStripItem)uncheckAll,
				(ToolStripItem)excludeEventIfInFUTUREDATToolStripMenuItem,
				(ToolStripItem)toolStripSeparator1,
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)printPreviewToolStripMenuItem,
				(ToolStripItem)printToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem,
				(ToolStripItem)createGoogleKMLFileOfEventsToolStripMenuItem,
				(ToolStripItem)autoOutToolStripMenuItem
			});
			((ToolStripItem)withPredictionsToolStripMenuItem).set_Name("withPredictionsToolStripMenuItem");
			((ToolStripItem)withPredictionsToolStripMenuItem).set_Size(new Size(113, 23));
			((ToolStripItem)withPredictionsToolStripMenuItem).set_Text("with Predictions...");
			((ToolStripItem)plotPathsOnAMapToolStripMenuItem).set_Name("plotPathsOnAMapToolStripMenuItem");
			((ToolStripItem)plotPathsOnAMapToolStripMenuItem).set_Size(new Size(304, 22));
			((ToolStripItem)plotPathsOnAMapToolStripMenuItem).set_Text("Plot paths on a map");
			((ToolStripItem)plotPathsOnAMapToolStripMenuItem).add_Click((EventHandler)plotPathsOnAMapToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator2).set_Name("toolStripSeparator2");
			((ToolStripItem)toolStripSeparator2).set_Size(new Size(301, 6));
			((ToolStripItem)abbreviatedOutputToolStripMenuItem).set_Name("abbreviatedOutputToolStripMenuItem");
			((ToolStripItem)abbreviatedOutputToolStripMenuItem).set_Size(new Size(304, 22));
			((ToolStripItem)abbreviatedOutputToolStripMenuItem).set_Text("Abbreviated output");
			((ToolStripItem)abbreviatedOutputToolStripMenuItem).add_Click((EventHandler)abbreviatedOutputToolStripMenuItem_Click);
			stepthroughGraphicsToolStripMenuItem.set_Checked(true);
			stepthroughGraphicsToolStripMenuItem.set_CheckState((CheckState)1);
			((ToolStripItem)stepthroughGraphicsToolStripMenuItem).set_Name("stepthroughGraphicsToolStripMenuItem");
			((ToolStripItem)stepthroughGraphicsToolStripMenuItem).set_Size(new Size(304, 22));
			((ToolStripItem)stepthroughGraphicsToolStripMenuItem).set_Text("Auto-display of plot");
			((ToolStripItem)stepthroughGraphicsToolStripMenuItem).add_Click((EventHandler)stepthroughGraphicsToolStripMenuItem_Click);
			CombinedMagnitude.set_Checked(true);
			CombinedMagnitude.set_CheckState((CheckState)1);
			((ToolStripItem)CombinedMagnitude).set_Name("CombinedMagnitude");
			((ToolStripItem)CombinedMagnitude).set_Size(new Size(304, 22));
			((ToolStripItem)CombinedMagnitude).set_Text("Star mag includes Asteroid  (star + asteroid)");
			((ToolStripItem)CombinedMagnitude).add_Click((EventHandler)CombinedMagnitude_Click);
			((ToolStripItem)toolStripSeparator5).set_Name("toolStripSeparator5");
			((ToolStripItem)toolStripSeparator5).set_Size(new Size(301, 6));
			((ToolStripItem)checkAll).set_Name("checkAll");
			((ToolStripItem)checkAll).set_Size(new Size(304, 22));
			((ToolStripItem)checkAll).set_Text("Check all");
			((ToolStripItem)checkAll).add_Click((EventHandler)checkAll_Click);
			((ToolStripItem)uncheckAll).set_Name("uncheckAll");
			((ToolStripItem)uncheckAll).set_Size(new Size(304, 22));
			((ToolStripItem)uncheckAll).set_Text("Uncheck all");
			((ToolStripItem)uncheckAll).add_Click((EventHandler)uncheckAll_Click);
			((ToolStripItem)excludeEventIfInFUTUREDATToolStripMenuItem).set_Enabled(false);
			((ToolStripItem)excludeEventIfInFUTUREDATToolStripMenuItem).set_Name("excludeEventIfInFUTUREDATToolStripMenuItem");
			((ToolStripItem)excludeEventIfInFUTUREDATToolStripMenuItem).set_Size(new Size(304, 22));
			((ToolStripItem)excludeEventIfInFUTUREDATToolStripMenuItem).set_Text("Exclude events included in FUTURE.DAT");
			((ToolStripItem)excludeEventIfInFUTUREDATToolStripMenuItem).add_Click((EventHandler)excludeEventIfInFUTUREDATToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator1).set_Name("toolStripSeparator1");
			((ToolStripItem)toolStripSeparator1).set_Size(new Size(301, 6));
			((ToolStripItem)copyToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyToolStripMenuItem).set_Name("copyToolStripMenuItem");
			copyToolStripMenuItem.set_ShortcutKeys((Keys)131139);
			((ToolStripItem)copyToolStripMenuItem).set_Size(new Size(304, 22));
			((ToolStripItem)copyToolStripMenuItem).set_Text("&Copy");
			((ToolStripItem)copyToolStripMenuItem).add_Click((EventHandler)copyToolStripMenuItem_Click);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Image((Image)Resources.PrintPreview);
			((ToolStripItem)printPreviewToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Name("printPreviewToolStripMenuItem");
			((ToolStripItem)printPreviewToolStripMenuItem).set_Size(new Size(304, 22));
			((ToolStripItem)printPreviewToolStripMenuItem).set_Text("Print preview");
			((ToolStripItem)printPreviewToolStripMenuItem).add_Click((EventHandler)printPreviewToolStripMenuItem_Click);
			((ToolStripItem)printToolStripMenuItem).set_Image((Image)Resources.Print);
			((ToolStripItem)printToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printToolStripMenuItem).set_Name("printToolStripMenuItem");
			printToolStripMenuItem.set_ShortcutKeys((Keys)131152);
			((ToolStripItem)printToolStripMenuItem).set_Size(new Size(304, 22));
			((ToolStripItem)printToolStripMenuItem).set_Text("&Print");
			((ToolStripItem)printToolStripMenuItem).add_Click((EventHandler)printToolStripMenuItem_Click);
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			saveToolStripMenuItem.set_ShortcutKeys((Keys)131155);
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(304, 22));
			((ToolStripItem)saveToolStripMenuItem).set_Text("&Save");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
			((ToolStripItem)createGoogleKMLFileOfEventsToolStripMenuItem).set_Image((Image)Resources.kml_file);
			((ToolStripItem)createGoogleKMLFileOfEventsToolStripMenuItem).set_Name("createGoogleKMLFileOfEventsToolStripMenuItem");
			createGoogleKMLFileOfEventsToolStripMenuItem.set_ShowShortcutKeys(false);
			((ToolStripItem)createGoogleKMLFileOfEventsToolStripMenuItem).set_Size(new Size(304, 22));
			((ToolStripItem)createGoogleKMLFileOfEventsToolStripMenuItem).set_Text("Save paths as KMZ, for GoogleEarth");
			((ToolStripItem)createGoogleKMLFileOfEventsToolStripMenuItem).set_Visible(false);
			((ToolStripItem)createGoogleKMLFileOfEventsToolStripMenuItem).add_Click((EventHandler)createGoogleKMLFileOfEventsToolStripMenuItem_Click);
			autoOutToolStripMenuItem.set_CheckOnClick(true);
			((ToolStripItem)autoOutToolStripMenuItem).set_Name("autoOutToolStripMenuItem");
			((ToolStripItem)autoOutToolStripMenuItem).set_Size(new Size(304, 22));
			((ToolStripItem)autoOutToolStripMenuItem).set_Text("Enable generation of output files");
			((ToolStripItem)autoOutToolStripMenuItem).set_Visible(false);
			((ToolStripItem)autoOutToolStripMenuItem).add_Click((EventHandler)autoOutToolStripMenuItem_Click);
			((ToolStripDropDownItem)sortToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[16]
			{
				(ToolStripItem)byDateToolStripMenuItem,
				(ToolStripItem)byDiameterkmToolStripMenuItem,
				(ToolStripItem)byDiameterapparentToolStripMenuItem,
				(ToolStripItem)byDurationToolStripMenuItem,
				(ToolStripItem)byStarMagToolStripMenuItem,
				(ToolStripItem)byCombinedMagnitudeToolStripMenuItem,
				(ToolStripItem)byMagnitudeDropToolStripMenuItem,
				(ToolStripItem)byMagnitudeDropredToolStripMenuItem,
				(ToolStripItem)byElongationToolStripMenuItem,
				(ToolStripItem)byDistanceToolStripMenuItem,
				(ToolStripItem)byProbabilityToolStripMenuItem,
				(ToolStripItem)byNameToolStripMenuItem,
				(ToolStripItem)byNumberToolStripMenuItem,
				(ToolStripItem)byStarNumberToolStripMenuItem,
				(ToolStripItem)toolStripSeparator3,
				(ToolStripItem)listStarsOccultToolStripMenuItem
			});
			((ToolStripItem)sortToolStripMenuItem).set_Image((Image)Resources.Sort);
			((ToolStripItem)sortToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)sortToolStripMenuItem).set_Name("sortToolStripMenuItem");
			((ToolStripItem)sortToolStripMenuItem).set_Size(new Size(65, 23));
			((ToolStripItem)sortToolStripMenuItem).set_Text("Sort...");
			((ToolStripItem)byDateToolStripMenuItem).set_Name("byDateToolStripMenuItem");
			((ToolStripItem)byDateToolStripMenuItem).set_Size(new Size(255, 22));
			((ToolStripItem)byDateToolStripMenuItem).set_Text("by Date");
			((ToolStripItem)byDateToolStripMenuItem).add_Click((EventHandler)byDateToolStripMenuItem_Click);
			((ToolStripItem)byDiameterkmToolStripMenuItem).set_Name("byDiameterkmToolStripMenuItem");
			((ToolStripItem)byDiameterkmToolStripMenuItem).set_Size(new Size(255, 22));
			((ToolStripItem)byDiameterkmToolStripMenuItem).set_Text("by Diameter (km)");
			((ToolStripItem)byDiameterkmToolStripMenuItem).add_Click((EventHandler)byDiameterkmToolStripMenuItem_Click);
			((ToolStripItem)byDiameterapparentToolStripMenuItem).set_Name("byDiameterapparentToolStripMenuItem");
			((ToolStripItem)byDiameterapparentToolStripMenuItem).set_Size(new Size(255, 22));
			((ToolStripItem)byDiameterapparentToolStripMenuItem).set_Text("by Diameter (apparent)");
			((ToolStripItem)byDiameterapparentToolStripMenuItem).add_Click((EventHandler)byDiameterapparentToolStripMenuItem_Click);
			((ToolStripItem)byDurationToolStripMenuItem).set_Name("byDurationToolStripMenuItem");
			((ToolStripItem)byDurationToolStripMenuItem).set_Size(new Size(255, 22));
			((ToolStripItem)byDurationToolStripMenuItem).set_Text("by Duration");
			((ToolStripItem)byDurationToolStripMenuItem).add_Click((EventHandler)byDurationToolStripMenuItem_Click);
			((ToolStripItem)byStarMagToolStripMenuItem).set_Name("byStarMagToolStripMenuItem");
			((ToolStripItem)byStarMagToolStripMenuItem).set_Size(new Size(255, 22));
			((ToolStripItem)byStarMagToolStripMenuItem).set_Text("by Star Magnitude");
			((ToolStripItem)byStarMagToolStripMenuItem).add_Click((EventHandler)byStarMagToolStripMenuItem_Click);
			((ToolStripItem)byCombinedMagnitudeToolStripMenuItem).set_Name("byCombinedMagnitudeToolStripMenuItem");
			((ToolStripItem)byCombinedMagnitudeToolStripMenuItem).set_Size(new Size(255, 22));
			((ToolStripItem)byCombinedMagnitudeToolStripMenuItem).set_Text("by Combined Magnitude");
			((ToolStripItem)byCombinedMagnitudeToolStripMenuItem).add_Click((EventHandler)byCombinedMagnitudeToolStripMenuItem_Click);
			((ToolStripItem)byMagnitudeDropToolStripMenuItem).set_Name("byMagnitudeDropToolStripMenuItem");
			((ToolStripItem)byMagnitudeDropToolStripMenuItem).set_Size(new Size(255, 22));
			((ToolStripItem)byMagnitudeDropToolStripMenuItem).set_Text("by Magnitude drop (Visual)");
			((ToolStripItem)byMagnitudeDropToolStripMenuItem).add_Click((EventHandler)byMagnitudeDropToolStripMenuItem_Click);
			((ToolStripItem)byMagnitudeDropredToolStripMenuItem).set_Name("byMagnitudeDropredToolStripMenuItem");
			((ToolStripItem)byMagnitudeDropredToolStripMenuItem).set_Size(new Size(255, 22));
			((ToolStripItem)byMagnitudeDropredToolStripMenuItem).set_Text("by Magnitude drop (Red)");
			((ToolStripItem)byMagnitudeDropredToolStripMenuItem).add_Click((EventHandler)byMagnitudeDropredToolStripMenuItem_Click);
			((ToolStripItem)byElongationToolStripMenuItem).set_Name("byElongationToolStripMenuItem");
			((ToolStripItem)byElongationToolStripMenuItem).set_Size(new Size(255, 22));
			((ToolStripItem)byElongationToolStripMenuItem).set_Text("by Elongation");
			((ToolStripItem)byElongationToolStripMenuItem).add_Click((EventHandler)byElongationToolStripMenuItem_Click);
			((ToolStripItem)byDistanceToolStripMenuItem).set_Name("byDistanceToolStripMenuItem");
			((ToolStripItem)byDistanceToolStripMenuItem).set_Size(new Size(255, 22));
			((ToolStripItem)byDistanceToolStripMenuItem).set_Text("by Distance");
			((ToolStripItem)byDistanceToolStripMenuItem).add_Click((EventHandler)byDistanceToolStripMenuItem_Click);
			((ToolStripItem)byProbabilityToolStripMenuItem).set_Name("byProbabilityToolStripMenuItem");
			((ToolStripItem)byProbabilityToolStripMenuItem).set_Size(new Size(255, 22));
			((ToolStripItem)byProbabilityToolStripMenuItem).set_Text("by Probability");
			((ToolStripItem)byProbabilityToolStripMenuItem).add_Click((EventHandler)byProbabilityToolStripMenuItem_Click);
			((ToolStripItem)byNameToolStripMenuItem).set_Name("byNameToolStripMenuItem");
			((ToolStripItem)byNameToolStripMenuItem).set_Size(new Size(255, 22));
			((ToolStripItem)byNameToolStripMenuItem).set_Text("by Name");
			((ToolStripItem)byNameToolStripMenuItem).add_Click((EventHandler)byNameToolStripMenuItem_Click);
			((ToolStripItem)byNumberToolStripMenuItem).set_Name("byNumberToolStripMenuItem");
			((ToolStripItem)byNumberToolStripMenuItem).set_Size(new Size(255, 22));
			((ToolStripItem)byNumberToolStripMenuItem).set_Text("by Number");
			((ToolStripItem)byNumberToolStripMenuItem).add_Click((EventHandler)byNumberToolStripMenuItem_Click);
			((ToolStripItem)byStarNumberToolStripMenuItem).set_Name("byStarNumberToolStripMenuItem");
			((ToolStripItem)byStarNumberToolStripMenuItem).set_Size(new Size(255, 22));
			((ToolStripItem)byStarNumberToolStripMenuItem).set_Text("by Star Number");
			((ToolStripItem)byStarNumberToolStripMenuItem).add_Click((EventHandler)byStarNumberToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator3).set_Name("toolStripSeparator3");
			((ToolStripItem)toolStripSeparator3).set_Size(new Size(252, 6));
			((ToolStripItem)listStarsOccultToolStripMenuItem).set_Name("listStarsOccultToolStripMenuItem");
			((ToolStripItem)listStarsOccultToolStripMenuItem).set_Size(new Size(255, 22));
			((ToolStripItem)listStarsOccultToolStripMenuItem).set_Text("List stars occulted more than once");
			((ToolStripItem)listStarsOccultToolStripMenuItem).add_Click((EventHandler)listStarsOccultToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(60, 23));
			((ToolStripItem)helpToolStripMenuItem).set_Text("&Help");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			exitToolStripMenuItem.set_Checked(true);
			exitToolStripMenuItem.set_CheckState((CheckState)1);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(153, 23));
			((ToolStripItem)exitToolStripMenuItem).set_Text(" E&xit                                ");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			cmbSites.set_DropDownStyle((ComboBoxStyle)2);
			cmbSites.set_MaxDropDownItems(20);
			((ToolStripItem)cmbSites).set_Name("cmbSites");
			((ToolStripItem)cmbSites).set_Size(new Size(150, 23));
			cmbSites.set_Sorted(true);
			((ToolStripItem)cmbSites).set_ToolTipText("Site file for MultiSite prediction");
			((Control)PBarSummary).set_Location(new Point(859, 5));
			((Control)PBarSummary).set_Name("PBarSummary");
			((Control)PBarSummary).set_Size(new Size(135, 14));
			((Control)PBarSummary).set_TabIndex(2);
			((Control)PBarSummary).set_Visible(false);
			((Control)cmdAutoOutput).set_Location(new Point(575, 2));
			((Control)cmdAutoOutput).set_Name("cmdAutoOutput");
			((Control)cmdAutoOutput).set_Size(new Size(123, 21));
			((Control)cmdAutoOutput).set_TabIndex(3);
			((Control)cmdAutoOutput).set_Text("Auto file generation");
			((ButtonBase)cmdAutoOutput).set_UseVisualStyleBackColor(true);
			((Control)cmdAutoOutput).add_Click((EventHandler)cmdAutoOutput_Click);
			((Control)chkNewMultiFile).set_AutoSize(true);
			((Control)chkNewMultiFile).set_Location(new Point(708, 5));
			((Control)chkNewMultiFile).set_Name("chkNewMultiFile");
			((Control)chkNewMultiFile).set_Size(new Size(149, 17));
			((Control)chkNewMultiFile).set_TabIndex(4);
			((Control)chkNewMultiFile).set_Text("Site file has been updated");
			((ButtonBase)chkNewMultiFile).set_UseVisualStyleBackColor(true);
			((Control)cmdAutoCancel).set_Location(new Point(575, 14));
			((Control)cmdAutoCancel).set_Name("cmdAutoCancel");
			((Control)cmdAutoCancel).set_Size(new Size(123, 21));
			((Control)cmdAutoCancel).set_TabIndex(5);
			((Control)cmdAutoCancel).set_Text("Cancel");
			((ButtonBase)cmdAutoCancel).set_UseVisualStyleBackColor(true);
			((Control)cmdAutoCancel).add_Click((EventHandler)cmdAutoCancel_Click);
			((Control)lstChkSummary).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstChkSummary).set_FormattingEnabled(true);
			((ListBox)lstChkSummary).set_HorizontalExtent(1204);
			((ListBox)lstChkSummary).set_HorizontalScrollbar(true);
			((Control)lstChkSummary).set_Location(new Point(10, 56));
			((Control)lstChkSummary).set_Name("lstChkSummary");
			((Control)lstChkSummary).set_Size(new Size(1215, 454));
			((Control)lstChkSummary).set_TabIndex(0);
			lstChkSummary.add_ItemCheck(new ItemCheckEventHandler(lstChkSummary_ItemCheck));
			((ListBox)lstChkSummary).add_SelectedIndexChanged((EventHandler)lstChkSummary_SelectedIndexChanged);
			((Control)lstChkSummary).add_DoubleClick((EventHandler)lstChkSummary_DoubleClick);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(1094, 543));
			((Control)this).get_Controls().Add((Control)(object)lstChkSummary);
			((Control)this).get_Controls().Add((Control)(object)cmdAutoOutput);
			((Control)this).get_Controls().Add((Control)(object)cmdAutoCancel);
			((Control)this).get_Controls().Add((Control)(object)PBarSummary);
			((Control)this).get_Controls().Add((Control)(object)chkNewMultiFile);
			((Control)this).get_Controls().Add((Control)(object)lstSummary);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationAsterSummary", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_Icon((Icon)componentResourceManager.GetObject("$this.Icon"));
			((Form)this).set_Location(Settings.Default.LocationAsterSummary);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_Name("AsteroidSummary");
			((Control)this).set_Text("Asteroid - Summary of events");
			((Form)this).add_Load((EventHandler)AsteroidSummary_Load);
			((Control)this).add_Resize((EventHandler)AsteroidSummary_Resize);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
