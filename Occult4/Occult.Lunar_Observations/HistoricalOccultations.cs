using System;
using System.Collections;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Text;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Lunar_Observations
{
	public class HistoricalOccultations : Form
	{
		private readonly string AppPath;

		private readonly string ZCNameFile;

		private const double Radian = 180.0 / Math.PI;

		internal ArrayList StarList = new ArrayList();

		internal ArrayList HistoryFiles = new ArrayList();

		private static List<SiteList> Site_List = new List<SiteList>();

		internal static SiteList SiteListLine;

		private bool FormCreated;

		private bool ChangingStar;

		private bool ObservationsDisplayed;

		private bool MultiSelect;

		private bool RGOSiteDisplayed;

		private bool MergedList;

		private string SiteID;

		private double JD_atEvent;

		private const string HeaderLine = "   y m d h m  s   Star   DELG pe  MMT Acc C snD drn LSTRtmpACTM          C gYr g#   o '  \"    o '  \"   D Alt  D ILOC #   RGO # Location name                                     TMD Ap  FL Observer                    y m d h m  s    Star     o '  \"    o '  \"   D Alt  D";

		private IContainer components;

		private MenuStrip menuStrip1;

		private Button cmdRead;

		private ListBox lstReduction;

		private GroupBox grpAdjustments;

		private TextBox txtXZ;

		private ComboBox cmbNames;

		private Label label6;

		private Label label5;

		private Label label4;

		private Label label3;

		private TextBox txtZC;

		private TextBox txtSAO;

		private Panel panelStar;

		private Panel panelFile;

		private RadioButton optFile;

		private RadioButton optStar;

		private Panel panel1;

		private ToolStripMenuItem withListToolStripMenuItem;

		private ToolStripMenuItem nameOfStarListToolStripMenuItem;

		private ToolStripMenuItem allNamesToolStripMenuItem;

		private ToolStripMenuItem properNamesToolStripMenuItem;

		private ToolStripMenuItem bayerLettersToolStripMenuItem;

		private ToolStripMenuItem brighterThanMag2ToolStripMenuItem;

		private ToolStripMenuItem brighterThanMag3ToolStripMenuItem;

		private ToolStripMenuItem brighterThanMag4ToolStripMenuItem;

		private ProgressBar pbarStar;

		private Label label1;

		private CheckBox chkUnidentifiedDoubles;

		private CheckBox chkDoublesOnly;

		private Label label2;

		private CheckBox chkHighlightDoubles;

		private NumericUpDown updnYear;

		private Button cmdAdjustedReduce;

		private Label label12;

		private Label label11;

		private Label label10;

		private Label label9;

		private Label label8;

		private Label label7;

		private NumericUpDown updnDay;

		private NumericUpDown updnHour;

		private NumericUpDown updnMinute;

		private NumericUpDown updnMonth;

		private RadioButton optXZ;

		private RadioButton optSAO;

		private RadioButton optZC;

		private TextBox txtStar;

		private Label label14;

		private NumericUpDown updnSecond;

		private Label label13;

		private NumericUpDown updnPE;

		private Button cmdCreateReportFromSite;

		internal ListBox lstEvents;

		private ComboBox cmbFile;

		internal ProgressBar pbarRead;

		private Button cmdMergeReportFromSite;

		private ToolStripMenuItem siteStatisticsToolStripMenuItem;

		private ToolStripMenuItem generateILOCStatisticsToolStripMenuItem;

		private ToolStripMenuItem generateRGOStatisticsToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private ToolStripMenuItem createOBSFFromSourceToolStripMenuItem;

		private ToolStripMenuItem createTELFFromSourceToolStripMenuItem;

		private ToolStripMenuItem sortSitesToolStripMenuItem;

		private ToolStripMenuItem byIDToolStripMenuItem;

		private ToolStripMenuItem byLongitudeToolStripMenuItem;

		private ToolStripMenuItem byLatitudeToolStripMenuItem;

		private ToolStripMenuItem bySiteNameToolStripMenuItem;

		private ToolStripMenuItem byObserverToolStripMenuItem;

		private ToolStripMenuItem byObservationsToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem printPreviewToolStripMenuItem;

		private ToolStripMenuItem prToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripSeparator toolStripMenuItem1;

		private ToolStripMenuItem reduceAllSaveToolStripMenuItem;

		private ToolStripMenuItem tHESEAREMAINTENANCEROUTINESToolStripMenuItem;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator1;

		private Button cmdReadFile;

		private TextBox txtPlanet;

		private Label label15;

		private RadioButton optPlanet;

		private ContextMenuStrip contextMenuStrip1;

		private ToolStripMenuItem displayEventAgainstProfileToolStripMenuItem;

		private Label LabelRightClick;

		private ToolStripMenuItem reduceAllValidSaveToolStripMenuItem;

		private RadioButton optSearchName;

		private Panel panelName;

		private CheckBox chkUnidentifiedDoublesNames;

		private CheckBox chkDoublesOnlyNames;

		private Button cmdReadNames;

		private CheckBox chkHighlightDoublesNames;

		private ProgressBar progressBar1;

		private Label label18;

		private Label label19;

		private TextBox txtName;

		private TextBox txtSite;

		private Label label21;

		private CheckBox chkDoubleEdit;

		private GroupBox grpDoubleEditor;

		private Button cmdMoveDown;

		private Button cmdMoveUp;

		private Button cmdDeleteChecked;

		private ToolStripMenuItem openSavedListToolStripMenuItem;

		private Button cmdReverseHighlight;

		private Button cmdHighlightDs;

		private Button cmdHighlightBrightLimb;

		private ToolStripSeparator toolStripSeparator2;

		private ToolStripMenuItem createDoubleStarFilesToolStripMenuItem;

		private ToolStripMenuItem ValidateDataForDoubleStarFilesToolStripMenuItem;

		private ToolStripMenuItem doubleStarExtractionToolStripMenuItem;

		private ToolStripMenuItem mergePreviouslySavedListToolStripMenuItem;

		private Button cmdWithin15Secs;

		private ToolStripMenuItem SortEventsToolStripMenuItem;

		private ToolStripMenuItem byLongitudeToolStripMenuItem1;

		private ToolStripMenuItem byDateTimeToolStripMenuItem;

		private Button cmdCreateReportForlocation;

		private Button cmdMergeReportForLocation;

		private RadioButton optAsteroid;

		private CheckBox chkAsteroid;

		public HistoricalOccultations()
		{
			InitializeComponent();
			((Control)this).set_Height(Screen.GetWorkingArea((Control)(object)this).Height - 20);
			((Control)this).set_Width(Screen.GetWorkingArea((Control)(object)this).Width - 100);
			((Control)this).set_Top(10);
			((Control)this).set_Left(50);
			AppPath = Utilities.AppPath;
			ZCNameFile = AppPath + "\\Resource Files\\ZCNames.dat";
			Panel obj = panelStar;
			int top;
			((Control)panelName).set_Top(top = ((Control)panelFile).get_Top());
			((Control)obj).set_Top(top);
		}

		private void HistoricalOccultations_Load(object sender, EventArgs e)
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
			cmbFile.get_Items().Clear();
			HistoryFiles.Clear();
			((Control)grpDoubleEditor).set_Top(((Control)panelFile).get_Top());
			cmbFile.get_Items().Add((object)"RGO Sites");
			cmbFile.get_Items().Add((object)"ILOC Sites");
			cmbFile.get_Items().Add((object)"");
			string[] files = Directory.GetFiles(AppPath + "\\Resource Files\\", "Archive Observations*.*");
			foreach (string path in files)
			{
				cmbFile.get_Items().Add((object)Path.GetFileName(path));
				HistoryFiles.Add(Path.GetFileName(path));
			}
			((ListControl)cmbFile).set_SelectedIndex(0);
			FormCreated = true;
			SetPanelVisibility();
			CreateStarList(3);
		}

		private void allNamesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			CreateStarList(2);
		}

		private void properNamesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			CreateStarList(0);
		}

		private void bayerLettersToolStripMenuItem_Click(object sender, EventArgs e)
		{
			CreateStarList(1);
		}

		private void brighterThanMag2ToolStripMenuItem_Click(object sender, EventArgs e)
		{
			CreateStarList(3);
		}

		private void brighterThanMag3ToolStripMenuItem_Click(object sender, EventArgs e)
		{
			CreateStarList(4);
		}

		private void brighterThanMag4ToolStripMenuItem_Click(object sender, EventArgs e)
		{
			CreateStarList(5);
		}

		private void CreateStarList(int ListType)
		{
			if (!FormCreated)
			{
				return;
			}
			Cursor.set_Current(Cursors.get_WaitCursor());
			StarList.Clear();
			cmbNames.get_Items().Clear();
			cmbNames.set_Sorted(ListType != 1 && ListType != 2);
			StarList.Add(0);
			cmbNames.get_Items().Add((object)"");
			StreamReader streamReader = new StreamReader(ZCNameFile);
			do
			{
				string? text = streamReader.ReadLine();
				int num = int.Parse(text!.Substring(0, 4));
				string text2 = text!.Substring(5).Trim();
				int num2 = text2.IndexOf("=");
				int num3 = text2.IndexOf("=", num2 + 2);
				if (num3 > num2)
				{
					text2 = text2.Substring(0, num3 - 1);
				}
				int result;
				switch (ListType)
				{
				case 0:
					if (num2 > 0 && !int.TryParse(text2.Substring(0, 2), out result))
					{
						int index = cmbNames.get_Items().Add((object)text2);
						StarList.Insert(index, num);
					}
					break;
				case 1:
					if (num2 > 0)
					{
						if (!int.TryParse(text2.Substring(num2 + 1, 2), out result))
						{
							int index = cmbNames.get_Items().Add((object)text2.Substring(num2 + 1));
							StarList.Insert(index, num);
						}
					}
					else if (!int.TryParse(text2.Substring(0, 2), out result))
					{
						int index = cmbNames.get_Items().Add((object)text2.Trim());
						StarList.Insert(index, num);
					}
					break;
				case 2:
				{
					int index = cmbNames.get_Items().Add((object)text2);
					StarList.Insert(index, num);
					break;
				}
				case 3:
					if (ZCMagnitude(num) < 2.0)
					{
						int index = cmbNames.get_Items().Add((object)text2);
						StarList.Insert(index, num);
					}
					break;
				case 4:
					if (ZCMagnitude(num) < 3.0)
					{
						int index = cmbNames.get_Items().Add((object)text2);
						StarList.Insert(index, num);
					}
					break;
				case 5:
					if (ZCMagnitude(num) < 4.0)
					{
						int index = cmbNames.get_Items().Add((object)text2);
						StarList.Insert(index, num);
					}
					break;
				}
			}
			while (!streamReader.EndOfStream);
			streamReader.Close();
			Cursor.set_Current(Cursors.get_Default());
			((ListControl)cmbNames).set_SelectedIndex(1);
			properNamesToolStripMenuItem.set_Checked(ListType == 0);
			bayerLettersToolStripMenuItem.set_Checked(ListType == 1);
			allNamesToolStripMenuItem.set_Checked(ListType == 2);
			brighterThanMag2ToolStripMenuItem.set_Checked(ListType == 3);
			brighterThanMag3ToolStripMenuItem.set_Checked(ListType == 4);
			brighterThanMag4ToolStripMenuItem.set_Checked(ListType == 5);
		}

		private double ZCMagnitude(int ZC)
		{
			XZ80Q.Get_ZC_Star(ZC);
			return XZ80Q.Mv;
		}

		private void SetcmbNames(int ZC)
		{
			for (int i = 1; i < cmbNames.get_Items().get_Count(); i++)
			{
				if (ZC == (int)StarList[i])
				{
					((ListControl)cmbNames).set_SelectedIndex(i);
					return;
				}
			}
			((ListControl)cmbNames).set_SelectedIndex(0);
		}

		private void txtXZ_TextChanged(object sender, EventArgs e)
		{
			if (!ChangingStar)
			{
				ChangingStar = true;
				if (!int.TryParse(((Control)txtXZ).get_Text(), out var result))
				{
					result = 0;
				}
				XZ80Q.Get_XZ_Star(result);
				((Control)txtZC).set_Text(XZ80Q.ZC.ToString());
				((Control)txtSAO).set_Text(XZ80Q.SAO.ToString());
				((Control)txtPlanet).set_Text("0");
				chkAsteroid.set_Checked(false);
				SetcmbNames(XZ80Q.ZC);
				ChangingStar = false;
			}
		}

		private void txtSAO_TextChanged(object sender, EventArgs e)
		{
			if (!ChangingStar)
			{
				ChangingStar = true;
				if (!int.TryParse(((Control)txtSAO).get_Text(), out var result))
				{
					result = 0;
				}
				if (XZ80Q.Get_SAO_Star(result))
				{
					((Control)txtZC).set_Text(XZ80Q.ZC.ToString());
					((Control)txtXZ).set_Text(XZ80Q.XZ.ToString());
				}
				else
				{
					TextBox obj = txtZC;
					string text;
					((Control)txtXZ).set_Text(text = "0");
					((Control)obj).set_Text(text);
				}
				((Control)txtPlanet).set_Text("0");
				chkAsteroid.set_Checked(false);
				SetcmbNames(XZ80Q.ZC);
				ChangingStar = false;
			}
		}

		private void txtZC_TextChanged(object sender, EventArgs e)
		{
			if (!ChangingStar)
			{
				ChangingStar = true;
				if (!int.TryParse(((Control)txtZC).get_Text(), out var result))
				{
					result = 0;
				}
				XZ80Q.Get_ZC_Star(result);
				((Control)txtSAO).set_Text(XZ80Q.SAO.ToString());
				((Control)txtXZ).set_Text(XZ80Q.XZ.ToString());
				((Control)txtPlanet).set_Text("0");
				chkAsteroid.set_Checked(false);
				SetcmbNames(XZ80Q.ZC);
				ChangingStar = false;
			}
		}

		private void txtPlanet_TextChanged(object sender, EventArgs e)
		{
			if (!ChangingStar)
			{
				ChangingStar = true;
				if (!int.TryParse(((Control)txtPlanet).get_Text(), out var result))
				{
					result = 0;
				}
				else
				{
					TextBox obj = txtZC;
					TextBox obj2 = txtSAO;
					string text;
					((Control)txtXZ).set_Text(text = "0");
					string text2;
					((Control)obj2).set_Text(text2 = text);
					((Control)obj).set_Text(text2);
					((ListControl)cmbNames).set_SelectedIndex(0);
				}
				chkAsteroid.set_Checked(false);
				ChangingStar = false;
			}
		}

		private void chkAsteroid_Click(object sender, EventArgs e)
		{
			if (!ChangingStar)
			{
				ChangingStar = true;
				chkAsteroid.set_Checked(true);
				TextBox obj = txtZC;
				TextBox obj2 = txtSAO;
				TextBox obj3 = txtXZ;
				string text;
				((Control)txtPlanet).set_Text(text = "0");
				string text2;
				((Control)obj3).set_Text(text2 = text);
				string text3;
				((Control)obj2).set_Text(text3 = text2);
				((Control)obj).set_Text(text3);
				((ListControl)cmbNames).set_SelectedIndex(0);
				ChangingStar = false;
			}
		}

		private void cmbNames_SelectedIndexChanged(object sender, EventArgs e)
		{
			if (!ChangingStar)
			{
				ChangingStar = true;
				XZ80Q.Get_ZC_Star((int)StarList[((ListControl)cmbNames).get_SelectedIndex()]);
				((Control)txtZC).set_Text(XZ80Q.ZC.ToString());
				((Control)txtSAO).set_Text(XZ80Q.SAO.ToString());
				((Control)txtXZ).set_Text(XZ80Q.XZ.ToString());
				ChangingStar = false;
			}
		}

		private void optFile_CheckedChanged(object sender, EventArgs e)
		{
			SetPanelVisibility();
		}

		private void optStar_CheckedChanged(object sender, EventArgs e)
		{
			SetPanelVisibility();
		}

		private void optSearchName_CheckedChanged(object sender, EventArgs e)
		{
			SetPanelVisibility();
		}

		private void SetPanelVisibility()
		{
			((Control)panelFile).set_Visible(optFile.get_Checked());
			((Control)panelName).set_Visible(optSearchName.get_Checked());
			Label labelRightClick = LabelRightClick;
			bool @checked;
			((Control)panelStar).set_Visible(@checked = optStar.get_Checked());
			((Control)labelRightClick).set_Visible(@checked);
			((ToolStripItem)withListToolStripMenuItem).set_Enabled(optStar.get_Checked() | optSearchName.get_Checked());
			if (optFile.get_Checked())
			{
				Label labelRightClick2 = LabelRightClick;
				((ToolStripItem)displayEventAgainstProfileToolStripMenuItem).set_Visible(@checked = false);
				((Control)labelRightClick2).set_Visible(@checked);
			}
		}

		private void cmbFile_SelectedIndexChanged(object sender, EventArgs e)
		{
			if (FormCreated)
			{
				cmdRead_Click(sender, e);
			}
		}

		private void cmdRead_Click(object sender, EventArgs e)
		{
			MergedList = false;
			ObservationsDisplayed = false;
			if (optFile.get_Checked() & cmbFile.get_Items().get_Item(((ListControl)cmbFile).get_SelectedIndex()).ToString()!.Contains("*"))
			{
				return;
			}
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			if (chkDoubleEdit.get_Checked())
			{
				((Control)grpDoubleEditor).set_Visible(true);
				lstEvents.get_Items().Clear();
				lstEvents.set_SelectionMode((SelectionMode)3);
				MultiSelect = false;
			}
			else
			{
				((Control)grpDoubleEditor).set_Visible(false);
				lstEvents.get_Items().Clear();
				lstEvents.set_SelectionMode((SelectionMode)1);
				MultiSelect = false;
			}
			((ToolStripItem)sortSitesToolStripMenuItem).set_Enabled(false);
			if (optFile.get_Checked())
			{
				if (cmbFile.get_Items().get_Item(((ListControl)cmbFile).get_SelectedIndex()).ToString() == "RGO Sites")
				{
					ObservationsDisplayed = false;
					pbarRead.set_Value(0);
					((Control)pbarRead).set_Visible(true);
					Site_List.Clear();
					((ToolStripItem)sortSitesToolStripMenuItem).set_Enabled(true);
					((ToolStripItem)byObserverToolStripMenuItem).set_Enabled(true);
					((ToolStripItem)withListToolStripMenuItem).set_Enabled(true);
					((ToolStripItem)reduceAllSaveToolStripMenuItem).set_Enabled(false);
					using (StreamReader streamReader = new StreamReader(AppPath + "\\Resource Files\\Archive RGO Sites.dat"))
					{
						int num = (int)streamReader.BaseStream.Length;
						do
						{
							string text = streamReader.ReadLine();
							if (!int.TryParse(text.Substring(0, 3) + text.Substring(4, 2), out var result))
							{
								result = 0;
							}
							SiteListLine = new SiteList();
							SiteListLine.Site = text.Substring(0, 6) + "    " + text.Substring(7, 3).Replace(" ", "0") + text.Substring(10, 10) + " " + text.Substring(20, 17) + " " + text.Substring(37, 2).Trim().PadRight(2) + text.Substring(40, 30) + GetRGOSiteStatistics(result) + text.Substring(70, 22);
							Site_List.Add(SiteListLine);
							if (lstEvents.get_Items().get_Count() % 250 == 0)
							{
								pbarRead.set_Value((int)(200 * streamReader.BaseStream.Position / num));
								Application.DoEvents();
							}
						}
						while (!streamReader.EndOfStream);
					}
					((Control)pbarRead).set_Visible(false);
					RGOSiteDisplayed = true;
					Sort_DisplaySites(0);
					((ListControl)lstEvents).set_SelectedIndex(0);
				}
				else if (cmbFile.get_Items().get_Item(((ListControl)cmbFile).get_SelectedIndex()).ToString() == "ILOC Sites")
				{
					ObservationsDisplayed = false;
					pbarRead.set_Value(0);
					((Control)pbarRead).set_Visible(true);
					Site_List.Clear();
					((ToolStripItem)sortSitesToolStripMenuItem).set_Enabled(true);
					((ToolStripItem)byObserverToolStripMenuItem).set_Enabled(true);
					((ToolStripItem)withListToolStripMenuItem).set_Enabled(true);
					((ToolStripItem)reduceAllSaveToolStripMenuItem).set_Enabled(false);
					ILOCsites.OpenILOCObserver();
					using (StreamReader streamReader2 = new StreamReader(AppPath + "\\Resource Files\\Archive ILOC Sites.dat"))
					{
						int num = (int)streamReader2.BaseStream.Length;
						do
						{
							string text = streamReader2.ReadLine();
							if (text.Length > 40)
							{
								SiteListLine = new SiteList();
								SiteListLine.Site = (text.Substring(0, 9) + " " + text.Substring(10, 4) + " " + text.Substring(14, 2) + " " + text.Substring(16, 5) + " " + text.Substring(21, 3) + " " + text.Substring(24, 2) + " " + text.Substring(26, 5) + " " + text.Substring(33, 4) + " " + text.Substring(31, 2) + " " + text.Substring(40, 25) + "     " + GetILOCSiteStatistics(text.Substring(0, 9)) + ILOCsites.ILOCObserver(text.Substring(0, 9)).Substring(3)).PadRight(95);
								Site_List.Add(SiteListLine);
								if (lstEvents.get_Items().get_Count() % 250 == 0)
								{
									pbarRead.set_Value((int)(200 * streamReader2.BaseStream.Position / num));
									Application.DoEvents();
								}
							}
						}
						while (!streamReader2.EndOfStream);
					}
					ILOCsites.CloseILOCObserver();
					((Control)pbarRead).set_Visible(false);
					RGOSiteDisplayed = false;
					Sort_DisplaySites(0);
					((ListControl)lstEvents).set_SelectedIndex(0);
				}
				else
				{
					lstEvents.get_Items().Add((object)"   y m d h m  s   Star   DELG pe  MMT Acc C snD drn LSTRtmpACTM          C gYr g#   o '  \"    o '  \"   D Alt  D ILOC #   RGO # Location name                                     TMD Ap  FL Observer                    y m d h m  s    Star     o '  \"    o '  \"   D Alt  D");
					ObservationsDisplayed = true;
					((ToolStripItem)withListToolStripMenuItem).set_Enabled(false);
					string text2 = cmbFile.get_Items().get_Item(((ListControl)cmbFile).get_SelectedIndex()).ToString();
					if (text2.Contains(".dat"))
					{
						pbarRead.set_Value(0);
						((Control)pbarRead).set_Visible(true);
						using (StreamReader streamReader3 = new StreamReader(AppPath + "\\Resource Files\\" + text2))
						{
							int num = (int)streamReader3.BaseStream.Length;
							lstEvents.BeginUpdate();
							do
							{
								lstEvents.get_Items().Add((object)streamReader3.ReadLine());
								if (lstEvents.get_Items().get_Count() % 250 == 0)
								{
									pbarRead.set_Value((int)(200 * streamReader3.BaseStream.Position / num));
									Application.DoEvents();
								}
							}
							while (!streamReader3.EndOfStream);
							lstEvents.EndUpdate();
						}
						((Control)pbarRead).set_Visible(false);
					}
				}
			}
			else if (optStar.get_Checked())
			{
				ObservationsDisplayed = true;
				((ToolStripItem)withListToolStripMenuItem).set_Enabled(true);
				((ToolStripItem)reduceAllSaveToolStripMenuItem).set_Enabled(true);
				bool @checked = chkUnidentifiedDoubles.get_Checked();
				bool checked2 = chkDoublesOnly.get_Checked();
				bool checked3 = chkHighlightDoubles.get_Checked();
				if (checked3)
				{
					lstEvents.set_SelectionMode((SelectionMode)2);
					MultiSelect = true;
				}
				if (!int.TryParse(((Control)txtZC).get_Text(), out var result2))
				{
					result2 = 0;
				}
				if (!int.TryParse(((Control)txtSAO).get_Text(), out var result3))
				{
					result3 = 0;
				}
				if (!int.TryParse(((Control)txtXZ).get_Text(), out var result4))
				{
					result4 = 0;
				}
				if (!int.TryParse(((Control)txtPlanet).get_Text(), out var result5))
				{
					result5 = 0;
				}
				pbarStar.set_Value(0);
				pbarStar.set_Maximum(100);
				((Control)pbarStar).set_Visible(true);
				for (int i = 0; i < HistoryFiles.Count; i++)
				{
					pbarStar.set_Value(100 * i / HistoryFiles.Count);
					Application.DoEvents();
					using StreamReader streamReader4 = new StreamReader(AppPath + "\\Resource Files\\" + HistoryFiles[i]!.ToString());
					lstEvents.BeginUpdate();
					bool flag = false;
					do
					{
						string text = streamReader4.ReadLine();
						if (text.Substring(0, 1) == "*")
						{
							if (flag)
							{
								lstEvents.get_Items().Add((object)text);
							}
							flag = false;
							continue;
						}
						flag = false;
						if (!@checked)
						{
							if (!int.TryParse(text.Substring(19, 6), out var result6))
							{
								continue;
							}
							bool flag2 = false;
							if (text.Substring(18, 1) == "R" && result2 > 0)
							{
								flag2 = result6 == result2;
							}
							if (text.Substring(18, 1) == "S" && result3 > 0)
							{
								flag2 = result6 == result3;
							}
							if (text.Substring(18, 1) == "X" && result4 > 0)
							{
								flag2 = result6 == result4;
							}
							if (text.Substring(18, 1) == "P" && result5 > 0)
							{
								flag2 = result6 / 1000 == result5;
							}
							if ((text.Substring(18, 1) == "A") & chkAsteroid.get_Checked())
							{
								flag2 = true;
							}
							if (!flag2)
							{
								continue;
							}
							if (chkDoubleEdit.get_Checked())
							{
								int num2 = lstEvents.get_Items().Add((object)text);
								continue;
							}
							bool flag3 = (text.Substring(55, 1) == "1") | (text.Substring(46, 1) != " ") | (text.Substring(60, 1) == "5") | (text.Substring(61, 1) == "8");
							if (!checked2 || (checked2 && flag3))
							{
								int num2 = lstEvents.get_Items().Add((object)text);
								flag = true;
								if (checked3 && flag3)
								{
									lstEvents.SetSelected(num2, true);
								}
							}
						}
						else if (text.Substring(55, 1) == "1")
						{
							lstEvents.get_Items().Add((object)text);
							flag = true;
						}
					}
					while (!streamReader4.EndOfStream);
					lstEvents.EndUpdate();
				}
				((Control)pbarStar).set_Visible(false);
			}
			else if (optSearchName.get_Checked())
			{
				ObservationsDisplayed = true;
				((ToolStripItem)withListToolStripMenuItem).set_Enabled(true);
				((ToolStripItem)reduceAllSaveToolStripMenuItem).set_Enabled(true);
				string text3 = ((Control)txtName).get_Text().Trim().ToUpper();
				string text4 = ((Control)txtSite).get_Text().Trim().ToUpper();
				bool flag4 = text3.Length > 0;
				bool flag5 = text4.Length > 0;
				bool flag6 = flag4 && flag5;
				bool checked4 = chkUnidentifiedDoublesNames.get_Checked();
				bool checked5 = chkDoublesOnlyNames.get_Checked();
				bool checked6 = chkHighlightDoublesNames.get_Checked();
				if (checked6)
				{
					lstEvents.set_SelectionMode((SelectionMode)2);
					MultiSelect = true;
				}
				pbarStar.set_Value(0);
				pbarStar.set_Maximum(100);
				((Control)pbarStar).set_Visible(true);
				for (int j = 0; j < HistoryFiles.Count; j++)
				{
					pbarStar.set_Value(100 * j / HistoryFiles.Count);
					Application.DoEvents();
					using StreamReader streamReader5 = new StreamReader(AppPath + "\\Resource Files\\" + HistoryFiles[j]!.ToString());
					lstEvents.BeginUpdate();
					bool flag7 = false;
					do
					{
						string text = streamReader5.ReadLine();
						if (text.Substring(0, 1) == "*")
						{
							if (flag7)
							{
								lstEvents.get_Items().Add((object)text);
							}
							flag7 = false;
							continue;
						}
						flag7 = false;
						if (!checked4)
						{
							bool flag8 = false;
							if (flag6)
							{
								flag8 = text.Substring(127, 50).ToUpper().Contains(text4) & text.Substring(188, 25).ToUpper().Contains(text3);
							}
							else if (flag4)
							{
								flag8 = text.Substring(188, 25).ToUpper().Contains(text3);
							}
							else if (flag5)
							{
								flag8 = text.Substring(127, 50).ToUpper().Contains(text4);
							}
							if (!flag8)
							{
								continue;
							}
							bool flag9 = (text.Substring(55, 1) == "1") | (text.Substring(46, 1) != " ") | (text.Substring(60, 1) == "5") | (text.Substring(61, 1) == "8");
							if (!checked5 || (checked5 && flag9))
							{
								int num3 = lstEvents.get_Items().Add((object)text);
								flag7 = true;
								if (checked6 && flag9)
								{
									lstEvents.SetSelected(num3, true);
								}
							}
						}
						else if (text.Substring(55, 1) == "1")
						{
							lstEvents.get_Items().Add((object)text);
							flag7 = true;
						}
					}
					while (!streamReader5.EndOfStream);
					lstEvents.EndUpdate();
				}
				((Control)pbarStar).set_Visible(false);
			}
			ListBox obj = lstReduction;
			bool observationsDisplayed;
			((Control)grpAdjustments).set_Enabled(observationsDisplayed = ObservationsDisplayed);
			((Control)obj).set_Enabled(observationsDisplayed);
			Button obj2 = cmdCreateReportFromSite;
			((Control)cmdMergeReportFromSite).set_Enabled(observationsDisplayed = !ObservationsDisplayed);
			((Control)obj2).set_Enabled(observationsDisplayed);
			Button obj3 = cmdCreateReportForlocation;
			((Control)cmdMergeReportForLocation).set_Enabled(observationsDisplayed = ObservationsDisplayed);
			((Control)obj3).set_Enabled(observationsDisplayed);
			Label labelRightClick = LabelRightClick;
			((ToolStripItem)displayEventAgainstProfileToolStripMenuItem).set_Visible(observationsDisplayed = true);
			((Control)labelRightClick).set_Visible(observationsDisplayed);
			((Control)lstEvents).Focus();
			((Control)this).set_Cursor(Cursors.get_Default());
		}

		private string GetRGOSiteStatistics(int Site)
		{
			FileStream fileStream = new FileStream(AppPath + "\\Resource Files\\RGO Site Statistics.dat", FileMode.Open, FileAccess.Read);
			BinaryReader binaryReader = new BinaryReader(fileStream);
			int num = 0;
			int num2 = (int)(fileStream.Length / 10) - 1;
			string result = "".PadRight(15);
			do
			{
				int num3 = (num2 + num) / 2;
				fileStream.Seek(10 * num3, SeekOrigin.Begin);
				int num4 = binaryReader.ReadInt32();
				if (num4 == Site)
				{
					short num5 = binaryReader.ReadInt16();
					short num6 = binaryReader.ReadInt16();
					short num7 = binaryReader.ReadInt16();
					result = string.Format("{0,5}{1,5}-{2,4}", num5, num6, num7);
					break;
				}
				if (num4 > Site)
				{
					num2 = num3 - 1;
				}
				else
				{
					num = num3 + 1;
				}
			}
			while (num <= num2);
			fileStream.Close();
			return result;
		}

		private string GetILOCSiteStatistics(string Site)
		{
			FileStream fileStream = new FileStream(AppPath + "\\Resource Files\\ILOC Site Statistics.dat", FileMode.Open, FileAccess.Read);
			BinaryReader binaryReader = new BinaryReader(fileStream);
			int num = 0;
			int num2 = (int)(fileStream.Length / 15) - 1;
			string result = "".PadRight(15);
			do
			{
				int num3 = (num2 + num) / 2;
				fileStream.Seek(15 * num3, SeekOrigin.Begin);
				string text = new string(binaryReader.ReadChars(9));
				if (text == Site)
				{
					short num4 = binaryReader.ReadInt16();
					short num5 = binaryReader.ReadInt16();
					short num6 = binaryReader.ReadInt16();
					result = string.Format("{0,5}{1,5}-{2,4}", num4, num5, num6);
					break;
				}
				if (text.CompareTo(Site) > 0)
				{
					num2 = num3 - 1;
				}
				else
				{
					num = num3 + 1;
				}
			}
			while (num <= num2);
			fileStream.Close();
			return result;
		}

		private void lstEvents_SelectedIndexChanged(object sender, EventArgs e)
		{
			if (CountlstEventsHighlighted() == 1)
			{
				GetDataToReduceLine(((ListControl)lstEvents).get_SelectedIndex(), ValidEventsOnly: false);
			}
		}

		private void lstEvents_MouseDown(object sender, MouseEventArgs e)
		{
			//IL_001d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0027: Invalid comparison between Unknown and I4
			//IL_0071: Unknown result type (might be due to invalid IL or missing references)
			//IL_007b: Invalid comparison between Unknown and I4
			if (lstEvents.get_Items().get_Count() < 1)
			{
				return;
			}
			if (!MultiSelect)
			{
				if ((int)e.get_Button() == 2097152)
				{
					int num = lstEvents.IndexFromPoint(e.get_X(), e.get_Y());
					if (num >= 0 && num < lstEvents.get_Items().get_Count())
					{
						((ListControl)lstEvents).set_SelectedIndex(num);
					}
					((Control)lstEvents).Refresh();
				}
			}
			else if ((int)e.get_Button() == 1048576)
			{
				int num2 = lstEvents.IndexFromPoint(e.get_X(), e.get_Y());
				if (num2 >= 0 && num2 < lstEvents.get_Items().get_Count())
				{
					GetDataToReduceLine(num2, ValidEventsOnly: false);
				}
			}
		}

		private bool GetDataToReduceLine(int LineNumber, bool ValidEventsOnly)
		{
			if (LineNumber < 0)
			{
				return false;
			}
			string text = lstEvents.get_Items().get_Item(LineNumber).ToString();
			if (ObservationsDisplayed)
			{
				if (ValidEventsOnly)
				{
					if ("DRBF".IndexOf(text.Substring(26, 1)) < 0)
					{
						return false;
					}
					if ("DIK".IndexOf(text.Substring(72, 1)) >= 0)
					{
						return false;
					}
					if ("TUWXYZ".IndexOf(text.Substring(73, 1)) >= 0)
					{
						return false;
					}
				}
				if (!decimal.TryParse(text.Substring(0, 4), out var result))
				{
					result = 2000m;
				}
				updnYear.set_Value(result);
				if (!decimal.TryParse(text.Substring(4, 2), out result))
				{
					result = 1m;
				}
				if (result < 1m)
				{
					result = 1m;
				}
				updnMonth.set_Value(result);
				if (!decimal.TryParse(text.Substring(6, 2), out result))
				{
					result = 1m;
				}
				if (result < 1m)
				{
					result = 1m;
				}
				updnDay.set_Value(result);
				if (!decimal.TryParse(text.Substring(8, 2), out result))
				{
					result = default(decimal);
				}
				updnHour.set_Value(result);
				if (!decimal.TryParse(text.Substring(10, 2), out result))
				{
					result = default(decimal);
				}
				updnMinute.set_Value(result);
				if (!decimal.TryParse(text.Substring(12, 6), out result))
				{
					result = default(decimal);
				}
				updnSecond.set_Value(result);
				if (text.Substring(18, 1) == "R")
				{
					optZC.set_Checked(true);
				}
				else if (text.Substring(18, 1) == "S")
				{
					optSAO.set_Checked(true);
				}
				else if (text.Substring(18, 1) == "P")
				{
					optPlanet.set_Checked(true);
				}
				else if (text.Substring(18, 1) == "A")
				{
					optAsteroid.set_Checked(true);
				}
				else
				{
					optXZ.set_Checked(true);
				}
				updnPE.set_Value(0m);
				if (text.Substring(33, 1) == "X")
				{
					updnPE.set_Value(0.99m);
					if ((text.Substring(26, 1) == "D") | (text.Substring(26, 1) == "B") | (text.Substring(26, 1) == "F"))
					{
						updnPE.set_Value(0.48m);
					}
				}
				else if (text.Substring(33, 1) == "U")
				{
					if (!double.TryParse(text.Substring(29, 4), out var result2))
					{
						result2 = 0.0;
					}
					updnPE.set_Value((decimal)result2);
				}
				((Control)txtStar).set_Text(text.Substring(19, 6));
				ReduceLine(LineNumber, ForSave: false);
			}
			else
			{
				if (RGOSiteDisplayed)
				{
					SiteID = text.Substring(0, 6).Remove(3, 1);
				}
				else
				{
					SiteID = text.Substring(0, 9);
				}
				((Control)cmdCreateReportFromSite).set_Text("Create report for \r\n" + SiteID);
				((Control)cmdMergeReportFromSite).set_Text("Merge report for \r\n" + SiteID);
			}
			return true;
		}

		private void cmdAdjustedReduce_Click(object sender, EventArgs e)
		{
			ReduceLine(((ListControl)lstEvents).get_SelectedIndex(), ForSave: false);
		}

		private string ReduceLine(int LineNumber, bool ForSave)
		{
			JD_atEvent = Utilities.JD_from_Date((int)updnYear.get_Value(), (int)updnMonth.get_Value(), (double)updnDay.get_Value());
			JD_atEvent += ((double)updnHour.get_Value() + (double)updnMinute.get_Value() / 60.0 + ((double)updnSecond.get_Value() - (double)updnPE.get_Value()) / 3600.0) / 24.0;
			string text = (optZC.get_Checked() ? "R" : (optSAO.get_Checked() ? "S" : (optPlanet.get_Checked() ? "P" : ((!optAsteroid.get_Checked()) ? "X" : "A"))));
			if (!int.TryParse(((Control)txtStar).get_Text(), out var result))
			{
				return Utilities.Date_from_JD(JD_atEvent) + " " + text + ((Control)txtStar).get_Text() + "  Invald star identifier";
			}
			string text2 = lstEvents.get_Items().get_Item(LineNumber).ToString();
			string text3 = text2.Substring(25, 1);
			if (chkDoubleEdit.get_Checked())
			{
				text3 = " ";
			}
			if (!int.TryParse(text2.Substring(81, 4).Replace("-", "").Replace("+", ""), out var result2))
			{
				result2 = 0;
			}
			if (!int.TryParse(text2.Substring(85, 2), out var result3))
			{
				result3 = 0;
			}
			if (!double.TryParse(text2.Substring(87, 5), out var result4))
			{
				result4 = 0.0;
			}
			double num = ((double)result2 + (double)result3 / 60.0 + result4 / 3600.0) / (180.0 / Math.PI);
			if (text2.Substring(81, 4).Contains("-"))
			{
				num = 0.0 - num;
			}
			if (!int.TryParse(text2.Substring(92, 3).Replace("-", "").Replace("+", ""), out var result5))
			{
				result5 = 0;
			}
			if (!int.TryParse(text2.Substring(95, 2), out var result6))
			{
				result6 = 0;
			}
			if (!double.TryParse(text2.Substring(97, 5), out var result7))
			{
				result7 = 0.0;
			}
			double num2 = ((double)result5 + (double)result6 / 60.0 + result7 / 3600.0) / (180.0 / Math.PI);
			if (text2.Substring(92, 3).Contains("-"))
			{
				num2 = 0.0 - num2;
			}
			if (!int.TryParse(text2.Substring(102, 2), out var result8))
			{
				result8 = 0;
			}
			if (result8 > 0 && result8 < 6)
			{
				result8 = Utilities.RGOdatum_to_ILOCdatum(result8, num * (180.0 / Math.PI), num2 * (180.0 / Math.PI));
			}
			if (!double.TryParse(text2.Substring(104, 6), out var result9))
			{
				result9 = 0.0;
			}
			bool height_is_MSL = text2.Substring(110, 1) == "M";
			string text4 = text2.Substring(188, 20);
			if (text4.Trim().Length == 0)
			{
				text4 = "---".PadRight(20);
			}
			LunarObservations.ReduceAnObservation(JD_atEvent, num, num2, result9, height_is_MSL, result8, text, result, UseOuterLimbOfSatelliteOrAsteroid: false, PE_ReductionIfBrightStar: false, text3, ApplyLimbCorrn: true, VizierReduction: false, out var Reduction, out var Residual, out var _, out var _, out var _, out var _, out var _, out var _);
			string text5 = text4 + " " + text2.Substring(18, 8) + " " + text2.Substring(0, 4) + " " + text2.Substring(4, 2) + " " + text2.Substring(6, 2) + " " + text2.Substring(8, 2) + " " + text2.Substring(10, 2) + " " + text2.Substring(12, 6) + " " + text2.Substring(26, 3) + text2.Substring(34, 1) + text2.Substring(42, 1) + text2.Substring(73, 1) + " " + Reduction;
			LunarObservations.Residuals.Clear();
			LunarObservations.Reduction_Line = new ReductionLine();
			LunarObservations.Reduction_Line.Observer = text4;
			LunarObservations.Reduction_Line.StarCat = text;
			LunarObservations.Reduction_Line.StarNumber = result;
			LunarObservations.Reduction_Line.WDSCode = text3;
			LunarObservations.Reduction_Line.SiteCode = " ";
			LunarObservations.Reduction_Line.Year = (int)updnYear.get_Value();
			LunarObservations.Reduction_Line.Month = (int)updnMonth.get_Value();
			LunarObservations.Reduction_Line.Day = (int)updnDay.get_Value();
			LunarObservations.Reduction_Line.Hour = (int)updnHour.get_Value();
			LunarObservations.Reduction_Line.Minute = (int)updnMinute.get_Value();
			LunarObservations.Reduction_Line.Second = (double)updnSecond.get_Value();
			LunarObservations.Reduction_Line.Second_DecPlaces = 2;
			LunarObservations.Reduction_Line.EventPhase = text2.Substring(26, 1);
			LunarObservations.Reduction_Line.EventLimb = text2.Substring(27, 1);
			LunarObservations.Reduction_Line.DoubleCode = text2.Substring(46, 1);
			if (!int.TryParse(text2.Substring(42, 1), out var result10))
			{
				result10 = 0;
			}
			LunarObservations.Reduction_Line.Certainty = result10;
			LunarObservations.Reduction_Line.GrazeFlag = text2.Substring(28, 1);
			LunarObservations.Reduction_Line.MethodTimeRecording = text2.Substring(34, 1);
			LunarObservations.Reduction_Line.DoubleStarComponent = text2.Substring(25, 1);
			if (!double.TryParse(text5.Substring(104, 5), out var result11))
			{
				result11 = 0.0;
			}
			LunarObservations.Reduction_Line.Scale = result11;
			LunarObservations.Reduction_Line.O_C = Residual;
			if (!double.TryParse(text5.Substring(98, 5), out result11))
			{
				result11 = 0.0;
			}
			LunarObservations.Reduction_Line.Limb = result11;
			if (!double.TryParse(text5.Substring(70, 6), out result11))
			{
				result11 = 0.0;
			}
			LunarObservations.Reduction_Line.PA = result11;
			if (!double.TryParse(text5.Substring(79, 5), out result11))
			{
				result11 = 0.0;
			}
			LunarObservations.Reduction_Line.l = result11;
			if (!double.TryParse(text5.Substring(85, 5), out result11))
			{
				result11 = 0.0;
			}
			LunarObservations.Reduction_Line.b = result11;
			if (!double.TryParse(text5.Substring(91, 6), out result11))
			{
				result11 = 0.0;
			}
			LunarObservations.Reduction_Line.AA = result11;
			if (!double.TryParse(text5.Substring(110, 6), out result11))
			{
				result11 = 0.0;
			}
			LunarObservations.Reduction_Line.P = result11;
			if (!double.TryParse(text5.Substring(117, 5), out result11))
			{
				result11 = 0.0;
			}
			LunarObservations.Reduction_Line.D = result11;
			LunarObservations.Residuals.Add(LunarObservations.Reduction_Line);
			if (!ForSave)
			{
				lstReduction.get_Items().Clear();
				lstReduction.get_Items().Add((object)"Observer             Star No.    y  m  d  h  m   s    PLGTCV     O-C    PA       l     b     AA    Limb Scale    P     D      rR    rM     pM");
				lstReduction.get_Items().Add((object)text5);
				lstReduction.get_Items().Add((object)(text2.Substring(81, 4) + " " + text2.Substring(85, 2) + " " + text2.Substring(87, 4) + "   " + text2.Substring(92, 3) + " " + text2.Substring(95, 2) + " " + text2.Substring(97, 4) + " [" + text2.Substring(102, 2) + "]  " + text2.Substring(104, 6) + "[" + text2.Substring(110, 1) + "] " + Utilities.ProperCase(text2.Substring(127, 50).Trim())));
			}
			return text5;
		}

		private void cmdCreateReportFromSite_Click(object sender, EventArgs e)
		{
			LunarObservations.CreateHistoricalFileInEditor(SiteID, RGOSiteDisplayed, "", "", "", "", Merge: false, OccultationReport.UseOldFormat);
		}

		private void cmdMergeReportFromSite_Click(object sender, EventArgs e)
		{
			LunarObservations.CreateHistoricalFileInEditor(SiteID, RGOSiteDisplayed, "", "", "", "", Merge: true, OccultationReport.UseOldFormat);
		}

		private void cmdCreateReportForlocation_Click(object sender, EventArgs e)
		{
			//IL_0019: Unknown result type (might be due to invalid IL or missing references)
			if (((ListControl)lstEvents).get_SelectedIndex() < 0)
			{
				MessageBox.Show("You must select a line of observations - to set the observer's coordinates", "No line set", (MessageBoxButtons)0);
			}
			else
			{
				LunarObservations.CreateHistoricalFileInEditor("", RGODisplayed: false, lstEvents.get_Items().get_Item(((ListControl)lstEvents).get_SelectedIndex()).ToString(), "", "", "", Merge: false, OccultationReport.UseOldFormat);
			}
		}

		private void cmdMergeReportForLocation_Click(object sender, EventArgs e)
		{
			LunarObservations.CreateHistoricalFileInEditor("", RGODisplayed: false, lstEvents.get_Items().get_Item(((ListControl)lstEvents).get_SelectedIndex()).ToString(), "", "", "", Merge: true, OccultationReport.UseOldFormat);
		}

		private void HistoricalOccultations_Resize(object sender, EventArgs e)
		{
			ListBox obj = lstEvents;
			int width;
			((Control)lstReduction).set_Width(width = ((Control)this).get_Width() - 28);
			((Control)obj).set_Width(width);
			((Control)lstEvents).set_Height(((Control)this).get_Height() - 284);
			((Control)lstReduction).set_Top(((Control)lstEvents).get_Top() + ((Control)lstEvents).get_Height() + 8);
			((Control)grpAdjustments).set_Top(((Control)lstReduction).get_Top() + ((Control)lstReduction).get_Height() + 15);
		}

		private void generateRGOStatisticsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0038: Unknown result type (might be due to invalid IL or missing references)
			//IL_003e: Invalid comparison between Unknown and I4
			short[] array = new short[100000];
			short[] array2 = new short[100000];
			short[] array3 = new short[100000];
			if ((int)MessageBox.Show("This re-creates the file giving statistic on the use of each site\r\nIt should not be necessary to use.\r\n\r\nDo you want to Continue?", "Recreate Stats", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 2)
			{
				return;
			}
			pbarRead.set_Value(0);
			((Control)pbarRead).set_Visible(true);
			for (int i = 0; i < HistoryFiles.Count; i++)
			{
				if (!(HistoryFiles[i]!.ToString()!.Substring(0, 10) == "Archive Ob"))
				{
					continue;
				}
				pbarRead.set_Value(100 * i / HistoryFiles.Count);
				Application.DoEvents();
				using StreamReader streamReader = new StreamReader(AppPath + "\\Resource Files\\" + HistoryFiles[i]!.ToString());
				do
				{
					string text = streamReader.ReadLine();
					if (!int.TryParse(text.Substring(120, 5), out var result))
					{
						result = 0;
					}
					if (result <= 0)
					{
						continue;
					}
					if (!short.TryParse(text.Substring(0, 4), out var result2))
					{
						result2 = 0;
					}
					array[result]++;
					if (array2[result] == 0)
					{
						array2[result] = result2;
						array3[result] = result2;
						continue;
					}
					if (result2 < array2[result])
					{
						array2[result] = result2;
					}
					if (result2 > array3[result])
					{
						array3[result] = result2;
					}
				}
				while (!streamReader.EndOfStream);
			}
			((Control)pbarRead).set_Visible(false);
			BinaryWriter binaryWriter = new BinaryWriter(new FileStream(AppPath + "\\Resource Files\\RGO Site Statistics.dat", FileMode.OpenOrCreate, FileAccess.Write));
			for (int j = 1; j <= 99999; j++)
			{
				if (array[j] > 0)
				{
					binaryWriter.Write(j);
					binaryWriter.Write(array[j]);
					binaryWriter.Write(array2[j]);
					binaryWriter.Write(array3[j]);
				}
			}
			binaryWriter.Close();
		}

		private void generateILOCStatisticsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0052: Unknown result type (might be due to invalid IL or missing references)
			//IL_0058: Invalid comparison between Unknown and I4
			ArrayList arrayList = new ArrayList();
			short[] array = new short[100000];
			short[] array2 = new short[100000];
			short[] array3 = new short[100000];
			_ = new char[9];
			int num = 0;
			int num2 = 0;
			int num3 = 0;
			int num4 = 0;
			if ((int)MessageBox.Show("This re-creates the file giving statistic on the use of each site\r\nIt should not be necessary to use. \r\n\r\nDo you want to Continue?", "Recreate Stats", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 2)
			{
				return;
			}
			using (StreamReader streamReader = new StreamReader(AppPath + "\\Resource Files\\Archive ILOC Sites.dat"))
			{
				do
				{
					string text = streamReader.ReadLine();
					arrayList.Add(text.Substring(0, 9));
				}
				while (!streamReader.EndOfStream);
				num = arrayList.Count;
				arrayList.Sort();
			}
			pbarRead.set_Value(0);
			((Control)pbarRead).set_Visible(true);
			for (int i = 0; i < HistoryFiles.Count; i++)
			{
				if (!(HistoryFiles[i]!.ToString()!.Substring(0, 10) == "Archive Ob"))
				{
					continue;
				}
				pbarRead.set_Value(100 * i / HistoryFiles.Count);
				Application.DoEvents();
				using StreamReader streamReader2 = new StreamReader(AppPath + "\\Resource Files\\" + HistoryFiles[i]!.ToString());
				do
				{
					string text = streamReader2.ReadLine();
					string text2 = text.Substring(111, 9);
					bool flag = false;
					num3 = 0;
					num2 = num - 1;
					do
					{
						num4 = (num2 + num3) / 2;
						if (text2 == arrayList[num4]!.ToString())
						{
							flag = true;
							break;
						}
						if (text2.CompareTo(arrayList[num4]) < 0)
						{
							num2 = num4 - 1;
						}
						else
						{
							num3 = num4 + 1;
						}
					}
					while (num3 <= num2);
					if (!flag)
					{
						continue;
					}
					if (!short.TryParse(text.Substring(0, 4), out var result))
					{
						result = 0;
					}
					array[num4]++;
					if (array2[num4] == 0)
					{
						array2[num4] = result;
						array3[num4] = result;
						continue;
					}
					if (result < array2[num4])
					{
						array2[num4] = result;
					}
					if (result > array3[num4])
					{
						array3[num4] = result;
					}
				}
				while (!streamReader2.EndOfStream);
			}
			((Control)pbarRead).set_Visible(false);
			BinaryWriter binaryWriter = new BinaryWriter(new FileStream(AppPath + "\\Resource Files\\ILOC Site Statistics.dat", FileMode.Create, FileAccess.Write));
			for (int j = 0; j <= 99999; j++)
			{
				if (array[j] > 0)
				{
					binaryWriter.Write(arrayList[j]!.ToString()!.ToCharArray());
					binaryWriter.Write(array[j]);
					binaryWriter.Write(array2[j]);
					binaryWriter.Write(array3[j]);
				}
			}
			binaryWriter.Close();
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private void createOBSFFromSourceToolStripMenuItem_Click(object sender, EventArgs e)
		{
			LunarObservations.Create_ILOC_Observers();
		}

		private void createTELFFromSourceToolStripMenuItem_Click(object sender, EventArgs e)
		{
			LunarObservations.Create_ILOC_Sites();
		}

		private void Sort_DisplaySites(int SortField)
		{
			SiteList.SortField = SortField;
			Site_List.Sort();
			lstEvents.get_Items().Clear();
			lstEvents.BeginUpdate();
			for (int i = 0; i < Site_List.Count; i++)
			{
				lstEvents.get_Items().Add((object)Site_List[i].Site);
			}
			lstEvents.EndUpdate();
		}

		private void byIDToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Sort_DisplaySites(0);
		}

		private void byLongitudeToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Sort_DisplaySites(1);
		}

		private void byLatitudeToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Sort_DisplaySites(2);
		}

		private void bySiteNameToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Sort_DisplaySites(3);
		}

		private void byObserverToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Sort_DisplaySites(4);
		}

		private void byObservationsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Sort_DisplaySites(5);
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
			string path = "";
			if (optStar.get_Checked())
			{
				path = ((((Control)txtZC).get_Text() != "0") ? ("Occultations of ZC" + ((Control)txtZC).get_Text().Trim() + ".txt") : ((!(((Control)txtSAO).get_Text() != "0")) ? ("Occultations of XZ" + ((Control)txtXZ).get_Text().Trim() + ".txt") : ("Occultations of SAO" + ((Control)txtSAO).get_Text().Trim() + ".txt")));
			}
			else if ((((Control)txtName).get_Text().Trim().Length > 0) & (((Control)txtSite).get_Text().Trim().Length > 0))
			{
				path = "NameAndSite_Obs";
			}
			else if (((Control)txtName).get_Text().Trim().Length > 0)
			{
				path = "Name_Obsns";
			}
			else if (((Control)txtSite).get_Text().Trim().Length > 0)
			{
				path = "Site_Obsns";
			}
			Settings.Default.Save_LunarResults = Output.SavePredictionText(CollectEvents(), Path.GetFileNameWithoutExtension(path), Settings.Default.Save_LunarResults);
		}

		private void openSavedListToolStripMenuItem_Click(object sender, EventArgs e)
		{
			OpenSavedList(Merge: false);
		}

		private void mergePreviouslySavedListToolStripMenuItem_Click(object sender, EventArgs e)
		{
			OpenSavedList(Merge: true);
		}

		private void OpenSavedList(bool Merge)
		{
			//IL_0000: Unknown result type (might be due to invalid IL or missing references)
			//IL_0006: Expected O, but got Unknown
			//IL_002d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0033: Invalid comparison between Unknown and I4
			OpenFileDialog val = new OpenFileDialog();
			((FileDialog)val).set_Title("Select saved list");
			((FileDialog)val).set_FileName("*.txt");
			((FileDialog)val).set_InitialDirectory(Settings.Default.Save_LunarResults);
			if ((int)((CommonDialog)val).ShowDialog() != 1)
			{
				return;
			}
			MergedList = Merge;
			if (MergedList)
			{
				lstEvents.set_Sorted(true);
			}
			if (!Merge)
			{
				lstEvents.get_Items().Clear();
			}
			using (StreamReader streamReader = new StreamReader(((FileDialog)val).get_FileName()))
			{
				do
				{
					string text = streamReader.ReadLine()!.Trim();
					if (text.Length > 50)
					{
						lstEvents.get_Items().Add((object)text);
					}
				}
				while (!streamReader.EndOfStream);
			}
			ObservationsDisplayed = true;
			if (!Merge && lstEvents.get_Items().get_Count() > 0)
			{
				string text2 = lstEvents.get_Items().get_Item(0).ToString()!.Substring(18, 1);
				string text3 = lstEvents.get_Items().get_Item(0).ToString()!.Substring(19, 6).Trim();
				switch (text2)
				{
				case "R":
					((Control)txtZC).set_Text(text3);
					break;
				case "S":
					((Control)txtSAO).set_Text(text3);
					break;
				case "X":
					((Control)txtXZ).set_Text(text3);
					break;
				}
				chkDoubleEdit.set_Checked(true);
			}
			lstEvents.set_Sorted(false);
		}

		private string CollectEvents()
		{
			StringBuilder stringBuilder = new StringBuilder();
			int count = lstEvents.get_Items().get_Count();
			for (int i = 0; i < count; i++)
			{
				stringBuilder.AppendLine(lstEvents.get_Items().get_Item(i).ToString());
			}
			stringBuilder.AppendLine("");
			return stringBuilder.ToString();
		}

		private void reduceAllSaveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ReduceAll(ValidEventsOnly: false);
		}

		private void reduceAllValidSaveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ReduceAll(ValidEventsOnly: true);
		}

		private void ReduceAll(bool ValidEventsOnly)
		{
			string text = "";
			string text2 = "";
			int num = 100000;
			string text3 = ((((Control)txtZC).get_Text() != "0") ? ("ZC" + ((Control)txtZC).get_Text().Trim()) : ((!(((Control)txtSAO).get_Text() != "0")) ? ("XZ" + ((Control)txtXZ).get_Text().Trim()) : ("SAO" + ((Control)txtSAO).get_Text().Trim())));
			if (optStar.get_Checked())
			{
				text2 = text3 + " Residuals";
			}
			else if ((((Control)txtName).get_Text().Trim().Length > 0) & (((Control)txtSite).get_Text().Trim().Length > 0))
			{
				text2 = "NameAndSite";
			}
			else if (((Control)txtName).get_Text().Trim().Length > 0)
			{
				text2 = "Name";
			}
			else if (((Control)txtSite).get_Text().Trim().Length > 0)
			{
				text2 = "Site";
			}
			if (text2 == "")
			{
				return;
			}
			int num2 = 0;
			do
			{
				text = AppPath + "\\Observations\\Results\\" + text2 + num2 + ".txt";
				if (!File.Exists(text))
				{
					break;
				}
				num2++;
			}
			while (num2 < 9999);
			if (num2 >= 9999)
			{
				return;
			}
			Cursor.set_Current(Cursors.get_WaitCursor());
			using (StreamWriter streamWriter = new StreamWriter(text))
			{
				if (optStar.get_Checked())
				{
					if (ValidEventsOnly)
					{
						streamWriter.WriteLine("Star           : " + text3 + "limited to 'valid' D, R, B & F events");
					}
					else
					{
						streamWriter.WriteLine("Star           : " + text3);
					}
				}
				else if (optSearchName.get_Checked())
				{
					if (((Control)txtName).get_Text().Trim().Length > 0)
					{
						streamWriter.WriteLine("Observed by    : " + ((Control)txtName).get_Text().Trim());
					}
					if (((Control)txtSite).get_Text().Trim().Length > 0)
					{
						streamWriter.WriteLine("Observed at    : " + ((Control)txtSite).get_Text().Trim());
					}
					if (ValidEventsOnly)
					{
						streamWriter.WriteLine("Reduction      :  - limited to 'valid' D, R, B & F events");
					}
				}
				streamWriter.WriteLine("Reduction date : " + DateTime.Now.Date.ToString("D"));
				streamWriter.WriteLine("Ephemeris      : " + Utilities.EphemerisBasis());
				if (Utilities.LOLAFileExists)
				{
					streamWriter.WriteLine("Limb basis     : LRO Lunar Orbiter Laser Altimeter [LOLA]");
				}
				else
				{
					streamWriter.WriteLine("Limb basis     : None");
				}
				streamWriter.WriteLine("O-C basis      : limb correction applied");
				streamWriter.WriteLine("");
				streamWriter.WriteLine("ref Observer             Star No.    y  m  d  h  m   s    PLGTCV     O-C    PA       l     b     AA    limb scale    P     D      rR    rM     pM");
				pbarStar.set_Value(0);
				pbarStar.set_Maximum(lstEvents.get_Items().get_Count());
				((Control)pbarStar).set_Visible(true);
				for (int i = 0; i < lstEvents.get_Items().get_Count(); i++)
				{
					if (lstEvents.get_Items().get_Item(i).ToString()!.Substring(0, 1) == "*")
					{
						streamWriter.WriteLine(lstEvents.get_Items().get_Item(i).ToString());
						continue;
					}
					if (GetDataToReduceLine(i, ValidEventsOnly))
					{
						num++;
						streamWriter.WriteLine(num.ToString().Substring(3, 3) + " " + ReduceLine(i, ForSave: true));
						pbarStar.set_Value(i);
					}
					Application.DoEvents();
				}
				((Control)pbarStar).set_Visible(false);
				streamWriter.WriteLine("");
				streamWriter.WriteLine("");
				streamWriter.WriteLine("Explanation of columns 'PLGTCV'");
				streamWriter.WriteLine("P - Phase of the event. D = disappear, R = reappear, B = blink, F = flash, M = Miss");
				streamWriter.WriteLine("L - Limb. D = dark limb, B = bright limb, U = in umbra of lunar eclipse");
				streamWriter.WriteLine("G - G if the event is during a graze");
				streamWriter.WriteLine("T - Method of timing and recording. Main types are:");
				streamWriter.WriteLine("     G = video with time insertion, V = video with other time linking");
				streamWriter.WriteLine("     S = visual using a stopwatch, T = visual using a tape recorder, E = eye/ear");
				streamWriter.WriteLine("C - Certainty. 1 = certain, 2 = may be spurious, 3 = most likely spurious");
				streamWriter.WriteLine("V - Observation validity. A to G, valid after adjustments were made");
				streamWriter.WriteLine("     S to Z, invalid observation or unresolved problems.");
			}
			Cursor.set_Current(Cursors.get_Default());
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Lunar Occultations - Historical occultations");
		}

		private void cmdReadFile_Click(object sender, EventArgs e)
		{
			if (FormCreated)
			{
				((Control)lstEvents).set_Visible(true);
				((Control)grpDoubleEditor).set_Visible(false);
				MergedList = false;
				cmdRead_Click(sender, e);
				Label labelRightClick = LabelRightClick;
				bool visible;
				((ToolStripItem)displayEventAgainstProfileToolStripMenuItem).set_Visible(visible = false);
				((Control)labelRightClick).set_Visible(visible);
			}
		}

		private void displayEventAgainstProfileToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ReductionProfile.Show_ReductionProfile(0);
		}

		private void cmdReadNames_Click(object sender, EventArgs e)
		{
			if (FormCreated)
			{
				((Control)lstEvents).set_Visible(true);
				((Control)grpDoubleEditor).set_Visible(false);
				MergedList = false;
				cmdRead_Click(sender, e);
				Label labelRightClick = LabelRightClick;
				bool visible;
				((ToolStripItem)displayEventAgainstProfileToolStripMenuItem).set_Visible(visible = false);
				((Control)labelRightClick).set_Visible(visible);
			}
		}

		private int CountlstEventsHighlighted()
		{
			int num = 0;
			for (int num2 = lstEvents.get_Items().get_Count() - 1; num2 >= 0; num2--)
			{
				try
				{
					if (lstEvents.GetSelected(num2))
					{
						num++;
					}
				}
				catch
				{
				}
				if (num > 2)
				{
					break;
				}
			}
			return num;
		}

		private void cmdMoveUp_Click(object sender, EventArgs e)
		{
			if (lstEvents.get_Items().get_Count() >= 3 && CountlstEventsHighlighted() == 1)
			{
				int selectedIndex = ((ListControl)lstEvents).get_SelectedIndex();
				if (selectedIndex >= 1)
				{
					string text = lstEvents.get_Items().get_Item(selectedIndex).ToString();
					lstEvents.get_Items().set_Item(selectedIndex, lstEvents.get_Items().get_Item(selectedIndex - 1));
					lstEvents.get_Items().set_Item(selectedIndex - 1, (object)text);
					lstEvents.SetSelected(selectedIndex, false);
					lstEvents.SetSelected(selectedIndex - 1, true);
				}
			}
		}

		private void cmdMoveDown_Click(object sender, EventArgs e)
		{
			if (lstEvents.get_Items().get_Count() >= 3 && CountlstEventsHighlighted() == 1)
			{
				int selectedIndex = ((ListControl)lstEvents).get_SelectedIndex();
				if (selectedIndex < lstEvents.get_Items().get_Count())
				{
					string text = lstEvents.get_Items().get_Item(selectedIndex).ToString();
					lstEvents.get_Items().set_Item(selectedIndex, lstEvents.get_Items().get_Item(selectedIndex + 1));
					lstEvents.get_Items().set_Item(selectedIndex + 1, (object)text);
					lstEvents.SetSelected(selectedIndex, false);
					lstEvents.SetSelected(selectedIndex + 1, true);
				}
			}
		}

		private void cmdDeleteChecked_Click(object sender, EventArgs e)
		{
			DeleteHighlighted(Confirm: true);
		}

		private void DeleteHighlighted(bool Confirm)
		{
			//IL_001a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0020: Invalid comparison between Unknown and I4
			if (Confirm && (int)MessageBox.Show("This will delete all checked lines. \r\n\r\nDo you want to continue?", "confirm delete", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
			{
				return;
			}
			for (int num = lstEvents.get_Items().get_Count() - 1; num >= 0; num--)
			{
				if (lstEvents.GetSelected(num))
				{
					lstEvents.get_Items().RemoveAt(num);
				}
			}
		}

		private void chkDoubleEdit_CheckedChanged(object sender, EventArgs e)
		{
			((Control)grpDoubleEditor).set_Visible(chkDoubleEdit.get_Checked());
		}

		private void cmdHighlightDs_Click(object sender, EventArgs e)
		{
			if (lstEvents.get_Items().get_Count() < 3)
			{
				return;
			}
			for (int i = 0; i < lstEvents.get_Items().get_Count(); i++)
			{
				if (lstEvents.get_Items().get_Item(i).ToString()!.Substring(26, 1) == "D")
				{
					lstEvents.SetSelected(i, true);
				}
				else
				{
					lstEvents.SetSelected(i, false);
				}
			}
		}

		private void cmdWithin15Secs_Click(object sender, EventArgs e)
		{
			double num = 0.0;
			if (lstEvents.get_Items().get_Count() < 3)
			{
				return;
			}
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			lstEvents.BeginUpdate();
			for (int i = 0; i < lstEvents.get_Items().get_Count(); i++)
			{
				if (lstEvents.get_Items().get_Item(i).ToString()!.Substring(28, 1) == "G")
				{
					lstEvents.SetSelected(i, true);
				}
				else
				{
					lstEvents.SetSelected(i, false);
				}
			}
			DeleteHighlighted(Confirm: false);
			lstEvents.EndUpdate();
			Application.DoEvents();
			for (int j = 0; j < lstEvents.get_Items().get_Count(); j++)
			{
				if (lstEvents.get_Items().get_Item(j).ToString()!.Substring(27, 1) == "B")
				{
					lstEvents.SetSelected(j, true);
				}
				else
				{
					lstEvents.SetSelected(j, false);
				}
			}
			Sort_DisplayEvents(1);
			Application.DoEvents();
			lstEvents.BeginUpdate();
			for (int k = 0; k < lstEvents.get_Items().get_Count(); k++)
			{
				GetDataToReduceLine(k, ValidEventsOnly: false);
				double num2 = Utilities.JD_from_Date((int)updnYear.get_Value(), (int)updnMonth.get_Value(), (double)updnDay.get_Value());
				num2 += ((double)updnHour.get_Value() + (double)updnMinute.get_Value() / 60.0 + ((double)updnSecond.get_Value() - (double)updnPE.get_Value()) / 3600.0) / 24.0;
				if (k > 0 && Math.Abs(num - num2) < 0.00017361111111111112 && lstEvents.get_Items().get_Item(k - 1).ToString()!.Substring(81, 11) == lstEvents.get_Items().get_Item(k).ToString()!.Substring(81, 11))
				{
					lstEvents.SetSelected(k - 1, true);
					lstEvents.SetSelected(k, true);
				}
				num = num2;
			}
			for (int l = 0; l < lstEvents.get_Items().get_Count(); l++)
			{
				if (lstEvents.GetSelected(l))
				{
					lstEvents.SetSelected(l, false);
				}
				else
				{
					lstEvents.SetSelected(l, true);
				}
			}
			DeleteHighlighted(Confirm: false);
			lstEvents.EndUpdate();
			((Control)this).set_Cursor(Cursors.get_Default());
		}

		private void cmdReverseHighlight_Click(object sender, EventArgs e)
		{
			if (lstEvents.get_Items().get_Count() < 1)
			{
				return;
			}
			for (int i = 0; i < lstEvents.get_Items().get_Count(); i++)
			{
				if (lstEvents.GetSelected(i))
				{
					lstEvents.SetSelected(i, false);
				}
				else
				{
					lstEvents.SetSelected(i, true);
				}
			}
		}

		private void cmdHighlightBrightLimb_Click(object sender, EventArgs e)
		{
			if (lstEvents.get_Items().get_Count() < 3)
			{
				return;
			}
			for (int i = 0; i < lstEvents.get_Items().get_Count(); i++)
			{
				if (lstEvents.get_Items().get_Item(i).ToString()!.Substring(27, 1) == "B")
				{
					lstEvents.SetSelected(i, true);
				}
				else
				{
					lstEvents.SetSelected(i, false);
				}
			}
		}

		private void ValidateDataForDoubleStarFilesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_015b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0179: Unknown result type (might be due to invalid IL or missing references)
			int num = 0;
			if (lstEvents.get_Items().get_Count() < 2)
			{
				return;
			}
			string doubleStarNumber = GetDoubleStarNumber();
			if (doubleStarNumber.Length == 0)
			{
				return;
			}
			using StreamWriter streamWriter = new StreamWriter(AppPath + "\\Observations\\Doubles\\" + doubleStarNumber + "\\Test.txt");
			for (int i = 0; i < lstEvents.get_Items().get_Count() - 2; i += 2)
			{
				bool flag = true;
				string text = lstEvents.get_Items().get_Item(i).ToString();
				string text2 = lstEvents.get_Items().get_Item(i + 1).ToString();
				flag &= text.Substring(0, 8) == text2.Substring(0, 8);
				if (!MergedList)
				{
					flag &= text.Substring(18, 7) == text2.Substring(18, 7);
				}
				flag &= text.Substring(81, 30) == text2.Substring(81, 30);
				flag &= text.Substring(127, 20) == text2.Substring(127, 20);
				if (!(flag & (text.Substring(188, 24) == text2.Substring(188, 24))))
				{
					streamWriter.WriteLine(text);
					streamWriter.WriteLine(text2);
					streamWriter.WriteLine();
					num++;
				}
			}
			if (num == 0)
			{
				MessageBox.Show("Data appears ready to generate double star files");
			}
			else
			{
				MessageBox.Show("There are " + num + " errors in the data for generating the double star files");
			}
		}

		private string GetDoubleStarNumber()
		{
			string result = "";
			if (((Control)txtZC).get_Text().Trim().Length > 0)
			{
				result = "R" + ((Control)txtZC).get_Text().Trim();
			}
			else if (((Control)txtSAO).get_Text().Trim().Length > 0)
			{
				result = "S" + ((Control)txtSAO).get_Text().Trim();
			}
			else
			{
				if (((Control)txtXZ).get_Text().Trim().Length <= 0)
				{
					return result;
				}
				result = "X" + ((Control)txtXZ).get_Text().Trim();
			}
			string path = AppPath + "\\Observations\\Doubles\\" + result;
			if (!Directory.Exists(path))
			{
				Directory.CreateDirectory(path);
			}
			return result;
		}

		private void createDoubleStarFilesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			double num = 0.0;
			double num2 = 0.0;
			double num3 = 0.0;
			double num4 = 0.0;
			double num5 = 0.0;
			double num6 = 0.0;
			double PA_atOrigin = 0.0;
			double num7 = 0.0;
			double PA_atOrigin2 = 0.0;
			double num8 = 16.0;
			double MoonRadius_Radians = 1.0;
			double l = 0.0;
			double b = 0.0;
			double num9 = 0.0;
			double num10 = 0.0;
			double num11 = 0.0;
			string text = "";
			string doubleStarNumber = GetDoubleStarNumber();
			for (int i = 0; i < lstEvents.get_Items().get_Count() - 2; i += 2)
			{
				string text2 = lstEvents.get_Items().get_Item(i).ToString();
				string text3 = lstEvents.get_Items().get_Item(i + 1).ToString();
				if (!double.TryParse(text2.Substring(82, 3), out var result))
				{
					result = 0.0;
				}
				if (!double.TryParse(text2.Substring(85, 2), out var result2))
				{
					result2 = 0.0;
				}
				if (!double.TryParse(text2.Substring(87, 5), out var result3))
				{
					result3 = 0.0;
				}
				double num12 = (result + result2 / 60.0 + result3 / 3600.0) / (180.0 / Math.PI);
				if (text2.Substring(81, 1) == "-")
				{
					num12 = 0.0 - num12;
				}
				if (!double.TryParse(text2.Substring(93, 2), out result))
				{
					result = 0.0;
				}
				if (!double.TryParse(text2.Substring(95, 2), out result2))
				{
					result2 = 0.0;
				}
				if (!double.TryParse(text2.Substring(97, 5), out result3))
				{
					result3 = 0.0;
				}
				double num13 = (result + result2 / 60.0 + result3 / 3600.0) / (180.0 / Math.PI);
				if (text2.Substring(92, 1) == "-")
				{
					num13 = 0.0 - num13;
				}
				if (!double.TryParse(text2.Substring(104, 4), out result))
				{
					result = 0.0;
				}
				double elevation_metres = (int)result;
				bool altitude_is_MSL = text2.Substring(110, 1) == "M";
				if (!double.TryParse(text2.Substring(102, 2), out result))
				{
					result = 0.0;
				}
				int num14 = (int)result;
				if (num14 > 0 && num14 < 6)
				{
					num14 = Utilities.RGOdatum_to_ILOCdatum(num14, num12 * (180.0 / Math.PI), num13 * (180.0 / Math.PI));
				}
				Utilities.EarthOrientationParameters(num, out var x, out var y, out var _);
				num12 += (x * Math.Cos(num12) - y * Math.Sin(num12)) / 3600.0 / (180.0 / Math.PI);
				num13 += (x * Math.Sin(num12) + y * Math.Cos(num12)) * Math.Tan(num13) / 3600.0 / (180.0 / Math.PI);
				num = Utilities.JD_from_Date(int.Parse(text2.Substring(0, 4)), int.Parse(text2.Substring(4, 2)), int.Parse(text2.Substring(6, 2))) + ((double)int.Parse(text2.Substring(8, 2)) + (double)int.Parse(text2.Substring(10, 2)) / 60.0 + (double.Parse(text2.Substring(12, 5)) + num3) / 3600.0) / 24.0;
				num2 = Utilities.JD_from_Date(int.Parse(text3.Substring(0, 4)), int.Parse(text3.Substring(4, 2)), int.Parse(text3.Substring(6, 2))) + ((double)int.Parse(text3.Substring(8, 2)) + (double)int.Parse(text3.Substring(10, 2)) / 60.0 + (double.Parse(text3.Substring(12, 5)) + num3) / 3600.0) / 24.0;
				bool flag = false;
				num3 = 0.0;
				if (text2.Substring(33, 1) == "U")
				{
					if (!double.TryParse(text2.Substring(29, 4), out result))
					{
						result = 0.0;
					}
					num3 = result;
				}
				else if (text2.Substring(33, 1) == "X")
				{
					num3 = ((!(text2.Substring(26, 1) == "D")) ? 0.99 : 0.48);
					flag = true;
				}
				int xZ = int.Parse(((Control)txtXZ).get_Text());
				if (!XZ80Q.Get_XZ_Star(xZ))
				{
					continue;
				}
				num4 = XZ80Q.RA_rad;
				num5 = XZ80Q.Dec_rad;
				num8 = XZ80Q.Mv;
				if (XZ80Q.Mv <= 4.0 && flag)
				{
					num += 2.3148148148148148E-06;
				}
				Utilities.ApparentStarPosition(ref num4, ref num5, XZ80Q.PMRA_rad, XZ80Q.PMDec_rad, 2000, num, use2006values_Not1976: false);
				Utilities.TopocentricMoon(num + 0.00011574074074074075, num12, num13, elevation_metres, altitude_is_MSL, num14, out var RA, out var Dec, out MoonRadius_Radians, out var MoonScale, out l, out b, out var C, out var MoonAlt_Deg);
				Utilities.Distance(RA, Dec, num4, num5, out var Distance, out PA_atOrigin2);
				Utilities.TopocentricMoon(num, num12, num13, elevation_metres, altitude_is_MSL, num14, out RA, out Dec, out MoonRadius_Radians, out MoonScale, out l, out b, out C, out MoonAlt_Deg);
				Utilities.Distance(RA, Dec, num4, num5, out var Distance2, out PA_atOrigin);
				num9 = PA_atOrigin * (180.0 / Math.PI) - C;
				double num15 = ((!Utilities.LOLAFileExists) ? 0.0 : (Utilities.LOLA_LimbHeight_ActualDistance(num9, l, b, MoonScale) / MoonScale));
				num6 = (Distance2 - MoonRadius_Radians) * (180.0 / Math.PI) * 3600.0 - MoonScale * num15;
				num7 = (Distance - MoonRadius_Radians) * (180.0 / Math.PI) * 3600.0 - MoonScale * num15;
				num11 = Utilities.Altitude(num, num4, num5, num12, num13) * (180.0 / Math.PI);
				Utilities.QuickPlanet(num, 3, EquinoxOfDate: true, out var RA2, out var Dec2, out var _);
				double num16 = RA2 - RA;
				if (num16 > Math.PI)
				{
					num16 -= Math.PI * 2.0;
				}
				if (num16 < -Math.PI)
				{
					num16 += Math.PI * 2.0;
				}
				double y2 = Math.Cos(Dec2) * Math.Sin(num16);
				double x2 = Math.Sin(Dec2) * Math.Cos(Dec) - Math.Cos(Dec2) * Math.Sin(Dec) * Math.Cos(num16);
				double num17;
				for (num17 = Math.Atan2(y2, x2) * (180.0 / Math.PI) - 90.0; num17 > 90.0; num17 -= 180.0)
				{
				}
				for (; num17 < -90.0; num17 += 180.0)
				{
				}
				double num18 = (double)Math.Sign(num16) * (num17 - PA_atOrigin * (180.0 / Math.PI));
				if (num18 > 180.0)
				{
					num18 -= 360.0;
				}
				if (num18 < -180.0)
				{
					num18 += 360.0;
				}
				text = "N";
				if (Math.Abs(num18) > 90.0)
				{
					num18 = (double)Math.Sign(num18) * (180.0 - Math.Abs(num18));
					text = "S";
				}
				text = $"{num18:0;-0}" + text;
				double num19 = Math.Sin(Dec2) * Math.Sin(Dec) + Math.Cos(Dec2) * Math.Cos(Dec) * Math.Cos(num16);
				num10 = 100.0 * (0.5 - num19 / 2.0);
				double num20 = (num7 - num6) / 10.0;
				double num21 = PA_atOrigin2 - PA_atOrigin;
				if (num21 > 3.0)
				{
					num21 -= Math.PI * 2.0;
				}
				if (num21 < -3.0)
				{
					num21 += Math.PI * 2.0;
				}
				double num22 = Math.Atan2(Math.Abs(MoonRadius_Radians * (180.0 / Math.PI) * 3600.0 * num21 / 10.0), Math.Abs(num20)) * (180.0 / Math.PI);
				if (num20 > 0.0)
				{
					num22 = 180.0 - num22;
				}
				if (num21 > 0.0)
				{
					num22 = 0.0 - num22;
				}
				if (num9 < 0.0)
				{
					num9 += 360.0;
				}
				if (num9 >= 360.0)
				{
					num9 -= 360.0;
				}
				LunarObservations.Show_DoubleStarReport();
				LunarObservations.DoubleStarReport.Observer = text2.Substring(188, 25).Trim();
				LunarObservations.DoubleStarReport.Site = text2.Substring(181, 3).Trim() + "cm at " + text2.Substring(81, 20).Insert(16, " ").Insert(14, " ")
					.Insert(11, "  ")
					.Insert(6, " ")
					.Insert(4, " ");
				((Control)LunarObservations.DoubleStarReport.txtMag1).set_Text(string.Format("{0,1:F2}", num8));
				((Control)LunarObservations.DoubleStarReport.lblEvent).set_Text(text2.Substring(26, 1) + " " + text2.Substring(27, 1));
				((Control)LunarObservations.DoubleStarReport.txtPA).set_Text(string.Format("{0,1:F3}", PA_atOrigin * (180.0 / Math.PI)));
				((Control)LunarObservations.DoubleStarReport.txtAA).set_Text(string.Format("{0,1:F3}", num9));
				((Control)LunarObservations.DoubleStarReport.txtCA).set_Text(text);
				((Control)LunarObservations.DoubleStarReport.txtIllum).set_Text(string.Format("{0,1:F0}%", num10));
				((Control)LunarObservations.DoubleStarReport.txtL).set_Text(string.Format("{0,1:F3}", l));
				((Control)LunarObservations.DoubleStarReport.txtB).set_Text(string.Format("{0,1:F3}", b));
				((Control)LunarObservations.DoubleStarReport.txtAlt).set_Text(string.Format("{0,1:F0}", num11));
				((Control)LunarObservations.DoubleStarReport.txtRV).set_Text(string.Format("{0,1:F4}", Math.Abs(num20)));
				((Control)LunarObservations.DoubleStarReport.txtMoonScale).set_Text(string.Format("{0,1:F3}", MoonScale));
				((Control)LunarObservations.DoubleStarReport.txtCCT).set_Text(string.Format("{0,1:F2}", num22));
				((Control)LunarObservations.DoubleStarReport.txtT1).set_Text(text2.Substring(12, 5));
				((Control)LunarObservations.DoubleStarReport.txtT2).set_Text(string.Format("{0,1:F2}", (num2 - num) * 86400.0));
				((Control)LunarObservations.DoubleStarReport.lblDate).set_Text(text2.Substring(0, 4) + " " + Utilities.ShortMonths[int.Parse(text2.Substring(4, 2))] + " " + text2.Substring(6, 2));
				LunarObservations.DoubleStarReport.EventDateYYYYMMDD = text2.Substring(0, 8);
				((Control)LunarObservations.DoubleStarReport.lblTime).set_Text("at " + text2.Substring(8, 2) + "h " + text2.Substring(10, 2) + "m");
				((Control)LunarObservations.DoubleStarReport.lblTime).set_Left(((Control)LunarObservations.DoubleStarReport.lblDate).get_Left() + ((Control)LunarObservations.DoubleStarReport.lblDate).get_Width());
				string text4 = "";
				if (((Control)txtZC).get_Text().Trim() != "0")
				{
					text4 = "R" + ((Control)txtZC).get_Text().Trim();
				}
				else if (((Control)txtSAO).get_Text().Trim() != "0")
				{
					text4 = "S" + ((Control)txtSAO).get_Text().Trim();
				}
				else if (((Control)txtXZ).get_Text().Trim() != "0")
				{
					text4 = "X" + ((Control)txtXZ).get_Text().Trim();
				}
				((Control)LunarObservations.DoubleStarReport.lblStar).set_Text(text4);
				string text5 = "";
				XZ80Q.Get_XZ_Star(xZ);
				if (XZ80Q.DoubleFlag != " ")
				{
					DoubleStars.GetXZDoubleList(xZ);
					for (int j = 0; j < DoubleStars.XZDoubleList.Count; j++)
					{
						text5 = text5 + "\r\n" + DoubleStars.XZDoubleList[j]!.ToString();
					}
					for (int k = 0; k < DoubleStars.XZDoubleList.Count; k++)
					{
						if (!(DoubleStars.XZDoubleList[k]!.ToString()!.Substring(0, 3) == "OCC"))
						{
							continue;
						}
						StreamReader streamReader = new StreamReader(Utilities.AppPath + "\\Resource Files\\xzdoubles discoveries.dat");
						int num23 = int.Parse(DoubleStars.XZDoubleList[k]!.ToString()!.Substring(3, 4));
						string text6;
						try
						{
							streamReader.BaseStream.Seek(151 * (num23 - 1), SeekOrigin.Begin);
							text6 = streamReader.ReadLine();
						}
						catch
						{
							text6 = "".PadRight(149);
						}
						streamReader.Close();
						string text7 = DoubleStars.XZDoubleList[k]!.ToString()!.Substring(0, 7);
						if (!int.TryParse(text6.Substring(29, 2), out var result4))
						{
							result4 = 0;
						}
						text5 = text5 + "\r\n" + text7 + " observed by " + text6.Substring(34, 24).Trim() + " on " + text6.Substring(25, 4) + " " + Utilities.ShortMonths[result4] + " " + text6.Substring(31, 2);
						StreamReader streamReader2 = new StreamReader(Utilities.AppPath + "\\Resource Files\\xzConfirmations.dat");
						do
						{
							string text8 = streamReader2.ReadLine();
							if (text8.Substring(0, 7) == text7)
							{
								if (!int.TryParse(text8.Substring(18, 2), out result4))
								{
									result4 = 0;
								}
								text5 = text5 + "\r\n        ........ by " + text8.Substring(24, 24).Trim() + " on " + text8.Substring(14, 4) + " " + Utilities.ShortMonths[result4] + " " + text8.Substring(20, 2);
							}
						}
						while (int.Parse(DoubleStars.XZDoubleList[k]!.ToString()!.Substring(3, 4)) >= num23 && !streamReader2.EndOfStream);
						streamReader2.Close();
					}
					if (DoubleStars.XZOrbitsList.Count > 0)
					{
						text5 += "\r\n\r\nPositions from orbit, on event date";
						for (int m = 0; m < DoubleStars.XZOrbitsList.Count; m++)
						{
							DoubleStars.XZOrbitsList[m].PAandSep(Utilities.BesselianYear(int.Parse(text2.Substring(0, 4)), int.Parse(text2.Substring(4, 2)), double.Parse(text2.Substring(6, 2)) + double.Parse(text2.Substring(8, 2)) / 24.0), out var PA, out var Sep);
							text5 += string.Format("\r\nPA = {0,6:F2},  Separation  = {1,2:F3}\"", PA, Sep);
						}
					}
				}
				LunarObservations.DoubleStarReport.DoubleStarDetails = text5;
				string text9 = text2.Substring(0, 8) + "_" + doubleStarNumber + "_";
				text9 = ((LunarObservations.DoubleStarReport.Observer.Length <= 0) ? (text9 + text2.Substring(127, 40).Replace("  ", " ").Replace("*", "")
					.Replace(",", "")
					.Replace(".", "_")) : (text9 + LunarObservations.DoubleStarReport.Observer.Replace("*", "").Replace(",", "").Replace(".", "_")));
				LunarObservations.DoubleStarReport.AutoSaveReport(AppPath + "\\Observations\\Doubles\\" + doubleStarNumber + "\\" + text9 + ".txt");
			}
		}

		private void byLongitudeToolStripMenuItem1_Click(object sender, EventArgs e)
		{
			Sort_DisplayEvents(1);
		}

		private void txtName_KeyDown(object sender, KeyEventArgs e)
		{
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_0008: Invalid comparison between Unknown and I4
			if ((int)e.get_KeyCode() == 13)
			{
				cmdReadNames_Click(sender, (EventArgs)(object)e);
			}
		}

		private void txtSite_KeyDown(object sender, KeyEventArgs e)
		{
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_0008: Invalid comparison between Unknown and I4
			if ((int)e.get_KeyCode() == 13)
			{
				cmdReadNames_Click(sender, (EventArgs)(object)e);
			}
		}

		private void byDateTimeToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Sort_DisplayEvents(0);
		}

		private void Sort_DisplayEvents(int SortField)
		{
			List<EventList> list = new List<EventList>();
			for (int i = 0; i < lstEvents.get_Items().get_Count(); i++)
			{
				EventList eventList = new EventList();
				eventList.Occevent = lstEvents.get_Items().get_Item(i).ToString();
				list.Add(eventList);
			}
			EventList.SortField = SortField;
			list.Sort();
			lstEvents.get_Items().Clear();
			lstEvents.BeginUpdate();
			for (int j = 0; j < list.Count; j++)
			{
				lstEvents.get_Items().Add((object)list[j].Occevent);
			}
			lstEvents.EndUpdate();
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
			//IL_001d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0027: Expected O, but got Unknown
			//IL_0028: Unknown result type (might be due to invalid IL or missing references)
			//IL_0032: Expected O, but got Unknown
			//IL_0033: Unknown result type (might be due to invalid IL or missing references)
			//IL_003d: Expected O, but got Unknown
			//IL_003e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0048: Expected O, but got Unknown
			//IL_0049: Unknown result type (might be due to invalid IL or missing references)
			//IL_0053: Expected O, but got Unknown
			//IL_0054: Unknown result type (might be due to invalid IL or missing references)
			//IL_005e: Expected O, but got Unknown
			//IL_005f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0069: Expected O, but got Unknown
			//IL_006a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0074: Expected O, but got Unknown
			//IL_0075: Unknown result type (might be due to invalid IL or missing references)
			//IL_007f: Expected O, but got Unknown
			//IL_0080: Unknown result type (might be due to invalid IL or missing references)
			//IL_008a: Expected O, but got Unknown
			//IL_008b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0095: Expected O, but got Unknown
			//IL_0096: Unknown result type (might be due to invalid IL or missing references)
			//IL_00a0: Expected O, but got Unknown
			//IL_00a1: Unknown result type (might be due to invalid IL or missing references)
			//IL_00ab: Expected O, but got Unknown
			//IL_00ac: Unknown result type (might be due to invalid IL or missing references)
			//IL_00b6: Expected O, but got Unknown
			//IL_00b7: Unknown result type (might be due to invalid IL or missing references)
			//IL_00c1: Expected O, but got Unknown
			//IL_00c2: Unknown result type (might be due to invalid IL or missing references)
			//IL_00cc: Expected O, but got Unknown
			//IL_00cd: Unknown result type (might be due to invalid IL or missing references)
			//IL_00d7: Expected O, but got Unknown
			//IL_00d8: Unknown result type (might be due to invalid IL or missing references)
			//IL_00e2: Expected O, but got Unknown
			//IL_00e3: Unknown result type (might be due to invalid IL or missing references)
			//IL_00ed: Expected O, but got Unknown
			//IL_00ee: Unknown result type (might be due to invalid IL or missing references)
			//IL_00f8: Expected O, but got Unknown
			//IL_00f9: Unknown result type (might be due to invalid IL or missing references)
			//IL_0103: Expected O, but got Unknown
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
			//IL_02fe: Unknown result type (might be due to invalid IL or missing references)
			//IL_0308: Expected O, but got Unknown
			//IL_0309: Unknown result type (might be due to invalid IL or missing references)
			//IL_0313: Expected O, but got Unknown
			//IL_0314: Unknown result type (might be due to invalid IL or missing references)
			//IL_031e: Expected O, but got Unknown
			//IL_031f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0329: Expected O, but got Unknown
			//IL_032a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0334: Expected O, but got Unknown
			//IL_0335: Unknown result type (might be due to invalid IL or missing references)
			//IL_033f: Expected O, but got Unknown
			//IL_0340: Unknown result type (might be due to invalid IL or missing references)
			//IL_034a: Expected O, but got Unknown
			//IL_034b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0355: Expected O, but got Unknown
			//IL_0356: Unknown result type (might be due to invalid IL or missing references)
			//IL_0360: Expected O, but got Unknown
			//IL_0361: Unknown result type (might be due to invalid IL or missing references)
			//IL_036b: Expected O, but got Unknown
			//IL_036c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0376: Expected O, but got Unknown
			//IL_0377: Unknown result type (might be due to invalid IL or missing references)
			//IL_0381: Expected O, but got Unknown
			//IL_0382: Unknown result type (might be due to invalid IL or missing references)
			//IL_038c: Expected O, but got Unknown
			//IL_038d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0397: Expected O, but got Unknown
			//IL_0398: Unknown result type (might be due to invalid IL or missing references)
			//IL_03a2: Expected O, but got Unknown
			//IL_03a3: Unknown result type (might be due to invalid IL or missing references)
			//IL_03ad: Expected O, but got Unknown
			//IL_03ae: Unknown result type (might be due to invalid IL or missing references)
			//IL_03b8: Expected O, but got Unknown
			//IL_03b9: Unknown result type (might be due to invalid IL or missing references)
			//IL_03c3: Expected O, but got Unknown
			//IL_03c4: Unknown result type (might be due to invalid IL or missing references)
			//IL_03ce: Expected O, but got Unknown
			//IL_03cf: Unknown result type (might be due to invalid IL or missing references)
			//IL_03d9: Expected O, but got Unknown
			//IL_03da: Unknown result type (might be due to invalid IL or missing references)
			//IL_03e4: Expected O, but got Unknown
			//IL_03e5: Unknown result type (might be due to invalid IL or missing references)
			//IL_03ef: Expected O, but got Unknown
			//IL_03f0: Unknown result type (might be due to invalid IL or missing references)
			//IL_03fa: Expected O, but got Unknown
			//IL_03fb: Unknown result type (might be due to invalid IL or missing references)
			//IL_0405: Expected O, but got Unknown
			//IL_0406: Unknown result type (might be due to invalid IL or missing references)
			//IL_0410: Expected O, but got Unknown
			//IL_0411: Unknown result type (might be due to invalid IL or missing references)
			//IL_041b: Expected O, but got Unknown
			//IL_041c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0426: Expected O, but got Unknown
			//IL_0427: Unknown result type (might be due to invalid IL or missing references)
			//IL_0431: Expected O, but got Unknown
			//IL_0432: Unknown result type (might be due to invalid IL or missing references)
			//IL_043c: Expected O, but got Unknown
			//IL_043d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0447: Expected O, but got Unknown
			//IL_0448: Unknown result type (might be due to invalid IL or missing references)
			//IL_0452: Expected O, but got Unknown
			//IL_0453: Unknown result type (might be due to invalid IL or missing references)
			//IL_045d: Expected O, but got Unknown
			//IL_045e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0468: Expected O, but got Unknown
			//IL_0469: Unknown result type (might be due to invalid IL or missing references)
			//IL_0473: Expected O, but got Unknown
			//IL_0474: Unknown result type (might be due to invalid IL or missing references)
			//IL_047e: Expected O, but got Unknown
			//IL_047f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0489: Expected O, but got Unknown
			//IL_048a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0494: Expected O, but got Unknown
			//IL_0495: Unknown result type (might be due to invalid IL or missing references)
			//IL_049f: Expected O, but got Unknown
			//IL_04a0: Unknown result type (might be due to invalid IL or missing references)
			//IL_04aa: Expected O, but got Unknown
			//IL_04ab: Unknown result type (might be due to invalid IL or missing references)
			//IL_04b5: Expected O, but got Unknown
			//IL_04b6: Unknown result type (might be due to invalid IL or missing references)
			//IL_04c0: Expected O, but got Unknown
			//IL_04c1: Unknown result type (might be due to invalid IL or missing references)
			//IL_04cb: Expected O, but got Unknown
			//IL_04cc: Unknown result type (might be due to invalid IL or missing references)
			//IL_04d6: Expected O, but got Unknown
			//IL_04d7: Unknown result type (might be due to invalid IL or missing references)
			//IL_04e1: Expected O, but got Unknown
			//IL_04e2: Unknown result type (might be due to invalid IL or missing references)
			//IL_04ec: Expected O, but got Unknown
			//IL_04ed: Unknown result type (might be due to invalid IL or missing references)
			//IL_04f7: Expected O, but got Unknown
			//IL_04f8: Unknown result type (might be due to invalid IL or missing references)
			//IL_0502: Expected O, but got Unknown
			//IL_0503: Unknown result type (might be due to invalid IL or missing references)
			//IL_050d: Expected O, but got Unknown
			//IL_050e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0518: Expected O, but got Unknown
			//IL_0519: Unknown result type (might be due to invalid IL or missing references)
			//IL_0523: Expected O, but got Unknown
			//IL_0524: Unknown result type (might be due to invalid IL or missing references)
			//IL_052e: Expected O, but got Unknown
			//IL_052f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0539: Expected O, but got Unknown
			//IL_06c0: Unknown result type (might be due to invalid IL or missing references)
			//IL_06ca: Expected O, but got Unknown
			//IL_381a: Unknown result type (might be due to invalid IL or missing references)
			//IL_3824: Expected O, but got Unknown
			//IL_3879: Unknown result type (might be due to invalid IL or missing references)
			//IL_3883: Expected O, but got Unknown
			//IL_4004: Unknown result type (might be due to invalid IL or missing references)
			//IL_400e: Expected O, but got Unknown
			components = new Container();
			lstEvents = new ListBox();
			contextMenuStrip1 = new ContextMenuStrip(components);
			displayEventAgainstProfileToolStripMenuItem = new ToolStripMenuItem();
			menuStrip1 = new MenuStrip();
			withListToolStripMenuItem = new ToolStripMenuItem();
			copyToolStripMenuItem = new ToolStripMenuItem();
			printPreviewToolStripMenuItem = new ToolStripMenuItem();
			prToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			openSavedListToolStripMenuItem = new ToolStripMenuItem();
			mergePreviouslySavedListToolStripMenuItem = new ToolStripMenuItem();
			toolStripMenuItem1 = new ToolStripSeparator();
			reduceAllValidSaveToolStripMenuItem = new ToolStripMenuItem();
			reduceAllSaveToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator2 = new ToolStripSeparator();
			doubleStarExtractionToolStripMenuItem = new ToolStripMenuItem();
			ValidateDataForDoubleStarFilesToolStripMenuItem = new ToolStripMenuItem();
			createDoubleStarFilesToolStripMenuItem = new ToolStripMenuItem();
			nameOfStarListToolStripMenuItem = new ToolStripMenuItem();
			allNamesToolStripMenuItem = new ToolStripMenuItem();
			properNamesToolStripMenuItem = new ToolStripMenuItem();
			bayerLettersToolStripMenuItem = new ToolStripMenuItem();
			brighterThanMag2ToolStripMenuItem = new ToolStripMenuItem();
			brighterThanMag3ToolStripMenuItem = new ToolStripMenuItem();
			brighterThanMag4ToolStripMenuItem = new ToolStripMenuItem();
			sortSitesToolStripMenuItem = new ToolStripMenuItem();
			byObservationsToolStripMenuItem = new ToolStripMenuItem();
			byIDToolStripMenuItem = new ToolStripMenuItem();
			byLatitudeToolStripMenuItem = new ToolStripMenuItem();
			byLongitudeToolStripMenuItem = new ToolStripMenuItem();
			byObserverToolStripMenuItem = new ToolStripMenuItem();
			bySiteNameToolStripMenuItem = new ToolStripMenuItem();
			SortEventsToolStripMenuItem = new ToolStripMenuItem();
			byLongitudeToolStripMenuItem1 = new ToolStripMenuItem();
			byDateTimeToolStripMenuItem = new ToolStripMenuItem();
			siteStatisticsToolStripMenuItem = new ToolStripMenuItem();
			tHESEAREMAINTENANCEROUTINESToolStripMenuItem = new ToolStripMenuItem();
			createTELFFromSourceToolStripMenuItem = new ToolStripMenuItem();
			createOBSFFromSourceToolStripMenuItem = new ToolStripMenuItem();
			generateILOCStatisticsToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator1 = new ToolStripSeparator();
			generateRGOStatisticsToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			cmbFile = new ComboBox();
			cmdRead = new Button();
			pbarRead = new ProgressBar();
			lstReduction = new ListBox();
			grpAdjustments = new GroupBox();
			optAsteroid = new RadioButton();
			optPlanet = new RadioButton();
			label14 = new Label();
			updnSecond = new NumericUpDown();
			label13 = new Label();
			updnPE = new NumericUpDown();
			cmdAdjustedReduce = new Button();
			optXZ = new RadioButton();
			optSAO = new RadioButton();
			optZC = new RadioButton();
			txtStar = new TextBox();
			label12 = new Label();
			label11 = new Label();
			label10 = new Label();
			label9 = new Label();
			label8 = new Label();
			label7 = new Label();
			updnDay = new NumericUpDown();
			updnHour = new NumericUpDown();
			updnMinute = new NumericUpDown();
			updnMonth = new NumericUpDown();
			updnYear = new NumericUpDown();
			txtXZ = new TextBox();
			cmbNames = new ComboBox();
			label6 = new Label();
			label5 = new Label();
			label4 = new Label();
			label3 = new Label();
			txtZC = new TextBox();
			txtSAO = new TextBox();
			panelStar = new Panel();
			chkAsteroid = new CheckBox();
			chkDoubleEdit = new CheckBox();
			txtPlanet = new TextBox();
			label15 = new Label();
			chkUnidentifiedDoubles = new CheckBox();
			chkDoublesOnly = new CheckBox();
			chkHighlightDoubles = new CheckBox();
			pbarStar = new ProgressBar();
			label2 = new Label();
			panelFile = new Panel();
			cmdReadFile = new Button();
			label1 = new Label();
			optFile = new RadioButton();
			optStar = new RadioButton();
			panel1 = new Panel();
			optSearchName = new RadioButton();
			cmdCreateReportFromSite = new Button();
			cmdMergeReportFromSite = new Button();
			LabelRightClick = new Label();
			panelName = new Panel();
			chkUnidentifiedDoublesNames = new CheckBox();
			chkDoublesOnlyNames = new CheckBox();
			cmdReadNames = new Button();
			chkHighlightDoublesNames = new CheckBox();
			progressBar1 = new ProgressBar();
			label18 = new Label();
			label19 = new Label();
			txtName = new TextBox();
			txtSite = new TextBox();
			label21 = new Label();
			grpDoubleEditor = new GroupBox();
			cmdWithin15Secs = new Button();
			cmdReverseHighlight = new Button();
			cmdHighlightBrightLimb = new Button();
			cmdDeleteChecked = new Button();
			cmdMoveDown = new Button();
			cmdHighlightDs = new Button();
			cmdMoveUp = new Button();
			cmdCreateReportForlocation = new Button();
			cmdMergeReportForLocation = new Button();
			((Control)contextMenuStrip1).SuspendLayout();
			((Control)menuStrip1).SuspendLayout();
			((Control)grpAdjustments).SuspendLayout();
			((ISupportInitialize)updnSecond).BeginInit();
			((ISupportInitialize)updnPE).BeginInit();
			((ISupportInitialize)updnDay).BeginInit();
			((ISupportInitialize)updnHour).BeginInit();
			((ISupportInitialize)updnMinute).BeginInit();
			((ISupportInitialize)updnMonth).BeginInit();
			((ISupportInitialize)updnYear).BeginInit();
			((Control)panelStar).SuspendLayout();
			((Control)panelFile).SuspendLayout();
			((Control)panel1).SuspendLayout();
			((Control)panelName).SuspendLayout();
			((Control)grpDoubleEditor).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)lstEvents).set_ContextMenuStrip(contextMenuStrip1);
			((Control)lstEvents).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstEvents).set_FormattingEnabled(true);
			lstEvents.set_HorizontalExtent(1900);
			lstEvents.set_HorizontalScrollbar(true);
			lstEvents.set_ItemHeight(14);
			((Control)lstEvents).set_Location(new Point(3, 126));
			((Control)lstEvents).set_Name("lstEvents");
			lstEvents.set_SelectionMode((SelectionMode)3);
			((Control)lstEvents).set_Size(new Size(800, 452));
			((Control)lstEvents).set_TabIndex(0);
			lstEvents.add_SelectedIndexChanged((EventHandler)lstEvents_SelectedIndexChanged);
			((Control)lstEvents).add_MouseDown(new MouseEventHandler(lstEvents_MouseDown));
			((ToolStrip)contextMenuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[1] { (ToolStripItem)displayEventAgainstProfileToolStripMenuItem });
			((Control)contextMenuStrip1).set_Name("contextMenuStrip1");
			((Control)contextMenuStrip1).set_Size(new Size(223, 26));
			((ToolStripItem)displayEventAgainstProfileToolStripMenuItem).set_Name("displayEventAgainstProfileToolStripMenuItem");
			((ToolStripItem)displayEventAgainstProfileToolStripMenuItem).set_Size(new Size(222, 22));
			((ToolStripItem)displayEventAgainstProfileToolStripMenuItem).set_Text("Display event against profile");
			((ToolStripItem)displayEventAgainstProfileToolStripMenuItem).add_Click((EventHandler)displayEventAgainstProfileToolStripMenuItem_Click);
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[7]
			{
				(ToolStripItem)withListToolStripMenuItem,
				(ToolStripItem)nameOfStarListToolStripMenuItem,
				(ToolStripItem)sortSitesToolStripMenuItem,
				(ToolStripItem)SortEventsToolStripMenuItem,
				(ToolStripItem)siteStatisticsToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(1018, 24));
			((Control)menuStrip1).set_TabIndex(1);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withListToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[13]
			{
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)printPreviewToolStripMenuItem,
				(ToolStripItem)prToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem,
				(ToolStripItem)openSavedListToolStripMenuItem,
				(ToolStripItem)mergePreviouslySavedListToolStripMenuItem,
				(ToolStripItem)toolStripMenuItem1,
				(ToolStripItem)reduceAllValidSaveToolStripMenuItem,
				(ToolStripItem)reduceAllSaveToolStripMenuItem,
				(ToolStripItem)toolStripSeparator2,
				(ToolStripItem)doubleStarExtractionToolStripMenuItem,
				(ToolStripItem)ValidateDataForDoubleStarFilesToolStripMenuItem,
				(ToolStripItem)createDoubleStarFilesToolStripMenuItem
			});
			((ToolStripItem)withListToolStripMenuItem).set_Name("withListToolStripMenuItem");
			withListToolStripMenuItem.set_ShortcutKeys((Keys)131151);
			((ToolStripItem)withListToolStripMenuItem).set_Size(new Size(87, 20));
			((ToolStripItem)withListToolStripMenuItem).set_Text("with List ...    ");
			((ToolStripItem)copyToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyToolStripMenuItem).set_Name("copyToolStripMenuItem");
			copyToolStripMenuItem.set_ShortcutKeys((Keys)131139);
			((ToolStripItem)copyToolStripMenuItem).set_Size(new Size(254, 24));
			((ToolStripItem)copyToolStripMenuItem).set_Text("&Copy");
			((ToolStripItem)copyToolStripMenuItem).add_Click((EventHandler)copyToolStripMenuItem_Click);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Image((Image)Resources.PrintPreview);
			((ToolStripItem)printPreviewToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Name("printPreviewToolStripMenuItem");
			((ToolStripItem)printPreviewToolStripMenuItem).set_Size(new Size(254, 24));
			((ToolStripItem)printPreviewToolStripMenuItem).set_Text("Print Preview");
			((ToolStripItem)printPreviewToolStripMenuItem).add_Click((EventHandler)printPreviewToolStripMenuItem_Click);
			((ToolStripItem)prToolStripMenuItem).set_Image((Image)Resources.Print);
			((ToolStripItem)prToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)prToolStripMenuItem).set_Name("prToolStripMenuItem");
			prToolStripMenuItem.set_ShortcutKeys((Keys)131152);
			((ToolStripItem)prToolStripMenuItem).set_Size(new Size(254, 24));
			((ToolStripItem)prToolStripMenuItem).set_Text("&Print");
			((ToolStripItem)prToolStripMenuItem).add_Click((EventHandler)printToolStripMenuItem_Click);
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			saveToolStripMenuItem.set_ShortcutKeys((Keys)131155);
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(254, 24));
			((ToolStripItem)saveToolStripMenuItem).set_Text("&Save");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
			((ToolStripItem)openSavedListToolStripMenuItem).set_Image((Image)Resources.openfolderHS);
			((ToolStripItem)openSavedListToolStripMenuItem).set_Name("openSavedListToolStripMenuItem");
			((ToolStripItem)openSavedListToolStripMenuItem).set_RightToLeftAutoMirrorImage(true);
			openSavedListToolStripMenuItem.set_ShortcutKeys((Keys)131151);
			((ToolStripItem)openSavedListToolStripMenuItem).set_Size(new Size(254, 24));
			((ToolStripItem)openSavedListToolStripMenuItem).set_Text("Open previously saved list");
			((ToolStripItem)openSavedListToolStripMenuItem).add_Click((EventHandler)openSavedListToolStripMenuItem_Click);
			((ToolStripItem)mergePreviouslySavedListToolStripMenuItem).set_Image((Image)Resources.openfolderHS);
			((ToolStripItem)mergePreviouslySavedListToolStripMenuItem).set_Name("mergePreviouslySavedListToolStripMenuItem");
			((ToolStripItem)mergePreviouslySavedListToolStripMenuItem).set_Size(new Size(254, 24));
			((ToolStripItem)mergePreviouslySavedListToolStripMenuItem).set_Text("Merge previously saved list");
			((ToolStripItem)mergePreviouslySavedListToolStripMenuItem).add_Click((EventHandler)mergePreviouslySavedListToolStripMenuItem_Click);
			((ToolStripItem)toolStripMenuItem1).set_Name("toolStripMenuItem1");
			((ToolStripItem)toolStripMenuItem1).set_Size(new Size(251, 6));
			((ToolStripItem)reduceAllValidSaveToolStripMenuItem).set_Name("reduceAllValidSaveToolStripMenuItem");
			reduceAllValidSaveToolStripMenuItem.set_ShortcutKeys((Keys)131153);
			((ToolStripItem)reduceAllValidSaveToolStripMenuItem).set_Size(new Size(254, 24));
			((ToolStripItem)reduceAllValidSaveToolStripMenuItem).set_Text("Reduce all valid, && Save");
			((ToolStripItem)reduceAllValidSaveToolStripMenuItem).add_Click((EventHandler)reduceAllValidSaveToolStripMenuItem_Click);
			((ToolStripItem)reduceAllSaveToolStripMenuItem).set_Name("reduceAllSaveToolStripMenuItem");
			reduceAllSaveToolStripMenuItem.set_ShortcutKeys((Keys)131154);
			((ToolStripItem)reduceAllSaveToolStripMenuItem).set_Size(new Size(254, 24));
			((ToolStripItem)reduceAllSaveToolStripMenuItem).set_Text("Reduce all && Save");
			((ToolStripItem)reduceAllSaveToolStripMenuItem).add_Click((EventHandler)reduceAllSaveToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator2).set_Name("toolStripSeparator2");
			((ToolStripItem)toolStripSeparator2).set_Size(new Size(251, 6));
			((ToolStripItem)doubleStarExtractionToolStripMenuItem).set_Enabled(false);
			((ToolStripItem)doubleStarExtractionToolStripMenuItem).set_Font(new Font("Segoe UI", 10f, FontStyle.Bold));
			((ToolStripItem)doubleStarExtractionToolStripMenuItem).set_Name("doubleStarExtractionToolStripMenuItem");
			((ToolStripItem)doubleStarExtractionToolStripMenuItem).set_Size(new Size(254, 24));
			((ToolStripItem)doubleStarExtractionToolStripMenuItem).set_Text("Double star extraction");
			((ToolStripItem)ValidateDataForDoubleStarFilesToolStripMenuItem).set_Name("ValidateDataForDoubleStarFilesToolStripMenuItem");
			((ToolStripItem)ValidateDataForDoubleStarFilesToolStripMenuItem).set_Size(new Size(254, 24));
			((ToolStripItem)ValidateDataForDoubleStarFilesToolStripMenuItem).set_Text("Validate data");
			((ToolStripItem)ValidateDataForDoubleStarFilesToolStripMenuItem).add_Click((EventHandler)ValidateDataForDoubleStarFilesToolStripMenuItem_Click);
			((ToolStripItem)createDoubleStarFilesToolStripMenuItem).set_Name("createDoubleStarFilesToolStripMenuItem");
			((ToolStripItem)createDoubleStarFilesToolStripMenuItem).set_Size(new Size(254, 24));
			((ToolStripItem)createDoubleStarFilesToolStripMenuItem).set_Text("Create double star files");
			((ToolStripItem)createDoubleStarFilesToolStripMenuItem).add_Click((EventHandler)createDoubleStarFilesToolStripMenuItem_Click);
			((ToolStripDropDownItem)nameOfStarListToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[6]
			{
				(ToolStripItem)allNamesToolStripMenuItem,
				(ToolStripItem)properNamesToolStripMenuItem,
				(ToolStripItem)bayerLettersToolStripMenuItem,
				(ToolStripItem)brighterThanMag2ToolStripMenuItem,
				(ToolStripItem)brighterThanMag3ToolStripMenuItem,
				(ToolStripItem)brighterThanMag4ToolStripMenuItem
			});
			((ToolStripItem)nameOfStarListToolStripMenuItem).set_Name("nameOfStarListToolStripMenuItem");
			((ToolStripItem)nameOfStarListToolStripMenuItem).set_Size(new Size(133, 20));
			((ToolStripItem)nameOfStarListToolStripMenuItem).set_Text("'Name of Star' list...    ");
			((ToolStripItem)allNamesToolStripMenuItem).set_Name("allNamesToolStripMenuItem");
			((ToolStripItem)allNamesToolStripMenuItem).set_Size(new Size(213, 22));
			((ToolStripItem)allNamesToolStripMenuItem).set_Text("All names");
			((ToolStripItem)allNamesToolStripMenuItem).add_Click((EventHandler)allNamesToolStripMenuItem_Click);
			((ToolStripItem)properNamesToolStripMenuItem).set_Name("properNamesToolStripMenuItem");
			((ToolStripItem)properNamesToolStripMenuItem).set_Size(new Size(213, 22));
			((ToolStripItem)properNamesToolStripMenuItem).set_Text("Proper names");
			((ToolStripItem)properNamesToolStripMenuItem).add_Click((EventHandler)properNamesToolStripMenuItem_Click);
			((ToolStripItem)bayerLettersToolStripMenuItem).set_Name("bayerLettersToolStripMenuItem");
			((ToolStripItem)bayerLettersToolStripMenuItem).set_Size(new Size(213, 22));
			((ToolStripItem)bayerLettersToolStripMenuItem).set_Text("Bayer letters");
			((ToolStripItem)bayerLettersToolStripMenuItem).add_Click((EventHandler)bayerLettersToolStripMenuItem_Click);
			((ToolStripItem)brighterThanMag2ToolStripMenuItem).set_Name("brighterThanMag2ToolStripMenuItem");
			((ToolStripItem)brighterThanMag2ToolStripMenuItem).set_Size(new Size(213, 22));
			((ToolStripItem)brighterThanMag2ToolStripMenuItem).set_Text("Magnitude brighter than 2");
			((ToolStripItem)brighterThanMag2ToolStripMenuItem).add_Click((EventHandler)brighterThanMag2ToolStripMenuItem_Click);
			((ToolStripItem)brighterThanMag3ToolStripMenuItem).set_Name("brighterThanMag3ToolStripMenuItem");
			((ToolStripItem)brighterThanMag3ToolStripMenuItem).set_Size(new Size(213, 22));
			((ToolStripItem)brighterThanMag3ToolStripMenuItem).set_Text("Magnitude brighter than 3");
			((ToolStripItem)brighterThanMag3ToolStripMenuItem).add_Click((EventHandler)brighterThanMag3ToolStripMenuItem_Click);
			((ToolStripItem)brighterThanMag4ToolStripMenuItem).set_Name("brighterThanMag4ToolStripMenuItem");
			((ToolStripItem)brighterThanMag4ToolStripMenuItem).set_Size(new Size(213, 22));
			((ToolStripItem)brighterThanMag4ToolStripMenuItem).set_Text("Magnitude brighter than 4");
			((ToolStripItem)brighterThanMag4ToolStripMenuItem).add_Click((EventHandler)brighterThanMag4ToolStripMenuItem_Click);
			((ToolStripDropDownItem)sortSitesToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[6]
			{
				(ToolStripItem)byObservationsToolStripMenuItem,
				(ToolStripItem)byIDToolStripMenuItem,
				(ToolStripItem)byLatitudeToolStripMenuItem,
				(ToolStripItem)byLongitudeToolStripMenuItem,
				(ToolStripItem)byObserverToolStripMenuItem,
				(ToolStripItem)bySiteNameToolStripMenuItem
			});
			((ToolStripItem)sortSitesToolStripMenuItem).set_Enabled(false);
			((ToolStripItem)sortSitesToolStripMenuItem).set_Image((Image)Resources.Sort);
			((ToolStripItem)sortSitesToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)sortSitesToolStripMenuItem).set_Name("sortSitesToolStripMenuItem");
			((ToolStripItem)sortSitesToolStripMenuItem).set_Size(new Size(94, 20));
			((ToolStripItem)sortSitesToolStripMenuItem).set_Text("Sort sites....");
			((ToolStripItem)byObservationsToolStripMenuItem).set_Name("byObservationsToolStripMenuItem");
			((ToolStripItem)byObservationsToolStripMenuItem).set_Size(new Size(167, 22));
			((ToolStripItem)byObservationsToolStripMenuItem).set_Text("by # observations");
			((ToolStripItem)byObservationsToolStripMenuItem).add_Click((EventHandler)byObservationsToolStripMenuItem_Click);
			((ToolStripItem)byIDToolStripMenuItem).set_Name("byIDToolStripMenuItem");
			((ToolStripItem)byIDToolStripMenuItem).set_Size(new Size(167, 22));
			((ToolStripItem)byIDToolStripMenuItem).set_Text("by ID");
			((ToolStripItem)byIDToolStripMenuItem).add_Click((EventHandler)byIDToolStripMenuItem_Click);
			((ToolStripItem)byLatitudeToolStripMenuItem).set_Name("byLatitudeToolStripMenuItem");
			((ToolStripItem)byLatitudeToolStripMenuItem).set_Size(new Size(167, 22));
			((ToolStripItem)byLatitudeToolStripMenuItem).set_Text("by Latitude");
			((ToolStripItem)byLatitudeToolStripMenuItem).add_Click((EventHandler)byLatitudeToolStripMenuItem_Click);
			((ToolStripItem)byLongitudeToolStripMenuItem).set_Name("byLongitudeToolStripMenuItem");
			((ToolStripItem)byLongitudeToolStripMenuItem).set_Size(new Size(167, 22));
			((ToolStripItem)byLongitudeToolStripMenuItem).set_Text("by Longitude");
			((ToolStripItem)byLongitudeToolStripMenuItem).add_Click((EventHandler)byLongitudeToolStripMenuItem_Click);
			((ToolStripItem)byObserverToolStripMenuItem).set_Name("byObserverToolStripMenuItem");
			((ToolStripItem)byObserverToolStripMenuItem).set_Size(new Size(167, 22));
			((ToolStripItem)byObserverToolStripMenuItem).set_Text("by Observer");
			((ToolStripItem)byObserverToolStripMenuItem).add_Click((EventHandler)byObserverToolStripMenuItem_Click);
			((ToolStripItem)bySiteNameToolStripMenuItem).set_Name("bySiteNameToolStripMenuItem");
			((ToolStripItem)bySiteNameToolStripMenuItem).set_Size(new Size(167, 22));
			((ToolStripItem)bySiteNameToolStripMenuItem).set_Text("by Site Name");
			((ToolStripItem)bySiteNameToolStripMenuItem).add_Click((EventHandler)bySiteNameToolStripMenuItem_Click);
			((ToolStripDropDownItem)SortEventsToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)byLongitudeToolStripMenuItem1,
				(ToolStripItem)byDateTimeToolStripMenuItem
			});
			((ToolStripItem)SortEventsToolStripMenuItem).set_Image((Image)Resources.Sort);
			((ToolStripItem)SortEventsToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)SortEventsToolStripMenuItem).set_Name("SortEventsToolStripMenuItem");
			((ToolStripItem)SortEventsToolStripMenuItem).set_Size(new Size(93, 20));
			((ToolStripItem)SortEventsToolStripMenuItem).set_Text("Sort events");
			((ToolStripItem)byLongitudeToolStripMenuItem1).set_Name("byLongitudeToolStripMenuItem1");
			((ToolStripItem)byLongitudeToolStripMenuItem1).set_Size(new Size(145, 22));
			((ToolStripItem)byLongitudeToolStripMenuItem1).set_Text("by Longitude");
			((ToolStripItem)byLongitudeToolStripMenuItem1).add_Click((EventHandler)byLongitudeToolStripMenuItem1_Click);
			((ToolStripItem)byDateTimeToolStripMenuItem).set_Name("byDateTimeToolStripMenuItem");
			((ToolStripItem)byDateTimeToolStripMenuItem).set_Size(new Size(145, 22));
			((ToolStripItem)byDateTimeToolStripMenuItem).set_Text("by Date/Time");
			((ToolStripItem)byDateTimeToolStripMenuItem).add_Click((EventHandler)byDateTimeToolStripMenuItem_Click);
			((ToolStripDropDownItem)siteStatisticsToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[6]
			{
				(ToolStripItem)tHESEAREMAINTENANCEROUTINESToolStripMenuItem,
				(ToolStripItem)createTELFFromSourceToolStripMenuItem,
				(ToolStripItem)createOBSFFromSourceToolStripMenuItem,
				(ToolStripItem)generateILOCStatisticsToolStripMenuItem,
				(ToolStripItem)toolStripSeparator1,
				(ToolStripItem)generateRGOStatisticsToolStripMenuItem
			});
			((ToolStripItem)siteStatisticsToolStripMenuItem).set_Font(new Font("Segoe UI", 7f, FontStyle.Italic));
			((ToolStripItem)siteStatisticsToolStripMenuItem).set_Name("siteStatisticsToolStripMenuItem");
			((ToolStripItem)siteStatisticsToolStripMenuItem).set_Size(new Size(96, 20));
			((ToolStripItem)siteStatisticsToolStripMenuItem).set_Text(" Site statistics...     ");
			((ToolStripItem)tHESEAREMAINTENANCEROUTINESToolStripMenuItem).set_Name("tHESEAREMAINTENANCEROUTINESToolStripMenuItem");
			((ToolStripItem)tHESEAREMAINTENANCEROUTINESToolStripMenuItem).set_Size(new Size(250, 22));
			((ToolStripItem)tHESEAREMAINTENANCEROUTINESToolStripMenuItem).set_Text("[ THESE ARE MAINTENANCE ROUTINES ]");
			((ToolStripItem)createTELFFromSourceToolStripMenuItem).set_Name("createTELFFromSourceToolStripMenuItem");
			((ToolStripItem)createTELFFromSourceToolStripMenuItem).set_Size(new Size(250, 22));
			((ToolStripItem)createTELFFromSourceToolStripMenuItem).set_Text("#1 Create ILOC Sites from source");
			((ToolStripItem)createTELFFromSourceToolStripMenuItem).add_Click((EventHandler)createTELFFromSourceToolStripMenuItem_Click);
			((ToolStripItem)createOBSFFromSourceToolStripMenuItem).set_Name("createOBSFFromSourceToolStripMenuItem");
			((ToolStripItem)createOBSFFromSourceToolStripMenuItem).set_Size(new Size(250, 22));
			((ToolStripItem)createOBSFFromSourceToolStripMenuItem).set_Text("#2 Create ILOC Observers from source");
			((ToolStripItem)createOBSFFromSourceToolStripMenuItem).add_Click((EventHandler)createOBSFFromSourceToolStripMenuItem_Click);
			((ToolStripItem)generateILOCStatisticsToolStripMenuItem).set_Name("generateILOCStatisticsToolStripMenuItem");
			((ToolStripItem)generateILOCStatisticsToolStripMenuItem).set_Size(new Size(250, 22));
			((ToolStripItem)generateILOCStatisticsToolStripMenuItem).set_Text("#3 Generate ILOC statistics");
			((ToolStripItem)generateILOCStatisticsToolStripMenuItem).add_Click((EventHandler)generateILOCStatisticsToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator1).set_Name("toolStripSeparator1");
			((ToolStripItem)toolStripSeparator1).set_Size(new Size(247, 6));
			((ToolStripItem)generateRGOStatisticsToolStripMenuItem).set_Name("generateRGOStatisticsToolStripMenuItem");
			((ToolStripItem)generateRGOStatisticsToolStripMenuItem).set_Size(new Size(250, 22));
			((ToolStripItem)generateRGOStatisticsToolStripMenuItem).set_Text("Generate RGO statistics");
			((ToolStripItem)generateRGOStatisticsToolStripMenuItem).add_Click((EventHandler)generateRGOStatisticsToolStripMenuItem_Click);
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
			cmbFile.set_DropDownStyle((ComboBoxStyle)2);
			((ListControl)cmbFile).set_FormattingEnabled(true);
			((Control)cmbFile).set_Location(new Point(85, 28));
			cmbFile.set_MaxDropDownItems(20);
			((Control)cmbFile).set_Name("cmbFile");
			((Control)cmbFile).set_Size(new Size(210, 21));
			((Control)cmbFile).set_TabIndex(2);
			cmbFile.add_SelectedIndexChanged((EventHandler)cmbFile_SelectedIndexChanged);
			((Control)cmdRead).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdRead).set_Location(new Point(426, 18));
			((Control)cmdRead).set_Name("cmdRead");
			((Control)cmdRead).set_Size(new Size(54, 35));
			((Control)cmdRead).set_TabIndex(3);
			((Control)cmdRead).set_Text("Read");
			((ButtonBase)cmdRead).set_UseVisualStyleBackColor(true);
			((Control)cmdRead).add_Click((EventHandler)cmdRead_Click);
			((Control)pbarRead).set_Location(new Point(85, 55));
			pbarRead.set_Maximum(200);
			((Control)pbarRead).set_Name("pbarRead");
			((Control)pbarRead).set_Size(new Size(210, 9));
			((Control)pbarRead).set_TabIndex(4);
			((Control)pbarRead).set_Visible(false);
			((Control)lstReduction).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstReduction).set_FormattingEnabled(true);
			lstReduction.set_ItemHeight(14);
			((Control)lstReduction).set_Location(new Point(3, 586));
			((Control)lstReduction).set_Name("lstReduction");
			((Control)lstReduction).set_Size(new Size(800, 46));
			((Control)lstReduction).set_TabIndex(5);
			((Control)grpAdjustments).set_Anchor((AnchorStyles)1);
			((Control)grpAdjustments).get_Controls().Add((Control)(object)optAsteroid);
			((Control)grpAdjustments).get_Controls().Add((Control)(object)optPlanet);
			((Control)grpAdjustments).get_Controls().Add((Control)(object)label14);
			((Control)grpAdjustments).get_Controls().Add((Control)(object)updnSecond);
			((Control)grpAdjustments).get_Controls().Add((Control)(object)label13);
			((Control)grpAdjustments).get_Controls().Add((Control)(object)updnPE);
			((Control)grpAdjustments).get_Controls().Add((Control)(object)cmdAdjustedReduce);
			((Control)grpAdjustments).get_Controls().Add((Control)(object)optXZ);
			((Control)grpAdjustments).get_Controls().Add((Control)(object)optSAO);
			((Control)grpAdjustments).get_Controls().Add((Control)(object)optZC);
			((Control)grpAdjustments).get_Controls().Add((Control)(object)txtStar);
			((Control)grpAdjustments).get_Controls().Add((Control)(object)label12);
			((Control)grpAdjustments).get_Controls().Add((Control)(object)label11);
			((Control)grpAdjustments).get_Controls().Add((Control)(object)label10);
			((Control)grpAdjustments).get_Controls().Add((Control)(object)label9);
			((Control)grpAdjustments).get_Controls().Add((Control)(object)label8);
			((Control)grpAdjustments).get_Controls().Add((Control)(object)label7);
			((Control)grpAdjustments).get_Controls().Add((Control)(object)updnDay);
			((Control)grpAdjustments).get_Controls().Add((Control)(object)updnHour);
			((Control)grpAdjustments).get_Controls().Add((Control)(object)updnMinute);
			((Control)grpAdjustments).get_Controls().Add((Control)(object)updnMonth);
			((Control)grpAdjustments).get_Controls().Add((Control)(object)updnYear);
			((Control)grpAdjustments).set_Location(new Point(170, 636));
			((Control)grpAdjustments).set_Name("grpAdjustments");
			((Control)grpAdjustments).set_Size(new Size(673, 62));
			((Control)grpAdjustments).set_TabIndex(6);
			grpAdjustments.set_TabStop(false);
			((Control)grpAdjustments).set_Text("Adjustments - use the following date/time and star");
			((Control)optAsteroid).set_AutoSize(true);
			optAsteroid.set_CheckAlign(ContentAlignment.MiddleRight);
			((Control)optAsteroid).set_Location(new Point(439, 39));
			((Control)optAsteroid).set_Name("optAsteroid");
			((Control)optAsteroid).set_Size(new Size(63, 17));
			((Control)optAsteroid).set_TabIndex(33);
			((Control)optAsteroid).set_Text("Asteroid");
			((ButtonBase)optAsteroid).set_UseVisualStyleBackColor(true);
			((Control)optPlanet).set_AutoSize(true);
			optPlanet.set_CheckAlign(ContentAlignment.MiddleRight);
			((Control)optPlanet).set_Location(new Point(446, 24));
			((Control)optPlanet).set_Name("optPlanet");
			((Control)optPlanet).set_Size(new Size(55, 17));
			((Control)optPlanet).set_TabIndex(32);
			((Control)optPlanet).set_Text("Planet");
			((ButtonBase)optPlanet).set_UseVisualStyleBackColor(true);
			((Control)label14).set_AutoSize(true);
			((Control)label14).set_Location(new Point(238, 17));
			((Control)label14).set_Name("label14");
			((Control)label14).set_Size(new Size(26, 13));
			((Control)label14).set_TabIndex(31);
			((Control)label14).set_Text("Sec");
			updnSecond.set_DecimalPlaces(2);
			((Control)updnSecond).set_Enabled(false);
			((Control)updnSecond).set_Location(new Point(241, 30));
			updnSecond.set_Maximum(new decimal(new int[4] { 60, 0, 0, 0 }));
			((Control)updnSecond).set_Name("updnSecond");
			((Control)updnSecond).set_Size(new Size(51, 20));
			((Control)updnSecond).set_TabIndex(30);
			updnSecond.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)label13).set_AutoSize(true);
			((Control)label13).set_Location(new Point(297, 17));
			((Control)label13).set_Name("label13");
			((Control)label13).set_Size(new Size(61, 13));
			((Control)label13).set_TabIndex(29);
			((Control)label13).set_Text("PE to apply");
			updnPE.set_DecimalPlaces(2);
			((Control)updnPE).set_Enabled(false);
			((Control)updnPE).set_Location(new Point(302, 30));
			updnPE.set_Maximum(new decimal(new int[4] { 5, 0, 0, 0 }));
			((Control)updnPE).set_Name("updnPE");
			((Control)updnPE).set_Size(new Size(43, 20));
			((Control)updnPE).set_TabIndex(28);
			updnPE.set_Value(new decimal(new int[4] { 99, 0, 0, 131072 }));
			((Control)cmdAdjustedReduce).set_Location(new Point(587, 22));
			((Control)cmdAdjustedReduce).set_Name("cmdAdjustedReduce");
			((Control)cmdAdjustedReduce).set_Size(new Size(69, 25));
			((Control)cmdAdjustedReduce).set_TabIndex(27);
			((Control)cmdAdjustedReduce).set_Text("Compute");
			((ButtonBase)cmdAdjustedReduce).set_UseVisualStyleBackColor(true);
			((Control)cmdAdjustedReduce).add_Click((EventHandler)cmdAdjustedReduce_Click);
			((Control)optXZ).set_AutoSize(true);
			optXZ.set_CheckAlign(ContentAlignment.MiddleRight);
			((Control)optXZ).set_Location(new Point(462, 8));
			((Control)optXZ).set_Name("optXZ");
			((Control)optXZ).set_Size(new Size(39, 17));
			((Control)optXZ).set_TabIndex(14);
			((Control)optXZ).set_Text("XZ");
			((ButtonBase)optXZ).set_UseVisualStyleBackColor(true);
			((Control)optSAO).set_AutoSize(true);
			optSAO.set_CheckAlign(ContentAlignment.MiddleRight);
			((Control)optSAO).set_Location(new Point(393, 31));
			((Control)optSAO).set_Name("optSAO");
			((Control)optSAO).set_Size(new Size(47, 17));
			((Control)optSAO).set_TabIndex(13);
			((Control)optSAO).set_Text("SAO");
			((ButtonBase)optSAO).set_UseVisualStyleBackColor(true);
			((Control)optZC).set_AutoSize(true);
			optZC.set_CheckAlign(ContentAlignment.MiddleRight);
			optZC.set_Checked(true);
			((Control)optZC).set_Location(new Point(401, 14));
			((Control)optZC).set_Name("optZC");
			((Control)optZC).set_Size(new Size(39, 17));
			((Control)optZC).set_TabIndex(12);
			optZC.set_TabStop(true);
			((Control)optZC).set_Text("ZC");
			((ButtonBase)optZC).set_UseVisualStyleBackColor(true);
			((Control)txtStar).set_Location(new Point(524, 30));
			((Control)txtStar).set_Name("txtStar");
			((Control)txtStar).set_Size(new Size(57, 20));
			((Control)txtStar).set_TabIndex(11);
			((Control)label12).set_AutoSize(true);
			((Control)label12).set_Location(new Point(523, 15));
			((Control)label12).set_Name("label12");
			((Control)label12).set_Size(new Size(44, 13));
			((Control)label12).set_TabIndex(10);
			((Control)label12).set_Text("Number");
			((Control)label11).set_AutoSize(true);
			((Control)label11).set_Location(new Point(69, 17));
			((Control)label11).set_Name("label11");
			((Control)label11).set_Size(new Size(37, 13));
			((Control)label11).set_TabIndex(9);
			((Control)label11).set_Text("Month");
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Location(new Point(110, 17));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(26, 13));
			((Control)label10).set_TabIndex(8);
			((Control)label10).set_Text("Day");
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Location(new Point(152, 16));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(30, 13));
			((Control)label9).set_TabIndex(7);
			((Control)label9).set_Text("Hour");
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Location(new Point(195, 17));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(27, 13));
			((Control)label8).set_TabIndex(6);
			((Control)label8).set_Text("Min.");
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Location(new Point(16, 16));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(29, 13));
			((Control)label7).set_TabIndex(5);
			((Control)label7).set_Text("Year");
			((Control)updnDay).set_Location(new Point(114, 30));
			updnDay.set_Maximum(new decimal(new int[4] { 31, 0, 0, 0 }));
			updnDay.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnDay).set_Name("updnDay");
			((Control)updnDay).set_Size(new Size(34, 20));
			((Control)updnDay).set_TabIndex(4);
			updnDay.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnHour).set_Location(new Point(156, 30));
			updnHour.set_Maximum(new decimal(new int[4] { 23, 0, 0, 0 }));
			((Control)updnHour).set_Name("updnHour");
			((Control)updnHour).set_Size(new Size(34, 20));
			((Control)updnHour).set_TabIndex(3);
			updnHour.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnMinute).set_Location(new Point(198, 30));
			updnMinute.set_Maximum(new decimal(new int[4] { 59, 0, 0, 0 }));
			((Control)updnMinute).set_Name("updnMinute");
			((Control)updnMinute).set_Size(new Size(34, 20));
			((Control)updnMinute).set_TabIndex(2);
			updnMinute.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnMonth).set_Location(new Point(72, 30));
			updnMonth.set_Maximum(new decimal(new int[4] { 12, 0, 0, 0 }));
			updnMonth.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnMonth).set_Name("updnMonth");
			((Control)updnMonth).set_Size(new Size(34, 20));
			((Control)updnMonth).set_TabIndex(1);
			updnMonth.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnYear).set_Location(new Point(16, 30));
			updnYear.set_Maximum(new decimal(new int[4] { 2200, 0, 0, 0 }));
			updnYear.set_Minimum(new decimal(new int[4] { 1600, 0, 0, 0 }));
			((Control)updnYear).set_Name("updnYear");
			((Control)updnYear).set_Size(new Size(48, 20));
			((Control)updnYear).set_TabIndex(0);
			updnYear.set_Value(new decimal(new int[4] { 2008, 0, 0, 0 }));
			((Control)txtXZ).set_Location(new Point(288, 25));
			((Control)txtXZ).set_Name("txtXZ");
			((Control)txtXZ).set_Size(new Size(44, 20));
			((Control)txtXZ).set_TabIndex(21);
			((Control)txtXZ).add_TextChanged((EventHandler)txtXZ_TextChanged);
			cmbNames.set_DropDownStyle((ComboBoxStyle)2);
			((ListControl)cmbNames).set_FormattingEnabled(true);
			((Control)cmbNames).set_Location(new Point(4, 24));
			cmbNames.set_MaxDropDownItems(20);
			((Control)cmbNames).set_Name("cmbNames");
			((Control)cmbNames).set_Size(new Size(186, 21));
			((Control)cmbNames).set_TabIndex(15);
			cmbNames.add_SelectedIndexChanged((EventHandler)cmbNames_SelectedIndexChanged);
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Location(new Point(44, 7));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(67, 13));
			((Control)label6).set_TabIndex(14);
			((Control)label6).set_Text("Name of star");
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Location(new Point(205, 10));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(21, 13));
			((Control)label5).set_TabIndex(16);
			((Control)label5).set_Text("ZC");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Location(new Point(246, 10));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(29, 13));
			((Control)label4).set_TabIndex(18);
			((Control)label4).set_Text("SAO");
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Location(new Point(300, 10));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(21, 13));
			((Control)label3).set_TabIndex(20);
			((Control)label3).set_Text("XZ");
			((Control)txtZC).set_Location(new Point(198, 25));
			((Control)txtZC).set_Name("txtZC");
			((Control)txtZC).set_Size(new Size(34, 20));
			((Control)txtZC).set_TabIndex(17);
			((Control)txtZC).add_TextChanged((EventHandler)txtZC_TextChanged);
			((Control)txtSAO).set_Location(new Point(238, 25));
			((Control)txtSAO).set_Name("txtSAO");
			((Control)txtSAO).set_Size(new Size(44, 20));
			((Control)txtSAO).set_TabIndex(19);
			((Control)txtSAO).add_TextChanged((EventHandler)txtSAO_TextChanged);
			((Control)panelStar).get_Controls().Add((Control)(object)cmdRead);
			((Control)panelStar).get_Controls().Add((Control)(object)chkAsteroid);
			((Control)panelStar).get_Controls().Add((Control)(object)chkDoubleEdit);
			((Control)panelStar).get_Controls().Add((Control)(object)txtPlanet);
			((Control)panelStar).get_Controls().Add((Control)(object)label15);
			((Control)panelStar).get_Controls().Add((Control)(object)chkUnidentifiedDoubles);
			((Control)panelStar).get_Controls().Add((Control)(object)chkDoublesOnly);
			((Control)panelStar).get_Controls().Add((Control)(object)chkHighlightDoubles);
			((Control)panelStar).get_Controls().Add((Control)(object)pbarStar);
			((Control)panelStar).get_Controls().Add((Control)(object)txtXZ);
			((Control)panelStar).get_Controls().Add((Control)(object)cmbNames);
			((Control)panelStar).get_Controls().Add((Control)(object)label6);
			((Control)panelStar).get_Controls().Add((Control)(object)label5);
			((Control)panelStar).get_Controls().Add((Control)(object)label4);
			((Control)panelStar).get_Controls().Add((Control)(object)label3);
			((Control)panelStar).get_Controls().Add((Control)(object)txtZC);
			((Control)panelStar).get_Controls().Add((Control)(object)txtSAO);
			((Control)panelStar).get_Controls().Add((Control)(object)label2);
			((Control)panelStar).set_Location(new Point(113, 113));
			((Control)panelStar).set_Name("panelStar");
			((Control)panelStar).set_Size(new Size(487, 89));
			((Control)panelStar).set_TabIndex(22);
			((Control)chkAsteroid).set_AutoSize(true);
			chkAsteroid.set_CheckAlign(ContentAlignment.BottomCenter);
			((Control)chkAsteroid).set_Location(new Point(369, 1));
			((Control)chkAsteroid).set_Name("chkAsteroid");
			((Control)chkAsteroid).set_Size(new Size(53, 44));
			((Control)chkAsteroid).set_TabIndex(30);
			((Control)chkAsteroid).set_Text("All\r\nasteroids");
			((ButtonBase)chkAsteroid).set_TextAlign(ContentAlignment.TopCenter);
			((ButtonBase)chkAsteroid).set_UseVisualStyleBackColor(true);
			((Control)chkAsteroid).add_Click((EventHandler)chkAsteroid_Click);
			((Control)chkDoubleEdit).set_AutoSize(true);
			((Control)chkDoubleEdit).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)chkDoubleEdit).set_ForeColor(Color.Maroon);
			((Control)chkDoubleEdit).set_Location(new Point(410, 62));
			((Control)chkDoubleEdit).set_Name("chkDoubleEdit");
			((Control)chkDoubleEdit).set_Size(new Size(81, 30));
			((Control)chkDoubleEdit).set_TabIndex(29);
			((Control)chkDoubleEdit).set_Text("Select for\r\nAnalysis");
			((ButtonBase)chkDoubleEdit).set_UseVisualStyleBackColor(true);
			chkDoubleEdit.add_CheckedChanged((EventHandler)chkDoubleEdit_CheckedChanged);
			((Control)txtPlanet).set_Location(new Point(344, 25));
			((Control)txtPlanet).set_Name("txtPlanet");
			((Control)txtPlanet).set_Size(new Size(19, 20));
			((Control)txtPlanet).set_TabIndex(28);
			((Control)txtPlanet).add_TextChanged((EventHandler)txtPlanet_TextChanged);
			((Control)label15).set_AutoSize(true);
			((Control)label15).set_Location(new Point(334, 10));
			((Control)label15).set_Name("label15");
			((Control)label15).set_Size(new Size(37, 13));
			((Control)label15).set_TabIndex(27);
			((Control)label15).set_Text("Planet");
			((Control)chkUnidentifiedDoubles).set_AutoSize(true);
			((Control)chkUnidentifiedDoubles).set_Location(new Point(249, 69));
			((Control)chkUnidentifiedDoubles).set_Name("chkUnidentifiedDoubles");
			((Control)chkUnidentifiedDoubles).set_Size(new Size(161, 17));
			((Control)chkUnidentifiedDoubles).set_TabIndex(26);
			((Control)chkUnidentifiedDoubles).set_Text("list ALL 'unidentified' doubles");
			((ButtonBase)chkUnidentifiedDoubles).set_UseVisualStyleBackColor(true);
			((Control)chkDoublesOnly).set_AutoSize(true);
			((Control)chkDoublesOnly).set_Location(new Point(149, 69));
			((Control)chkDoublesOnly).set_Name("chkDoublesOnly");
			((Control)chkDoublesOnly).set_Size(new Size(100, 17));
			((Control)chkDoublesOnly).set_TabIndex(25);
			((Control)chkDoublesOnly).set_Text("only list doubles");
			((ButtonBase)chkDoublesOnly).set_UseVisualStyleBackColor(true);
			((Control)chkHighlightDoubles).set_AutoSize(true);
			((Control)chkHighlightDoubles).set_Location(new Point(82, 69));
			((Control)chkHighlightDoubles).set_Name("chkHighlightDoubles");
			((Control)chkHighlightDoubles).set_Size(new Size(67, 17));
			((Control)chkHighlightDoubles).set_TabIndex(23);
			((Control)chkHighlightDoubles).set_Text("Highlight");
			((ButtonBase)chkHighlightDoubles).set_UseVisualStyleBackColor(true);
			((Control)pbarStar).set_Location(new Point(4, 47));
			((Control)pbarStar).set_Name("pbarStar");
			((Control)pbarStar).set_Size(new Size(186, 8));
			((Control)pbarStar).set_TabIndex(22);
			((Control)pbarStar).set_Visible(false);
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label2).set_Location(new Point(0, 69));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(82, 13));
			((Control)label2).set_TabIndex(24);
			((Control)label2).set_Text("Double stars:");
			((Control)panelFile).get_Controls().Add((Control)(object)cmdReadFile);
			((Control)panelFile).get_Controls().Add((Control)(object)label1);
			((Control)panelFile).get_Controls().Add((Control)(object)pbarRead);
			((Control)panelFile).get_Controls().Add((Control)(object)cmbFile);
			((Control)panelFile).set_Location(new Point(113, 27));
			((Control)panelFile).set_Name("panelFile");
			((Control)panelFile).set_Size(new Size(487, 89));
			((Control)panelFile).set_TabIndex(23);
			((Control)cmdReadFile).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdReadFile).set_Location(new Point(426, 18));
			((Control)cmdReadFile).set_Name("cmdReadFile");
			((Control)cmdReadFile).set_Size(new Size(54, 35));
			((Control)cmdReadFile).set_TabIndex(16);
			((Control)cmdReadFile).set_Text("Read");
			((ButtonBase)cmdReadFile).set_UseVisualStyleBackColor(true);
			((Control)cmdReadFile).add_Click((EventHandler)cmdReadFile_Click);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(134, 12));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(99, 13));
			((Control)label1).set_TabIndex(15);
			((Control)label1).set_Text("Name of file to read");
			((Control)optFile).set_AutoSize(true);
			((Control)optFile).set_Location(new Point(3, 39));
			((Control)optFile).set_Name("optFile");
			((Control)optFile).set_Size(new Size(71, 17));
			((Control)optFile).set_TabIndex(24);
			((Control)optFile).set_Text("read a file");
			((ButtonBase)optFile).set_UseVisualStyleBackColor(true);
			optFile.add_CheckedChanged((EventHandler)optFile_CheckedChanged);
			((Control)optStar).set_AutoSize(true);
			optStar.set_Checked(true);
			((Control)optStar).set_Location(new Point(3, 3));
			((Control)optStar).set_Name("optStar");
			((Control)optStar).set_Size(new Size(92, 17));
			((Control)optStar).set_TabIndex(25);
			optStar.set_TabStop(true);
			((Control)optStar).set_Text("search for star");
			((ButtonBase)optStar).set_UseVisualStyleBackColor(true);
			optStar.add_CheckedChanged((EventHandler)optStar_CheckedChanged);
			((Control)panel1).get_Controls().Add((Control)(object)optSearchName);
			((Control)panel1).get_Controls().Add((Control)(object)optStar);
			((Control)panel1).get_Controls().Add((Control)(object)optFile);
			((Control)panel1).set_Location(new Point(9, 34));
			((Control)panel1).set_Name("panel1");
			((Control)panel1).set_Size(new Size(101, 62));
			((Control)panel1).set_TabIndex(26);
			((Control)optSearchName).set_AutoSize(true);
			((Control)optSearchName).set_Location(new Point(3, 21));
			((Control)optSearchName).set_Name("optSearchName");
			((Control)optSearchName).set_Size(new Size(101, 17));
			((Control)optSearchName).set_TabIndex(26);
			((Control)optSearchName).set_Text("search for name");
			((ButtonBase)optSearchName).set_UseVisualStyleBackColor(true);
			optSearchName.add_CheckedChanged((EventHandler)optSearchName_CheckedChanged);
			((Control)cmdCreateReportFromSite).set_Enabled(false);
			((Control)cmdCreateReportFromSite).set_Location(new Point(648, 45));
			((Control)cmdCreateReportFromSite).set_Name("cmdCreateReportFromSite");
			((Control)cmdCreateReportFromSite).set_Size(new Size(94, 46));
			((Control)cmdCreateReportFromSite).set_TabIndex(27);
			((Control)cmdCreateReportFromSite).set_Text("Create report\r\nusing site ID");
			((ButtonBase)cmdCreateReportFromSite).set_UseVisualStyleBackColor(true);
			((Control)cmdCreateReportFromSite).add_Click((EventHandler)cmdCreateReportFromSite_Click);
			((Control)cmdMergeReportFromSite).set_Enabled(false);
			((Control)cmdMergeReportFromSite).set_Location(new Point(756, 45));
			((Control)cmdMergeReportFromSite).set_Name("cmdMergeReportFromSite");
			((Control)cmdMergeReportFromSite).set_Size(new Size(94, 46));
			((Control)cmdMergeReportFromSite).set_TabIndex(28);
			((Control)cmdMergeReportFromSite).set_Text("Merge report\r\nusing site");
			((ButtonBase)cmdMergeReportFromSite).set_UseVisualStyleBackColor(true);
			((Control)cmdMergeReportFromSite).add_Click((EventHandler)cmdMergeReportFromSite_Click);
			((Control)LabelRightClick).set_AutoSize(true);
			((Control)LabelRightClick).set_Font(new Font("Microsoft Sans Serif", 7.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)LabelRightClick).set_Location(new Point(2, 112));
			((Control)LabelRightClick).set_Name("LabelRightClick");
			((Control)LabelRightClick).set_Size(new Size(201, 13));
			((Control)LabelRightClick).set_TabIndex(64);
			((Control)LabelRightClick).set_Text("Right-click on line to plot individual events");
			((Control)panelName).get_Controls().Add((Control)(object)chkUnidentifiedDoublesNames);
			((Control)panelName).get_Controls().Add((Control)(object)chkDoublesOnlyNames);
			((Control)panelName).get_Controls().Add((Control)(object)cmdReadNames);
			((Control)panelName).get_Controls().Add((Control)(object)chkHighlightDoublesNames);
			((Control)panelName).get_Controls().Add((Control)(object)progressBar1);
			((Control)panelName).get_Controls().Add((Control)(object)label18);
			((Control)panelName).get_Controls().Add((Control)(object)label19);
			((Control)panelName).get_Controls().Add((Control)(object)txtName);
			((Control)panelName).get_Controls().Add((Control)(object)txtSite);
			((Control)panelName).get_Controls().Add((Control)(object)label21);
			((Control)panelName).set_Location(new Point(113, 207));
			((Control)panelName).set_Name("panelName");
			((Control)panelName).set_Size(new Size(487, 89));
			((Control)panelName).set_TabIndex(65);
			((Control)chkUnidentifiedDoublesNames).set_AutoSize(true);
			((Control)chkUnidentifiedDoublesNames).set_Location(new Point(249, 69));
			((Control)chkUnidentifiedDoublesNames).set_Name("chkUnidentifiedDoublesNames");
			((Control)chkUnidentifiedDoublesNames).set_Size(new Size(161, 17));
			((Control)chkUnidentifiedDoublesNames).set_TabIndex(26);
			((Control)chkUnidentifiedDoublesNames).set_Text("list ALL 'unidentified' doubles");
			((ButtonBase)chkUnidentifiedDoublesNames).set_UseVisualStyleBackColor(true);
			((Control)chkDoublesOnlyNames).set_AutoSize(true);
			((Control)chkDoublesOnlyNames).set_Location(new Point(149, 69));
			((Control)chkDoublesOnlyNames).set_Name("chkDoublesOnlyNames");
			((Control)chkDoublesOnlyNames).set_Size(new Size(100, 17));
			((Control)chkDoublesOnlyNames).set_TabIndex(25);
			((Control)chkDoublesOnlyNames).set_Text("only list doubles");
			((ButtonBase)chkDoublesOnlyNames).set_UseVisualStyleBackColor(true);
			((Control)cmdReadNames).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdReadNames).set_Location(new Point(426, 18));
			((Control)cmdReadNames).set_Name("cmdReadNames");
			((Control)cmdReadNames).set_Size(new Size(54, 35));
			((Control)cmdReadNames).set_TabIndex(3);
			((Control)cmdReadNames).set_Text("Read");
			((ButtonBase)cmdReadNames).set_UseVisualStyleBackColor(true);
			((Control)cmdReadNames).add_Click((EventHandler)cmdReadNames_Click);
			((Control)chkHighlightDoublesNames).set_AutoSize(true);
			((Control)chkHighlightDoublesNames).set_Location(new Point(82, 69));
			((Control)chkHighlightDoublesNames).set_Name("chkHighlightDoublesNames");
			((Control)chkHighlightDoublesNames).set_Size(new Size(67, 17));
			((Control)chkHighlightDoublesNames).set_TabIndex(23);
			((Control)chkHighlightDoublesNames).set_Text("Highlight");
			((ButtonBase)chkHighlightDoublesNames).set_UseVisualStyleBackColor(true);
			((Control)progressBar1).set_Location(new Point(4, 47));
			((Control)progressBar1).set_Name("progressBar1");
			((Control)progressBar1).set_Size(new Size(186, 8));
			((Control)progressBar1).set_TabIndex(22);
			((Control)progressBar1).set_Visible(false);
			((Control)label18).set_AutoSize(true);
			((Control)label18).set_Location(new Point(23, 10));
			((Control)label18).set_Name("label18");
			((Control)label18).set_Size(new Size(138, 13));
			((Control)label18).set_TabIndex(16);
			((Control)label18).set_Text("Observer's name contains...");
			((Control)label19).set_AutoSize(true);
			((Control)label19).set_Location(new Point(198, 10));
			((Control)label19).set_Name("label19");
			((Control)label19).set_Size(new Size(109, 13));
			((Control)label19).set_TabIndex(18);
			((Control)label19).set_Text("Site name contains....");
			((Control)txtName).set_Location(new Point(17, 25));
			((Control)txtName).set_Name("txtName");
			((Control)txtName).set_Size(new Size(150, 20));
			((Control)txtName).set_TabIndex(17);
			((Control)txtName).add_KeyDown(new KeyEventHandler(txtName_KeyDown));
			((Control)txtSite).set_Location(new Point(198, 25));
			((Control)txtSite).set_Name("txtSite");
			((Control)txtSite).set_Size(new Size(113, 20));
			((Control)txtSite).set_TabIndex(19);
			((Control)txtSite).add_KeyDown(new KeyEventHandler(txtSite_KeyDown));
			((Control)label21).set_AutoSize(true);
			((Control)label21).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label21).set_Location(new Point(0, 69));
			((Control)label21).set_Name("label21");
			((Control)label21).set_Size(new Size(82, 13));
			((Control)label21).set_TabIndex(24);
			((Control)label21).set_Text("Double stars:");
			((Control)grpDoubleEditor).get_Controls().Add((Control)(object)cmdWithin15Secs);
			((Control)grpDoubleEditor).get_Controls().Add((Control)(object)cmdReverseHighlight);
			((Control)grpDoubleEditor).get_Controls().Add((Control)(object)cmdHighlightBrightLimb);
			((Control)grpDoubleEditor).get_Controls().Add((Control)(object)cmdDeleteChecked);
			((Control)grpDoubleEditor).get_Controls().Add((Control)(object)cmdMoveDown);
			((Control)grpDoubleEditor).get_Controls().Add((Control)(object)cmdHighlightDs);
			((Control)grpDoubleEditor).get_Controls().Add((Control)(object)cmdMoveUp);
			((Control)grpDoubleEditor).set_Location(new Point(642, 91));
			((Control)grpDoubleEditor).set_Name("grpDoubleEditor");
			((Control)grpDoubleEditor).set_Size(new Size(421, 93));
			((Control)grpDoubleEditor).set_TabIndex(67);
			grpDoubleEditor.set_TabStop(false);
			((Control)grpDoubleEditor).set_Text("Edit controls - double stars");
			((Control)grpDoubleEditor).set_Visible(false);
			((Control)cmdWithin15Secs).set_Location(new Point(6, 16));
			((Control)cmdWithin15Secs).set_Name("cmdWithin15Secs");
			((Control)cmdWithin15Secs).set_Size(new Size(103, 35));
			((Control)cmdWithin15Secs).set_TabIndex(3);
			((Control)cmdWithin15Secs).set_Text("Delete: Grazes,\r\n&& Tdiff > 15-secs");
			((ButtonBase)cmdWithin15Secs).set_TextAlign(ContentAlignment.MiddleLeft);
			((ButtonBase)cmdWithin15Secs).set_UseVisualStyleBackColor(true);
			((Control)cmdWithin15Secs).add_Click((EventHandler)cmdWithin15Secs_Click);
			((Control)cmdReverseHighlight).set_Location(new Point(234, 64));
			((Control)cmdReverseHighlight).set_Name("cmdReverseHighlight");
			((Control)cmdReverseHighlight).set_Size(new Size(133, 21));
			((Control)cmdReverseHighlight).set_TabIndex(2);
			((Control)cmdReverseHighlight).set_Text("Reverse HIGHLIGHTED");
			((ButtonBase)cmdReverseHighlight).set_TextAlign(ContentAlignment.MiddleLeft);
			((ButtonBase)cmdReverseHighlight).set_UseVisualStyleBackColor(true);
			((Control)cmdReverseHighlight).add_Click((EventHandler)cmdReverseHighlight_Click);
			((Control)cmdHighlightBrightLimb).set_Location(new Point(131, 40));
			((Control)cmdHighlightBrightLimb).set_Name("cmdHighlightBrightLimb");
			((Control)cmdHighlightBrightLimb).set_Size(new Size(98, 21));
			((Control)cmdHighlightBrightLimb).set_TabIndex(2);
			((Control)cmdHighlightBrightLimb).set_Text("Bright limb events");
			((ButtonBase)cmdHighlightBrightLimb).set_TextAlign(ContentAlignment.MiddleLeft);
			((ButtonBase)cmdHighlightBrightLimb).set_UseVisualStyleBackColor(true);
			((Control)cmdHighlightBrightLimb).add_Click((EventHandler)cmdHighlightBrightLimb_Click);
			((Control)cmdDeleteChecked).set_Location(new Point(104, 64));
			((Control)cmdDeleteChecked).set_Name("cmdDeleteChecked");
			((Control)cmdDeleteChecked).set_Size(new Size(124, 21));
			((Control)cmdDeleteChecked).set_TabIndex(3);
			((Control)cmdDeleteChecked).set_Text("Delete HIGHLIGHTED");
			((ButtonBase)cmdDeleteChecked).set_TextAlign(ContentAlignment.MiddleLeft);
			((ButtonBase)cmdDeleteChecked).set_UseVisualStyleBackColor(true);
			((Control)cmdDeleteChecked).add_Click((EventHandler)cmdDeleteChecked_Click);
			((Control)cmdMoveDown).set_Location(new Point(231, 40));
			((Control)cmdMoveDown).set_Name("cmdMoveDown");
			((Control)cmdMoveDown).set_Size(new Size(136, 21));
			((Control)cmdMoveDown).set_TabIndex(1);
			((Control)cmdMoveDown).set_Text("Move highlighted DOWN");
			((ButtonBase)cmdMoveDown).set_TextAlign(ContentAlignment.MiddleLeft);
			((ButtonBase)cmdMoveDown).set_UseVisualStyleBackColor(true);
			((Control)cmdMoveDown).add_Click((EventHandler)cmdMoveDown_Click);
			((Control)cmdHighlightDs).set_Location(new Point(131, 16));
			((Control)cmdHighlightDs).set_Name("cmdHighlightDs");
			((Control)cmdHighlightDs).set_Size(new Size(98, 21));
			((Control)cmdHighlightDs).set_TabIndex(1);
			((Control)cmdHighlightDs).set_Text("Disappear events");
			((ButtonBase)cmdHighlightDs).set_TextAlign(ContentAlignment.MiddleLeft);
			((ButtonBase)cmdHighlightDs).set_UseVisualStyleBackColor(true);
			((Control)cmdHighlightDs).add_Click((EventHandler)cmdHighlightDs_Click);
			((Control)cmdMoveUp).set_Location(new Point(231, 16));
			((Control)cmdMoveUp).set_Name("cmdMoveUp");
			((Control)cmdMoveUp).set_Size(new Size(136, 21));
			((Control)cmdMoveUp).set_TabIndex(0);
			((Control)cmdMoveUp).set_Text("Move highlighted UP");
			((ButtonBase)cmdMoveUp).set_TextAlign(ContentAlignment.MiddleLeft);
			((ButtonBase)cmdMoveUp).set_UseVisualStyleBackColor(true);
			((Control)cmdMoveUp).add_Click((EventHandler)cmdMoveUp_Click);
			((Control)cmdCreateReportForlocation).set_Enabled(false);
			((Control)cmdCreateReportForlocation).set_Location(new Point(866, 45));
			((Control)cmdCreateReportForlocation).set_Name("cmdCreateReportForlocation");
			((Control)cmdCreateReportForlocation).set_Size(new Size(94, 46));
			((Control)cmdCreateReportForlocation).set_TabIndex(68);
			((Control)cmdCreateReportForlocation).set_Text("Create report\r\nusing coords");
			((ButtonBase)cmdCreateReportForlocation).set_UseVisualStyleBackColor(true);
			((Control)cmdCreateReportForlocation).add_Click((EventHandler)cmdCreateReportForlocation_Click);
			((Control)cmdMergeReportForLocation).set_Enabled(false);
			((Control)cmdMergeReportForLocation).set_Location(new Point(967, 45));
			((Control)cmdMergeReportForLocation).set_Name("cmdMergeReportForLocation");
			((Control)cmdMergeReportForLocation).set_Size(new Size(94, 46));
			((Control)cmdMergeReportForLocation).set_TabIndex(69);
			((Control)cmdMergeReportForLocation).set_Text("Merge report\r\nusing coords");
			((ButtonBase)cmdMergeReportForLocation).set_UseVisualStyleBackColor(true);
			((Control)cmdMergeReportForLocation).add_Click((EventHandler)cmdMergeReportForLocation_Click);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(1018, 700));
			((Control)this).get_Controls().Add((Control)(object)grpDoubleEditor);
			((Control)this).get_Controls().Add((Control)(object)panelName);
			((Control)this).get_Controls().Add((Control)(object)panel1);
			((Control)this).get_Controls().Add((Control)(object)panelFile);
			((Control)this).get_Controls().Add((Control)(object)panelStar);
			((Control)this).get_Controls().Add((Control)(object)grpAdjustments);
			((Control)this).get_Controls().Add((Control)(object)lstReduction);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_Controls().Add((Control)(object)lstEvents);
			((Control)this).get_Controls().Add((Control)(object)LabelRightClick);
			((Control)this).get_Controls().Add((Control)(object)cmdMergeReportForLocation);
			((Control)this).get_Controls().Add((Control)(object)cmdCreateReportForlocation);
			((Control)this).get_Controls().Add((Control)(object)cmdMergeReportFromSite);
			((Control)this).get_Controls().Add((Control)(object)cmdCreateReportFromSite);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationLunarHistorical", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_Location(Settings.Default.LocationLunarHistorical);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_MinimumSize(new Size(828, 336));
			((Control)this).set_Name("HistoricalOccultations");
			((Form)this).set_StartPosition((FormStartPosition)1);
			((Control)this).set_Text("View/analyse  historical occultation occultations");
			((Form)this).add_Load((EventHandler)HistoricalOccultations_Load);
			((Control)this).add_Resize((EventHandler)HistoricalOccultations_Resize);
			((Control)contextMenuStrip1).ResumeLayout(false);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)grpAdjustments).ResumeLayout(false);
			((Control)grpAdjustments).PerformLayout();
			((ISupportInitialize)updnSecond).EndInit();
			((ISupportInitialize)updnPE).EndInit();
			((ISupportInitialize)updnDay).EndInit();
			((ISupportInitialize)updnHour).EndInit();
			((ISupportInitialize)updnMinute).EndInit();
			((ISupportInitialize)updnMonth).EndInit();
			((ISupportInitialize)updnYear).EndInit();
			((Control)panelStar).ResumeLayout(false);
			((Control)panelStar).PerformLayout();
			((Control)panelFile).ResumeLayout(false);
			((Control)panelFile).PerformLayout();
			((Control)panel1).ResumeLayout(false);
			((Control)panel1).PerformLayout();
			((Control)panelName).ResumeLayout(false);
			((Control)panelName).PerformLayout();
			((Control)grpDoubleEditor).ResumeLayout(false);
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
