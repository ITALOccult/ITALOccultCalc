using System;
using System.Collections;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Text;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Lunar_Observations
{
	public class Grazes : Form
	{
		private readonly string AppPath;

		private readonly string ZCNameFile;

		private const double Radian = 180.0 / Math.PI;

		internal ArrayList StarList = new ArrayList();

		private bool FormCreated;

		private bool ChangingStar;

		private IContainer components;

		private Label label1;

		private TextBox txtYear;

		private TextBox txtSAO;

		private TextBox txtZC;

		private TextBox txtDay;

		private TextBox txtMonth;

		private TextBox txtObserver;

		private Label label2;

		private Label label3;

		private Label label4;

		private Label label5;

		private Label label6;

		private Label label7;

		private Label label8;

		private Button cmdList;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem withListToolStripMenuItem;

		private ToolStripMenuItem CopyToolStripMenuItem;

		private ToolStripMenuItem PrintPreviewToolStripMenuItem;

		private ToolStripMenuItem printToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripMenuItem nameOfStarListToolStripMenuItem;

		private ToolStripMenuItem allNamesToolStripMenuItem;

		private ToolStripMenuItem properNamesToolStripMenuItem;

		private ToolStripMenuItem bayerLettersToolStripMenuItem;

		private ToolStripMenuItem brighterThanMag2ToolStripMenuItem;

		private ToolStripMenuItem brighterThanMag3ToolStripMenuItem;

		private ToolStripMenuItem brighterThanMag4ToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private ComboBox cmbNames;

		private TextBox txtXZ;

		internal ListBox lstEvents;

		private ContextMenuStrip contextMenuStrip1;

		private ToolStripMenuItem placeEventInEditorToolStripMenuItem;

		private ToolStripMenuItem merToolStripMenuItem;

		private Label label9;

		private ToolStripMenuItem summaryOfGrazesToolStripMenuItem;

		private ToolStripMenuItem byStarNumberToolStripMenuItem;

		private ToolStripMenuItem byDateToolStripMenuItem;

		private ToolStripMenuItem byObserverToolStripMenuItem;

		private ToolStripMenuItem forAnObserverToolStripMenuItem;

		private ToolStripTextBox txtSearchString;

		private ToolStripMenuItem helpToolStripMenuItem;

		private CheckBox chkOldFormat;

		private TextBox txtPlanet;

		private Label label10;

		private Panel panel1;

		private CheckBox chkIncludeInvalid;

		public Grazes()
		{
			InitializeComponent();
			AppPath = Utilities.AppPath;
			ZCNameFile = AppPath + "\\Resource Files\\ZCNames.dat";
		}

		private void Grazes_Load(object sender, EventArgs e)
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
			FormCreated = true;
			CreateStarList(3);
			((Control)txtXZ).set_Text("0");
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

		private void Grazes_Resize(object sender, EventArgs e)
		{
			((Control)lstEvents).set_Height(((Control)this).get_Height() - 151);
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
					TextBox obj2 = txtXZ;
					string text;
					((Control)txtPlanet).set_Text(text = "0");
					string text2;
					((Control)obj2).set_Text(text2 = text);
					((Control)obj).set_Text(text2);
				}
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
				TextBox obj = txtSAO;
				TextBox obj2 = txtXZ;
				string text;
				((Control)txtZC).set_Text(text = "0");
				string text2;
				((Control)obj2).set_Text(text2 = text);
				((Control)obj).set_Text(text2);
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
				((Control)txtPlanet).set_Text("0");
				ChangingStar = false;
			}
		}

		private void cmdList_Click(object sender, EventArgs e)
		{
			bool flag = false;
			bool flag2 = false;
			string[] array = new string[2] { "OccultGrazes.dat", "RecentGrazes.dat" };
			ArrayList arrayList = new ArrayList();
			string text = ((Control)txtYear).get_Text().Trim();
			string text2 = ((Control)txtMonth).get_Text().Trim();
			int length = text2.Length;
			string text3 = ((Control)txtDay).get_Text().Trim();
			int length2 = text3.Length;
			string text4 = ((Control)txtXZ).get_Text().PadLeft(6);
			string text5 = ((Control)txtSAO).get_Text().PadLeft(6);
			string text6 = ((Control)txtZC).get_Text().PadLeft(6);
			string text7 = ((Control)txtPlanet).get_Text().Trim();
			string text8 = ((Control)txtObserver).get_Text().Trim().ToUpper();
			lstEvents.get_Items().Clear();
			if (chkOldFormat.get_Checked())
			{
				for (int i = 0; i <= 1; i++)
				{
					using StreamReader streamReader = new StreamReader(AppPath + "\\Resource Files\\" + array[i]);
					do
					{
						arrayList.Clear();
						flag2 = text8.Length < 1;
						int result;
						string text9;
						do
						{
							text9 = streamReader.ReadLine()!.PadRight(4);
							if (int.TryParse(text9.Substring(0, 4), out result))
							{
								break;
							}
							if (!flag2 && text9.Substring(0, 1) == "O" && text9.Substring(2).ToUpper().Contains(text8))
							{
								flag2 = true;
							}
							arrayList.Add(text9);
						}
						while (result < 1);
						flag = flag2;
						if (text.Length > 0)
						{
							flag &= text9.Substring(2, 4).Contains(text);
						}
						if (length == 2)
						{
							flag &= text9.Substring(6, 2).Contains(text2);
						}
						if (length == 1)
						{
							flag &= text9.Substring(7, 1).Contains(text2);
						}
						if (length2 == 2)
						{
							flag &= text9.Substring(8, 2).Contains(text3);
						}
						if (length2 == 1)
						{
							flag &= text9.Substring(9, 1).Contains(text3);
						}
						flag &= "RSXP".Contains(text9.Substring(19, 1));
						if (text9.Substring(19, 1) == "P")
						{
							if (text7 != "0")
							{
								flag &= text9.Substring(23, 1) == text7;
							}
						}
						else if (text4 != "     0")
						{
							string text10 = ((text9.Substring(19, 1) == "R") ? text6 : ((!(text9.Substring(19, 1) == "S")) ? text4 : text5));
							flag &= text9.Substring(21, 6) == text10;
						}
						if (flag)
						{
							for (int j = 0; j < arrayList.Count; j++)
							{
								lstEvents.get_Items().Add((object)arrayList[j]!.ToString());
							}
							lstEvents.get_Items().Add((object)text9);
						}
						do
						{
							text9 = streamReader.ReadLine()!.PadRight(4);
							if (flag)
							{
								lstEvents.get_Items().Add((object)text9);
							}
						}
						while ((text9.Substring(0, 3) != "***") & !streamReader.EndOfStream);
						flag = true;
					}
					while (!streamReader.EndOfStream);
				}
				return;
			}
			string text11 = "";
			string text12 = "";
			string text13 = "";
			string text14 = "";
			string text15 = "";
			using StreamReader streamReader2 = new StreamReader(AppPath + "\\Resource Files\\Archive Graze Index.dat");
			while (!streamReader2.EndOfStream)
			{
				text11 = streamReader2.ReadLine();
				text12 = streamReader2.ReadLine();
				text13 = streamReader2.ReadLine();
				text14 = streamReader2.ReadLine();
				text15 = streamReader2.ReadLine();
				flag = "RSXP".Contains(text13.Substring(0, 1));
				if (text.Trim().Length > 0 && !text11.Substring(0, 4).Contains(text.Trim()))
				{
					flag = false;
				}
				if (text2.Trim().Length > 0 && flag && !text11.Substring(5, 2).Contains(text2.PadLeft(2)))
				{
					flag = false;
				}
				if (text3.Trim().Length > 0 && flag && !text11.Substring(8, 2).Contains(text3.PadLeft(2)))
				{
					flag = false;
				}
				if (text7 != "0")
				{
					flag = text13.PadRight(1).Substring(0, 1) == "P" && (flag & (text13.Substring(3, 1) == text7));
				}
				else if (text4 != "     0")
				{
					if (text13.Substring(0, 1) == "R")
					{
						if (!(text13.Substring(1, 6) == text6))
						{
							flag = false;
						}
					}
					else if (text13.Substring(0, 1) == "S")
					{
						if (!(text13.Substring(1, 6) == text5))
						{
							flag = false;
						}
					}
					else if (text13.Substring(0, 1) == "X")
					{
						if (!(text13.Substring(1, 6) == text4))
						{
							flag = false;
						}
					}
					else if (text13.PadRight(1).Substring(0, 1) == "P")
					{
						flag = false;
					}
				}
				if (text8.Length > 0 && !text15.ToUpper().Contains(text8))
				{
					flag = false;
				}
				if (!flag)
				{
					continue;
				}
				string[] array2 = text15.Split(new char[1] { ',' });
				int upperBound = array2.GetUpperBound(0);
				if (!int.TryParse(text11.Substring(5, 2), out var result2))
				{
					result2 = 1;
				}
				lstEvents.get_Items().Add((object)("Date      : " + text11.Substring(0, 5) + Utilities.ShortMonths[result2] + text11.Substring(7)));
				lstEvents.get_Items().Add((object)("Event ID  : " + text12));
				lstEvents.get_Items().Add((object)("Star      : " + text13));
				lstEvents.get_Items().Add((object)("# events  : " + text14));
				lstEvents.get_Items().Add((object)("Observers : " + (upperBound + 1)));
				string text16 = "     ";
				for (int k = 0; k <= upperBound; k++)
				{
					text16 += array2[k].TrimStart(Array.Empty<char>()).PadRight(25);
					if (k % 4 == 3)
					{
						lstEvents.get_Items().Add((object)text16);
						text16 = "     ";
					}
				}
				if (text16.Length > 0)
				{
					lstEvents.get_Items().Add((object)text16);
				}
				lstEvents.get_Items().Add((object)"");
				lstEvents.get_Items().Add((object)"***");
			}
		}

		private void lstEvents_MouseDown(object sender, MouseEventArgs e)
		{
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_000b: Invalid comparison between Unknown and I4
			if ((int)e.get_Button() == 2097152)
			{
				int num = lstEvents.IndexFromPoint(e.get_X(), e.get_Y());
				if (num >= 0 && num < lstEvents.get_Items().get_Count())
				{
					((ListControl)lstEvents).set_SelectedIndex(num);
				}
				else
				{
					((ListControl)lstEvents).set_SelectedIndex(0);
				}
				((Control)lstEvents).Refresh();
			}
		}

		private void placeEventInEditorToolStripMenuItem_Click(object sender, EventArgs e)
		{
			LunarObservations.ShowGrazeFileEventInEditor(FirstLine(((ListControl)lstEvents).get_SelectedIndex()), LastLine(((ListControl)lstEvents).get_SelectedIndex()), chkIncludeInvalid.get_Checked(), chkOldFormat.get_Checked());
		}

		private void merToolStripMenuItem_Click(object sender, EventArgs e)
		{
			LunarObservations.MergeGrazeFileEventInEditor(FirstLine(((ListControl)lstEvents).get_SelectedIndex()), LastLine(((ListControl)lstEvents).get_SelectedIndex()), chkIncludeInvalid.get_Checked(), OccultationReport.UseOldFormat, chkOldFormat.get_Checked());
		}

		private int FirstLine(int Location)
		{
			for (int num = Location - 1; num >= 0; num--)
			{
				if (lstEvents.get_Items().get_Item(num).ToString()!.Substring(0, 3) == "***")
				{
					return num + 1;
				}
			}
			return 0;
		}

		private int LastLine(int Location)
		{
			for (int i = Location; i < lstEvents.get_Items().get_Count(); i++)
			{
				if (lstEvents.get_Items().get_Item(i).ToString()!.PadRight(3).Substring(0, 3) == "***")
				{
					return i;
				}
			}
			return Location;
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
			Settings.Default.Save_LunarResults = Output.SavePredictionText(CollectEvents(), "Grazing Occultation", Settings.Default.Save_LunarResults);
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

		private void byStarNumberToolStripMenuItem_Click(object sender, EventArgs e)
		{
			LunarObservations.SummaryGrazeList(0);
		}

		private void byDateToolStripMenuItem_Click(object sender, EventArgs e)
		{
			LunarObservations.SummaryGrazeList(1);
		}

		private void byObserverToolStripMenuItem_Click(object sender, EventArgs e)
		{
			LunarObservations.SummaryGrazeList(2);
		}

		private void forAnObserverToolStripMenuItem_Click(object sender, EventArgs e)
		{
			LunarObservations.SearchText = ((ToolStripItem)txtSearchString).get_Text().Trim().ToUpper();
			LunarObservations.SummaryGrazeList(3);
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Lunar Occultations - Observed grazes");
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
			//IL_02f2: Unknown result type (might be due to invalid IL or missing references)
			//IL_02fc: Expected O, but got Unknown
			//IL_1795: Unknown result type (might be due to invalid IL or missing references)
			//IL_179f: Expected O, but got Unknown
			components = new Container();
			lstEvents = new ListBox();
			contextMenuStrip1 = new ContextMenuStrip(components);
			placeEventInEditorToolStripMenuItem = new ToolStripMenuItem();
			merToolStripMenuItem = new ToolStripMenuItem();
			label1 = new Label();
			txtYear = new TextBox();
			txtSAO = new TextBox();
			txtZC = new TextBox();
			txtDay = new TextBox();
			txtMonth = new TextBox();
			txtObserver = new TextBox();
			label2 = new Label();
			label3 = new Label();
			label4 = new Label();
			label5 = new Label();
			label6 = new Label();
			label7 = new Label();
			label8 = new Label();
			cmdList = new Button();
			menuStrip1 = new MenuStrip();
			withListToolStripMenuItem = new ToolStripMenuItem();
			CopyToolStripMenuItem = new ToolStripMenuItem();
			PrintPreviewToolStripMenuItem = new ToolStripMenuItem();
			printToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			summaryOfGrazesToolStripMenuItem = new ToolStripMenuItem();
			byStarNumberToolStripMenuItem = new ToolStripMenuItem();
			byDateToolStripMenuItem = new ToolStripMenuItem();
			byObserverToolStripMenuItem = new ToolStripMenuItem();
			forAnObserverToolStripMenuItem = new ToolStripMenuItem();
			txtSearchString = new ToolStripTextBox();
			nameOfStarListToolStripMenuItem = new ToolStripMenuItem();
			allNamesToolStripMenuItem = new ToolStripMenuItem();
			properNamesToolStripMenuItem = new ToolStripMenuItem();
			bayerLettersToolStripMenuItem = new ToolStripMenuItem();
			brighterThanMag2ToolStripMenuItem = new ToolStripMenuItem();
			brighterThanMag3ToolStripMenuItem = new ToolStripMenuItem();
			brighterThanMag4ToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			cmbNames = new ComboBox();
			txtXZ = new TextBox();
			label9 = new Label();
			chkOldFormat = new CheckBox();
			txtPlanet = new TextBox();
			label10 = new Label();
			panel1 = new Panel();
			chkIncludeInvalid = new CheckBox();
			((Control)contextMenuStrip1).SuspendLayout();
			((Control)menuStrip1).SuspendLayout();
			((Control)panel1).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)lstEvents).set_Anchor((AnchorStyles)1);
			((Control)lstEvents).set_ContextMenuStrip(contextMenuStrip1);
			((Control)lstEvents).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstEvents).set_FormattingEnabled(true);
			lstEvents.set_ItemHeight(14);
			((Control)lstEvents).set_Location(new Point(6, 98));
			((Control)lstEvents).set_Name("lstEvents");
			((Control)lstEvents).set_Size(new Size(720, 368));
			((Control)lstEvents).set_TabIndex(17);
			((Control)lstEvents).add_MouseDown(new MouseEventHandler(lstEvents_MouseDown));
			((ToolStrip)contextMenuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)placeEventInEditorToolStripMenuItem,
				(ToolStripItem)merToolStripMenuItem
			});
			((Control)contextMenuStrip1).set_Name("contextMenuStrip1");
			((Control)contextMenuStrip1).set_Size(new Size(246, 48));
			((ToolStripItem)placeEventInEditorToolStripMenuItem).set_Name("placeEventInEditorToolStripMenuItem");
			((ToolStripItem)placeEventInEditorToolStripMenuItem).set_Size(new Size(245, 22));
			((ToolStripItem)placeEventInEditorToolStripMenuItem).set_Text("Place event in Editor");
			((ToolStripItem)placeEventInEditorToolStripMenuItem).add_Click((EventHandler)placeEventInEditorToolStripMenuItem_Click);
			((ToolStripItem)merToolStripMenuItem).set_Name("merToolStripMenuItem");
			((ToolStripItem)merToolStripMenuItem).set_Size(new Size(245, 22));
			((ToolStripItem)merToolStripMenuItem).set_Text("Merge event with event in Editor");
			((ToolStripItem)merToolStripMenuItem).add_Click((EventHandler)merToolStripMenuItem_Click);
			((Control)label1).set_Anchor((AnchorStyles)1);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(12, 32));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(29, 13));
			((Control)label1).set_TabIndex(0);
			((Control)label1).set_Text("Year");
			((Control)txtYear).set_Anchor((AnchorStyles)1);
			((Control)txtYear).set_Location(new Point(10, 47));
			((Control)txtYear).set_Name("txtYear");
			((Control)txtYear).set_Size(new Size(35, 20));
			((Control)txtYear).set_TabIndex(1);
			((Control)txtSAO).set_Anchor((AnchorStyles)1);
			((Control)txtSAO).set_Location(new Point(372, 47));
			((Control)txtSAO).set_Name("txtSAO");
			((Control)txtSAO).set_Size(new Size(46, 20));
			((Control)txtSAO).set_TabIndex(11);
			((Control)txtSAO).add_TextChanged((EventHandler)txtSAO_TextChanged);
			((Control)txtZC).set_Anchor((AnchorStyles)1);
			((Control)txtZC).set_Location(new Point(326, 47));
			((Control)txtZC).set_Name("txtZC");
			((Control)txtZC).set_Size(new Size(40, 20));
			((Control)txtZC).set_TabIndex(9);
			((Control)txtZC).add_TextChanged((EventHandler)txtZC_TextChanged);
			((Control)txtDay).set_Anchor((AnchorStyles)1);
			((Control)txtDay).set_Location(new Point(81, 47));
			((Control)txtDay).set_Name("txtDay");
			((Control)txtDay).set_Size(new Size(25, 20));
			((Control)txtDay).set_TabIndex(5);
			((Control)txtMonth).set_Anchor((AnchorStyles)1);
			((Control)txtMonth).set_Location(new Point(51, 47));
			((Control)txtMonth).set_Name("txtMonth");
			((Control)txtMonth).set_Size(new Size(24, 20));
			((Control)txtMonth).set_TabIndex(3);
			((Control)txtObserver).set_Anchor((AnchorStyles)1);
			((Control)txtObserver).set_Location(new Point(513, 47));
			((Control)txtObserver).set_Name("txtObserver");
			((Control)txtObserver).set_Size(new Size(114, 20));
			((Control)txtObserver).set_TabIndex(15);
			((Control)label2).set_Anchor((AnchorStyles)1);
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(524, 32));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(93, 13));
			((Control)label2).set_TabIndex(14);
			((Control)label2).set_Text("Name of Observer");
			((Control)label3).set_Anchor((AnchorStyles)1);
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Location(new Point(438, 32));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(21, 13));
			((Control)label3).set_TabIndex(12);
			((Control)label3).set_Text("XZ");
			((Control)label4).set_Anchor((AnchorStyles)1);
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Location(new Point(381, 32));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(29, 13));
			((Control)label4).set_TabIndex(10);
			((Control)label4).set_Text("SAO");
			((Control)label5).set_Anchor((AnchorStyles)1);
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Location(new Point(336, 32));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(21, 13));
			((Control)label5).set_TabIndex(8);
			((Control)label5).set_Text("ZC");
			((Control)label6).set_Anchor((AnchorStyles)1);
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Location(new Point(162, 32));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(67, 13));
			((Control)label6).set_TabIndex(6);
			((Control)label6).set_Text("Name of star");
			((Control)label7).set_Anchor((AnchorStyles)1);
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Location(new Point(81, 32));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(26, 13));
			((Control)label7).set_TabIndex(4);
			((Control)label7).set_Text("Day");
			((Control)label8).set_Anchor((AnchorStyles)1);
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Location(new Point(44, 32));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(37, 13));
			((Control)label8).set_TabIndex(2);
			((Control)label8).set_Text("Month");
			((Control)cmdList).set_Anchor((AnchorStyles)1);
			((Control)cmdList).set_Location(new Point(637, 43));
			((Control)cmdList).set_Name("cmdList");
			((Control)cmdList).set_Size(new Size(79, 35));
			((Control)cmdList).set_TabIndex(16);
			((Control)cmdList).set_Text("&List events");
			((ButtonBase)cmdList).set_UseVisualStyleBackColor(true);
			((Control)cmdList).add_Click((EventHandler)cmdList_Click);
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[5]
			{
				(ToolStripItem)withListToolStripMenuItem,
				(ToolStripItem)summaryOfGrazesToolStripMenuItem,
				(ToolStripItem)nameOfStarListToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(731, 24));
			((Control)menuStrip1).set_TabIndex(18);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withListToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[4]
			{
				(ToolStripItem)CopyToolStripMenuItem,
				(ToolStripItem)PrintPreviewToolStripMenuItem,
				(ToolStripItem)printToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem
			});
			((ToolStripItem)withListToolStripMenuItem).set_Name("withListToolStripMenuItem");
			((ToolStripItem)withListToolStripMenuItem).set_Size(new Size(81, 20));
			((ToolStripItem)withListToolStripMenuItem).set_Text("with List...   ");
			((ToolStripItem)CopyToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)CopyToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)CopyToolStripMenuItem).set_Name("CopyToolStripMenuItem");
			CopyToolStripMenuItem.set_ShortcutKeys((Keys)131139);
			((ToolStripItem)CopyToolStripMenuItem).set_Size(new Size(144, 22));
			((ToolStripItem)CopyToolStripMenuItem).set_Text("&Copy");
			((ToolStripItem)CopyToolStripMenuItem).add_Click((EventHandler)copyToolStripMenuItem_Click);
			((ToolStripItem)PrintPreviewToolStripMenuItem).set_Image((Image)Resources.PrintPreview);
			((ToolStripItem)PrintPreviewToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)PrintPreviewToolStripMenuItem).set_Name("PrintPreviewToolStripMenuItem");
			((ToolStripItem)PrintPreviewToolStripMenuItem).set_Size(new Size(144, 22));
			((ToolStripItem)PrintPreviewToolStripMenuItem).set_Text("Print preview");
			((ToolStripItem)PrintPreviewToolStripMenuItem).add_Click((EventHandler)printPreviewToolStripMenuItem_Click);
			((ToolStripItem)printToolStripMenuItem).set_Image((Image)Resources.Print);
			((ToolStripItem)printToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printToolStripMenuItem).set_Name("printToolStripMenuItem");
			printToolStripMenuItem.set_ShortcutKeys((Keys)131152);
			((ToolStripItem)printToolStripMenuItem).set_Size(new Size(144, 22));
			((ToolStripItem)printToolStripMenuItem).set_Text("&Print");
			((ToolStripItem)printToolStripMenuItem).add_Click((EventHandler)printToolStripMenuItem_Click);
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			saveToolStripMenuItem.set_ShortcutKeys((Keys)131155);
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(144, 22));
			((ToolStripItem)saveToolStripMenuItem).set_Text("&Save");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
			((ToolStripDropDownItem)summaryOfGrazesToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[5]
			{
				(ToolStripItem)byStarNumberToolStripMenuItem,
				(ToolStripItem)byDateToolStripMenuItem,
				(ToolStripItem)byObserverToolStripMenuItem,
				(ToolStripItem)forAnObserverToolStripMenuItem,
				(ToolStripItem)txtSearchString
			});
			((ToolStripItem)summaryOfGrazesToolStripMenuItem).set_Enabled(false);
			((ToolStripItem)summaryOfGrazesToolStripMenuItem).set_Name("summaryOfGrazesToolStripMenuItem");
			((ToolStripItem)summaryOfGrazesToolStripMenuItem).set_Size(new Size(169, 20));
			((ToolStripItem)summaryOfGrazesToolStripMenuItem).set_Text("Make summary of grazes...   ");
			((ToolStripItem)byStarNumberToolStripMenuItem).set_Name("byStarNumberToolStripMenuItem");
			((ToolStripItem)byStarNumberToolStripMenuItem).set_Size(new Size(212, 22));
			((ToolStripItem)byStarNumberToolStripMenuItem).set_Text("by Star Number");
			((ToolStripItem)byStarNumberToolStripMenuItem).add_Click((EventHandler)byStarNumberToolStripMenuItem_Click);
			((ToolStripItem)byDateToolStripMenuItem).set_Name("byDateToolStripMenuItem");
			((ToolStripItem)byDateToolStripMenuItem).set_Size(new Size(212, 22));
			((ToolStripItem)byDateToolStripMenuItem).set_Text("by Date");
			((ToolStripItem)byDateToolStripMenuItem).add_Click((EventHandler)byDateToolStripMenuItem_Click);
			((ToolStripItem)byObserverToolStripMenuItem).set_Name("byObserverToolStripMenuItem");
			((ToolStripItem)byObserverToolStripMenuItem).set_Size(new Size(212, 22));
			((ToolStripItem)byObserverToolStripMenuItem).set_Text("by Observer");
			((ToolStripItem)byObserverToolStripMenuItem).add_Click((EventHandler)byObserverToolStripMenuItem_Click);
			((ToolStripItem)forAnObserverToolStripMenuItem).set_Name("forAnObserverToolStripMenuItem");
			((ToolStripItem)forAnObserverToolStripMenuItem).set_Size(new Size(212, 22));
			((ToolStripItem)forAnObserverToolStripMenuItem).set_Text("for the following Observer");
			((ToolStripItem)forAnObserverToolStripMenuItem).add_Click((EventHandler)forAnObserverToolStripMenuItem_Click);
			((ToolStripItem)txtSearchString).set_Name("txtSearchString");
			((ToolStripItem)txtSearchString).set_Size(new Size(100, 23));
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
			((ToolStripItem)nameOfStarListToolStripMenuItem).set_Size(new Size(155, 20));
			((ToolStripItem)nameOfStarListToolStripMenuItem).set_Text("Set  '&Name of Star' list...    ");
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
			((Control)cmbNames).set_Anchor((AnchorStyles)1);
			cmbNames.set_DropDownStyle((ComboBoxStyle)2);
			((ListControl)cmbNames).set_FormattingEnabled(true);
			((Control)cmbNames).set_Location(new Point(122, 47));
			cmbNames.set_MaxDropDownItems(20);
			((Control)cmbNames).set_Name("cmbNames");
			((Control)cmbNames).set_Size(new Size(186, 21));
			((Control)cmbNames).set_TabIndex(7);
			cmbNames.add_SelectedIndexChanged((EventHandler)cmbNames_SelectedIndexChanged);
			((Control)txtXZ).set_Anchor((AnchorStyles)1);
			((Control)txtXZ).set_Location(new Point(425, 47));
			((Control)txtXZ).set_Name("txtXZ");
			((Control)txtXZ).set_Size(new Size(46, 20));
			((Control)txtXZ).set_TabIndex(13);
			((Control)txtXZ).add_TextChanged((EventHandler)txtXZ_TextChanged);
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label9).set_Location(new Point(10, 6));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(340, 13));
			((Control)label9).set_TabIndex(20);
			((Control)label9).set_Text("Right-click to place event in Editor, where it can be saved, or analysed");
			((Control)chkOldFormat).set_AutoSize(true);
			((Control)chkOldFormat).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkOldFormat).set_Location(new Point(612, 7));
			((Control)chkOldFormat).set_Name("chkOldFormat");
			((Control)chkOldFormat).set_Size(new Size(113, 17));
			((Control)chkOldFormat).set_TabIndex(21);
			((Control)chkOldFormat).set_Text("use old format files");
			((ButtonBase)chkOldFormat).set_UseVisualStyleBackColor(true);
			((Control)txtPlanet).set_Anchor((AnchorStyles)1);
			((Control)txtPlanet).set_Location(new Point(477, 47));
			((Control)txtPlanet).set_Name("txtPlanet");
			((Control)txtPlanet).set_Size(new Size(20, 20));
			((Control)txtPlanet).set_TabIndex(23);
			((Control)txtPlanet).add_TextChanged((EventHandler)txtPlanet_TextChanged);
			((Control)label10).set_Anchor((AnchorStyles)1);
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Location(new Point(471, 32));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(37, 13));
			((Control)label10).set_TabIndex(22);
			((Control)label10).set_Text("Planet");
			((Control)panel1).set_Anchor((AnchorStyles)1);
			((Control)panel1).set_BackColor(Color.Honeydew);
			panel1.set_BorderStyle((BorderStyle)2);
			((Control)panel1).get_Controls().Add((Control)(object)chkIncludeInvalid);
			((Control)panel1).get_Controls().Add((Control)(object)label9);
			((Control)panel1).set_Location(new Point(6, 70));
			((Control)panel1).set_Name("panel1");
			((Control)panel1).set_Size(new Size(592, 27));
			((Control)panel1).set_TabIndex(24);
			((Control)chkIncludeInvalid).set_AutoSize(true);
			((Control)chkIncludeInvalid).set_Location(new Point(388, 5));
			((Control)chkIncludeInvalid).set_Name("chkIncludeInvalid");
			((Control)chkIncludeInvalid).set_Size(new Size(192, 17));
			((Control)chkIncludeInvalid).set_TabIndex(21);
			((Control)chkIncludeInvalid).set_Text("Include spurious and invalid events");
			((ButtonBase)chkIncludeInvalid).set_UseVisualStyleBackColor(true);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(731, 480));
			((Control)this).get_Controls().Add((Control)(object)panel1);
			((Control)this).get_Controls().Add((Control)(object)txtPlanet);
			((Control)this).get_Controls().Add((Control)(object)label10);
			((Control)this).get_Controls().Add((Control)(object)chkOldFormat);
			((Control)this).get_Controls().Add((Control)(object)txtXZ);
			((Control)this).get_Controls().Add((Control)(object)cmbNames);
			((Control)this).get_Controls().Add((Control)(object)cmdList);
			((Control)this).get_Controls().Add((Control)(object)label8);
			((Control)this).get_Controls().Add((Control)(object)label7);
			((Control)this).get_Controls().Add((Control)(object)label6);
			((Control)this).get_Controls().Add((Control)(object)label5);
			((Control)this).get_Controls().Add((Control)(object)label4);
			((Control)this).get_Controls().Add((Control)(object)label3);
			((Control)this).get_Controls().Add((Control)(object)label2);
			((Control)this).get_Controls().Add((Control)(object)txtObserver);
			((Control)this).get_Controls().Add((Control)(object)txtMonth);
			((Control)this).get_Controls().Add((Control)(object)txtDay);
			((Control)this).get_Controls().Add((Control)(object)txtZC);
			((Control)this).get_Controls().Add((Control)(object)txtSAO);
			((Control)this).get_Controls().Add((Control)(object)txtYear);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)lstEvents);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationLunarGrazeHistory", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_Location(Settings.Default.LocationLunarGrazeHistory);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_MinimumSize(new Size(747, 300));
			((Control)this).set_Name("Grazes");
			((Control)this).set_Text("View/analyse historical grazes");
			((Form)this).add_Load((EventHandler)Grazes_Load);
			((Control)this).add_Resize((EventHandler)Grazes_Resize);
			((Control)contextMenuStrip1).ResumeLayout(false);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)panel1).ResumeLayout(false);
			((Control)panel1).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
