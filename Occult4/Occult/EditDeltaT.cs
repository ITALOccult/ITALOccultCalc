using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.Drawing;
using System.IO;
using System.Text;
using System.Windows.Forms;
using Occult.Lunar_Observations;
using Occult.Properties;

namespace Occult
{
	public class EditDeltaT : Form
	{
		private readonly string AppPath;

		private static bool DataSaved = true;

		private static bool EditFlag = false;

		private int Tindex;

		private int TAindex;

		private deltaT dTline;

		private deltaTA dTAline;

		private List<deltaT> dTlist;

		private List<deltaTA> dTAlist;

		private List<dTResiduals> dTresidualsAll;

		private List<dTResiduals> dTresidualsYear;

		private dTResiduals dTinSecs;

		private double[] dT_Raw_Year = new double[261];

		private double[] dT_Raw_Value = new double[261];

		private int[] dT_Raw_Count = new int[261];

		private int NumRawVals;

		private double[] dT_RunAve_Year = new double[261];

		private double[] dT_RunAve_Value = new double[261];

		private double[] dT_ByYear_Value = new double[261];

		private const int ResidualColumn = 303;

		private const int ResidualRateColumn = 320;

		private const int MoonRateColumn = 327;

		private const int LimbHeightColumn = 296;

		private const int PAcolumn = 314;

		private IContainer components;

		private ListBox lstDeltaT;

		private ListBox lstDeltaTA;

		private Label label1;

		private Label label2;

		private Label label4;

		private Label label5;

		private Label label6;

		private Label label7;

		private Label label8;

		private Label label9;

		private GroupBox grpTA;

		private Label label12;

		private TextBox txtdUT;

		private Label label11;

		private Label label10;

		private TextBox txtDTA;

		private TextBox txtYearDTA;

		private GroupBox grpT;

		private Label label14;

		private Label label13;

		private TextBox txtDT;

		private TextBox txtYearDT;

		private Button cmdCancel_TA;

		private Button cmdOK_TA;

		private Button cmdCancelT;

		private Button cmdOKT;

		private Panel panelTA;

		private Button cmdDeleteTA;

		private Button cmdEditTA;

		private Button cmdAddTA;

		private Panel panelT;

		private Button cmdDeleteT;

		private Button cmdEditT;

		private Button cmdAddT;

		private Button cmdSave;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem fileToolStripMenuItem;

		private ToolStripMenuItem openToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator2;

		private ToolStripMenuItem helpToolStripMenuItem;

		private Button cmdDowload_EOP_Old;

		private Button cmdDowload_EOP_new;

		private Label label16;

		private Label label17;

		private Label label20;

		private ToolStripMenuItem exitToolStripMenuItem1;

		private Button cmdGetAverages;

		private Label label3;

		private Panel panelCorrect;

		private Label lblCorrectionMessage;

		private Panel panelCorrection;

		private Label label21;

		private ToolStripMenuItem copyDeltaTCorrection16001960ToolStripMenuItem;

		private Button cmdComputeVizier;

		private ListBox lstRawValues;

		private ToolStripMenuItem copyDeltaTRawValuesToolStripMenuItem;

		private Button cmdCopyList;

		private LinkLabel LinkEarthsRotation;

		private LinkLabel LinkSplineValues;

		private Label label22;

		private LinkLabel LinkAddendum;

		public EditDeltaT()
		{
			InitializeComponent();
			AppPath = Utilities.AppPath;
			((Control)panelCorrect).set_Enabled(Settings.Default.Administrator);
			((ToolStripItem)copyDeltaTRawValuesToolStripMenuItem).set_Visible(Settings.Default.Administrator);
		}

		public void EditDeltaT_Load(object sender, EventArgs e)
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
			dTlist = new List<deltaT>();
			dTAlist = new List<deltaTA>();
			if (!File.Exists(AppPath + "\\Resource Files\\Vizier Archive.txt"))
			{
				((Control)panelCorrect).set_Enabled(false);
				((Control)panelCorrection).set_Visible(true);
			}
			LoadFiles();
		}

		private void LoadFiles()
		{
			lstDeltaT.get_Items().Clear();
			lstDeltaTA.get_Items().Clear();
			dTlist.Clear();
			dTAlist.Clear();
			for (int i = -2000; i < 1600; i += 100)
			{
				dTline = new deltaT();
				dTline.Year = i;
				dTline.dT = (float)Utilities.delta_T(i, 1, 1.0);
				dTlist.Add(dTline);
			}
			for (int j = 1600; j < 1800; j += 10)
			{
				dTline = new deltaT();
				dTline.Year = j;
				dTline.dT = (float)Utilities.delta_T(j, 1, 1.0);
				dTlist.Add(dTline);
			}
			for (int k = 1800; k < 2025; k++)
			{
				dTline = new deltaT();
				dTline.Year = k;
				dTline.dT = (float)Utilities.delta_T(k, 1, 1.0);
				dTlist.Add(dTline);
			}
			for (int l = 2025; l < 2100; l += 10)
			{
				dTline = new deltaT();
				dTline.Year = l;
				dTline.dT = (float)Utilities.delta_T(l, 1, 1.0);
				dTlist.Add(dTline);
			}
			Refresh_deltaTlist();
			((ListControl)lstDeltaT).set_SelectedIndex(dTlist.Count - 12);
			StreamReader streamReader = new StreamReader(AppPath + "\\Resource Files\\DeltaTA.dat");
			do
			{
				dTAline = new deltaTA();
				dTAline.AddYearEntry(streamReader.ReadLine());
				dTAlist.Add(dTAline);
			}
			while (!streamReader.EndOfStream);
			streamReader.Close();
			Refresh_deltaTAlist();
			if (lstDeltaTA.get_Items().get_Count() > 0)
			{
				TAindex = lstDeltaTA.get_Items().get_Count() - 1;
				((ListControl)lstDeltaTA).set_SelectedIndex(TAindex);
				DecodeTA();
			}
			DataSaved = true;
		}

		private void EditDeltaT_FormClosing(object sender, FormClosingEventArgs e)
		{
			//IL_0014: Unknown result type (might be due to invalid IL or missing references)
			//IL_001a: Invalid comparison between Unknown and I4
			if (!DataSaved && (int)MessageBox.Show("Some changes to the data has not been Saved\n\nDo you want to Save the data tables?", "Changes have not been saved!", (MessageBoxButtons)4, (MessageBoxIcon)48) == 6)
			{
				SaveFiles();
			}
		}

		private void lstDeltaT_KeyUp(object sender, KeyEventArgs e)
		{
			DecodeT();
		}

		private void lstDeltaT_Click(object sender, EventArgs e)
		{
			DecodeT();
		}

		private void DecodeT()
		{
			int selectedIndex = ((ListControl)lstDeltaT).get_SelectedIndex();
			((Control)txtYearDT).set_Text(dTlist[selectedIndex].Year.ToString());
			((Control)txtDT).set_Text(dTlist[selectedIndex].dT.ToString());
		}

		private void cmdAddT_Click(object sender, EventArgs e)
		{
			HideControls();
			((Control)grpT).set_Enabled(true);
			EditFlag = false;
		}

		private void cmdEditT_Click(object sender, EventArgs e)
		{
			HideControls();
			((Control)grpT).set_Enabled(true);
			EditFlag = true;
		}

		private void cmdDeleteT_Click(object sender, EventArgs e)
		{
			//IL_0029: Unknown result type (might be due to invalid IL or missing references)
			//IL_002f: Invalid comparison between Unknown and I4
			if ((int)MessageBox.Show("Delete  " + lstDeltaT.get_SelectedItem(), "Delete a delta T entry", (MessageBoxButtons)4, (MessageBoxIcon)32) == 6)
			{
				Tindex = ((ListControl)lstDeltaT).get_SelectedIndex();
				dTlist.RemoveAt(Tindex);
				Refresh_deltaTlist();
				if ((Tindex >= lstDeltaT.get_Items().get_Count()) & (Tindex > 0))
				{
					Tindex--;
				}
				((ListControl)lstDeltaT).set_SelectedIndex(Tindex);
				ResetControls();
				DataSaved = false;
			}
		}

		private void cmdOKT_Click(object sender, EventArgs e)
		{
			Tindex = ((ListControl)lstDeltaT).get_SelectedIndex();
			if (EditFlag)
			{
				dTlist[Tindex].Year = Convert.ToInt32(((Control)txtYearDT).get_Text());
				dTlist[Tindex].dT = Convert.ToSingle(((Control)txtDT).get_Text());
			}
			else
			{
				dTline = new deltaT();
				dTline.AddYearEntry(((Control)txtYearDT).get_Text().Trim().PadRight(6) + ": " + ((Control)txtDT).get_Text().Trim());
				dTlist.Add(dTline);
				lstDeltaT.get_Items().Add((object)(((Control)txtYearDT).get_Text().Trim().PadRight(5) + ": " + ((Control)txtDT).get_Text().Trim()));
			}
			Refresh_deltaTlist();
			((ListControl)lstDeltaT).set_SelectedIndex(Tindex);
			ResetControls();
			DataSaved = false;
		}

		private void cmdCancelT_Click(object sender, EventArgs e)
		{
			ResetControls();
		}

		private void Refresh_deltaTlist()
		{
			lstDeltaT.get_Items().Clear();
			int count = dTlist.Count;
			dTlist.Sort();
			for (int i = 0; i < count; i++)
			{
				lstDeltaT.get_Items().Add((object)dTlist[i].ToString());
			}
		}

		private void lstDeltaTA_Click(object sender, EventArgs e)
		{
			DecodeTA();
		}

		private void lstDeltaTA_KeyUp(object sender, KeyEventArgs e)
		{
			DecodeTA();
		}

		private void DecodeTA()
		{
			int selectedIndex = ((ListControl)lstDeltaTA).get_SelectedIndex();
			((Control)txtYearDTA).set_Text(dTAlist[selectedIndex].Year.ToString());
			((Control)txtDTA).set_Text(dTAlist[selectedIndex].dTA.ToString());
			((Control)txtdUT).set_Text(dTAlist[selectedIndex].dUT.ToString());
		}

		private void cmdAddTA_Click(object sender, EventArgs e)
		{
			HideControls();
			((Control)grpTA).set_Enabled(true);
			EditFlag = false;
		}

		private void cmdEditTA_Click(object sender, EventArgs e)
		{
			HideControls();
			((Control)grpTA).set_Enabled(true);
			EditFlag = true;
		}

		private void cmdDeleteTA_Click(object sender, EventArgs e)
		{
			//IL_0029: Unknown result type (might be due to invalid IL or missing references)
			//IL_002f: Invalid comparison between Unknown and I4
			if ((int)MessageBox.Show("Delete  " + lstDeltaTA.get_SelectedItem(), "Delete a delta TA entry", (MessageBoxButtons)4, (MessageBoxIcon)32) == 6)
			{
				TAindex = ((ListControl)lstDeltaTA).get_SelectedIndex();
				dTAlist.RemoveAt(TAindex);
				Refresh_deltaTAlist();
				if ((TAindex >= lstDeltaTA.get_Items().get_Count()) & (TAindex > 0))
				{
					TAindex--;
				}
				((ListControl)lstDeltaTA).set_SelectedIndex(TAindex);
				ResetControls();
				DataSaved = false;
			}
		}

		private void cmdOK_TA_Click(object sender, EventArgs e)
		{
			TAindex = ((ListControl)lstDeltaTA).get_SelectedIndex();
			if (EditFlag)
			{
				dTAlist[TAindex].Year = Convert.ToSingle(((Control)txtYearDTA).get_Text());
				dTAlist[TAindex].dTA = (int)Convert.ToSingle(((Control)txtDTA).get_Text());
				dTAlist[TAindex].dUT = Convert.ToSingle(((Control)txtdUT).get_Text());
			}
			else
			{
				dTAline = new deltaTA();
				dTAline.AddYearEntry(((Control)txtYearDTA).get_Text().Trim().PadRight(7) + ":" + ((Control)txtDTA).get_Text().Trim().PadLeft(4) + ((Control)txtdUT).get_Text().Trim().PadLeft(5));
				dTAlist.Add(dTAline);
			}
			Refresh_deltaTAlist();
			((ListControl)lstDeltaTA).set_SelectedIndex(TAindex);
			ResetControls();
			DataSaved = false;
		}

		private void cmdCancel_TA_Click(object sender, EventArgs e)
		{
			ResetControls();
		}

		private void Refresh_deltaTAlist()
		{
			lstDeltaTA.get_Items().Clear();
			int count = dTAlist.Count;
			dTAlist.Sort();
			for (int i = 0; i < count; i++)
			{
				lstDeltaTA.get_Items().Add((object)dTAlist[i].ToString());
			}
		}

		private void ResetControls()
		{
			((Control)lstDeltaTA).set_Enabled(true);
			((Control)lstDeltaT).set_Enabled(true);
			((Control)grpTA).set_Enabled(false);
			((Control)grpT).set_Enabled(false);
			((Control)panelTA).set_Visible(true);
			((Control)panelT).set_Visible(true);
			DecodeTA();
			DecodeT();
		}

		private void HideControls()
		{
			((Control)lstDeltaTA).set_Enabled(false);
			((Control)lstDeltaT).set_Enabled(false);
			((Control)panelTA).set_Visible(false);
			((Control)panelT).set_Visible(false);
		}

		private void cmdSave_Click(object sender, EventArgs e)
		{
			SaveFiles();
		}

		private void SaveFiles()
		{
			StreamWriter streamWriter = new StreamWriter(AppPath + "\\Resource Files\\DeltaTA.dat");
			int count = dTAlist.Count;
			dTAlist.Sort();
			for (int i = 0; i < count; i++)
			{
				streamWriter.WriteLine(dTAlist[i].ToString());
			}
			streamWriter.Close();
			DataSaved = true;
		}

		private void openToolStripMenuItem_Click(object sender, EventArgs e)
		{
			LoadFiles();
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SaveFiles();
		}

		private void exitToolStripMenuItem1_Click_1(object sender, EventArgs e)
		{
			((Form)this).Close();
		}

		private void txtYearDTA_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtYearDTA).SelectAll();
		}

		private void txtYearDT_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtYearDT).SelectAll();
		}

		private void txtDTA_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtDTA).SelectAll();
		}

		private void txtDT_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtDT).SelectAll();
		}

		private void txtdUT_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtdUT).SelectAll();
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"DeltaT");
		}

		private void cmdDowload_EOP_Old_Click(object sender, EventArgs e)
		{
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			if (!Utilities.InternetIsAvailable())
			{
				MessageBox.Show("This functionality requires internet access. You are not connected to the internet", "No internet");
				return;
			}
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			ftp.Download_EOP_Old(SupressMessages: false);
			((Control)this).set_Cursor(Cursors.get_Default());
		}

		private void cmdDowload_EOP_new_Click(object sender, EventArgs e)
		{
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			if (!Utilities.InternetIsAvailable())
			{
				MessageBox.Show("This functionality requires internet access. You are not connected to the internet", "No internet");
				return;
			}
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			ftp.Download_EOP_current(SupressMessages: false);
			((Control)this).set_Cursor(Cursors.get_Default());
		}

		private void cmdGetAverages_Click(object sender, EventArgs e)
		{
			GetAverageValues_new();
		}

		internal void GetAverageValues_new()
		{
			//IL_0039: Unknown result type (might be due to invalid IL or missing references)
			//IL_0128: Unknown result type (might be due to invalid IL or missing references)
			//IL_012e: Invalid comparison between Unknown and I4
			double num = 0.5;
			string text = Utilities.AppPath + "\\Resource Files\\Vizier Archive.txt";
			if (!File.Exists(text))
			{
				MessageBox.Show("The file:\r\n   " + Utilities.AppPath + "\\Resource Files\\Vizier Archive.txt \\r\\ndoes not exist. \\r\\n\\r\\n The process cannot run until it is created");
				return;
			}
			new FileInfo(text).Refresh();
			DateTime lastAccessTime = File.GetLastAccessTime(text);
			new FileInfo(Utilities.AppPath + "\\Resource Files\\DeltaT.dat").Refresh();
			DateTime lastWriteTime = File.GetLastWriteTime(Utilities.AppPath + "\\Resource Files\\DeltaT.dat");
			if (lastAccessTime.CompareTo(lastWriteTime) < 0)
			{
				StringBuilder stringBuilder = new StringBuilder();
				stringBuilder.AppendLine("This process is only valid when the file");
				stringBuilder.AppendLine("   " + Utilities.AppPath + "\\Resource Files\\Vizier Archive.txt");
				stringBuilder.AppendLine("has been created after the file of deltaT used to reduce observations.\r\n");
				stringBuilder.AppendLine("The current " + Utilities.AppPath + "\\Resource Files\\Vizier Archive.txt file was created on " + lastAccessTime.ToLongDateString());
				stringBuilder.AppendLine("The current deltaT file was created on " + lastWriteTime.ToLongDateString());
				stringBuilder.AppendLine("\r\nDo you want to continue?");
				if ((int)MessageBox.Show(stringBuilder.ToString(), "Vizier file created before deltaT file", (MessageBoxButtons)4, (MessageBoxIcon)16, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
				{
					return;
				}
			}
			Cursor.set_Current(Cursors.get_WaitCursor());
			dTresidualsAll = new List<dTResiduals>();
			using (StreamReader streamReader = new StreamReader(text))
			{
				do
				{
					string text2 = streamReader.ReadLine();
					if (!"SUWXZ".Contains(text2.Substring(73, 1)) && !"BFMSEOX".Contains(text2.Substring(26, 1)))
					{
						double num2 = Utilities.BesselianYear(int.Parse(text2.Substring(0, 4)), int.Parse(text2.Substring(4, 2)), int.Parse(text2.Substring(6, 2)));
						double num3 = double.Parse(text2.Substring(303, 10));
						double num4 = double.Parse(text2.Substring(320, 7));
						double num5 = double.Parse(text2.Substring(327, 6));
						num = ((num2 < 1800.0) ? 0.5 : ((!(num2 < 1830.0)) ? 0.7 : 0.6));
						if (!(Math.Abs(num4 / num5) < num))
						{
							dTinSecs = new dTResiduals();
							dTinSecs.Year = num2;
							dTinSecs.ResidualArcSec = num3;
							dTinSecs.TimeCorrection = (0.0 - num3) / num4;
							dTresidualsAll.Add(dTinSecs);
						}
					}
				}
				while (!streamReader.EndOfStream);
			}
			lstRawValues.get_Items().Clear();
			lstRawValues.get_Items().Add((object)"         deltaT   corrn           #");
			lstRawValues.get_Items().Add((object)"Year      used   to add   SDev   obs");
			NumRawVals = 0;
			int num6 = 7;
			double num7 = (double)(1700 - num6) - (double)num6 / 2.0;
			double num8 = 0.0;
			do
			{
				num7 += (double)num6;
				num6 = ((num7 < 1750.0) ? 7 : ((num7 < 1800.0) ? 5 : ((!(num7 < 1830.0)) ? 1 : 3)));
				num8 = num7 + (double)num6;
				dTresidualsYear = new List<dTResiduals>();
				for (int i = 0; i < dTresidualsAll.Count; i++)
				{
					if ((dTresidualsAll[i].Year >= num7) & (dTresidualsAll[i].Year < num8))
					{
						dTresidualsYear.Add(dTresidualsAll[i]);
					}
				}
				dTresidualsYear.Sort();
				double num9 = 20.0;
				double Mean = 0.0;
				double Mean2 = 0.0;
				double SDev = 0.0;
				List<double> list = new List<double>();
				for (int j = 0; j < 10; j++)
				{
					list = new List<double>();
					List<double> list2 = new List<double>();
					for (int k = 0; k < dTresidualsYear.Count; k++)
					{
						if (dTresidualsYear[k].ResidualInTimeAbs - Mean < num9)
						{
							list.Add(dTresidualsYear[k].TimeCorrection);
							list2.Add(dTresidualsYear[k].Year);
						}
					}
					Utilities.Mean_Sdev(list, out Mean, out SDev);
					Utilities.Mean_Sdev(list2, out Mean2, out var _);
					num9 = 3.0 * SDev;
				}
				double num10 = Utilities.delta_T(Utilities.BesselianYear_to_JD(Mean2));
				lstRawValues.get_Items().Add((object)string.Format("{0,7:f2}:  {1,5:f2}:  {2,5:f2}: {3,5:f2}: {4,4:f0}", Mean2, num10, Mean, SDev, list.Count));
			}
			while (num7 < 2021.0);
			Cursor.set_Current(Cursors.get_Default());
		}

		private void copyDeltaTRawValuesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			string text = "";
			for (int i = 0; i < lstRawValues.get_Items().get_Count(); i++)
			{
				text = text + lstRawValues.get_Items().get_Item(i).ToString() + "\r\n";
			}
			Clipboard.SetText(text);
		}

		private void copyDeltaTCorrection16001960ToolStripMenuItem_Click(object sender, EventArgs e)
		{
			string text = "deltaT for 1620-1960 (from saved deltaT file), with all corrections included\r\n\r\n";
			for (int i = 0; i < 100; i++)
			{
				text = ((i >= 20) ? ((i >= 61) ? (text + string.Format("{0,6:f1} {1,5:f1}    {2,6:f1} {3,5:f1}    {4,6:f1} {5,5:f1}", 1600 + i, Utilities.delta_T(1600 + i, 1, 1.0), 1700 + i, Utilities.delta_T(1700 + i, 1, 1.0), 1800 + i, Utilities.delta_T(1800 + i, 1, 1.0)) + "\r\n") : (text + string.Format("{0,6:f1} {1,5:f1}    {2,6:f1} {3,5:f1}    {4,6:f1} {5,5:f1}    {6,6:f1} {7,5:f1}", 1600 + i, Utilities.delta_T(1600 + i, 1, 1.0), 1700 + i, Utilities.delta_T(1700 + i, 1, 1.0), 1800 + i, Utilities.delta_T(1800 + i, 1, 1.0), 1900 + i, Utilities.delta_T(1900 + i, 1, 1.0)) + "\r\n")) : (text + string.Format("                {0,6:f1} {1,5:f1}    {2,6:f1} {3,5:f1}    {4,6:f1} {5,5:f1}", 1700 + i, Utilities.delta_T(1700 + i, 1, 1.0), 1800 + i, Utilities.delta_T(1800 + i, 1, 1.0), 1900 + i, Utilities.delta_T(1900 + i, 1, 1.0)) + "\r\n"));
			}
			Clipboard.SetText(text);
		}

		private void cmdComputeVizier_Click(object sender, EventArgs e)
		{
			//IL_0017: Unknown result type (might be due to invalid IL or missing references)
			//IL_001d: Invalid comparison between Unknown and I4
			if ((int)MessageBox.Show("Creating the Vizier archive will take several hours\r\n\r\nAre you sure you want to continue?", "Confirm", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 7)
			{
				LunarObservations.CreateVizierArchiveFile();
			}
		}

		private void cmdCopyList_Click(object sender, EventArgs e)
		{
			string text = "";
			for (int i = 0; i < lstRawValues.get_Items().get_Count(); i++)
			{
				text = text + lstRawValues.get_Items().get_Item(i).ToString() + "\r\n";
			}
			Clipboard.SetText(text);
		}

		private void LinkEarthsRotation_LinkClicked(object sender, LinkLabelLinkClickedEventArgs e)
		{
			LinkEarthsRotation.set_LinkVisited(true);
			Process.Start("https://royalsocietypublishing.org/doi/10.1098/rspa.2016.0404");
		}

		private void LinkSplineValues_LinkClicked(object sender, LinkLabelLinkClickedEventArgs e)
		{
			LinkEarthsRotation.set_LinkVisited(true);
			Process.Start("https://astro.ukho.gov.uk/nao/lvm/");
		}

		private void LinkAddendum_LinkClicked(object sender, LinkLabelLinkClickedEventArgs e)
		{
			LinkEarthsRotation.set_LinkVisited(true);
			Process.Start("https://royalsocietypublishing.org/doi/10.1098/rspa.2020.0776");
		}

		private void cmdStdDevs_Click(object sender, EventArgs e)
		{
			//IL_005a: Unknown result type (might be due to invalid IL or missing references)
			double[] res = new double[60000];
			double num = 0.0;
			int count = 0;
			int num2 = 1680;
			int num3 = 5;
			string path = Utilities.AppPath + "\\Resource Files\\Vizier Archive.txt";
			if (!File.Exists(path))
			{
				path = Utilities.AppPath + "\\Resource Files\\Vizier Archive before 1970.txt";
			}
			if (!File.Exists(path))
			{
				MessageBox.Show("'Vizier Archive {or Vizier Archive before 1970}.txt' file does not exist. Process cannot continue");
				return;
			}
			using StreamWriter streamWriter = new StreamWriter(Utilities.AppPath + "\\Resource Files\\Vizier_Residual_SDs.txt");
			using StreamReader streamReader = new StreamReader(path);
			num2 = 1635;
			int num4 = num2 + num3;
			do
			{
				string text = streamReader.ReadLine();
				if ("SUWXZ".Contains(text.Substring(73, 1)) || "BMMSEOX".Contains(text.Substring(26, 1)))
				{
					continue;
				}
				double num5 = Utilities.BesselianYear(int.Parse(text.Substring(0, 4)), int.Parse(text.Substring(4, 2)), int.Parse(text.Substring(6, 2)));
				if (num5 < (double)num2)
				{
					continue;
				}
				if (num5 < (double)num4)
				{
					res[count] = double.Parse(text.Substring(303, 10));
					if (Math.Abs(res[count]) < 2.0)
					{
						count++;
					}
					else if ((num5 < 1830.0 && num5 >= 1800.0) & (Math.Abs(res[count]) < 4.0))
					{
						count++;
					}
					else if ((num5 < 1800.0 && num5 >= 1715.0) & (Math.Abs(res[count]) < 8.0))
					{
						count++;
					}
					else if ((num5 < 1715.0) & (Math.Abs(res[count]) < 100.0))
					{
						count++;
					}
				}
				else
				{
					num = SDev(ref res, ref count);
					streamWriter.WriteLine(string.Format("{0,4:f0}  {1,5:f2}  {2,5:f0}", num2, num, count));
					count = 0;
					num2 = num4;
					num4 += num3;
				}
			}
			while (!streamReader.EndOfStream);
		}

		private double SDev(ref double[] res, ref int count)
		{
			double num = 0.0;
			double num2 = 0.0;
			double num3 = 0.0;
			for (int i = 0; i < count; i++)
			{
				num += res[i];
			}
			num2 = num / (double)count;
			for (int j = 0; j < count; j++)
			{
				num3 += (res[j] - num2) * (res[j] - num2);
			}
			if (count > 1)
			{
				return Math.Sqrt(num3 / (double)(count - 1));
			}
			return Math.Sqrt(num3);
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
			//IL_0200: Unknown result type (might be due to invalid IL or missing references)
			//IL_020a: Expected O, but got Unknown
			//IL_020b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0215: Expected O, but got Unknown
			//IL_0216: Unknown result type (might be due to invalid IL or missing references)
			//IL_0220: Expected O, but got Unknown
			//IL_0221: Unknown result type (might be due to invalid IL or missing references)
			//IL_022b: Expected O, but got Unknown
			//IL_022c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0236: Expected O, but got Unknown
			//IL_0237: Unknown result type (might be due to invalid IL or missing references)
			//IL_0241: Expected O, but got Unknown
			//IL_0242: Unknown result type (might be due to invalid IL or missing references)
			//IL_024c: Expected O, but got Unknown
			//IL_024d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0257: Expected O, but got Unknown
			//IL_0258: Unknown result type (might be due to invalid IL or missing references)
			//IL_0262: Expected O, but got Unknown
			//IL_0263: Unknown result type (might be due to invalid IL or missing references)
			//IL_026d: Expected O, but got Unknown
			//IL_026e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0278: Expected O, but got Unknown
			//IL_0279: Unknown result type (might be due to invalid IL or missing references)
			//IL_0283: Expected O, but got Unknown
			//IL_0284: Unknown result type (might be due to invalid IL or missing references)
			//IL_028e: Expected O, but got Unknown
			//IL_028f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0299: Expected O, but got Unknown
			//IL_029a: Unknown result type (might be due to invalid IL or missing references)
			//IL_02a4: Expected O, but got Unknown
			//IL_02a5: Unknown result type (might be due to invalid IL or missing references)
			//IL_02af: Expected O, but got Unknown
			//IL_02b0: Unknown result type (might be due to invalid IL or missing references)
			//IL_02ba: Expected O, but got Unknown
			//IL_02bb: Unknown result type (might be due to invalid IL or missing references)
			//IL_02c5: Expected O, but got Unknown
			//IL_03bf: Unknown result type (might be due to invalid IL or missing references)
			//IL_03c9: Expected O, but got Unknown
			//IL_0479: Unknown result type (might be due to invalid IL or missing references)
			//IL_0483: Expected O, but got Unknown
			//IL_1fca: Unknown result type (might be due to invalid IL or missing references)
			//IL_1fd4: Expected O, but got Unknown
			//IL_2054: Unknown result type (might be due to invalid IL or missing references)
			//IL_205e: Expected O, but got Unknown
			//IL_2145: Unknown result type (might be due to invalid IL or missing references)
			//IL_214f: Expected O, but got Unknown
			//IL_2373: Unknown result type (might be due to invalid IL or missing references)
			//IL_237d: Expected O, but got Unknown
			//IL_23f3: Unknown result type (might be due to invalid IL or missing references)
			//IL_23fd: Expected O, but got Unknown
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(EditDeltaT));
			lstDeltaT = new ListBox();
			lstDeltaTA = new ListBox();
			label1 = new Label();
			label2 = new Label();
			label4 = new Label();
			label5 = new Label();
			label6 = new Label();
			label7 = new Label();
			label8 = new Label();
			label9 = new Label();
			grpTA = new GroupBox();
			cmdCancel_TA = new Button();
			cmdOK_TA = new Button();
			label12 = new Label();
			txtdUT = new TextBox();
			label11 = new Label();
			label10 = new Label();
			txtDTA = new TextBox();
			txtYearDTA = new TextBox();
			grpT = new GroupBox();
			cmdCancelT = new Button();
			cmdOKT = new Button();
			label14 = new Label();
			label13 = new Label();
			txtDT = new TextBox();
			txtYearDT = new TextBox();
			panelTA = new Panel();
			cmdDeleteTA = new Button();
			cmdEditTA = new Button();
			cmdAddTA = new Button();
			panelT = new Panel();
			cmdDeleteT = new Button();
			cmdEditT = new Button();
			cmdAddT = new Button();
			cmdSave = new Button();
			menuStrip1 = new MenuStrip();
			fileToolStripMenuItem = new ToolStripMenuItem();
			openToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator = new ToolStripSeparator();
			saveToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator2 = new ToolStripSeparator();
			copyDeltaTCorrection16001960ToolStripMenuItem = new ToolStripMenuItem();
			copyDeltaTRawValuesToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem1 = new ToolStripMenuItem();
			cmdDowload_EOP_Old = new Button();
			cmdDowload_EOP_new = new Button();
			label16 = new Label();
			label17 = new Label();
			label20 = new Label();
			cmdGetAverages = new Button();
			label3 = new Label();
			panelCorrect = new Panel();
			cmdCopyList = new Button();
			panelCorrection = new Panel();
			lblCorrectionMessage = new Label();
			lstRawValues = new ListBox();
			cmdComputeVizier = new Button();
			label21 = new Label();
			LinkEarthsRotation = new LinkLabel();
			LinkSplineValues = new LinkLabel();
			label22 = new Label();
			LinkAddendum = new LinkLabel();
			((Control)grpTA).SuspendLayout();
			((Control)grpT).SuspendLayout();
			((Control)panelTA).SuspendLayout();
			((Control)panelT).SuspendLayout();
			((Control)menuStrip1).SuspendLayout();
			((Control)panelCorrect).SuspendLayout();
			((Control)panelCorrection).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)lstDeltaT).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstDeltaT).set_FormattingEnabled(true);
			lstDeltaT.set_ItemHeight(14);
			((Control)lstDeltaT).set_Location(new Point(351, 53));
			((Control)lstDeltaT).set_Name("lstDeltaT");
			((Control)lstDeltaT).set_Size(new Size(137, 228));
			((Control)lstDeltaT).set_TabIndex(4);
			lstDeltaT.add_Click((EventHandler)lstDeltaT_Click);
			((Control)lstDeltaT).add_KeyUp(new KeyEventHandler(lstDeltaT_KeyUp));
			((Control)lstDeltaTA).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstDeltaTA).set_FormattingEnabled(true);
			lstDeltaTA.set_ItemHeight(14);
			((Control)lstDeltaTA).set_Location(new Point(12, 53));
			((Control)lstDeltaTA).set_Name("lstDeltaTA");
			((Control)lstDeltaTA).set_Size(new Size(174, 228));
			lstDeltaTA.set_Sorted(true);
			((Control)lstDeltaTA).set_TabIndex(0);
			lstDeltaTA.add_Click((EventHandler)lstDeltaTA_Click);
			((Control)lstDeltaTA).add_KeyUp(new KeyEventHandler(lstDeltaTA_KeyUp));
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(15, 419));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(512, 17));
			((Control)label1).set_TabIndex(9);
			((Control)label1).set_Text("OCCULT corrects Universal Time to obtain Terrestial Time as follows:");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(17, 440));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(280, 13));
			((Control)label2).set_TabIndex(10);
			((Control)label2).set_Text("* For years covered by the table of ΔTA :   ΔTA + 32.184  ");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Location(new Point(17, 566));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(467, 13));
			((Control)label4).set_TabIndex(12);
			((Control)label4).set_Text("* Observations from 1962:  for rotational position of the Earth, correct UTC to UT1 using EOP data");
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Location(new Point(17, 459));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(477, 13));
			((Control)label5).set_TabIndex(13);
			((Control)label5).set_Text("* For the years 1961 to 1971 : internal expressions giving the difference from UTC  [see the Help file]");
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Location(new Point(17, 478));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(153, 13));
			((Control)label6).set_TabIndex(14);
			((Control)label6).set_Text("* For the years -2000 to +2500:");
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Location(new Point(17, 516));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(315, 13));
			((Control)label7).set_TabIndex(15);
			((Control)label7).set_Text("* For any other year: -20 + 32 . t . t, where t is centuries from 1820");
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label8).set_Location(new Point(80, 33));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(38, 17));
			((Control)label8).set_TabIndex(1);
			((Control)label8).set_Text("ΔTA");
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label9).set_Location(new Point(344, 33));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(151, 17));
			((Control)label9).set_TabIndex(5);
			((Control)label9).set_Text("ΔT - internal values");
			((Control)grpTA).get_Controls().Add((Control)(object)cmdCancel_TA);
			((Control)grpTA).get_Controls().Add((Control)(object)cmdOK_TA);
			((Control)grpTA).get_Controls().Add((Control)(object)label12);
			((Control)grpTA).get_Controls().Add((Control)(object)txtdUT);
			((Control)grpTA).get_Controls().Add((Control)(object)label11);
			((Control)grpTA).get_Controls().Add((Control)(object)label10);
			((Control)grpTA).get_Controls().Add((Control)(object)txtDTA);
			((Control)grpTA).get_Controls().Add((Control)(object)txtYearDTA);
			((Control)grpTA).set_Enabled(false);
			((Control)grpTA).set_Location(new Point(192, 53));
			((Control)grpTA).set_Name("grpTA");
			((Control)grpTA).set_Size(new Size(117, 133));
			((Control)grpTA).set_TabIndex(2);
			grpTA.set_TabStop(false);
			((Control)grpTA).set_Text("Edit  ΔTA");
			((Control)cmdCancel_TA).set_Location(new Point(58, 104));
			((Control)cmdCancel_TA).set_Name("cmdCancel_TA");
			((Control)cmdCancel_TA).set_Size(new Size(50, 20));
			((Control)cmdCancel_TA).set_TabIndex(7);
			((Control)cmdCancel_TA).set_Text("Cancel");
			((ButtonBase)cmdCancel_TA).set_UseVisualStyleBackColor(true);
			((Control)cmdCancel_TA).add_Click((EventHandler)cmdCancel_TA_Click);
			((Control)cmdOK_TA).set_Location(new Point(11, 104));
			((Control)cmdOK_TA).set_Name("cmdOK_TA");
			((Control)cmdOK_TA).set_Size(new Size(31, 20));
			((Control)cmdOK_TA).set_TabIndex(6);
			((Control)cmdOK_TA).set_Text("OK");
			((ButtonBase)cmdOK_TA).set_UseVisualStyleBackColor(true);
			((Control)cmdOK_TA).add_Click((EventHandler)cmdOK_TA_Click);
			((Control)label12).set_AutoSize(true);
			((Control)label12).set_Location(new Point(24, 80));
			((Control)label12).set_Name("label12");
			((Control)label12).set_Size(new Size(28, 13));
			((Control)label12).set_TabIndex(4);
			((Control)label12).set_Text("dUT");
			label12.set_TextAlign(ContentAlignment.MiddleRight);
			((Control)txtdUT).set_Location(new Point(58, 77));
			((Control)txtdUT).set_Name("txtdUT");
			((Control)txtdUT).set_Size(new Size(50, 20));
			((Control)txtdUT).set_TabIndex(5);
			((Control)txtdUT).add_Enter((EventHandler)txtdUT_Enter);
			((Control)label11).set_AutoSize(true);
			((Control)label11).set_Location(new Point(24, 51));
			((Control)label11).set_Name("label11");
			((Control)label11).set_Size(new Size(28, 13));
			((Control)label11).set_TabIndex(2);
			((Control)label11).set_Text("ΔTA");
			label11.set_TextAlign(ContentAlignment.MiddleRight);
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Location(new Point(23, 18));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(29, 13));
			((Control)label10).set_TabIndex(0);
			((Control)label10).set_Text("Year");
			label10.set_TextAlign(ContentAlignment.MiddleRight);
			((Control)txtDTA).set_Location(new Point(58, 48));
			((Control)txtDTA).set_Name("txtDTA");
			((Control)txtDTA).set_Size(new Size(50, 20));
			((Control)txtDTA).set_TabIndex(3);
			((Control)txtDTA).add_Enter((EventHandler)txtDTA_Enter);
			((Control)txtYearDTA).set_Location(new Point(58, 15));
			((Control)txtYearDTA).set_Name("txtYearDTA");
			((Control)txtYearDTA).set_Size(new Size(50, 20));
			((Control)txtYearDTA).set_TabIndex(1);
			((Control)txtYearDTA).add_Enter((EventHandler)txtYearDTA_Enter);
			((Control)grpT).get_Controls().Add((Control)(object)cmdCancelT);
			((Control)grpT).get_Controls().Add((Control)(object)cmdOKT);
			((Control)grpT).get_Controls().Add((Control)(object)label14);
			((Control)grpT).get_Controls().Add((Control)(object)label13);
			((Control)grpT).get_Controls().Add((Control)(object)txtDT);
			((Control)grpT).get_Controls().Add((Control)(object)txtYearDT);
			((Control)grpT).set_Enabled(false);
			((Control)grpT).set_Location(new Point(496, 53));
			((Control)grpT).set_Name("grpT");
			((Control)grpT).set_Size(new Size(110, 133));
			((Control)grpT).set_TabIndex(6);
			grpT.set_TabStop(false);
			((Control)grpT).set_Text("Edit  ΔT");
			((Control)grpT).set_Visible(false);
			((Control)cmdCancelT).set_Location(new Point(51, 104));
			((Control)cmdCancelT).set_Name("cmdCancelT");
			((Control)cmdCancelT).set_Size(new Size(50, 20));
			((Control)cmdCancelT).set_TabIndex(5);
			((Control)cmdCancelT).set_Text("Cancel");
			((ButtonBase)cmdCancelT).set_UseVisualStyleBackColor(true);
			((Control)cmdCancelT).add_Click((EventHandler)cmdCancelT_Click);
			((Control)cmdOKT).set_Location(new Point(5, 104));
			((Control)cmdOKT).set_Name("cmdOKT");
			((Control)cmdOKT).set_Size(new Size(31, 20));
			((Control)cmdOKT).set_TabIndex(4);
			((Control)cmdOKT).set_Text("OK");
			((ButtonBase)cmdOKT).set_UseVisualStyleBackColor(true);
			((Control)cmdOKT).add_Click((EventHandler)cmdOKT_Click);
			((Control)label14).set_AutoSize(true);
			((Control)label14).set_Location(new Point(23, 56));
			((Control)label14).set_Name("label14");
			((Control)label14).set_Size(new Size(21, 13));
			((Control)label14).set_TabIndex(2);
			((Control)label14).set_Text("ΔT");
			label14.set_TextAlign(ContentAlignment.MiddleRight);
			((Control)label13).set_AutoSize(true);
			((Control)label13).set_Location(new Point(15, 22));
			((Control)label13).set_Name("label13");
			((Control)label13).set_Size(new Size(29, 13));
			((Control)label13).set_TabIndex(0);
			((Control)label13).set_Text("Year");
			label13.set_TextAlign(ContentAlignment.MiddleRight);
			((Control)txtDT).set_Location(new Point(51, 53));
			((Control)txtDT).set_Name("txtDT");
			((Control)txtDT).set_Size(new Size(50, 20));
			((Control)txtDT).set_TabIndex(3);
			((Control)txtDT).add_Enter((EventHandler)txtDT_Enter);
			((Control)txtYearDT).set_Location(new Point(51, 19));
			((Control)txtYearDT).set_Name("txtYearDT");
			((Control)txtYearDT).set_Size(new Size(50, 20));
			((Control)txtYearDT).set_TabIndex(1);
			((Control)txtYearDT).add_Enter((EventHandler)txtYearDT_Enter);
			((Control)panelTA).get_Controls().Add((Control)(object)cmdDeleteTA);
			((Control)panelTA).get_Controls().Add((Control)(object)cmdEditTA);
			((Control)panelTA).get_Controls().Add((Control)(object)cmdAddTA);
			((Control)panelTA).set_Location(new Point(192, 192));
			((Control)panelTA).set_Name("panelTA");
			((Control)panelTA).set_Size(new Size(127, 79));
			((Control)panelTA).set_TabIndex(3);
			((Control)cmdDeleteTA).set_Location(new Point(6, 56));
			((Control)cmdDeleteTA).set_Name("cmdDeleteTA");
			((Control)cmdDeleteTA).set_Size(new Size(115, 22));
			((Control)cmdDeleteTA).set_TabIndex(2);
			((Control)cmdDeleteTA).set_Text("Delete selected year");
			((ButtonBase)cmdDeleteTA).set_UseVisualStyleBackColor(true);
			((Control)cmdDeleteTA).add_Click((EventHandler)cmdDeleteTA_Click);
			((Control)cmdEditTA).set_Location(new Point(6, 28));
			((Control)cmdEditTA).set_Name("cmdEditTA");
			((Control)cmdEditTA).set_Size(new Size(115, 22));
			((Control)cmdEditTA).set_TabIndex(1);
			((Control)cmdEditTA).set_Text("Edit selected year");
			((ButtonBase)cmdEditTA).set_UseVisualStyleBackColor(true);
			((Control)cmdEditTA).add_Click((EventHandler)cmdEditTA_Click);
			((Control)cmdAddTA).set_Location(new Point(5, 0));
			((Control)cmdAddTA).set_Name("cmdAddTA");
			((Control)cmdAddTA).set_Size(new Size(115, 22));
			((Control)cmdAddTA).set_TabIndex(0);
			((Control)cmdAddTA).set_Text("Add a new year");
			((ButtonBase)cmdAddTA).set_UseVisualStyleBackColor(true);
			((Control)cmdAddTA).add_Click((EventHandler)cmdAddTA_Click);
			((Control)panelT).get_Controls().Add((Control)(object)cmdDeleteT);
			((Control)panelT).get_Controls().Add((Control)(object)cmdEditT);
			((Control)panelT).get_Controls().Add((Control)(object)cmdAddT);
			((Control)panelT).set_Enabled(false);
			((Control)panelT).set_Location(new Point(496, 193));
			((Control)panelT).set_Name("panelT");
			((Control)panelT).set_Size(new Size(123, 78));
			((Control)panelT).set_TabIndex(7);
			((Control)panelT).set_Visible(false);
			((Control)cmdDeleteT).set_Location(new Point(4, 56));
			((Control)cmdDeleteT).set_Name("cmdDeleteT");
			((Control)cmdDeleteT).set_Size(new Size(119, 22));
			((Control)cmdDeleteT).set_TabIndex(2);
			((Control)cmdDeleteT).set_Text("Delete selected year");
			((ButtonBase)cmdDeleteT).set_UseVisualStyleBackColor(true);
			((Control)cmdDeleteT).add_Click((EventHandler)cmdDeleteT_Click);
			((Control)cmdEditT).set_Location(new Point(4, 28));
			((Control)cmdEditT).set_Name("cmdEditT");
			((Control)cmdEditT).set_Size(new Size(119, 22));
			((Control)cmdEditT).set_TabIndex(1);
			((Control)cmdEditT).set_Text("Edit selected year");
			((ButtonBase)cmdEditT).set_UseVisualStyleBackColor(true);
			((Control)cmdEditT).add_Click((EventHandler)cmdEditT_Click);
			((Control)cmdAddT).set_Location(new Point(4, 0));
			((Control)cmdAddT).set_Name("cmdAddT");
			((Control)cmdAddT).set_Size(new Size(119, 22));
			((Control)cmdAddT).set_TabIndex(0);
			((Control)cmdAddT).set_Text("Add a new year");
			((ButtonBase)cmdAddT).set_UseVisualStyleBackColor(true);
			((Control)cmdAddT).add_Click((EventHandler)cmdAddT_Click);
			((Control)cmdSave).set_Location(new Point(152, 300));
			((Control)cmdSave).set_Name("cmdSave");
			((Control)cmdSave).set_Size(new Size(240, 24));
			((Control)cmdSave).set_TabIndex(8);
			((Control)cmdSave).set_Text("Save ΔTA  table");
			((ButtonBase)cmdSave).set_UseVisualStyleBackColor(true);
			((Control)cmdSave).add_Click((EventHandler)cmdSave_Click);
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)fileToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem1
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(870, 24));
			((Control)menuStrip1).set_TabIndex(16);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)fileToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[6]
			{
				(ToolStripItem)openToolStripMenuItem,
				(ToolStripItem)toolStripSeparator,
				(ToolStripItem)saveToolStripMenuItem,
				(ToolStripItem)toolStripSeparator2,
				(ToolStripItem)copyDeltaTCorrection16001960ToolStripMenuItem,
				(ToolStripItem)copyDeltaTRawValuesToolStripMenuItem
			});
			((ToolStripItem)fileToolStripMenuItem).set_Name("fileToolStripMenuItem");
			((ToolStripItem)fileToolStripMenuItem).set_Size(new Size(61, 20));
			((ToolStripItem)fileToolStripMenuItem).set_Text("&File...     ");
			((ToolStripItem)openToolStripMenuItem).set_Image((Image)componentResourceManager.GetObject("openToolStripMenuItem.Image"));
			((ToolStripItem)openToolStripMenuItem).set_ImageTransparentColor(Color.Magenta);
			((ToolStripItem)openToolStripMenuItem).set_Name("openToolStripMenuItem");
			openToolStripMenuItem.set_ShortcutKeys((Keys)131151);
			((ToolStripItem)openToolStripMenuItem).set_Size(new Size(225, 22));
			((ToolStripItem)openToolStripMenuItem).set_Text("&re-Load");
			((ToolStripItem)openToolStripMenuItem).add_Click((EventHandler)openToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator).set_Name("toolStripSeparator");
			((ToolStripItem)toolStripSeparator).set_Size(new Size(222, 6));
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)componentResourceManager.GetObject("saveToolStripMenuItem.Image"));
			((ToolStripItem)saveToolStripMenuItem).set_ImageTransparentColor(Color.Magenta);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			saveToolStripMenuItem.set_ShortcutKeys((Keys)131155);
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(225, 22));
			((ToolStripItem)saveToolStripMenuItem).set_Text("&Save");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator2).set_Name("toolStripSeparator2");
			((ToolStripItem)toolStripSeparator2).set_Size(new Size(222, 6));
			((ToolStripItem)copyDeltaTCorrection16001960ToolStripMenuItem).set_Image((Image)Resources.deltaT);
			((ToolStripItem)copyDeltaTCorrection16001960ToolStripMenuItem).set_Name("copyDeltaTCorrection16001960ToolStripMenuItem");
			((ToolStripItem)copyDeltaTCorrection16001960ToolStripMenuItem).set_Size(new Size(225, 22));
			((ToolStripItem)copyDeltaTCorrection16001960ToolStripMenuItem).set_Text("Copy ΔT values 1600-1960");
			((ToolStripItem)copyDeltaTCorrection16001960ToolStripMenuItem).add_Click((EventHandler)copyDeltaTCorrection16001960ToolStripMenuItem_Click);
			((ToolStripItem)copyDeltaTRawValuesToolStripMenuItem).set_Name("copyDeltaTRawValuesToolStripMenuItem");
			((ToolStripItem)copyDeltaTRawValuesToolStripMenuItem).set_Size(new Size(225, 22));
			((ToolStripItem)copyDeltaTRawValuesToolStripMenuItem).set_Text("Copy new  'raw' values of ΔT");
			((ToolStripItem)copyDeltaTRawValuesToolStripMenuItem).add_Click((EventHandler)copyDeltaTRawValuesToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(72, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("&Help    ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem1).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem1).set_Name("exitToolStripMenuItem1");
			((ToolStripItem)exitToolStripMenuItem1).set_Size(new Size(54, 20));
			((ToolStripItem)exitToolStripMenuItem1).set_Text("Exit");
			((ToolStripItem)exitToolStripMenuItem1).add_Click((EventHandler)exitToolStripMenuItem1_Click_1);
			((Control)cmdDowload_EOP_Old).set_Location(new Point(153, 357));
			((Control)cmdDowload_EOP_Old).set_Name("cmdDowload_EOP_Old");
			((Control)cmdDowload_EOP_Old).set_Size(new Size(141, 39));
			((Control)cmdDowload_EOP_Old).set_TabIndex(18);
			((Control)cmdDowload_EOP_Old).set_Text("Download EOP series\r\n- pre 1962");
			((ButtonBase)cmdDowload_EOP_Old).set_UseVisualStyleBackColor(true);
			((Control)cmdDowload_EOP_Old).add_Click((EventHandler)cmdDowload_EOP_Old_Click);
			((Control)cmdDowload_EOP_new).set_Location(new Point(343, 357));
			((Control)cmdDowload_EOP_new).set_Name("cmdDowload_EOP_new");
			((Control)cmdDowload_EOP_new).set_Size(new Size(141, 39));
			((Control)cmdDowload_EOP_new).set_TabIndex(19);
			((Control)cmdDowload_EOP_new).set_Text("Download EOP series\r\n- 1962 to present");
			((ButtonBase)cmdDowload_EOP_new).set_UseVisualStyleBackColor(true);
			((Control)cmdDowload_EOP_new).add_Click((EventHandler)cmdDowload_EOP_new_Click);
			((Control)label16).set_AutoSize(true);
			((Control)label16).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label16).set_Location(new Point(117, 337));
			((Control)label16).set_Name("label16");
			((Control)label16).set_Size(new Size(406, 17));
			((Control)label16).set_TabIndex(20);
			((Control)label16).set_Text("Earth Orientation Parameters (Polar motion, UT1-UTC)");
			((Control)label17).set_AutoSize(true);
			((Control)label17).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label17).set_Location(new Point(15, 548));
			((Control)label17).set_Name("label17");
			((Control)label17).set_Size(new Size(540, 17));
			((Control)label17).set_TabIndex(21);
			((Control)label17).set_Text("OCCULT computes the geocentric coordinates of the observer as follows:");
			((Control)label20).set_AutoSize(true);
			((Control)label20).set_Location(new Point(17, 580));
			((Control)label20).set_Name("label20");
			((Control)label20).set_Size(new Size(427, 13));
			((Control)label20).set_TabIndex(24);
			((Control)label20).set_Text("* Observations from 1846:  correct site longitude/latitude for polar motion, using EOP data");
			((Control)cmdGetAverages).set_Location(new Point(29, 456));
			((Control)cmdGetAverages).set_Name("cmdGetAverages");
			((Control)cmdGetAverages).set_Size(new Size(103, 31));
			((Control)cmdGetAverages).set_TabIndex(25);
			((Control)cmdGetAverages).set_Text("Get new values");
			((ButtonBase)cmdGetAverages).set_UseVisualStyleBackColor(true);
			((Control)cmdGetAverages).add_Click((EventHandler)cmdGetAverages_Click);
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label3).set_Location(new Point(570, 33));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(292, 17));
			((Control)label3).set_TabIndex(28);
			((Control)label3).set_Text(" ΔT correction - from the Vizier Archive");
			((Control)panelCorrect).get_Controls().Add((Control)(object)cmdCopyList);
			((Control)panelCorrect).get_Controls().Add((Control)(object)panelCorrection);
			((Control)panelCorrect).get_Controls().Add((Control)(object)lstRawValues);
			((Control)panelCorrect).get_Controls().Add((Control)(object)cmdComputeVizier);
			((Control)panelCorrect).get_Controls().Add((Control)(object)cmdGetAverages);
			((Control)panelCorrect).set_Enabled(false);
			((Control)panelCorrect).set_Location(new Point(566, 46));
			((Control)panelCorrect).set_Name("panelCorrect");
			((Control)panelCorrect).set_Size(new Size(300, 542));
			((Control)panelCorrect).set_TabIndex(34);
			((Control)cmdCopyList).set_Location(new Point(155, 456));
			((Control)cmdCopyList).set_Name("cmdCopyList");
			((Control)cmdCopyList).set_Size(new Size(120, 31));
			((Control)cmdCopyList).set_TabIndex(45);
			((Control)cmdCopyList).set_Text("Copy displayed list");
			((ButtonBase)cmdCopyList).set_UseVisualStyleBackColor(true);
			((Control)cmdCopyList).add_Click((EventHandler)cmdCopyList_Click);
			panelCorrection.set_BorderStyle((BorderStyle)1);
			((Control)panelCorrection).get_Controls().Add((Control)(object)lblCorrectionMessage);
			((Control)panelCorrection).set_Location(new Point(10, 71));
			((Control)panelCorrection).set_Name("panelCorrection");
			((Control)panelCorrection).set_Size(new Size(247, 75));
			((Control)panelCorrection).set_TabIndex(36);
			((Control)panelCorrection).set_Visible(false);
			((Control)lblCorrectionMessage).set_AutoSize(true);
			((Control)lblCorrectionMessage).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblCorrectionMessage).set_Location(new Point(13, 8));
			((Control)lblCorrectionMessage).set_Name("lblCorrectionMessage");
			((Control)lblCorrectionMessage).set_Size(new Size(172, 60));
			((Control)lblCorrectionMessage).set_TabIndex(35);
			((Control)lblCorrectionMessage).set_Text("ΔT correction functionality\r\n  is not available as the \r\n    'Vizier Archive'\r\n  file is not present");
			lblCorrectionMessage.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)lstRawValues).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstRawValues).set_FormattingEnabled(true);
			lstRawValues.set_ItemHeight(14);
			((Control)lstRawValues).set_Location(new Point(6, 8));
			((Control)lstRawValues).set_Name("lstRawValues");
			((Control)lstRawValues).set_Size(new Size(289, 438));
			((Control)lstRawValues).set_TabIndex(42);
			((Control)cmdComputeVizier).set_Location(new Point(71, 502));
			((Control)cmdComputeVizier).set_Name("cmdComputeVizier");
			((Control)cmdComputeVizier).set_Size(new Size(158, 26));
			((Control)cmdComputeVizier).set_TabIndex(39);
			((Control)cmdComputeVizier).set_Text("Generate the Vizier Archive ");
			((ButtonBase)cmdComputeVizier).set_UseVisualStyleBackColor(true);
			((Control)cmdComputeVizier).add_Click((EventHandler)cmdComputeVizier_Click);
			((Control)label21).set_AutoSize(true);
			((Control)label21).set_Location(new Point(46, 497));
			((Control)label21).set_Name("label21");
			((Control)label21).set_Size(new Size(172, 13));
			((Control)label21).set_TabIndex(35);
			((Control)label21).set_Text("with the values from the Tables  at ");
			((Control)LinkEarthsRotation).set_AutoSize(true);
			((Control)LinkEarthsRotation).set_Location(new Point(167, 478));
			((Control)LinkEarthsRotation).set_Name("LinkEarthsRotation");
			((Control)LinkEarthsRotation).set_Size(new Size(273, 13));
			((Control)LinkEarthsRotation).set_TabIndex(36);
			LinkEarthsRotation.set_TabStop(true);
			((Control)LinkEarthsRotation).set_Text("Measurement of the Earth’s rotation: 720 BC to AD 2015");
			LinkEarthsRotation.add_LinkClicked(new LinkLabelLinkClickedEventHandler(LinkEarthsRotation_LinkClicked));
			((Control)LinkSplineValues).set_AutoSize(true);
			((Control)LinkSplineValues).set_Location(new Point(213, 497));
			((Control)LinkSplineValues).set_Name("LinkSplineValues");
			((Control)LinkSplineValues).set_Size(new Size(112, 13));
			((Control)LinkSplineValues).set_TabIndex(37);
			LinkSplineValues.set_TabStop(true);
			((Control)LinkSplineValues).set_Text("Table of Spline values");
			LinkSplineValues.add_LinkClicked(new LinkLabelLinkClickedEventHandler(LinkSplineValues_LinkClicked));
			((Control)label22).set_AutoSize(true);
			((Control)label22).set_Location(new Point(436, 478));
			((Control)label22).set_Name("label22");
			((Control)label22).set_Size(new Size(71, 13));
			((Control)label22).set_TabIndex(38);
			((Control)label22).set_Text(", and its 2020");
			((Control)LinkAddendum).set_AutoSize(true);
			((Control)LinkAddendum).set_Location(new Point(505, 478));
			((Control)LinkAddendum).set_Name("LinkAddendum");
			((Control)LinkAddendum).set_Size(new Size(58, 13));
			((Control)LinkAddendum).set_TabIndex(39);
			LinkAddendum.set_TabStop(true);
			((Control)LinkAddendum).set_Text("Addendum");
			LinkAddendum.add_LinkClicked(new LinkLabelLinkClickedEventHandler(LinkAddendum_LinkClicked));
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(870, 605));
			((Control)this).get_Controls().Add((Control)(object)label3);
			((Control)this).get_Controls().Add((Control)(object)LinkAddendum);
			((Control)this).get_Controls().Add((Control)(object)label22);
			((Control)this).get_Controls().Add((Control)(object)LinkSplineValues);
			((Control)this).get_Controls().Add((Control)(object)LinkEarthsRotation);
			((Control)this).get_Controls().Add((Control)(object)label21);
			((Control)this).get_Controls().Add((Control)(object)panelCorrect);
			((Control)this).get_Controls().Add((Control)(object)label20);
			((Control)this).get_Controls().Add((Control)(object)label17);
			((Control)this).get_Controls().Add((Control)(object)label16);
			((Control)this).get_Controls().Add((Control)(object)cmdDowload_EOP_new);
			((Control)this).get_Controls().Add((Control)(object)cmdDowload_EOP_Old);
			((Control)this).get_Controls().Add((Control)(object)cmdSave);
			((Control)this).get_Controls().Add((Control)(object)panelT);
			((Control)this).get_Controls().Add((Control)(object)panelTA);
			((Control)this).get_Controls().Add((Control)(object)grpT);
			((Control)this).get_Controls().Add((Control)(object)grpTA);
			((Control)this).get_Controls().Add((Control)(object)label9);
			((Control)this).get_Controls().Add((Control)(object)label8);
			((Control)this).get_Controls().Add((Control)(object)label7);
			((Control)this).get_Controls().Add((Control)(object)label6);
			((Control)this).get_Controls().Add((Control)(object)label5);
			((Control)this).get_Controls().Add((Control)(object)label4);
			((Control)this).get_Controls().Add((Control)(object)label2);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)lstDeltaTA);
			((Control)this).get_Controls().Add((Control)(object)lstDeltaT);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationDefaultsDeltaT", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_FormBorderStyle((FormBorderStyle)2);
			((Form)this).set_Icon((Icon)componentResourceManager.GetObject("$this.Icon"));
			((Form)this).set_Location(Settings.Default.LocationDefaultsDeltaT);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Form)this).set_MaximizeBox(false);
			((Control)this).set_Name("EditDeltaT");
			((Control)this).set_Text("    Edit  ΔTA  and  ΔT ;             download Earth Orientation Parameters");
			((Form)this).add_FormClosing(new FormClosingEventHandler(EditDeltaT_FormClosing));
			((Form)this).add_Load((EventHandler)EditDeltaT_Load);
			((Control)grpTA).ResumeLayout(false);
			((Control)grpTA).PerformLayout();
			((Control)grpT).ResumeLayout(false);
			((Control)grpT).PerformLayout();
			((Control)panelTA).ResumeLayout(false);
			((Control)panelT).ResumeLayout(false);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)panelCorrect).ResumeLayout(false);
			((Control)panelCorrection).ResumeLayout(false);
			((Control)panelCorrection).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
