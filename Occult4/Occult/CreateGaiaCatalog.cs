using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Threading;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult
{
	public class CreateGaiaCatalog : Form
	{
		internal static string CurrentInputSourcePath = "c:\\";

		private const double Radian = 180.0 / Math.PI;

		private const double MilliSecInDeg = 3600000.0;

		private const double GaiaEDR3Epoch = 16.0;

		private bool FromMatchToU5;

		private string[] ReservedFilesLower;

		private List<GAIAsource> GaiaStars = new List<GAIAsource>();

		private GAIAsource Gsource;

		private List<TGASsource> TgasStars = new List<TGASsource>();

		private TGASsource Tsource;

		private List<HIPsource> HipStars = new List<HIPsource>();

		private HIPsource Hsource;

		private List<U4> U4Current = new List<U4>();

		private U4 U4;

		private List<FindClosest> Closest = new List<FindClosest>();

		private FindClosest Nearest;

		private static string WorkingDirectory = Utilities.AppPath + "\\Resource Files\\Gaia\\";

		private IContainer components;

		private ListBox lstGaiaFiles;

		private TextBox txtNewName;

		private Label label2;

		private OpenFileDialog openFileDialog_TapVizieR;

		private Label label5;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private Label label11;

		private Label label12;

		private Button cmdCreateDR3;

		private Label label22;

		private TabControl tabGaia;

		private TabPage tabPage1;

		private TabPage tabPage2;

		private TabPage tabPage3;

		private Label label38;

		private Label label37;

		private TextBox textBox6;

		private TextBox textBox3;

		private TextBox textBox2;

		private Label label36;

		private Label label35;

		private Label label33;

		private TextBox textBox1;

		private Label label32;

		private Label label31;

		private Label label29;

		private TextBox txtTapVizieR;

		private TextBox txtTGAS_ADQL;

		private Label label27;

		private TextBox txtHIP_ADQL;

		private Label label26;

		private GroupBox groupBox1;

		private Button cmdSelectDirectory;

		private TextBox txtWorkingDirectory;

		private FolderBrowserDialog folderBrowserDialog1;

		private Label label43;

		private Label label41;

		private Label label40;

		private GroupBox groupBox3;

		private Panel panel1;

		private Label label1;

		private Button cmdFieldUp;

		private Button cmdFieldDown;

		private Label label46;

		private Label label45;

		private Label label44;

		private Label label7;

		private Label label4;

		private ListBox lstColumns;

		private ListBox lstFields;

		private GroupBox groupBox2;

		private Panel panel3;

		private Label label8;

		private Button cmdFileDown;

		private Button cmdFileUp;

		private Label label39;

		private ListBox lstFilesToConvert;

		private Button cmdSelectFile;

		private Label label3;

		private ListBox lstDecNth;

		private Label label6;

		private ComboBox cmbWidth;

		private ListBox lstDecSth;

		private CheckBox chkAssume;

		private Label label10;

		private GroupBox groupBox5;

		private CheckBox chkMatchToUCAC4;

		private GroupBox groupBox4;

		private Label label25;

		private GroupBox groupBox6;

		private Button cmdCreateSubset;

		private NumericUpDown updnLimitingMag;

		private Label label19;

		private Button cmdCreateHipparcosIndex;

		private Button cmdUCAC4Index;

		private GroupBox groupBox7;

		private Label lblProcess;

		private Label label52;

		private Label lblZone;

		private Label label21;

		private Label lblTGAStotalAddedCount;

		private Label lblHIPtotalMatchedCount;

		private Label lblHIPtotalAddedCount;

		private Label lblUCAC4_Zone;

		private Label label59;

		private Label label61;

		private Label lblUCAC4BandGoodMatch;

		private Label label64;

		private Label lblTotalTGASreadCount;

		private Label label62;

		private Label lblTotalHIPReadCount;

		private Label label60;

		private Label lblTotalGaiaReadCount;

		private Label label58;

		private Label label57;

		private Label lblTGAStotalMatchedCount;

		private Label lblUCAC4ZoneGoodMatch;

		private Label lblUCAC4BandCount;

		private Label lblUCAC4ZoneCount;

		private Label label51;

		private Label label73;

		private Label label72;

		private Label lblTGASzoneaddedCount;

		private Label lblHIPzonematchedCount;

		private Label lblHIPzoneaddedCount;

		private Label lblTGASzoneCount;

		private Label lblHIPzoneCount;

		private Label lblGaiazoneCount;

		private Label lblTGASzonematchedCount;

		private Label label53;

		private Label label9;

		private Label label14;

		private Label lblUCAC4TotalNoMatch;

		private Label lblUCAC4TotalPoorMatch;

		private Label lblUCAC4TotalGoodMatch;

		private Label lblUCAC4BandNoMatch;

		private Label lblUCAC4ZoneNoMatch;

		private Label lblUCAC4BandPoorMatch;

		private Label lblUCAC4ZonePoorMatch;

		private Label label20;

		private Label label18;

		private Label label17;

		private Label label16;

		private Label label15;

		private Label lblUCAC4ZoneDoubleMatchRemoved;

		private Label lblUCAC4TotalDoubleMatchRemoved;

		private Label label23;

		private Label label28;

		private Label lblUCAC4BandDoubleMatchRemoved;

		private Label lblTotalNoProperMotion;

		private Label label42;

		private Label lblZoneNoProperMotion;

		private Label lblu4Files;

		private Label lblUCAC4BandPM_Added;

		private Label lblUCAC4ZonePM_Added;

		private Label lblUCAC4TotalPM_Added;

		private Label label49;

		private Label label34;

		private Label label47;

		private Label label30;

		private CheckBox chkIgnore;

		private Label label48;

		private TextBox textBox7;

		private TextBox textBox5;

		private TextBox textBox4;

		private GroupBox groupBox8;

		private Label label54;

		private Label label24;

		private Label label50;

		private Label lblTotalZoneStars;

		private Label label55;

		private Label lblTotalStars;

		private Button cmdBadHipStars;

		public CreateGaiaCatalog()
		{
			InitializeComponent();
			if (!Directory.Exists(WorkingDirectory))
			{
				Directory.CreateDirectory(WorkingDirectory);
			}
			ReservedFilesLower = new string[Gaia.ReservedGaiaFiles.Length];
			for (int i = 0; i < Gaia.ReservedGaiaFiles.Length; i++)
			{
				ReservedFilesLower[i] = Gaia.ReservedGaiaFiles[i].ToLower();
			}
		}

		private void CreateGaiaCatalog_Load(object sender, EventArgs e)
		{
			Settings.Default.GaiaWorkingDir = ((Control)txtWorkingDirectory).get_Text();
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			ListExistingCatalogues();
			CheckBox obj = chkMatchToUCAC4;
			bool enabled;
			chkMatchToUCAC4.set_Checked(enabled = File.Exists(Settings.Default.UCAC4_Path + "\\u4b\\z001"));
			((Control)obj).set_Enabled(enabled);
			((ListControl)cmbWidth).set_SelectedIndex(2);
			((Control)cmdUCAC4Index).set_Visible(Settings.Default.AdministratorGlobal);
		}

		private void ListExistingCatalogues()
		{
			lstGaiaFiles.get_Items().Clear();
			if (File.Exists(Utilities.AppPath + "\\Resource Files\\Gaia\\" + Gaia.ReservedGaiaFiles[0] + ".bin"))
			{
				lstGaiaFiles.get_Items().Add((object)Gaia.ReservedGaiaFiles[0]);
			}
			if (File.Exists(Utilities.AppPath + "\\Resource Files\\Gaia\\" + Gaia.ReservedGaiaFiles[1] + ".bin"))
			{
				lstGaiaFiles.get_Items().Add((object)Gaia.ReservedGaiaFiles[1]);
			}
			if (File.Exists(Utilities.AppPath + "\\Resource Files\\Gaia\\" + Gaia.ReservedGaiaFiles[2] + ".bin"))
			{
				lstGaiaFiles.get_Items().Add((object)Gaia.ReservedGaiaFiles[2]);
			}
			if (File.Exists(Utilities.AppPath + "\\Resource Files\\Gaia\\" + Gaia.ReservedGaiaFiles[3] + ".bin"))
			{
				lstGaiaFiles.get_Items().Add((object)Gaia.ReservedGaiaFiles[3]);
			}
			lstGaiaFiles.get_Items().Add((object)"=============================");
			string[] files = Directory.GetFiles(Utilities.AppPath + "\\Resource Files\\Gaia");
			string Fname;
			foreach (string path in files)
			{
				Fname = Path.GetFileName(path);
				if (!Array.Exists(ReservedFilesLower, (string element) => element == Fname.Replace(".bin", "").ToLower()) && Fname.ToLower().EndsWith(".bin"))
				{
					lstGaiaFiles.get_Items().Add((object)Path.GetFileNameWithoutExtension(Fname));
				}
			}
		}

		private void txtNewName_Leave(object sender, EventArgs e)
		{
			//IL_006f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0075: Invalid comparison between Unknown and I4
			//IL_00ce: Unknown result type (might be due to invalid IL or missing references)
			//IL_00d4: Invalid comparison between Unknown and I4
			if (FromMatchToU5)
			{
				return;
			}
			string A = ((Control)txtNewName).get_Text().Trim() + "_EDR3.bin";
			if (Array.Exists(ReservedFilesLower, (string element) => element == A.Replace(".bin", "").ToLower()) && (int)MessageBox.Show("The file you have specified\r\n    " + A + "\r\nis an Occult downloadable catalogue.\r\n\r\nAre you sure you want to use this file name?", "System catalogue file", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
			{
				((Control)txtNewName).Focus();
				return;
			}
			for (int i = 0; i < lstGaiaFiles.get_Items().get_Count(); i++)
			{
				string text = lstGaiaFiles.get_Items().get_Item(i).ToString()!.ToLower();
				if (A.ToLower() == text)
				{
					if ((int)MessageBox.Show("The specified file already exists.\r\n\r\nDo you want to overwrite this file?", "File exists", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 6)
					{
						((Control)txtNewName).Focus();
					}
					break;
				}
			}
		}

		private void cmdSelectFile_Click(object sender, EventArgs e)
		{
			//IL_0053: Unknown result type (might be due to invalid IL or missing references)
			//IL_0059: Invalid comparison between Unknown and I4
			//IL_0250: Unknown result type (might be due to invalid IL or missing references)
			((FileDialog)openFileDialog_TapVizieR).set_Title("Select ADQL CSVfiles to convert");
			((FileDialog)openFileDialog_TapVizieR).set_Filter("CSV Files (.csv)|*.csv|All Files (*.*)|*.*");
			((FileDialog)openFileDialog_TapVizieR).set_FilterIndex(1);
			openFileDialog_TapVizieR.set_Multiselect(true);
			((FileDialog)openFileDialog_TapVizieR).set_InitialDirectory(Settings.Default.GaiaWorkingDir);
			if ((int)((CommonDialog)openFileDialog_TapVizieR).ShowDialog() != 1)
			{
				return;
			}
			string fileName = ((FileDialog)openFileDialog_TapVizieR).get_FileName();
			string text = (CurrentInputSourcePath = (Settings.Default.TAPVizier_Directory = Path.GetDirectoryName(fileName)));
			lstFilesToConvert.get_Items().Clear();
			lstDecNth.get_Items().Clear();
			lstDecSth.get_Items().Clear();
			string North = "";
			string South = "";
			int num = ((FileDialog)openFileDialog_TapVizieR).get_FileNames().Length;
			int num2 = 0;
			string[] array = new string[num];
			string[] fileNames = ((FileDialog)openFileDialog_TapVizieR).get_FileNames();
			foreach (string text2 in fileNames)
			{
				lstFilesToConvert.get_Items().Add((object)Path.GetFileName(text2));
				GetDeclinationRange(text2, out North, out South, out array[num2]);
				num2++;
				lstDecNth.get_Items().Add((object)North);
				lstDecSth.get_Items().Add((object)South);
			}
			BubbleSort();
			using (StreamReader streamReader = new StreamReader(fileName))
			{
				string[] array2 = streamReader.ReadLine()!.Split(new char[1] { ',' });
				lstColumns.get_Items().Clear();
				for (int j = 0; j < array2.Length; j++)
				{
					lstColumns.get_Items().Add((object)array2[j]);
				}
			}
			string text3 = "";
			for (int k = 1; k < lstFilesToConvert.get_Items().get_Count(); k++)
			{
				if (array[k] != array[0])
				{
					text3 = text3 + string.Format("Line {0,1:f0}   ", k + 1) + lstFilesToConvert.get_Items().get_Item(k).ToString() + "\r\n";
				}
			}
			if (text3.Length > 0)
			{
				MessageBox.Show("The following files have a different header to the first file:\r\n" + text3 + "Are you sure you have correctly selected the files to use?\r\nAre you sure your TAPVizier ADQL queries were consistent?", "Different file formats", (MessageBoxButtons)0, (MessageBoxIcon)16);
			}
		}

		private void BubbleSort()
		{
			bool flag = false;
			int count = lstFilesToConvert.get_Items().get_Count();
			do
			{
				flag = false;
				for (int i = 0; i < count - 1; i++)
				{
					if (float.Parse(lstDecSth.get_Items().get_Item(i).ToString()) < float.Parse(lstDecSth.get_Items().get_Item(i + 1).ToString()))
					{
						MoveSelectedFileDown(i);
						flag = true;
					}
				}
			}
			while (flag);
		}

		private void MoveSelectedFileUp(int i)
		{
			if (i > 0)
			{
				string text = lstFilesToConvert.get_Items().get_Item(i).ToString();
				lstFilesToConvert.get_Items().RemoveAt(i);
				lstFilesToConvert.get_Items().Insert(i - 1, (object)text);
				((ListControl)lstFilesToConvert).set_SelectedIndex(i - 1);
				text = lstDecNth.get_Items().get_Item(i).ToString();
				lstDecNth.get_Items().RemoveAt(i);
				lstDecNth.get_Items().Insert(i - 1, (object)text);
				((ListControl)lstDecNth).set_SelectedIndex(i - 1);
				text = lstDecSth.get_Items().get_Item(i).ToString();
				lstDecSth.get_Items().RemoveAt(i);
				lstDecSth.get_Items().Insert(i - 1, (object)text);
				((ListControl)lstDecSth).set_SelectedIndex(i - 1);
			}
		}

		private void MoveSelectedFileDown(int i)
		{
			if (i < lstFilesToConvert.get_Items().get_Count() - 1)
			{
				string text = lstFilesToConvert.get_Items().get_Item(i).ToString();
				lstFilesToConvert.get_Items().RemoveAt(i);
				lstFilesToConvert.get_Items().Insert(i + 1, (object)text);
				((ListControl)lstFilesToConvert).set_SelectedIndex(i + 1);
				text = lstDecNth.get_Items().get_Item(i).ToString();
				lstDecNth.get_Items().RemoveAt(i);
				lstDecNth.get_Items().Insert(i + 1, (object)text);
				((ListControl)lstDecNth).set_SelectedIndex(i + 1);
				text = lstDecSth.get_Items().get_Item(i).ToString();
				lstDecSth.get_Items().RemoveAt(i);
				lstDecSth.get_Items().Insert(i + 1, (object)text);
				((ListControl)lstDecSth).set_SelectedIndex(i + 1);
			}
		}

		private void cmdFieldUp_Click(object sender, EventArgs e)
		{
			int selectedIndex = ((ListControl)lstFields).get_SelectedIndex();
			if (selectedIndex > 0)
			{
				string text = lstFields.get_Items().get_Item(selectedIndex).ToString();
				lstFields.get_Items().RemoveAt(selectedIndex);
				lstFields.get_Items().Insert(selectedIndex - 1, (object)text);
				((ListControl)lstFields).set_SelectedIndex(selectedIndex - 1);
			}
		}

		private void cmdFieldDown_Click(object sender, EventArgs e)
		{
			int selectedIndex = ((ListControl)lstFields).get_SelectedIndex();
			if (selectedIndex < lstFields.get_Items().get_Count() - 1)
			{
				string text = lstFields.get_Items().get_Item(selectedIndex).ToString();
				lstFields.get_Items().RemoveAt(selectedIndex);
				lstFields.get_Items().Insert(selectedIndex + 1, (object)text);
				((ListControl)lstFields).set_SelectedIndex(selectedIndex + 1);
			}
		}

		private void GetDeclinationRange(string File, out string North, out string South, out string HeaderLine)
		{
			double num = 10.0;
			double[] array = new double[5] { 20.0, 10.0, 5.0, 2.0, 180.0 };
			GetDeclinationRange(File, out double Max, out double Min, out HeaderLine);
			if (chkAssume.get_Checked())
			{
				num = array[((ListControl)cmbWidth).get_SelectedIndex()];
				Max = (int)(num * Math.Ceiling(Max / num));
				Min = (int)(num * Math.Floor(Min / num));
			}
			North = string.Format("{0,1:f1}", Max);
			South = string.Format("{0,1:f1}", Min);
		}

		private void GetDeclinationRange(string File, out double Max, out double Min, out string HeaderLine)
		{
			Max = -100.0;
			Min = 100.0;
			int num = 0;
			int num2 = 0;
			using StreamReader streamReader = new StreamReader(File);
			HeaderLine = streamReader.ReadLine();
			string[] array = HeaderLine.Split(new char[1] { ',' });
			for (int i = 0; i < array.Length; i++)
			{
				if (array[i].ToUpper().Contains("DE"))
				{
					num = i;
					break;
				}
			}
			do
			{
				array = streamReader.ReadLine()!.Split(new char[1] { ',' });
				double num3 = double.Parse(array[num]) / 3600000.0;
				if (num3 < Min)
				{
					Min = num3;
				}
				if (num3 > Max)
				{
					Max = num3;
				}
				num2++;
			}
			while (num2 <= 500 && !streamReader.EndOfStream);
			if (Max > 85.0)
			{
				Max = 90.0;
			}
			if (Min < -85.0)
			{
				Min = -90.0;
			}
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"User Gaia");
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private void lstGaiaFiles_Click(object sender, EventArgs e)
		{
			if (!lstGaiaFiles.get_Items().get_Item(((ListControl)lstGaiaFiles).get_SelectedIndex()).ToString()!.Contains("==="))
			{
				Gaia.ShowGaiaMap();
				((Control)Gaia.GaiaMap).set_Left(((Control)this).get_Left() - 100);
				((Control)Gaia.GaiaMap).set_Top(((Control)this).get_Top() + 100);
				Gaia.GaiaMap.MapCatalogueCoverage(lstGaiaFiles.get_Items().get_Item(((ListControl)lstGaiaFiles).get_SelectedIndex()).ToString());
			}
		}

		private void lstGaiaFiles_Leave(object sender, EventArgs e)
		{
			if (Gaia.GaiaMap != null)
			{
				((Form)Gaia.GaiaMap).Close();
				((Component)(object)Gaia.GaiaMap).Dispose();
			}
		}

		private void cmdUCAC4Index_Click(object sender, EventArgs e)
		{
			Gaia.CreateUCAC4IndexFile();
		}

		private void cmdCreateHipparcosIndex_Click(object sender, EventArgs e)
		{
			//IL_0046: Unknown result type (might be due to invalid IL or missing references)
			//IL_006b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0071: Invalid comparison between Unknown and I4
			string text = lstGaiaFiles.get_Items().get_Item(((ListControl)lstGaiaFiles).get_SelectedIndex()).ToString();
			if (text.Contains("==="))
			{
				MessageBox.Show("The currently selected file is :    " + text + "\r\n\r\nThis is not a valid file Please select a valid file", "Select GAIA catalogue", (MessageBoxButtons)0, (MessageBoxIcon)48);
			}
			else if ((int)MessageBox.Show("This function will create an index file for Hipparcos stars in the currently highlighted 'Existing Gaia file'\r\n\r\nThe currently selected file is :    " + text + "\r\n\r\nDo you want to creat a Hipparcos index file for this Gaia file", "Select GAIA catalogue", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152) != 7)
			{
				Gaia.CreateHipparcosIndexFile(text);
			}
		}

		private void cmdCreateSubset_Click(object sender, EventArgs e)
		{
			//IL_001b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0139: Unknown result type (might be due to invalid IL or missing references)
			//IL_0179: Unknown result type (might be due to invalid IL or missing references)
			//IL_017f: Invalid comparison between Unknown and I4
			if (((ListControl)lstGaiaFiles).get_SelectedIndex() < 0)
			{
				MessageBox.Show("You haven't selected a file tyo create the subset from.\r\n\r\nSelect the relevant file in the Existing Gaia Files list", "No source file", (MessageBoxButtons)0, (MessageBoxIcon)48);
				return;
			}
			string text = lstGaiaFiles.get_Items().get_Item(((ListControl)lstGaiaFiles).get_SelectedIndex()).ToString();
			double num = (double)updnLimitingMag.get_Value();
			string text2 = num.ToString().Replace(".", "");
			string text3 = text + "_" + text2;
			if (text == "Gaia16_EDR3" && num < 16.0)
			{
				text3 = "Gaia" + text2 + "_EDR3";
			}
			if (text == "Gaia14_EDR3" && num < 14.0)
			{
				text3 = "Gaia" + text2 + "_EDR3";
			}
			PBar pBar = new PBar();
			((Control)pBar).set_Text("Creating a subset file");
			pBar.pBarFTP.set_Minimum(0);
			pBar.pBarFTP.set_Maximum(361);
			pBar.pBarFTP.set_Value(0);
			((Control)pBar).Show();
			if (text.Contains("==="))
			{
				MessageBox.Show("The currently selected file is :    " + text + "\r\n\r\nThis is not a valid file Please select a valid file", "Select GAIA catalogue", (MessageBoxButtons)0, (MessageBoxIcon)48);
			}
			else
			{
				if ((int)MessageBox.Show("This function will create a subset of the currently highlighted 'Existing Gaia file', limited to stars with Visual mag brighter than " + text2 + ".00\r\n\r\nThe currently selected file is :    " + text + "\r\n\r\nDo you want to create a subset file from this Gaia file", "Select GAIA catalogue", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152) == 7)
				{
					return;
				}
				using (FileStream fileStream = new FileStream(Utilities.AppPath + "\\Resource Files\\Gaia\\" + text + ".bin", FileMode.Open))
				{
					BinaryReader gaia = new BinaryReader(fileStream);
					if (text.Contains("DR3"))
					{
						Gaia.CurrentRecordLength = 58L;
					}
					else
					{
						Gaia.CurrentRecordLength = 48L;
					}
					using FileStream fileStream2 = new FileStream(Utilities.AppPath + "\\Resource Files\\Gaia\\" + text + ".inx", FileMode.Open);
					BinaryReader binaryReader = new BinaryReader(fileStream2);
					using FileStream output = new FileStream(Utilities.AppPath + "\\Resource Files\\Gaia\\" + text3 + ".bin", FileMode.Create);
					BinaryWriter gaiaWriteFile = new BinaryWriter(output);
					using FileStream output2 = new FileStream(Utilities.AppPath + "\\Resource Files\\Gaia\\" + text3 + ".inx", FileMode.Create);
					BinaryWriter binaryWriter = new BinaryWriter(output2);
					int num2 = 0;
					int num3 = 0;
					int num4 = 0;
					int num5 = 0;
					int num6 = 0;
					for (int num7 = 179; num7 >= -180; num7--)
					{
						pBar.pBarFTP.set_Value(-num7 + 180);
						((Control)pBar).set_Text(string.Format("Creating a subset file. Zone {0,1:f1}°", (double)num7 / 2.0));
						Application.DoEvents();
						num2 = 361 * (179 - num7);
						if (!(num2 < 0 || num2 >= 129959))
						{
							fileStream2.Seek(num2 * 4, SeekOrigin.Begin);
							num3 = binaryReader.ReadInt32();
							if (num3 < 0)
							{
								num3 = 0;
							}
							fileStream2.Seek((num2 + 360) * 4, SeekOrigin.Begin);
							num4 = binaryReader.ReadInt32();
							num6 = 0;
							for (int i = num3; i < num4; i++)
							{
								Gaia.ReadNext(fileStream, gaia, i);
								if (Gaia.MagGreen <= num)
								{
									Gaia.WriteCurrentAsNextGaiaDR3Record(gaiaWriteFile);
									num5++;
								}
								for (; Gaia.RA_deg > (double)num6 && num6 <= 360; num6++)
								{
									binaryWriter.Write(num5);
								}
							}
							for (; num6 <= 360; num6++)
							{
								binaryWriter.Write(num5);
							}
						}
					}
				}
				((Control)pBar).Hide();
				Gaia.CreateHipparcosIndexFile(text3);
			}
		}

		private void cmdSelectDirectory_Click(object sender, EventArgs e)
		{
			//IL_0000: Unknown result type (might be due to invalid IL or missing references)
			//IL_0006: Expected O, but got Unknown
			//IL_0007: Unknown result type (might be due to invalid IL or missing references)
			//IL_000d: Invalid comparison between Unknown and I4
			FolderBrowserDialog val = new FolderBrowserDialog();
			if ((int)((CommonDialog)val).ShowDialog() == 1)
			{
				((Control)txtWorkingDirectory).set_Text(val.get_SelectedPath());
				Settings.Default.GaiaWorkingDir = ((Control)txtWorkingDirectory).get_Text();
			}
		}

		private void cmdFileUp_Click(object sender, EventArgs e)
		{
			int selectedIndex = ((ListControl)lstFilesToConvert).get_SelectedIndex();
			MoveSelectedFileUp(selectedIndex);
		}

		private void cmdFileDown_Click(object sender, EventArgs e)
		{
			int selectedIndex = ((ListControl)lstFilesToConvert).get_SelectedIndex();
			MoveSelectedFileDown(selectedIndex);
		}

		private void lstFilesToConvert_Click(object sender, EventArgs e)
		{
			ListBox obj = lstDecSth;
			int selectedIndex;
			((ListControl)lstDecNth).set_SelectedIndex(selectedIndex = ((ListControl)lstFilesToConvert).get_SelectedIndex());
			((ListControl)obj).set_SelectedIndex(selectedIndex);
		}

		private void lstDecNth_Click(object sender, EventArgs e)
		{
			ListBox obj = lstFilesToConvert;
			int selectedIndex;
			((ListControl)lstDecSth).set_SelectedIndex(selectedIndex = ((ListControl)lstDecNth).get_SelectedIndex());
			((ListControl)obj).set_SelectedIndex(selectedIndex);
		}

		private void lstDecSth_Click(object sender, EventArgs e)
		{
			ListBox obj = lstDecNth;
			int selectedIndex;
			((ListControl)lstFilesToConvert).set_SelectedIndex(selectedIndex = ((ListControl)lstDecSth).get_SelectedIndex());
			((ListControl)obj).set_SelectedIndex(selectedIndex);
		}

		private void cmdCreateDR3_Click(object sender, EventArgs e)
		{
			//IL_0110: Unknown result type (might be due to invalid IL or missing references)
			//IL_0161: Unknown result type (might be due to invalid IL or missing references)
			//IL_0167: Invalid comparison between Unknown and I4
			//IL_0181: Unknown result type (might be due to invalid IL or missing references)
			//IL_0187: Invalid comparison between Unknown and I4
			//IL_01a2: Unknown result type (might be due to invalid IL or missing references)
			double[] array = new double[14411];
			for (int i = 0; i < 14411; i++)
			{
				array[i] = (double)i / 4.0 * 3600000.0;
			}
			_ = new int[200];
			int[] array2 = new int[1441];
			GaiaStars = new List<GAIAsource>();
			double Max = 90.0;
			double Min = -90.0;
			int[] array3 = new int[26];
			int num = 0;
			string text = "";
			int num2 = 0;
			string uCAC4_Path = Settings.Default.UCAC4_Path;
			string path = WorkingDirectory + "\\TestData.dat";
			if (File.Exists(path))
			{
				File.Delete(path);
			}
			StreamWriter streamWriter = new StreamWriter(path, append: true);
			using (streamWriter)
			{
				streamWriter.WriteLine("RA            Dec            Par     pmRA   pmDec  Mag   sdevRA sdevDE sdevPM sdevPM   RUWE  doub  Ct Number");
				streamWriter.WriteLine();
			}
			int num3 = 0;
			if (((Control)txtNewName).get_Text().Trim() == "")
			{
				MessageBox.Show("You haven't specified an output file name.", "Output file name", (MessageBoxButtons)0, (MessageBoxIcon)48);
				return;
			}
			string text2 = ((Control)txtNewName).get_Text().Trim() + "_EDR3";
			if ((File.Exists(WorkingDirectory + text2 + ".bin") && (int)MessageBox.Show("The selected catalogue name:\r\n      " + text2 + "\r\nExists.\r\n\r\nDo you want to overwrite this catalogue?", "Output file name", (MessageBoxButtons)4, (MessageBoxIcon)48) == 7) || (int)MessageBox.Show("Have you set the columns to match the tags?\r\n\r\nIf you are not sure, select No, and return to Step 2 and review.", "Check tags matched to columns", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
			{
				return;
			}
			if (lstFilesToConvert.get_Items().get_Count() < 1)
			{
				MessageBox.Show("No TapVizier files listed");
				return;
			}
			for (int j = 0; j < 19; j++)
			{
				array3[j] = 0;
			}
			for (int k = 0; k < lstFields.get_Items().get_Count(); k++)
			{
				int num4 = int.Parse(lstFields.get_Items().get_Item(k).ToString()!.Substring(0, 2)) + 1;
				if ((num4 > 0) & (k < lstColumns.get_Items().get_Count()))
				{
					array3[num4] = k + 1;
				}
			}
			int num5 = 0;
			int num6 = 0;
			int num7 = 0;
			int num8 = 0;
			int num9 = 0;
			int num10 = 0;
			int num11 = 0;
			int num12 = 0;
			int num13 = 0;
			int num14 = 0;
			int num15 = 0;
			int num16 = 0;
			int num17 = 0;
			int num18 = 0;
			int num19 = 0;
			int num20 = 0;
			int num21 = 0;
			int num22 = 0;
			int num23 = 0;
			int num24 = 0;
			int num25 = 0;
			int num26 = 0;
			int num27 = 0;
			uint num28 = 0u;
			double num29 = 0.0;
			double num30 = 0.0;
			int index = 0;
			bool flag = true;
			((Control)lblTotalGaiaReadCount).set_Text(num7.ToString());
			((Control)lblTotalTGASreadCount).set_Text(num5.ToString());
			((Control)lblTotalHIPReadCount).set_Text(num6.ToString());
			((Control)lblTGAStotalMatchedCount).set_Text(num8.ToString());
			((Control)lblTGAStotalAddedCount).set_Text(num9.ToString());
			((Control)lblHIPtotalMatchedCount).set_Text(num10.ToString());
			((Control)lblHIPtotalAddedCount).set_Text(num11.ToString());
			((Control)lblTotalStars).set_Text((num7 + num9 + num11).ToString());
			((Control)lblTGASzoneCount).set_Text(num12.ToString());
			((Control)lblHIPzoneCount).set_Text(num13.ToString());
			((Control)lblGaiazoneCount).set_Text(num14.ToString());
			((Control)lblTGASzonematchedCount).set_Text(num15.ToString());
			((Control)lblTGASzoneaddedCount).set_Text(num16.ToString());
			((Control)lblHIPzonematchedCount).set_Text(num17.ToString());
			((Control)lblHIPzoneaddedCount).set_Text(num18.ToString());
			((Control)lblTotalZoneStars).set_Text((num14 + num16 + num18).ToString());
			((Control)lblUCAC4TotalGoodMatch).set_Text(num21.ToString());
			((Control)lblUCAC4TotalPoorMatch).set_Text(num22.ToString());
			((Control)lblUCAC4TotalNoMatch).set_Text(num23.ToString());
			((Control)lblUCAC4TotalDoubleMatchRemoved).set_Text(num24.ToString());
			((Control)lblTotalNoProperMotion).set_Text(num26.ToString());
			((Control)lblZoneNoProperMotion).set_Text(num27.ToString());
			using (FileStream output = new FileStream(WorkingDirectory + text2 + ".bin", FileMode.Create, FileAccess.Write))
			{
				BinaryWriter gaiaWriteFile = new BinaryWriter(output);
				using FileStream output2 = new FileStream(WorkingDirectory + text2 + ".inx", FileMode.Create, FileAccess.Write);
				BinaryWriter binaryWriter = new BinaryWriter(output2);
				num3 = 0;
				for (int num31 = 179; num31 >= -180; num31--)
				{
					double num32 = (double)num31 / 2.0 * 3600000.0;
					double num33 = num32 + 1800000.0;
					_ = (double)num31 / 2.0;
					num = 0;
					while (num != lstFilesToConvert.get_Items().get_Count() - 1 && (int)Math.Floor(2.0 * double.Parse(lstDecSth.get_Items().get_Item(num).ToString())) > num31)
					{
						num++;
						if (num >= lstFilesToConvert.get_Items().get_Count() - 1)
						{
							break;
						}
					}
					text = lstFilesToConvert.get_Items().get_Item(num).ToString();
					GetDeclinationRange(CurrentInputSourcePath + "\\" + text, out Max, out Min, out string _);
					if (2.0 * Max >= (double)num31 && 2.0 * Min - 0.5 < (double)num31)
					{
						((Control)lblZone).set_Text(string.Format("{0,1:f1}° to {1,1:f1}°", (double)num31 / 2.0 + 0.5, (double)num31 / 2.0));
						num12 = (num13 = (num14 = (num15 = (num16 = (num17 = (num18 = (num27 = 0)))))));
						int num35;
						int num37;
						int num34;
						int num36;
						int num38 = (num37 = (num36 = (num35 = (num34 = 0))));
						((Control)lblTGASzoneCount).set_Text(num12.ToString());
						((Control)lblHIPzoneCount).set_Text(num13.ToString());
						((Control)lblGaiazoneCount).set_Text(num14.ToString());
						((Control)lblTGASzonematchedCount).set_Text(num15.ToString());
						((Control)lblTGASzoneaddedCount).set_Text(num16.ToString());
						((Control)lblHIPzonematchedCount).set_Text(num17.ToString());
						((Control)lblHIPzoneaddedCount).set_Text(num18.ToString());
						((Control)lblUCAC4ZoneGoodMatch).set_Text(num38.ToString());
						((Control)lblUCAC4ZonePoorMatch).set_Text(num37.ToString());
						((Control)lblUCAC4ZoneNoMatch).set_Text(num36.ToString());
						((Control)lblUCAC4ZoneDoubleMatchRemoved).set_Text(num34.ToString());
						((Control)lblZoneNoProperMotion).set_Text(num27.ToString());
						Application.DoEvents();
						double num39 = num33 + 3000.0;
						double num40 = num32 - 3000.0;
						GaiaStars.Clear();
						TgasStars.Clear();
						HipStars.Clear();
						GaiaStars = new List<GAIAsource>();
						((Control)lblProcess).set_Text("Reading Gaia source file");
						Application.DoEvents();
						array2 = new int[1441];
						int num41 = 0;
						array2[num41] = 0;
						num41 = 1;
						using (StreamReader streamReader = new StreamReader(CurrentInputSourcePath + "\\" + text))
						{
							streamReader.ReadLine();
							do
							{
								string text3 = streamReader.ReadLine();
								double.TryParse(text3.Split(new char[1] { ',' })[1], out var result);
								if (result > num32 && result < num33)
								{
									Gsource = new GAIAsource();
									Gsource.ImportStarFromGaiaZone = text3;
									GaiaStars.Add(Gsource);
									if (Gsource.Reliability == 0.0)
									{
										num27++;
										num26++;
									}
									for (; Gsource.RAmas > array[num41]; num41++)
									{
										array2[num41] = num14;
									}
									num7++;
									num14++;
									if (num7 % 100 == 0)
									{
										((Control)lblTotalGaiaReadCount).set_Text(num7.ToString());
										((Control)lblGaiazoneCount).set_Text(num14.ToString());
										Application.DoEvents();
									}
								}
							}
							while (!streamReader.EndOfStream);
							array2[360] = num14 - 1;
						}
						((Control)lblTotalGaiaReadCount).set_Text(num7.ToString());
						((Control)lblGaiazoneCount).set_Text(num14.ToString());
						((Control)lblTotalStars).set_Text((num7 + num9 + num11).ToString());
						((Control)lblTotalZoneStars).set_Text((num14 + num16 + num18).ToString());
						Application.DoEvents();
						if (GaiaStars.Count != 0)
						{
							GAIAsource.SortByRA = true;
							GaiaStars.Sort();
							if (!chkIgnore.get_Checked())
							{
								((Control)lblProcess).set_Text("Reading Tycho-Gaia for Declination  ");
								Application.DoEvents();
								TgasStars = new List<TGASsource>();
								using (StreamReader streamReader2 = new StreamReader(CurrentInputSourcePath + "\\TGASraw.dat"))
								{
									streamReader2.ReadLine();
									do
									{
										Tsource = new TGASsource();
										string text3 = streamReader2.ReadLine();
										double.TryParse(text3.Split(new char[1] { ',' })[1], out var result2);
										if (result2 > num40 && result2 < num39)
										{
											Tsource.TgasLine = text3;
											TgasStars.Add(Tsource);
											num5++;
											num12++;
										}
										if (num5 % 100 == 0)
										{
											((Control)lblTotalTGASreadCount).set_Text(num5.ToString());
											((Control)lblTGASzoneCount).set_Text(num12.ToString());
											Application.DoEvents();
										}
									}
									while (!streamReader2.EndOfStream);
								}
								TGASsource.SortByRA_NotHip = false;
								TgasStars.Sort();
								((Control)lblTotalTGASreadCount).set_Text(num5.ToString());
								((Control)lblTGASzoneCount).set_Text(num12.ToString());
								for (int l = 0; l < TgasStars.Count; l++)
								{
									if (TgasStars[l].CatID != "2")
									{
										num2 = l - 1;
										break;
									}
								}
								((Control)lblProcess).set_Text("Reading Hipparcos, & Matching to Tycho-Gaia");
								Application.DoEvents();
								HipStars = new List<HIPsource>();
								using (StreamReader streamReader3 = new StreamReader(CurrentInputSourcePath + "\\HIPraw.dat"))
								{
									bool flag2 = false;
									streamReader3.ReadLine();
									do
									{
										Hsource = new HIPsource();
										string text3 = streamReader3.ReadLine();
										double.TryParse(text3.Split(new char[1] { ',' })[2], out var result3);
										if (!(result3 > num40 && result3 < num39))
										{
											continue;
										}
										Hsource.HipLine = text3;
										int num42 = 0;
										int num43 = num2;
										flag2 = false;
										do
										{
											int num44 = num42 + num43;
											if (Hsource.CatNumber == TgasStars[num44].CatNumber)
											{
												flag2 = true;
												break;
											}
											if (Hsource.CatNumber > TgasStars[num44].CatNumber)
											{
												num42 = num44 + 1;
											}
											else
											{
												num43 = num44 - 1;
											}
										}
										while (num43 >= num42);
										if (!flag2)
										{
											HipStars.Add(Hsource);
											num6++;
											num13++;
											((Control)lblTotalHIPReadCount).set_Text(num6.ToString());
											((Control)lblHIPzoneCount).set_Text(num13.ToString());
											Application.DoEvents();
										}
									}
									while (!streamReader3.EndOfStream);
								}
								HIPsource.SortByRA_NotHip = false;
								HipStars.Sort();
								((Control)lblTotalHIPReadCount).set_Text(num6.ToString());
								((Control)lblHIPzoneCount).set_Text(num13.ToString());
								Application.DoEvents();
								TGASsource.SortByRA_NotHip = true;
								TgasStars.Sort();
							}
							if (!chkIgnore.get_Checked())
							{
								((Control)lblProcess).set_Text("Matching Tycho-Gaia to Gaia stars");
								Application.DoEvents();
								double num45 = 100.0;
								for (int m = 0; m < TgasStars.Count; m++)
								{
									num28 = 0u;
									num29 = 0.0;
									index = 0;
									double rA_ = TgasStars[m].RA_2016;
									double dec_ = TgasStars[m].Dec_2016;
									double num46 = rA_ / 3600000.0 * 4.0;
									double mag = TgasStars[m].Mag;
									double cosDecFactor = TgasStars[m].CosDecFactor;
									int num47 = (int)num46;
									int num48 = num47 + 1;
									if (num46 % 1.0 < 0.01)
									{
										num47--;
									}
									if (num47 < 0)
									{
										num47 = 0;
									}
									if (num46 % 1.0 > 0.99)
									{
										num48++;
									}
									if (num48 > 1440)
									{
										num48 = 1440;
									}
									int num42 = array2[num47];
									int num43 = array2[num48];
									int num44;
									bool flag3;
									do
									{
										num44 = (num42 + num43) / 2;
										double rAmas = GaiaStars[num44].RAmas;
										double num49 = (rA_ - rAmas) * cosDecFactor;
										if (Math.Abs(num49) < num45)
										{
											double dECmas = GaiaStars[num44].DECmas;
											if (Math.Abs(dec_ - dECmas) < num45)
											{
												flag3 = true;
												break;
											}
										}
										if (num49 > 0.0)
										{
											num42 = num44 + 1;
										}
										else
										{
											num43 = num44 - 1;
										}
									}
									while (num43 >= num42);
									int num50 = num44 - 10;
									if (num50 < array2[num47])
									{
										num50 = array2[num47];
									}
									int num51 = num44 + 10;
									if (num51 > array2[num48])
									{
										num51 = array2[num48];
									}
									Closest.Clear();
									for (int n = num50; n <= num51; n++)
									{
										Nearest = new FindClosest();
										Nearest.StarEntry = n;
										Nearest.MagDiff = Math.Abs(mag - GaiaStars[n].MagGreen);
										Nearest.Separation2016 = Math.Sqrt(Math.Pow((rA_ - GaiaStars[n].RAmas) * cosDecFactor, 2.0) + Math.Pow(dec_ - GaiaStars[n].DECmas, 2.0));
										Closest.Add(Nearest);
									}
									flag3 = false;
									int num52 = 0;
									FindClosest.SortBy2000 = false;
									Closest.Sort();
									for (num52 = 0; num52 < Closest.Count; num52++)
									{
										if ((Closest[num52].MagDiff < 2.0) & (Closest[num52].Separation < 200.0))
										{
											GaiaStars[Closest[num52].StarEntry].StarCatID = int.Parse(TgasStars[m].CatID);
											GaiaStars[Closest[num52].StarEntry].StarNumber = TgasStars[m].CatNumber;
											flag3 = true;
											num8++;
											num15++;
											break;
										}
									}
									if (!flag3 && ((TgasStars[m].Dec_2016 < num33) & (TgasStars[m].Dec_2016 > num32)))
									{
										Gsource = new GAIAsource();
										Gsource.ImportStarFromTGAS_or_HIP = TgasStars[m].ToString();
										GaiaStars.Add(Gsource);
										num9++;
										num16++;
									}
									((Control)lblTGAStotalMatchedCount).set_Text(num8.ToString());
									((Control)lblTGAStotalAddedCount).set_Text(num9.ToString());
									((Control)lblTGASzonematchedCount).set_Text(num15.ToString());
									((Control)lblTGASzoneaddedCount).set_Text(num16.ToString());
									((Control)lblTotalStars).set_Text((num7 + num9 + num11).ToString());
									((Control)lblTotalZoneStars).set_Text((num14 + num16 + num18).ToString());
									Application.DoEvents();
								}
								GAIAsource.SortByRA = true;
								GaiaStars.Sort();
								((Control)lblProcess).set_Text("Matching Hipparcos to Gaia stars");
								Application.DoEvents();
								num45 = 500.0;
								for (int num53 = 0; num53 < HipStars.Count; num53++)
								{
									double rA_2 = HipStars[num53].RA_2016;
									double dec_2 = HipStars[num53].Dec_2016;
									double num54 = rA_2 / 3600000.0 * 4.0;
									double mag2 = HipStars[num53].Mag;
									double cosDecFactor = HipStars[num53].CosDecFactor;
									int num47 = (int)num54;
									int num48 = num47 + 1;
									if (num54 % 1.0 < 0.01)
									{
										num47--;
									}
									if (num47 < 0)
									{
										num47 = 0;
									}
									if (num54 % 1.0 > 0.99)
									{
										num48++;
									}
									if (num48 > 1440)
									{
										num48 = 1440;
									}
									int num42 = array2[num47];
									int num43 = array2[num48];
									int num44;
									do
									{
										num44 = (num42 + num43) / 2;
										double rAmas = GaiaStars[num44].RAmas;
										double num49 = (rA_2 - rAmas) * cosDecFactor;
										if (Math.Abs(num49) < num45)
										{
											double dECmas = GaiaStars[num44].DECmas;
											if (Math.Abs(dec_2 - dECmas) < num45)
											{
												break;
											}
										}
										if (num49 > 0.0)
										{
											num42 = num44 + 1;
										}
										else
										{
											num43 = num44 - 1;
										}
									}
									while (num43 >= num42);
									int num50 = num44 - 20;
									if (num50 < array2[num47])
									{
										num50 = array2[num47];
									}
									int num51 = num44 + 20;
									if (num51 > array2[num48])
									{
										num51 = array2[num48];
									}
									Closest.Clear();
									for (int num55 = num50; num55 <= num51; num55++)
									{
										Nearest = new FindClosest();
										Nearest.StarEntry = num55;
										Nearest.MagDiff = Math.Abs(mag2 - GaiaStars[num55].MagGreen);
										Nearest.Separation2016 = Math.Sqrt(Math.Pow((rA_2 - GaiaStars[num55].RAmas) * cosDecFactor, 2.0) + Math.Pow(dec_2 - GaiaStars[num55].DECmas, 2.0));
										Closest.Add(Nearest);
									}
									bool flag3 = false;
									int num56 = 0;
									FindClosest.SortBy2000 = false;
									Closest.Sort();
									for (num56 = 0; num56 < Closest.Count; num56++)
									{
										if ((Closest[num56].MagDiff < 2.0) & (Closest[num56].Separation < 200.0))
										{
											GaiaStars[Closest[num56].StarEntry].StarCatID = int.Parse(HipStars[num53].CatID);
											GaiaStars[Closest[num56].StarEntry].StarNumber = (uint)HipStars[num53].CatNumber;
											flag3 = true;
											num10++;
											num17++;
											break;
										}
									}
									if (!flag3)
									{
										Gsource = new GAIAsource();
										Gsource.ImportStarFromTGAS_or_HIP = HipStars[num53].ToString();
										GaiaStars.Add(Gsource);
										num11++;
										num18++;
									}
									((Control)lblHIPtotalMatchedCount).set_Text(num10.ToString());
									((Control)lblHIPtotalAddedCount).set_Text(num11.ToString());
									((Control)lblHIPzonematchedCount).set_Text(num17.ToString());
									((Control)lblHIPzoneaddedCount).set_Text(num18.ToString());
									((Control)lblTotalStars).set_Text((num7 + num9 + num11).ToString());
									((Control)lblTotalZoneStars).set_Text((num14 + num16 + num18).ToString());
									Application.DoEvents();
								}
								GAIAsource.SortByRA = true;
								GaiaStars.Sort();
							}
							if (chkMatchToUCAC4.get_Checked())
							{
								int num57 = 180 + num31;
								int num58 = (int)Math.Floor(2.5 * (double)num57 - 0.001) + 1;
								int num59 = num58 + 3;
								if (num59 > 900)
								{
									num59 = 900;
								}
								if (num58 < 1)
								{
									num58 = 1;
								}
								((Control)lblProcess).set_Text("Matching Gaia stars to UCAC4");
								((Control)lblu4Files).set_Text("files z" + num59.ToString().PadLeft(3, '0') + " - z" + num58.ToString().PadLeft(3, '0'));
								Application.DoEvents();
								double num45 = 500.0;
								num38 = (num37 = (num34 = (num20 = 0)));
								num36 = num27;
								num23 += num27;
								int num60 = 0;
								for (int num61 = num59; num61 >= num58; num61--)
								{
									int num65;
									int num64;
									int num63;
									int num62;
									int num66 = (num65 = (num64 = (num19 = (num63 = (num62 = 0)))));
									num60++;
									double num67 = (double)(num61 - 450) / 5.0;
									double num68 = num67 - 0.2;
									num67 = num67 * 3600000.0 + 5000.0;
									num68 = num68 * 3600000.0 - 5000.0;
									U4Current = new List<U4>();
									((Control)lblUCAC4_Zone).set_Text("z" + num61.ToString().PadLeft(3, '0'));
									string text4 = uCAC4_Path + "\\u4b\\z" + num61.ToString().PadLeft(3, '0');
									int num69 = (int)new FileInfo(text4).Length / 78;
									FileStream fileStream = new FileStream(text4, FileMode.Open, FileAccess.Read);
									BinaryReader binaryReader = new BinaryReader(fileStream);
									int[] array4 = new int[1441];
									num41 = 0;
									array4[num41] = 0;
									num41 = 1;
									int num70;
									for (num70 = 0; num70 < num69; num70++)
									{
										fileStream.Seek(78 * num70, SeekOrigin.Begin);
										U4 = new U4();
										U4.Number = (uint)(num61 * 1000000 + num70 + 1);
										U4.RA2000_mas = binaryReader.ReadInt32();
										U4.Dec2000_mas = binaryReader.ReadInt32() - 324000000;
										binaryReader.ReadInt16();
										U4.ApertureMag = (double)binaryReader.ReadInt16() / 1000.0;
										fileStream.Seek(78 * num70 + 24, SeekOrigin.Begin);
										U4.PmRA_mas = (double)binaryReader.ReadInt16() / 10.0;
										U4.PMDecmas = (double)binaryReader.ReadInt16() / 10.0;
										U4Current.Add(U4);
										num66++;
										num35++;
										for (; U4.RA2000_mas > array[num41]; num41++)
										{
											array4[num41] = num70;
										}
										if (num70 % 100 == 0)
										{
											((Control)lblUCAC4BandCount).set_Text(num66.ToString());
											((Control)lblUCAC4ZoneCount).set_Text(num35.ToString());
										}
										Application.DoEvents();
									}
									((Control)lblUCAC4BandCount).set_Text(num66.ToString());
									((Control)lblUCAC4ZoneCount).set_Text(num35.ToString());
									Application.DoEvents();
									array4[360] = num70 - 1;
									for (int num71 = 0; num71 < GaiaStars.Count; num71++)
									{
										if (GaiaStars[num71].StarCatID > 0)
										{
											continue;
										}
										double dEC2000mas = GaiaStars[num71].DEC2000mas;
										if (dEC2000mas < num68 || dEC2000mas > num67)
										{
											continue;
										}
										double rA2000mas = GaiaStars[num71].RA2000mas;
										double rAmas2 = GaiaStars[num71].RAmas;
										double dECmas2 = GaiaStars[num71].DECmas;
										double num72 = rA2000mas / 3600000.0 * 4.0;
										double magGreen = GaiaStars[num71].MagGreen;
										double reliability = GaiaStars[num71].Reliability;
										double cosDecFactor = GaiaStars[num71].CosDecFactor;
										int num47 = (int)num72;
										int num48 = num47 + 1;
										if (num72 % 1.0 < 0.01)
										{
											num47--;
										}
										if (num47 < 0)
										{
											num47 = 0;
										}
										if (num72 % 1.0 > 0.99)
										{
											num48++;
										}
										if (num48 > 1440)
										{
											num48 = 1440;
										}
										int num42 = array4[num47];
										int num43 = array4[num48];
										int num44;
										bool flag3;
										do
										{
											num44 = (num42 + num43) / 2;
											double rAmas = U4Current[num44].RA2000_mas;
											double num49 = (rA2000mas - rAmas) * cosDecFactor;
											if (Math.Abs(num49) < num45)
											{
												double dECmas = U4Current[num44].Dec2000_mas;
												if (Math.Abs(dEC2000mas - dECmas) < num45)
												{
													flag3 = true;
													break;
												}
											}
											if (num49 > 0.0)
											{
												num42 = num44 + 1;
											}
											else
											{
												num43 = num44 - 1;
											}
										}
										while (num43 >= num42);
										int num50 = num44 - 30;
										if (num50 < array4[num47])
										{
											num50 = array4[num47];
										}
										int num51 = num44 + 30;
										if (num51 > array4[num48])
										{
											num51 = array4[num48];
										}
										Closest.Clear();
										for (int num73 = num50; num73 <= num51; num73++)
										{
											Nearest = new FindClosest();
											Nearest.StarEntry = num73;
											Nearest.MagDiff = magGreen - U4Current[num73].ApertureMag;
											Nearest.Separation2000 = Math.Sqrt(Math.Pow((rA2000mas - U4Current[num73].RA2000_mas) * cosDecFactor, 2.0) + Math.Pow(dEC2000mas - U4Current[num73].Dec2000_mas, 2.0));
											Nearest.Separation2016 = Math.Sqrt(Math.Pow((rAmas2 - U4Current[num73].RA2016mas) * cosDecFactor, 2.0) + Math.Pow(dECmas2 - U4Current[num73].Dec2016mas, 2.0));
											Closest.Add(Nearest);
										}
										FindClosest.SortBy2000 = reliability > 0.2;
										Closest.Sort();
										flag3 = false;
										for (int num74 = 0; num74 < Closest.Count; num74++)
										{
											bool flag4 = true;
											if (!((Math.Abs(Closest[num74].MagDiff) < 2.0) & (Closest[num74].Separation < 1000.0)))
											{
												continue;
											}
											flag3 = true;
											GaiaStars[num71].StarCatID = 6;
											bool flag5 = true;
											if (Closest[num74].Separation < 200.0)
											{
												flag5 = true;
												num65++;
												num38++;
												num21++;
												GaiaStars[num71].PoorUCAC4Match = 0;
											}
											else if (Closest[num74].Separation < 1000.0)
											{
												flag5 = false;
												num64++;
												num37++;
												num22++;
												GaiaStars[num71].PoorUCAC4Match = 1;
											}
											GaiaStars[num71].StarNumber = U4Current[Closest[num74].StarEntry].Number;
											if (GaiaStars[num71].Reliability == 0.0)
											{
												GaiaStars[num71].PMRA_mas = (rAmas2 - U4Current[Closest[num74].StarEntry].RA2000_mas) * cosDecFactor / 16.0;
												GaiaStars[num71].PMDec_mas = (dECmas2 - U4Current[Closest[num74].StarEntry].Dec2000_mas) / 16.0;
												GaiaStars[num71].PMFromUCAC4 = 1;
												num19++;
												num20++;
												num25++;
												if (flag5)
												{
													num65--;
													num38--;
													num21--;
												}
												else
												{
													num64--;
													num37--;
													num22--;
												}
											}
											bool flag6 = true;
											if (GaiaStars[num71].StarNumber == num28)
											{
												if (GaiaStars[num71].Reliability == 0.0 || num29 == 0.0)
												{
													flag4 = false;
													if (GaiaStars[num71].Reliability == 0.0)
													{
														GaiaStars[num71].StarCatID = 0;
														GaiaStars[num71].StarNumber = 0u;
														GaiaStars[num71].PoorUCAC4Match = 0;
														if (GaiaStars[num71].NoProperMotion > 0)
														{
															GaiaStars[num71].PMRA_mas = 0.0;
															GaiaStars[num71].PMDec_mas = 0.0;
															GaiaStars[num71].PMFromUCAC4 = 0;
														}
														num19--;
														num20--;
														num25--;
													}
													if (GaiaStars[index].Reliability == 0.0)
													{
														GaiaStars[index].StarCatID = 0;
														GaiaStars[index].StarNumber = 0u;
														GaiaStars[index].PoorUCAC4Match = 0;
														if (GaiaStars[index].NoProperMotion > 0)
														{
															GaiaStars[index].PMRA_mas = 0.0;
															GaiaStars[index].PMDec_mas = 0.0;
															GaiaStars[index].PMFromUCAC4 = 0;
														}
														num19--;
														num20--;
														num25--;
													}
												}
												else if ((GaiaStars[num71].PoorUCAC4Match == 1) | (GaiaStars[index].PoorUCAC4Match == 1))
												{
													flag4 = false;
													if (GaiaStars[num71].PoorUCAC4Match == 1)
													{
														GaiaStars[num71].StarCatID = 0;
														GaiaStars[num71].StarNumber = 0u;
														GaiaStars[num71].PoorUCAC4Match = 0;
														if (GaiaStars[num71].NoProperMotion > 0)
														{
															GaiaStars[num71].PMRA_mas = 0.0;
															GaiaStars[num71].PMDec_mas = 0.0;
															GaiaStars[num71].PMFromUCAC4 = 0;
														}
														num19--;
														num20--;
														num25--;
													}
													if (GaiaStars[index].PoorUCAC4Match == 1)
													{
														GaiaStars[index].StarCatID = 0;
														GaiaStars[index].StarNumber = 0u;
														GaiaStars[index].PoorUCAC4Match = 0;
														if (GaiaStars[index].NoProperMotion > 0)
														{
															GaiaStars[index].PMRA_mas = 0.0;
															GaiaStars[index].PMDec_mas = 0.0;
															GaiaStars[index].PMFromUCAC4 = 0;
														}
														num19--;
														num20--;
														num25--;
													}
												}
												else if (GaiaStars[num71].PoorUCAC4Match != GaiaStars[index].PoorUCAC4Match)
												{
													if (GaiaStars[num71].PoorUCAC4Match > 0)
													{
														GaiaStars[num71].StarCatID = 0;
														GaiaStars[num71].StarNumber = 0u;
														GaiaStars[num71].PoorUCAC4Match = 0;
														if (GaiaStars[num71].NoProperMotion > 0)
														{
															GaiaStars[num71].PMRA_mas = 0.0;
															GaiaStars[num71].PMDec_mas = 0.0;
															GaiaStars[num71].PMFromUCAC4 = 0;
														}
														flag6 = flag5;
													}
													else
													{
														GaiaStars[index].StarCatID = 0;
														GaiaStars[index].StarNumber = 0u;
														GaiaStars[index].PoorUCAC4Match = 0;
														if (GaiaStars[index].NoProperMotion > 0)
														{
															GaiaStars[index].PMRA_mas = 0.0;
															GaiaStars[index].PMDec_mas = 0.0;
															GaiaStars[index].PMFromUCAC4 = 0;
														}
														flag6 = flag;
													}
												}
												else if ((GaiaStars[num71].Reliability > 2.0 && num29 < 1.5 && num29 > 0.15) | (((GaiaStars[num71].Reliability < 1.5) & (GaiaStars[num71].Reliability > 0.15)) && num29 > 2.0))
												{
													if (GaiaStars[num71].Reliability > num29)
													{
														GaiaStars[num71].StarCatID = 0;
														GaiaStars[num71].StarNumber = 0u;
														GaiaStars[num71].PoorUCAC4Match = 0;
														if (GaiaStars[num71].NoProperMotion > 0)
														{
															GaiaStars[num71].PMRA_mas = 0.0;
															GaiaStars[num71].PMDec_mas = 0.0;
															GaiaStars[num71].PMFromUCAC4 = 0;
														}
														flag6 = flag5;
													}
													else
													{
														GaiaStars[index].StarCatID = 0;
														GaiaStars[index].StarNumber = 0u;
														GaiaStars[index].PoorUCAC4Match = 0;
														if (GaiaStars[index].NoProperMotion > 0)
														{
															GaiaStars[index].PMRA_mas = 0.0;
															GaiaStars[index].PMDec_mas = 0.0;
															GaiaStars[index].PMFromUCAC4 = 0;
														}
														flag6 = flag;
													}
												}
												else if (Math.Abs(Closest[num74].Separation - num30) > 100.0)
												{
													if (Closest[num74].Separation > num30)
													{
														GaiaStars[num71].StarCatID = 0;
														GaiaStars[num71].StarNumber = 0u;
														GaiaStars[num71].PoorUCAC4Match = 0;
														if (GaiaStars[num71].NoProperMotion > 0)
														{
															GaiaStars[num71].PMRA_mas = 0.0;
															GaiaStars[num71].PMDec_mas = 0.0;
															GaiaStars[num71].PMFromUCAC4 = 0;
														}
														flag6 = flag5;
													}
													else
													{
														GaiaStars[index].StarCatID = 0;
														GaiaStars[index].StarNumber = 0u;
														GaiaStars[index].PoorUCAC4Match = 0;
														if (GaiaStars[index].NoProperMotion > 0)
														{
															GaiaStars[index].PMRA_mas = 0.0;
															GaiaStars[index].PMDec_mas = 0.0;
															GaiaStars[index].PMFromUCAC4 = 0;
														}
														flag6 = flag;
													}
												}
												else if (GaiaStars[num71].MagGreen > GaiaStars[index].MagGreen)
												{
													GaiaStars[num71].StarCatID = 0;
													GaiaStars[num71].StarNumber = 0u;
													GaiaStars[num71].PoorUCAC4Match = 0;
													if (GaiaStars[num71].NoProperMotion > 0)
													{
														GaiaStars[num71].PMRA_mas = 0.0;
														GaiaStars[num71].PMDec_mas = 0.0;
														GaiaStars[num71].PMFromUCAC4 = 0;
													}
													flag6 = flag5;
												}
												else
												{
													GaiaStars[index].StarCatID = 0;
													GaiaStars[index].StarNumber = 0u;
													GaiaStars[index].PoorUCAC4Match = 0;
													if (GaiaStars[index].NoProperMotion > 0)
													{
														GaiaStars[index].PMRA_mas = 0.0;
														GaiaStars[index].PMDec_mas = 0.0;
														GaiaStars[index].PMFromUCAC4 = 0;
													}
													flag6 = flag;
												}
												if (flag4)
												{
													if (flag6)
													{
														num65--;
														num38--;
														num21--;
													}
													else
													{
														num64--;
														num37--;
														num22--;
													}
													num63++;
													num36++;
													num23++;
												}
												num62++;
												num34++;
												num24++;
											}
											num28 = GaiaStars[num71].StarNumber;
											num29 = GaiaStars[num71].Reliability;
											num30 = Closest[num74].Separation;
											index = num71;
											flag = flag5;
											break;
										}
										if (!flag3)
										{
											num63++;
											num36++;
											num23++;
										}
										((Control)lblUCAC4BandGoodMatch).set_Text(num65.ToString());
										((Control)lblUCAC4ZoneGoodMatch).set_Text(num38.ToString());
										((Control)lblUCAC4TotalGoodMatch).set_Text(num21.ToString());
										((Control)lblUCAC4BandPoorMatch).set_Text(num64.ToString());
										((Control)lblUCAC4ZonePoorMatch).set_Text(num37.ToString());
										((Control)lblUCAC4TotalPoorMatch).set_Text(num22.ToString());
										((Control)lblUCAC4BandPM_Added).set_Text(num19.ToString());
										((Control)lblUCAC4ZonePM_Added).set_Text(num20.ToString());
										((Control)lblUCAC4TotalPM_Added).set_Text(num25.ToString());
										((Control)lblUCAC4BandNoMatch).set_Text(num63.ToString());
										((Control)lblUCAC4ZoneNoMatch).set_Text(num36.ToString());
										((Control)lblUCAC4TotalNoMatch).set_Text(num23.ToString());
										((Control)lblUCAC4BandDoubleMatchRemoved).set_Text(num62.ToString());
										((Control)lblUCAC4ZoneDoubleMatchRemoved).set_Text(num34.ToString());
										((Control)lblUCAC4TotalDoubleMatchRemoved).set_Text(num24.ToString());
										((Control)lblZoneNoProperMotion).set_Text(num27.ToString());
										((Control)lblTotalNoProperMotion).set_Text(num26.ToString());
										if (num71 % 100 == 0)
										{
											Application.DoEvents();
										}
									}
									Application.DoEvents();
									Thread.Sleep(300);
								}
							}
							int num75 = 0;
							for (int num76 = 0; num76 < GaiaStars.Count; num76++)
							{
								Gaia.rAmas = (int)Math.Floor(GaiaStars[num76].RAmas);
								Gaia.rAmuas = (byte)((GaiaStars[num76].RAmas - (double)Gaia.rAmas) * 250.0);
								Gaia.dECmas = (int)Math.Floor(GaiaStars[num76].DECmas);
								Gaia.dECmuas = (byte)((GaiaStars[num76].DECmas - (double)Gaia.dECmas) * 250.0);
								if (GaiaStars[num76].Parallax_mas < 0.0)
								{
									Gaia.parallax_masX80 = 0;
								}
								else
								{
									Gaia.parallax_masX80 = (ushort)(GaiaStars[num76].Parallax_mas * 80.0);
								}
								Gaia.pmRA_muasec_yrCosDec = (int)(GaiaStars[num76].PMRA_mas * 1000.0);
								Gaia.pmDec_muasec_yr = (int)(GaiaStars[num76].PMDec_mas * 1000.0);
								Gaia.rV_kms = (short)GaiaStars[num76].RadialVelocityKmSec;
								Gaia.epoch2000 = (short)(GaiaStars[num76].Epoch_From_2000 * 1000.0);
								Gaia.mBlue = (short)(GaiaStars[num76].MagBlue * 1000.0);
								Gaia.mGreen = (short)(GaiaStars[num76].MagGreen * 1000.0);
								Gaia.mRed = (short)(GaiaStars[num76].MagRed * 1000.0);
								Gaia.sDev_RA_10muas = (ushort)(GaiaStars[num76].SDev_RA_mas * 100.0);
								Gaia.sDev_Dec_10muas = (ushort)(GaiaStars[num76].SDev_Dec_mas * 100.0);
								Gaia.sDev_Par_10muas = (ushort)(GaiaStars[num76].SDev_Parallax_mas * 100.0);
								Gaia.sDev_pmRA_muas_yr = (ushort)(GaiaStars[num76].SDev_pmRA_mas_yr * 1000.0);
								Gaia.sDev_pmDec_muas_yr = (ushort)(GaiaStars[num76].SDev_pmDec_mas_yr * 1000.0);
								Gaia.sDev_RV = (byte)GaiaStars[num76].SDev_RV;
								Gaia.reliability = (byte)(GaiaStars[num76].Reliability * 20.0);
								Gaia.flags = (byte)(GaiaStars[num76].DuplicateSource + 2 * GaiaStars[num76].NoProperMotion + 4 * GaiaStars[num76].PMFromUCAC4 + 8 * GaiaStars[num76].PoorUCAC4Match);
								Gaia.starDiameter_mas_x5 = (byte)(GaiaStars[num76].StarDiameter_mas * 5.0);
								Gaia.gaiaVersionOfStar = (byte)GaiaStars[num76].GaiaVersion;
								Gaia.source_ID = GaiaStars[num76].Source_ID;
								Gaia.catID = (byte)GaiaStars[num76].StarCatID;
								Gaia.catNumber = GaiaStars[num76].StarNumber;
								Gaia.WriteCurrentAsNextGaiaDR3Record(gaiaWriteFile);
								for (double num77 = GaiaStars[num76].RAmas / 3600000.0; num77 > (double)num75 && num75 <= 360; num75++)
								{
									binaryWriter.Write(num3);
								}
								num3++;
							}
							for (; num75 <= 360; num75++)
							{
								binaryWriter.Write(num3);
							}
						}
					}
					else
					{
						for (int num75 = 0; num75 <= 360; num75++)
						{
							binaryWriter.Write(num3);
						}
					}
				}
			}
			Gaia.CurrentRecordLength = 58L;
			Gaia.CreateHipparcosIndexFile(text2);
			using StreamWriter streamWriter3 = new StreamWriter(WorkingDirectory + text2 + "_Stats.txt");
			streamWriter3.WriteLine(((Control)label60).get_Text().PadLeft(31) + " : " + ((Control)lblTotalGaiaReadCount).get_Text());
			streamWriter3.WriteLine(((Control)label51).get_Text().PadLeft(31) + " : " + ((Control)lblTotalTGASreadCount).get_Text());
			streamWriter3.WriteLine(((Control)label62).get_Text().PadLeft(31) + " : " + ((Control)lblTotalHIPReadCount).get_Text());
			streamWriter3.WriteLine(((Control)label57).get_Text().PadLeft(31) + " : " + ((Control)lblTGAStotalMatchedCount).get_Text());
			streamWriter3.WriteLine(((Control)label58).get_Text().PadLeft(31) + " : " + ((Control)lblTGAStotalAddedCount).get_Text());
			streamWriter3.WriteLine(((Control)label61).get_Text().PadLeft(31) + " : " + ((Control)lblHIPtotalMatchedCount).get_Text());
			streamWriter3.WriteLine(((Control)label59).get_Text().PadLeft(31) + " : " + ((Control)lblHIPtotalAddedCount).get_Text());
			streamWriter3.WriteLine(((Control)label42).get_Text().PadLeft(31) + " : " + ((Control)lblTotalNoProperMotion).get_Text());
			streamWriter3.WriteLine(((Control)label55).get_Text().PadLeft(31) + " : " + ((Control)lblTotalStars).get_Text());
			streamWriter3.WriteLine();
			streamWriter3.WriteLine("UCAC4".PadLeft(31));
			streamWriter3.WriteLine(((Control)label53).get_Text().PadLeft(31) + " : " + ((Control)lblUCAC4TotalGoodMatch).get_Text());
			streamWriter3.WriteLine(((Control)label20).get_Text().PadLeft(31) + " : " + ((Control)lblUCAC4TotalPoorMatch).get_Text());
			streamWriter3.WriteLine(((Control)label49).get_Text().PadLeft(31) + " : " + ((Control)lblUCAC4TotalPM_Added).get_Text());
			streamWriter3.WriteLine(((Control)label18).get_Text().PadLeft(31) + " : " + ((Control)lblUCAC4TotalNoMatch).get_Text());
			streamWriter3.WriteLine(((Control)label23).get_Text().PadLeft(31) + " : " + ((Control)lblUCAC4TotalDoubleMatchRemoved).get_Text());
		}

		private void cmdBadHipStars_Click(object sender, EventArgs e)
		{
			string text = "Gaia14_EDR3";
			string path = Utilities.AppPath + "\\Resource Files\\Gaia\\Gaia14_ED3_BadHipStars.csv";
			List<HipStars> list = new List<HipStars>();
			List<HipStars> list2 = new List<HipStars>();
			list.Clear();
			list2.Clear();
			using (StreamReader streamReader = new StreamReader("d:\\Astronomy\\Catalogues\\GaiaDR3 Hipstars\\Hip2.csv"))
			{
				streamReader.ReadLine();
				do
				{
					string[] array = streamReader.ReadLine()!.Split(new char[1] { ',' });
					HipStars hipStars = new HipStars();
					hipStars.Hip = int.Parse(array[0]);
					hipStars.Source_ID = ulong.Parse(array[1]);
					list.Add(hipStars);
				}
				while (!streamReader.EndOfStream);
			}
			PBar pBar = new PBar();
			((Control)pBar).set_Text("Extracting Bad HIP stars");
			pBar.pBarFTP.set_Minimum(0);
			pBar.pBarFTP.set_Maximum(361);
			pBar.pBarFTP.set_Value(0);
			((Control)pBar).Show();
			using (StreamWriter streamWriter = new StreamWriter(path))
			{
				using (FileStream fileStream = new FileStream(Utilities.AppPath + "\\Resource Files\\Gaia\\" + text + ".bin", FileMode.Open))
				{
					BinaryReader gaia = new BinaryReader(fileStream);
					Gaia.CurrentRecordLength = 58L;
					using FileStream fileStream2 = new FileStream(Utilities.AppPath + "\\Resource Files\\Gaia\\" + text + ".inx", FileMode.Open);
					BinaryReader binaryReader = new BinaryReader(fileStream2);
					int num = 0;
					int num2 = 0;
					int num3 = 0;
					for (int num4 = 179; num4 >= -180; num4--)
					{
						pBar.pBarFTP.set_Value(-num4 + 180);
						((Control)pBar).set_Text(string.Format("Extracting Bad Hip stars. Zone {0,1:f1}°", (double)num4 / 2.0));
						Application.DoEvents();
						num = 361 * (179 - num4);
						if (!(num < 0 || num >= 129959))
						{
							fileStream2.Seek(num * 4, SeekOrigin.Begin);
							num2 = binaryReader.ReadInt32();
							if (num2 < 0)
							{
								num2 = 0;
							}
							fileStream2.Seek((num + 360) * 4, SeekOrigin.Begin);
							num3 = binaryReader.ReadInt32();
							for (int i = num2; i < num3; i++)
							{
								Gaia.ReadNext(fileStream, gaia, i);
								if (Gaia.GaiaVersionOfStar != 0)
								{
									continue;
								}
								bool flag = false;
								int.TryParse(Gaia.StarID.Replace("HIP ", ""), out var result);
								if (result <= 0)
								{
									continue;
								}
								int num5 = 0;
								int num6 = list.Count - 1;
								int num7;
								do
								{
									num7 = (num6 + num5) / 2;
									if (result == list[num7].Hip)
									{
										flag = true;
										break;
									}
									if (result > list[num7].Hip)
									{
										num5 = num7 + 1;
									}
									else
									{
										num6 = num7 - 1;
									}
								}
								while (num6 >= num5);
								if (!flag)
								{
									continue;
								}
								for (int j = -50; j <= 50; j++)
								{
									if (j != 0)
									{
										Gaia.ReadNext(fileStream, gaia, i + j);
										if (list[num7].Source_ID == Gaia.Source_ID)
										{
											HipStars hipStars = new HipStars();
											hipStars.Hip = result;
											hipStars.CatID = Gaia.StarID;
											list2.Add(hipStars);
											break;
										}
									}
								}
							}
						}
					}
				}
				list2.Sort();
				for (int k = 0; k < list2.Count; k++)
				{
					streamWriter.WriteLine(list2[k].ToString());
				}
			}
			((Control)pBar).Hide();
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
			//IL_02c6: Unknown result type (might be due to invalid IL or missing references)
			//IL_02d0: Expected O, but got Unknown
			//IL_02d1: Unknown result type (might be due to invalid IL or missing references)
			//IL_02db: Expected O, but got Unknown
			//IL_02dc: Unknown result type (might be due to invalid IL or missing references)
			//IL_02e6: Expected O, but got Unknown
			//IL_02e7: Unknown result type (might be due to invalid IL or missing references)
			//IL_02f1: Expected O, but got Unknown
			//IL_02f2: Unknown result type (might be due to invalid IL or missing references)
			//IL_02fc: Expected O, but got Unknown
			//IL_02fd: Unknown result type (might be due to invalid IL or missing references)
			//IL_0307: Expected O, but got Unknown
			//IL_0308: Unknown result type (might be due to invalid IL or missing references)
			//IL_0312: Expected O, but got Unknown
			//IL_0313: Unknown result type (might be due to invalid IL or missing references)
			//IL_031d: Expected O, but got Unknown
			//IL_031e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0328: Expected O, but got Unknown
			//IL_0329: Unknown result type (might be due to invalid IL or missing references)
			//IL_0333: Expected O, but got Unknown
			//IL_0334: Unknown result type (might be due to invalid IL or missing references)
			//IL_033e: Expected O, but got Unknown
			//IL_033f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0349: Expected O, but got Unknown
			//IL_034a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0354: Expected O, but got Unknown
			//IL_0355: Unknown result type (might be due to invalid IL or missing references)
			//IL_035f: Expected O, but got Unknown
			//IL_0360: Unknown result type (might be due to invalid IL or missing references)
			//IL_036a: Expected O, but got Unknown
			//IL_036b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0375: Expected O, but got Unknown
			//IL_0376: Unknown result type (might be due to invalid IL or missing references)
			//IL_0380: Expected O, but got Unknown
			//IL_0381: Unknown result type (might be due to invalid IL or missing references)
			//IL_038b: Expected O, but got Unknown
			//IL_038c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0396: Expected O, but got Unknown
			//IL_0397: Unknown result type (might be due to invalid IL or missing references)
			//IL_03a1: Expected O, but got Unknown
			//IL_03a2: Unknown result type (might be due to invalid IL or missing references)
			//IL_03ac: Expected O, but got Unknown
			//IL_03ad: Unknown result type (might be due to invalid IL or missing references)
			//IL_03b7: Expected O, but got Unknown
			//IL_03b8: Unknown result type (might be due to invalid IL or missing references)
			//IL_03c2: Expected O, but got Unknown
			//IL_03c3: Unknown result type (might be due to invalid IL or missing references)
			//IL_03cd: Expected O, but got Unknown
			//IL_03ce: Unknown result type (might be due to invalid IL or missing references)
			//IL_03d8: Expected O, but got Unknown
			//IL_03d9: Unknown result type (might be due to invalid IL or missing references)
			//IL_03e3: Expected O, but got Unknown
			//IL_03e4: Unknown result type (might be due to invalid IL or missing references)
			//IL_03ee: Expected O, but got Unknown
			//IL_03ef: Unknown result type (might be due to invalid IL or missing references)
			//IL_03f9: Expected O, but got Unknown
			//IL_03fa: Unknown result type (might be due to invalid IL or missing references)
			//IL_0404: Expected O, but got Unknown
			//IL_0405: Unknown result type (might be due to invalid IL or missing references)
			//IL_040f: Expected O, but got Unknown
			//IL_0410: Unknown result type (might be due to invalid IL or missing references)
			//IL_041a: Expected O, but got Unknown
			//IL_041b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0425: Expected O, but got Unknown
			//IL_0426: Unknown result type (might be due to invalid IL or missing references)
			//IL_0430: Expected O, but got Unknown
			//IL_0431: Unknown result type (might be due to invalid IL or missing references)
			//IL_043b: Expected O, but got Unknown
			//IL_043c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0446: Expected O, but got Unknown
			//IL_0447: Unknown result type (might be due to invalid IL or missing references)
			//IL_0451: Expected O, but got Unknown
			//IL_0452: Unknown result type (might be due to invalid IL or missing references)
			//IL_045c: Expected O, but got Unknown
			//IL_045d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0467: Expected O, but got Unknown
			//IL_0468: Unknown result type (might be due to invalid IL or missing references)
			//IL_0472: Expected O, but got Unknown
			//IL_0473: Unknown result type (might be due to invalid IL or missing references)
			//IL_047d: Expected O, but got Unknown
			//IL_047e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0488: Expected O, but got Unknown
			//IL_0489: Unknown result type (might be due to invalid IL or missing references)
			//IL_0493: Expected O, but got Unknown
			//IL_0494: Unknown result type (might be due to invalid IL or missing references)
			//IL_049e: Expected O, but got Unknown
			//IL_049f: Unknown result type (might be due to invalid IL or missing references)
			//IL_04a9: Expected O, but got Unknown
			//IL_04aa: Unknown result type (might be due to invalid IL or missing references)
			//IL_04b4: Expected O, but got Unknown
			//IL_04b5: Unknown result type (might be due to invalid IL or missing references)
			//IL_04bf: Expected O, but got Unknown
			//IL_04c0: Unknown result type (might be due to invalid IL or missing references)
			//IL_04ca: Expected O, but got Unknown
			//IL_04cb: Unknown result type (might be due to invalid IL or missing references)
			//IL_04d5: Expected O, but got Unknown
			//IL_04d6: Unknown result type (might be due to invalid IL or missing references)
			//IL_04e0: Expected O, but got Unknown
			//IL_04e1: Unknown result type (might be due to invalid IL or missing references)
			//IL_04eb: Expected O, but got Unknown
			//IL_04ec: Unknown result type (might be due to invalid IL or missing references)
			//IL_04f6: Expected O, but got Unknown
			//IL_04f7: Unknown result type (might be due to invalid IL or missing references)
			//IL_0501: Expected O, but got Unknown
			//IL_0502: Unknown result type (might be due to invalid IL or missing references)
			//IL_050c: Expected O, but got Unknown
			//IL_050d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0517: Expected O, but got Unknown
			//IL_0518: Unknown result type (might be due to invalid IL or missing references)
			//IL_0522: Expected O, but got Unknown
			//IL_0523: Unknown result type (might be due to invalid IL or missing references)
			//IL_052d: Expected O, but got Unknown
			//IL_052e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0538: Expected O, but got Unknown
			//IL_0539: Unknown result type (might be due to invalid IL or missing references)
			//IL_0543: Expected O, but got Unknown
			//IL_0544: Unknown result type (might be due to invalid IL or missing references)
			//IL_054e: Expected O, but got Unknown
			//IL_054f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0559: Expected O, but got Unknown
			//IL_055a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0564: Expected O, but got Unknown
			//IL_0565: Unknown result type (might be due to invalid IL or missing references)
			//IL_056f: Expected O, but got Unknown
			//IL_0570: Unknown result type (might be due to invalid IL or missing references)
			//IL_057a: Expected O, but got Unknown
			//IL_057b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0585: Expected O, but got Unknown
			//IL_0586: Unknown result type (might be due to invalid IL or missing references)
			//IL_0590: Expected O, but got Unknown
			//IL_0591: Unknown result type (might be due to invalid IL or missing references)
			//IL_059b: Expected O, but got Unknown
			//IL_059c: Unknown result type (might be due to invalid IL or missing references)
			//IL_05a6: Expected O, but got Unknown
			//IL_05a7: Unknown result type (might be due to invalid IL or missing references)
			//IL_05b1: Expected O, but got Unknown
			//IL_05b2: Unknown result type (might be due to invalid IL or missing references)
			//IL_05bc: Expected O, but got Unknown
			//IL_05bd: Unknown result type (might be due to invalid IL or missing references)
			//IL_05c7: Expected O, but got Unknown
			//IL_05c8: Unknown result type (might be due to invalid IL or missing references)
			//IL_05d2: Expected O, but got Unknown
			//IL_05d3: Unknown result type (might be due to invalid IL or missing references)
			//IL_05dd: Expected O, but got Unknown
			//IL_05de: Unknown result type (might be due to invalid IL or missing references)
			//IL_05e8: Expected O, but got Unknown
			//IL_05e9: Unknown result type (might be due to invalid IL or missing references)
			//IL_05f3: Expected O, but got Unknown
			//IL_05f4: Unknown result type (might be due to invalid IL or missing references)
			//IL_05fe: Expected O, but got Unknown
			//IL_05ff: Unknown result type (might be due to invalid IL or missing references)
			//IL_0609: Expected O, but got Unknown
			//IL_060a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0614: Expected O, but got Unknown
			//IL_0615: Unknown result type (might be due to invalid IL or missing references)
			//IL_061f: Expected O, but got Unknown
			//IL_0620: Unknown result type (might be due to invalid IL or missing references)
			//IL_062a: Expected O, but got Unknown
			//IL_062b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0635: Expected O, but got Unknown
			//IL_0636: Unknown result type (might be due to invalid IL or missing references)
			//IL_0640: Expected O, but got Unknown
			//IL_0641: Unknown result type (might be due to invalid IL or missing references)
			//IL_064b: Expected O, but got Unknown
			//IL_064c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0656: Expected O, but got Unknown
			//IL_0657: Unknown result type (might be due to invalid IL or missing references)
			//IL_0661: Expected O, but got Unknown
			//IL_0662: Unknown result type (might be due to invalid IL or missing references)
			//IL_066c: Expected O, but got Unknown
			//IL_066d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0677: Expected O, but got Unknown
			//IL_0678: Unknown result type (might be due to invalid IL or missing references)
			//IL_0682: Expected O, but got Unknown
			//IL_0683: Unknown result type (might be due to invalid IL or missing references)
			//IL_068d: Expected O, but got Unknown
			//IL_068e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0698: Expected O, but got Unknown
			//IL_0699: Unknown result type (might be due to invalid IL or missing references)
			//IL_06a3: Expected O, but got Unknown
			//IL_06a4: Unknown result type (might be due to invalid IL or missing references)
			//IL_06ae: Expected O, but got Unknown
			//IL_06af: Unknown result type (might be due to invalid IL or missing references)
			//IL_06b9: Expected O, but got Unknown
			//IL_1036: Unknown result type (might be due to invalid IL or missing references)
			//IL_1e48: Unknown result type (might be due to invalid IL or missing references)
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(CreateGaiaCatalog));
			lstGaiaFiles = new ListBox();
			txtNewName = new TextBox();
			label2 = new Label();
			openFileDialog_TapVizieR = new OpenFileDialog();
			label5 = new Label();
			menuStrip1 = new MenuStrip();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			label11 = new Label();
			label12 = new Label();
			cmdCreateDR3 = new Button();
			label22 = new Label();
			tabGaia = new TabControl();
			tabPage1 = new TabPage();
			label48 = new Label();
			textBox7 = new TextBox();
			textBox5 = new TextBox();
			textBox4 = new TextBox();
			label28 = new Label();
			label43 = new Label();
			label41 = new Label();
			label40 = new Label();
			label38 = new Label();
			label37 = new Label();
			textBox6 = new TextBox();
			textBox3 = new TextBox();
			textBox2 = new TextBox();
			label36 = new Label();
			label35 = new Label();
			label33 = new Label();
			textBox1 = new TextBox();
			label32 = new Label();
			label31 = new Label();
			label29 = new Label();
			txtTapVizieR = new TextBox();
			txtTGAS_ADQL = new TextBox();
			label27 = new Label();
			txtHIP_ADQL = new TextBox();
			label26 = new Label();
			tabPage2 = new TabPage();
			groupBox3 = new GroupBox();
			panel1 = new Panel();
			label1 = new Label();
			cmdFieldUp = new Button();
			cmdFieldDown = new Button();
			label46 = new Label();
			label45 = new Label();
			label44 = new Label();
			label7 = new Label();
			label4 = new Label();
			lstColumns = new ListBox();
			lstFields = new ListBox();
			groupBox2 = new GroupBox();
			panel3 = new Panel();
			label8 = new Label();
			cmdFileDown = new Button();
			cmdFileUp = new Button();
			label39 = new Label();
			lstFilesToConvert = new ListBox();
			cmdSelectFile = new Button();
			label3 = new Label();
			lstDecNth = new ListBox();
			label6 = new Label();
			cmbWidth = new ComboBox();
			lstDecSth = new ListBox();
			chkAssume = new CheckBox();
			label10 = new Label();
			groupBox1 = new GroupBox();
			cmdSelectDirectory = new Button();
			txtWorkingDirectory = new TextBox();
			tabPage3 = new TabPage();
			groupBox8 = new GroupBox();
			cmdBadHipStars = new Button();
			label54 = new Label();
			label24 = new Label();
			cmdCreateHipparcosIndex = new Button();
			cmdUCAC4Index = new Button();
			label47 = new Label();
			groupBox7 = new GroupBox();
			lblTotalZoneStars = new Label();
			label55 = new Label();
			lblTotalStars = new Label();
			lblUCAC4BandPM_Added = new Label();
			lblUCAC4ZonePM_Added = new Label();
			lblUCAC4TotalPM_Added = new Label();
			label49 = new Label();
			lblu4Files = new Label();
			lblZoneNoProperMotion = new Label();
			lblTotalNoProperMotion = new Label();
			label42 = new Label();
			lblUCAC4BandDoubleMatchRemoved = new Label();
			lblUCAC4ZoneDoubleMatchRemoved = new Label();
			lblUCAC4TotalDoubleMatchRemoved = new Label();
			label23 = new Label();
			lblUCAC4TotalNoMatch = new Label();
			lblUCAC4TotalPoorMatch = new Label();
			lblUCAC4TotalGoodMatch = new Label();
			lblUCAC4BandNoMatch = new Label();
			lblUCAC4ZoneNoMatch = new Label();
			lblUCAC4BandPoorMatch = new Label();
			lblUCAC4ZonePoorMatch = new Label();
			label20 = new Label();
			label18 = new Label();
			label17 = new Label();
			label16 = new Label();
			label15 = new Label();
			label9 = new Label();
			label14 = new Label();
			label53 = new Label();
			label73 = new Label();
			label72 = new Label();
			lblTGASzoneaddedCount = new Label();
			lblHIPzonematchedCount = new Label();
			lblHIPzoneaddedCount = new Label();
			lblTGASzoneCount = new Label();
			lblHIPzoneCount = new Label();
			lblGaiazoneCount = new Label();
			lblTGASzonematchedCount = new Label();
			lblTGAStotalAddedCount = new Label();
			lblHIPtotalMatchedCount = new Label();
			lblHIPtotalAddedCount = new Label();
			lblUCAC4_Zone = new Label();
			label59 = new Label();
			label61 = new Label();
			lblUCAC4BandGoodMatch = new Label();
			label64 = new Label();
			lblTotalTGASreadCount = new Label();
			label62 = new Label();
			lblTotalHIPReadCount = new Label();
			label60 = new Label();
			lblTotalGaiaReadCount = new Label();
			label58 = new Label();
			label57 = new Label();
			lblTGAStotalMatchedCount = new Label();
			lblUCAC4ZoneGoodMatch = new Label();
			lblUCAC4BandCount = new Label();
			lblUCAC4ZoneCount = new Label();
			label51 = new Label();
			lblProcess = new Label();
			label52 = new Label();
			lblZone = new Label();
			label21 = new Label();
			groupBox6 = new GroupBox();
			label50 = new Label();
			cmdCreateSubset = new Button();
			updnLimitingMag = new NumericUpDown();
			label19 = new Label();
			groupBox5 = new GroupBox();
			label30 = new Label();
			chkIgnore = new CheckBox();
			label25 = new Label();
			chkMatchToUCAC4 = new CheckBox();
			groupBox4 = new GroupBox();
			label34 = new Label();
			folderBrowserDialog1 = new FolderBrowserDialog();
			((Control)menuStrip1).SuspendLayout();
			((Control)tabGaia).SuspendLayout();
			((Control)tabPage1).SuspendLayout();
			((Control)tabPage2).SuspendLayout();
			((Control)groupBox3).SuspendLayout();
			((Control)panel1).SuspendLayout();
			((Control)groupBox2).SuspendLayout();
			((Control)panel3).SuspendLayout();
			((Control)groupBox1).SuspendLayout();
			((Control)tabPage3).SuspendLayout();
			((Control)groupBox8).SuspendLayout();
			((Control)groupBox7).SuspendLayout();
			((Control)groupBox6).SuspendLayout();
			((ISupportInitialize)updnLimitingMag).BeginInit();
			((Control)groupBox5).SuspendLayout();
			((Control)groupBox4).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)lstGaiaFiles).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstGaiaFiles).set_FormattingEnabled(true);
			((Control)lstGaiaFiles).set_Location(new Point(14, 35));
			((Control)lstGaiaFiles).set_Name("lstGaiaFiles");
			((Control)lstGaiaFiles).set_Size(new Size(183, 147));
			((Control)lstGaiaFiles).set_TabIndex(17);
			lstGaiaFiles.add_Click((EventHandler)lstGaiaFiles_Click);
			((Control)lstGaiaFiles).add_Leave((EventHandler)lstGaiaFiles_Leave);
			((Control)txtNewName).set_BackColor(Color.Cornsilk);
			((Control)txtNewName).set_Location(new Point(37, 45));
			((Control)txtNewName).set_Name("txtNewName");
			((Control)txtNewName).set_Size(new Size(106, 23));
			((Control)txtNewName).set_TabIndex(0);
			txtNewName.set_TextAlign((HorizontalAlignment)1);
			((Control)txtNewName).add_Leave((EventHandler)txtNewName_Leave);
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Font(new Font("Arial Black", 9f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label2).set_Location(new Point(75, 27));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(114, 17));
			((Control)label2).set_TabIndex(20);
			((Control)label2).set_Text("Catalogue name");
			((FileDialog)openFileDialog_TapVizieR).set_FileName("openFileDialog1");
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Location(new Point(692, 265));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(70, 13));
			((Control)label5).set_TabIndex(34);
			((Control)label5).set_Text("Selected files");
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(787, 24));
			((Control)menuStrip1).set_TabIndex(38);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)componentResourceManager.GetObject("helpToolStripMenuItem.Image"));
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(75, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("Help     ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)componentResourceManager.GetObject("exitToolStripMenuItem.Image"));
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(54, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("Exit");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((Control)label11).set_AutoSize(true);
			((Control)label11).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label11).set_Location(new Point(43, 20));
			((Control)label11).set_Name("label11");
			((Control)label11).set_Size(new Size(125, 13));
			((Control)label11).set_TabIndex(43);
			((Control)label11).set_Text("( Click to show star map )");
			((Control)label12).set_AutoSize(true);
			((Control)label12).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label12).set_Location(new Point(81, 77));
			((Control)label12).set_Name("label12");
			((Control)label12).set_Size(new Size(101, 13));
			((Control)label12).set_TabIndex(48);
			((Control)label12).set_Text("Reserved names");
			((Control)cmdCreateDR3).set_BackColor(SystemColors.ButtonFace);
			((Control)cmdCreateDR3).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdCreateDR3).set_Location(new Point(72, 405));
			((Control)cmdCreateDR3).set_Name("cmdCreateDR3");
			((Control)cmdCreateDR3).set_Size(new Size(122, 51));
			((Control)cmdCreateDR3).set_TabIndex(2);
			((Control)cmdCreateDR3).set_Text("Create file");
			((ButtonBase)cmdCreateDR3).set_UseVisualStyleBackColor(false);
			((Control)cmdCreateDR3).add_Click((EventHandler)cmdCreateDR3_Click);
			((Control)label22).set_AutoSize(true);
			((Control)label22).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label22).set_Location(new Point(18, 91));
			((Control)label22).set_Name("label22");
			((Control)label22).set_Size(new Size(226, 26));
			((Control)label22).set_TabIndex(72);
			((Control)label22).set_Text("Gaia16_EDR3, Gaia14_EDR3, Gaia12_EDR3\r\nGaia14_DR2, Gaia11_DR2, TychoGaia");
			label22.set_TextAlign(ContentAlignment.MiddleLeft);
			((Control)tabGaia).get_Controls().Add((Control)(object)tabPage1);
			((Control)tabGaia).get_Controls().Add((Control)(object)tabPage2);
			((Control)tabGaia).get_Controls().Add((Control)(object)tabPage3);
			((Control)tabGaia).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)tabGaia).set_Location(new Point(0, 37));
			((Control)tabGaia).set_Name("tabGaia");
			tabGaia.set_SelectedIndex(0);
			((Control)tabGaia).set_Size(new Size(781, 754));
			((Control)tabGaia).set_TabIndex(74);
			((Control)tabPage1).set_BackColor(Color.PapayaWhip);
			((Panel)tabPage1).set_BorderStyle((BorderStyle)2);
			((Control)tabPage1).get_Controls().Add((Control)(object)label48);
			((Control)tabPage1).get_Controls().Add((Control)(object)textBox7);
			((Control)tabPage1).get_Controls().Add((Control)(object)textBox5);
			((Control)tabPage1).get_Controls().Add((Control)(object)textBox4);
			((Control)tabPage1).get_Controls().Add((Control)(object)label28);
			((Control)tabPage1).get_Controls().Add((Control)(object)label43);
			((Control)tabPage1).get_Controls().Add((Control)(object)label41);
			((Control)tabPage1).get_Controls().Add((Control)(object)label40);
			((Control)tabPage1).get_Controls().Add((Control)(object)label38);
			((Control)tabPage1).get_Controls().Add((Control)(object)label37);
			((Control)tabPage1).get_Controls().Add((Control)(object)textBox6);
			((Control)tabPage1).get_Controls().Add((Control)(object)textBox3);
			((Control)tabPage1).get_Controls().Add((Control)(object)textBox2);
			((Control)tabPage1).get_Controls().Add((Control)(object)label36);
			((Control)tabPage1).get_Controls().Add((Control)(object)label35);
			((Control)tabPage1).get_Controls().Add((Control)(object)label33);
			((Control)tabPage1).get_Controls().Add((Control)(object)textBox1);
			((Control)tabPage1).get_Controls().Add((Control)(object)label32);
			((Control)tabPage1).get_Controls().Add((Control)(object)label31);
			((Control)tabPage1).get_Controls().Add((Control)(object)label29);
			((Control)tabPage1).get_Controls().Add((Control)(object)txtTapVizieR);
			((Control)tabPage1).get_Controls().Add((Control)(object)txtTGAS_ADQL);
			((Control)tabPage1).get_Controls().Add((Control)(object)label27);
			((Control)tabPage1).get_Controls().Add((Control)(object)txtHIP_ADQL);
			((Control)tabPage1).get_Controls().Add((Control)(object)label26);
			((Control)tabPage1).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Regular, GraphicsUnit.Point, 0));
			tabPage1.set_Location(new Point(4, 25));
			((Control)tabPage1).set_Name("tabPage1");
			((Control)tabPage1).set_Padding(new Padding(3));
			((Control)tabPage1).set_Size(new Size(773, 725));
			tabPage1.set_TabIndex(0);
			((Control)tabPage1).set_Text("Step 1 - TAP VizieR Downloads");
			((Control)label48).set_ForeColor(Color.Red);
			((Control)label48).set_Location(new Point(53, 654));
			((Control)label48).set_Name("label48");
			((Control)label48).set_Size(new Size(152, 58));
			((Control)label48).set_TabIndex(41);
			((Control)label48).set_Text("Examples of changes to the ADQL.  Vary the numbers as required");
			((Control)textBox7).set_BackColor(Color.Ivory);
			((Control)textBox7).set_Location(new Point(208, 672));
			((Control)textBox7).set_Name("textBox7");
			((Control)textBox7).set_Size(new Size(507, 23));
			((Control)textBox7).set_TabIndex(40);
			((Control)textBox7).set_Text("To limit to a range of magnitude, use:   AND Gmag BETWEEN 16.0 AND 18.0\r\n\r\n");
			((Control)textBox5).set_BackColor(Color.Ivory);
			((Control)textBox5).set_Location(new Point(208, 697));
			((Control)textBox5).set_Name("textBox5");
			((Control)textBox5).set_Size(new Size(507, 23));
			((Control)textBox5).set_TabIndex(39);
			((Control)textBox5).set_Text("To limit to a band in Ecliptic Latitude, add:  AND ELAT BETWEEN -10 AND 10");
			((Control)textBox4).set_BackColor(Color.Ivory);
			((Control)textBox4).set_Location(new Point(208, 647));
			((Control)textBox4).set_Name("textBox4");
			((Control)textBox4).set_Size(new Size(507, 23));
			((Control)textBox4).set_TabIndex(38);
			((Control)textBox4).set_Text("To limit to a band in R.A, (in degrees) add:  AND RA_ICRS BETWEEN 214.5 and 217\r\n");
			((Control)label28).set_AutoSize(true);
			((Control)label28).set_BackColor(Color.GreenYellow);
			label28.set_BorderStyle((BorderStyle)2);
			((Control)label28).set_Font(new Font("Microsoft Sans Serif", 16f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label28).set_ForeColor(Color.Brown);
			((Control)label28).set_Location(new Point(107, 11));
			((Control)label28).set_Name("label28");
			((Control)label28).set_Size(new Size(555, 28));
			((Control)label28).set_TabIndex(34);
			((Control)label28).set_Text("Use your browser to complete the following actions");
			((Control)label43).set_AutoSize(true);
			((Control)label43).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold | FontStyle.Italic, GraphicsUnit.Point, 0));
			((Control)label43).set_ForeColor(Color.DarkRed);
			((Control)label43).set_Location(new Point(696, 244));
			((Control)label43).set_Name("label43");
			((Control)label43).set_Size(new Size(36, 17));
			((Control)label43).set_TabIndex(33);
			((Control)label43).set_Text(".dat");
			((Control)label41).set_AutoSize(true);
			((Control)label41).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold | FontStyle.Italic, GraphicsUnit.Point, 0));
			((Control)label41).set_ForeColor(Color.DarkRed);
			((Control)label41).set_Location(new Point(696, 384));
			((Control)label41).set_Name("label41");
			((Control)label41).set_Size(new Size(37, 17));
			((Control)label41).set_TabIndex(31);
			((Control)label41).set_Text(".csv");
			((Control)label40).set_AutoSize(true);
			((Control)label40).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold | FontStyle.Italic, GraphicsUnit.Point, 0));
			((Control)label40).set_ForeColor(Color.DarkRed);
			((Control)label40).set_Location(new Point(697, 124));
			((Control)label40).set_Name("label40");
			((Control)label40).set_Size(new Size(36, 17));
			((Control)label40).set_TabIndex(30);
			((Control)label40).set_Text(".dat");
			((Control)label38).set_AutoSize(true);
			((Control)label38).set_BackColor(Color.PaleTurquoise);
			label38.set_BorderStyle((BorderStyle)2);
			((Control)label38).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label38).set_ForeColor(Color.DarkRed);
			((Control)label38).set_Location(new Point(438, 406));
			((Control)label38).set_Name("label38");
			((Control)label38).set_Size(new Size(256, 62));
			((Control)label38).set_TabIndex(29);
			((Control)label38).set_Text("You will need to rename the downloaded file\r\nSelect a file name of your choice. You will find\r\n it helpful to include the relevant declination \r\nin the name\r\n");
			((Control)label37).set_BackColor(Color.Moccasin);
			label37.set_BorderStyle((BorderStyle)2);
			((Control)label37).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label37).set_ForeColor(Color.DarkRed);
			((Control)label37).set_Location(new Point(400, 48));
			((Control)label37).set_Name("label37");
			((Control)label37).set_Size(new Size(290, 22));
			((Control)label37).set_TabIndex(28);
			((Control)label37).set_Text("Save all files in the same directory");
			((Control)textBox6).set_BackColor(Color.LightCyan);
			((Control)textBox6).set_Location(new Point(588, 381));
			((Control)textBox6).set_Name("textBox6");
			((TextBoxBase)textBox6).set_ReadOnly(true);
			((Control)textBox6).set_Size(new Size(106, 23));
			((Control)textBox6).set_TabIndex(27);
			((Control)textBox6).set_Text("xxxxxxx.csv");
			((Control)textBox3).set_BackColor(Color.LightCyan);
			((Control)textBox3).set_Location(new Point(588, 241));
			((Control)textBox3).set_Name("textBox3");
			((TextBoxBase)textBox3).set_ReadOnly(true);
			((Control)textBox3).set_Size(new Size(106, 23));
			((Control)textBox3).set_TabIndex(25);
			((Control)textBox3).set_Text("TGASraw.dat");
			((Control)textBox2).set_BackColor(Color.LightCyan);
			((Control)textBox2).set_Location(new Point(588, 121));
			((Control)textBox2).set_Name("textBox2");
			((TextBoxBase)textBox2).set_ReadOnly(true);
			((Control)textBox2).set_Size(new Size(106, 23));
			((Control)textBox2).set_TabIndex(24);
			((Control)textBox2).set_Text("HIPraw.dat");
			((Control)label36).set_AutoSize(true);
			((Control)label36).set_Location(new Point(505, 244));
			((Control)label36).set_Name("label36");
			((Control)label36).set_Size(new Size(81, 17));
			((Control)label36).set_TabIndex(23);
			((Control)label36).set_Text("Save file as");
			((Control)label35).set_AutoSize(true);
			((Control)label35).set_Location(new Point(498, 384));
			((Control)label35).set_Name("label35");
			((Control)label35).set_Size(new Size(88, 17));
			((Control)label35).set_TabIndex(22);
			((Control)label35).set_Text("Save files as");
			((Control)label33).set_AutoSize(true);
			((Control)label33).set_Location(new Point(505, 124));
			((Control)label33).set_Name("label33");
			((Control)label33).set_Size(new Size(81, 17));
			((Control)label33).set_TabIndex(20);
			((Control)label33).set_Text("Save file as");
			((Control)textBox1).set_BackColor(Color.Ivory);
			((Control)textBox1).set_Location(new Point(74, 495));
			((TextBoxBase)textBox1).set_Multiline(true);
			((Control)textBox1).set_Name("textBox1");
			((TextBoxBase)textBox1).set_ReadOnly(true);
			textBox1.set_ScrollBars((ScrollBars)2);
			((Control)textBox1).set_Size(new Size(620, 146));
			((Control)textBox1).set_TabIndex(19);
			((Control)textBox1).set_Text(componentResourceManager.GetString("textBox1.Text"));
			((Control)label32).set_BackColor(Color.PaleTurquoise);
			label32.set_BorderStyle((BorderStyle)2);
			((Control)label32).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label32).set_ForeColor(Color.DarkRed);
			((Control)label32).set_Location(new Point(77, 366));
			((Control)label32).set_Name("label32");
			((Control)label32).set_Size(new Size(343, 126));
			((Control)label32).set_TabIndex(18);
			((Control)label32).set_Text(componentResourceManager.GetString("label32.Text"));
			((Control)label31).set_AutoSize(true);
			((Control)label31).set_BackColor(Color.Moccasin);
			label31.set_BorderStyle((BorderStyle)2);
			((Control)label31).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label31).set_ForeColor(Color.DarkRed);
			((Control)label31).set_Location(new Point(79, 48));
			((Control)label31).set_Name("label31");
			((Control)label31).set_Size(new Size(290, 22));
			((Control)label31).set_TabIndex(17);
			((Control)label31).set_Text("Download all results as   .csv   files");
			((Control)label29).set_AutoSize(true);
			((Control)label29).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label29).set_ForeColor(Color.MidnightBlue);
			((Control)label29).set_Location(new Point(206, 80));
			((Control)label29).set_Name("label29");
			((Control)label29).set_Size(new Size(152, 17));
			((Control)label29).set_TabIndex(14);
			((Control)label29).set_Text("TAP VizieR address");
			((Control)txtTapVizieR).set_BackColor(Color.Ivory);
			((Control)txtTapVizieR).set_Location(new Point(360, 77));
			((Control)txtTapVizieR).set_Name("txtTapVizieR");
			((TextBoxBase)txtTapVizieR).set_ReadOnly(true);
			((Control)txtTapVizieR).set_Size(new Size(203, 23));
			((Control)txtTapVizieR).set_TabIndex(13);
			((Control)txtTapVizieR).set_Text("http://tapvizier.u-strasbg.fr/adql/");
			((Control)txtTGAS_ADQL).set_BackColor(Color.Ivory);
			((Control)txtTGAS_ADQL).set_Location(new Point(74, 266));
			((TextBoxBase)txtTGAS_ADQL).set_Multiline(true);
			((Control)txtTGAS_ADQL).set_Name("txtTGAS_ADQL");
			((TextBoxBase)txtTGAS_ADQL).set_ReadOnly(true);
			((Control)txtTGAS_ADQL).set_Size(new Size(620, 89));
			((Control)txtTGAS_ADQL).set_TabIndex(10);
			((Control)txtTGAS_ADQL).set_Text(componentResourceManager.GetString("txtTGAS_ADQL.Text"));
			((Control)label27).set_AutoSize(true);
			((Control)label27).set_BackColor(Color.PaleTurquoise);
			label27.set_BorderStyle((BorderStyle)2);
			((Control)label27).set_ForeColor(Color.DarkRed);
			((Control)label27).set_Location(new Point(77, 227));
			((Control)label27).set_Name("label27");
			((Control)label27).set_Size(new Size(354, 36));
			((Control)label27).set_TabIndex(9);
			((Control)label27).set_Text("Download required fields from TGas (2,057,051 stars) \r\nUse this ADQL with TAP VizieR");
			((Control)txtHIP_ADQL).set_BackColor(Color.Ivory);
			((Control)txtHIP_ADQL).set_Location(new Point(74, 146));
			((TextBoxBase)txtHIP_ADQL).set_Multiline(true);
			((Control)txtHIP_ADQL).set_Name("txtHIP_ADQL");
			((TextBoxBase)txtHIP_ADQL).set_ReadOnly(true);
			((Control)txtHIP_ADQL).set_Size(new Size(620, 71));
			((Control)txtHIP_ADQL).set_TabIndex(8);
			((Control)txtHIP_ADQL).set_Text("SELECT HIP,RArad*3600000,DErad*3600000, Plx,pmRA,pmDE,HPmag,\"B-V\",\"V-I\",e_RArad,e_DErad,\r\ne_Plx,e_pmRA,e_pmDE,F2,Ntr,Sn,n_HIP,Nc\r\nFROM \"I/311/hip2\"\r\nORDER BY RArad");
			((Control)label26).set_AutoSize(true);
			((Control)label26).set_BackColor(Color.PaleTurquoise);
			label26.set_BorderStyle((BorderStyle)2);
			((Control)label26).set_ForeColor(Color.DarkRed);
			((Control)label26).set_Location(new Point(77, 107));
			((Control)label26).set_Name("label26");
			((Control)label26).set_Size(new Size(371, 36));
			((Control)label26).set_TabIndex(7);
			((Control)label26).set_Text("Download required fields from Hipparcos (117,956 stars) \r\nUse this ADQL with TAP VizieR");
			((Control)tabPage2).set_BackColor(Color.Honeydew);
			((Panel)tabPage2).set_BorderStyle((BorderStyle)2);
			((Control)tabPage2).get_Controls().Add((Control)(object)groupBox3);
			((Control)tabPage2).get_Controls().Add((Control)(object)groupBox2);
			((Control)tabPage2).get_Controls().Add((Control)(object)groupBox1);
			((Control)tabPage2).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			tabPage2.set_Location(new Point(4, 25));
			((Control)tabPage2).set_Name("tabPage2");
			((Control)tabPage2).set_Padding(new Padding(3));
			((Control)tabPage2).set_Size(new Size(773, 686));
			tabPage2.set_TabIndex(1);
			((Control)tabPage2).set_Text("Step 2 - Select files && set fields");
			((Control)groupBox3).set_BackColor(Color.LightCyan);
			((Control)groupBox3).get_Controls().Add((Control)(object)panel1);
			((Control)groupBox3).get_Controls().Add((Control)(object)label46);
			((Control)groupBox3).get_Controls().Add((Control)(object)label45);
			((Control)groupBox3).get_Controls().Add((Control)(object)label44);
			((Control)groupBox3).get_Controls().Add((Control)(object)label7);
			((Control)groupBox3).get_Controls().Add((Control)(object)label4);
			((Control)groupBox3).get_Controls().Add((Control)(object)lstColumns);
			((Control)groupBox3).get_Controls().Add((Control)(object)lstFields);
			((Control)groupBox3).set_Location(new Point(407, 95));
			((Control)groupBox3).set_Name("groupBox3");
			((Control)groupBox3).set_Size(new Size(331, 581));
			((Control)groupBox3).set_TabIndex(96);
			groupBox3.set_TabStop(false);
			((Control)groupBox3).set_Text("Set .csv columns to match fields");
			panel1.set_BorderStyle((BorderStyle)2);
			((Control)panel1).get_Controls().Add((Control)(object)label1);
			((Control)panel1).get_Controls().Add((Control)(object)cmdFieldUp);
			((Control)panel1).get_Controls().Add((Control)(object)cmdFieldDown);
			((Control)panel1).set_Location(new Point(157, 493));
			((Control)panel1).set_Name("panel1");
			((Control)panel1).set_Size(new Size(128, 79));
			((Control)panel1).set_TabIndex(92);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(3, 6));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(121, 13));
			((Control)label1).set_TabIndex(88);
			((Control)label1).set_Text("Move selected Field");
			((Control)cmdFieldUp).set_Anchor((AnchorStyles)6);
			((Control)cmdFieldUp).set_Font(new Font("Microsoft Tai Le", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdFieldUp).set_Location(new Point(31, 23));
			((Control)cmdFieldUp).set_Name("cmdFieldUp");
			((Control)cmdFieldUp).set_Size(new Size(64, 22));
			((Control)cmdFieldUp).set_TabIndex(84);
			((Control)cmdFieldUp).set_Text("Up");
			((ButtonBase)cmdFieldUp).set_UseVisualStyleBackColor(true);
			((Control)cmdFieldUp).add_Click((EventHandler)cmdFieldUp_Click);
			((Control)cmdFieldDown).set_Anchor((AnchorStyles)6);
			((Control)cmdFieldDown).set_Font(new Font("Microsoft Tai Le", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdFieldDown).set_Location(new Point(31, 49));
			((Control)cmdFieldDown).set_Name("cmdFieldDown");
			((Control)cmdFieldDown).set_Size(new Size(64, 22));
			((Control)cmdFieldDown).set_TabIndex(85);
			((Control)cmdFieldDown).set_Text("Down");
			((ButtonBase)cmdFieldDown).set_UseVisualStyleBackColor(true);
			((Control)cmdFieldDown).add_Click((EventHandler)cmdFieldDown_Click);
			((Control)label46).set_AutoSize(true);
			label46.set_BorderStyle((BorderStyle)1);
			((Control)label46).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label46).set_Location(new Point(145, 468));
			((Control)label46).set_Name("label46");
			((Control)label46).set_Size(new Size(153, 15));
			((Control)label46).set_TabIndex(91);
			((Control)label46).set_Text("Move Fields to match Columns");
			label45.set_BorderStyle((BorderStyle)1);
			((Control)label45).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label45).set_Location(new Point(9, 527));
			((Control)label45).set_Name("label45");
			((Control)label45).set_Size(new Size(120, 41));
			((Control)label45).set_TabIndex(90);
			((Control)label45).set_Text("Move Field to the end  \r\nwhen a Column does \r\nnot exist");
			label44.set_BorderStyle((BorderStyle)1);
			((Control)label44).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label44).set_Location(new Point(9, 468));
			((Control)label44).set_Name("label44");
			((Control)label44).set_Size(new Size(120, 41));
			((Control)label44).set_TabIndex(89);
			((Control)label44).set_Text("Set Field to  ' -1 ' when \r\na column does not \r\ncorrespond to any Field");
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label7).set_Location(new Point(124, 40));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(101, 13));
			((Control)label7).set_TabIndex(87);
			((Control)label7).set_Text("Catalogue Fields");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label4).set_Location(new Point(9, 27));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(62, 26));
			((Control)label4).set_TabIndex(86);
			((Control)label4).set_Text("Column in\r\n.csv  file");
			((Control)lstColumns).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstColumns).set_FormattingEnabled(true);
			((Control)lstColumns).set_Location(new Point(9, 55));
			((Control)lstColumns).set_Name("lstColumns");
			((Control)lstColumns).set_Size(new Size(101, 394));
			((Control)lstColumns).set_TabIndex(83);
			((Control)lstFields).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstFields).set_FormattingEnabled(true);
			lstFields.get_Items().AddRange(new object[28]
			{
				"0   RA (mas)", "1   Dec (mas)", "2   Parallax (mas)", "3   pm in RA (mas/yr)", "4   pm in Dec (mas/yr)", "5   Radial velocity (km/s)", "6   Epoch after 2000 (years)", "7   Bp mag", "8   G mag", "9   Rp mag",
				"10  e in RA (mas)", "11  e in Dec (mas)", "12  e in Parallax (mas)", "13  e in pm RA (mas/yr)", "14  e in pm Dec (mas/yr)", "15  e in Radial Velocity (km/s)", "16 RUWE", "17 Duplicated source", "20 Gaia SOURCE_ID", "21 Hipparcos #",
				"22Tycho2 # (aaaa-bbbbb-c)", "23 UCAC4 # (zzz-nnnnnn)", "18 Star diameter (not available in EDR3)", "19 Gaia version. Set as '3' for EDR3", "-1", "-1", "-1", "-1"
			});
			((Control)lstFields).set_Location(new Point(120, 55));
			((Control)lstFields).set_Name("lstFields");
			((Control)lstFields).set_Size(new Size(201, 394));
			((Control)lstFields).set_TabIndex(82);
			((Control)groupBox2).set_BackColor(Color.AliceBlue);
			((Control)groupBox2).get_Controls().Add((Control)(object)panel3);
			((Control)groupBox2).get_Controls().Add((Control)(object)label39);
			((Control)groupBox2).get_Controls().Add((Control)(object)lstFilesToConvert);
			((Control)groupBox2).get_Controls().Add((Control)(object)cmdSelectFile);
			((Control)groupBox2).get_Controls().Add((Control)(object)label3);
			((Control)groupBox2).get_Controls().Add((Control)(object)lstDecNth);
			((Control)groupBox2).get_Controls().Add((Control)(object)label6);
			((Control)groupBox2).get_Controls().Add((Control)(object)cmbWidth);
			((Control)groupBox2).get_Controls().Add((Control)(object)lstDecSth);
			((Control)groupBox2).get_Controls().Add((Control)(object)chkAssume);
			((Control)groupBox2).get_Controls().Add((Control)(object)label10);
			((Control)groupBox2).set_Location(new Point(30, 95));
			((Control)groupBox2).set_Name("groupBox2");
			((Control)groupBox2).set_Size(new Size(368, 581));
			((Control)groupBox2).set_TabIndex(95);
			groupBox2.set_TabStop(false);
			((Control)groupBox2).set_Text("Select source  .csv  files");
			panel3.set_BorderStyle((BorderStyle)2);
			((Control)panel3).get_Controls().Add((Control)(object)label8);
			((Control)panel3).get_Controls().Add((Control)(object)cmdFileDown);
			((Control)panel3).get_Controls().Add((Control)(object)cmdFileUp);
			((Control)panel3).set_Location(new Point(12, 496));
			((Control)panel3).set_Name("panel3");
			((Control)panel3).set_Size(new Size(118, 76));
			((Control)panel3).set_TabIndex(95);
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label8).set_Location(new Point(0, 2));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(114, 13));
			((Control)label8).set_TabIndex(93);
			((Control)label8).set_Text("Move selected File");
			((Control)cmdFileDown).set_Anchor((AnchorStyles)6);
			((Control)cmdFileDown).set_Font(new Font("Microsoft Tai Le", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdFileDown).set_Location(new Point(25, 46));
			((Control)cmdFileDown).set_Name("cmdFileDown");
			((Control)cmdFileDown).set_Size(new Size(64, 22));
			((Control)cmdFileDown).set_TabIndex(80);
			((Control)cmdFileDown).set_Text("Down");
			((ButtonBase)cmdFileDown).set_UseVisualStyleBackColor(true);
			((Control)cmdFileDown).add_Click((EventHandler)cmdFileDown_Click);
			((Control)cmdFileUp).set_Anchor((AnchorStyles)6);
			((Control)cmdFileUp).set_Font(new Font("Microsoft Tai Le", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdFileUp).set_Location(new Point(25, 18));
			((Control)cmdFileUp).set_Name("cmdFileUp");
			((Control)cmdFileUp).set_Size(new Size(64, 22));
			((Control)cmdFileUp).set_TabIndex(79);
			((Control)cmdFileUp).set_Text("Up");
			((ButtonBase)cmdFileUp).set_UseVisualStyleBackColor(true);
			((Control)cmdFileUp).add_Click((EventHandler)cmdFileUp_Click);
			((Control)label39).set_AutoSize(true);
			((Control)label39).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label39).set_Location(new Point(253, 80));
			((Control)label39).set_Name("label39");
			((Control)label39).set_Size(new Size(40, 13));
			((Control)label39).set_TabIndex(94);
			((Control)label39).set_Text("South");
			((Control)lstFilesToConvert).set_BackColor(Color.OldLace);
			((Control)lstFilesToConvert).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstFilesToConvert).set_FormattingEnabled(true);
			((Control)lstFilesToConvert).set_Location(new Point(49, 94));
			((Control)lstFilesToConvert).set_Name("lstFilesToConvert");
			((Control)lstFilesToConvert).set_Size(new Size(140, 394));
			((Control)lstFilesToConvert).set_TabIndex(77);
			lstFilesToConvert.add_Click((EventHandler)lstFilesToConvert_Click);
			((Control)cmdSelectFile).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdSelectFile).set_Location(new Point(49, 58));
			((Control)cmdSelectFile).set_Name("cmdSelectFile");
			((Control)cmdSelectFile).set_Size(new Size(138, 28));
			((Control)cmdSelectFile).set_TabIndex(76);
			((Control)cmdSelectFile).set_Text("Select .csv files");
			((ButtonBase)cmdSelectFile).set_UseVisualStyleBackColor(true);
			((Control)cmdSelectFile).add_Click((EventHandler)cmdSelectFile_Click);
			((Control)label3).set_Anchor((AnchorStyles)6);
			((Control)label3).set_BackColor(Color.DarkRed);
			((Control)label3).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label3).set_ForeColor(Color.Yellow);
			((Control)label3).set_Location(new Point(143, 505));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(213, 58));
			((Control)label3).set_TabIndex(78);
			((Control)label3).set_Text("Files must be ordered by the \r\nSouthern declination limit - \r\nfrom North to South.");
			label3.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)lstDecNth).set_BackColor(Color.LemonChiffon);
			((Control)lstDecNth).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstDecNth).set_FormattingEnabled(true);
			((Control)lstDecNth).set_Location(new Point(197, 94));
			((Control)lstDecNth).set_Name("lstDecNth");
			((Control)lstDecNth).set_Size(new Size(52, 394));
			((Control)lstDecNth).set_TabIndex(86);
			lstDecNth.add_Click((EventHandler)lstDecNth_Click);
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label6).set_Location(new Point(197, 63));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(107, 13));
			((Control)label6).set_TabIndex(87);
			((Control)label6).set_Text("Declination range");
			((Control)cmbWidth).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbWidth).set_FormattingEnabled(true);
			cmbWidth.get_Items().AddRange(new object[4] { "20 deg", "10 deg", "5 deg", "2 deg" });
			((Control)cmbWidth).set_Location(new Point(200, 24));
			((Control)cmbWidth).set_Name("cmbWidth");
			((Control)cmbWidth).set_Size(new Size(61, 21));
			((Control)cmbWidth).set_TabIndex(92);
			((Control)lstDecSth).set_BackColor(Color.LemonChiffon);
			((Control)lstDecSth).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstDecSth).set_FormattingEnabled(true);
			((Control)lstDecSth).set_Location(new Point(253, 94));
			((Control)lstDecSth).set_Name("lstDecSth");
			((Control)lstDecSth).set_Size(new Size(52, 394));
			((Control)lstDecSth).set_TabIndex(89);
			lstDecSth.add_Click((EventHandler)lstDecSth_Click);
			((Control)chkAssume).set_AutoSize(true);
			chkAssume.set_Checked(true);
			chkAssume.set_CheckState((CheckState)1);
			((Control)chkAssume).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)chkAssume).set_Location(new Point(57, 20));
			((Control)chkAssume).set_Name("chkAssume");
			((Control)chkAssume).set_Size(new Size(141, 30));
			((Control)chkAssume).set_TabIndex(91);
			((Control)chkAssume).set_Text("Assume Declination \r\nwidth of each file is");
			((ButtonBase)chkAssume).set_UseVisualStyleBackColor(true);
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label10).set_Location(new Point(197, 80));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(38, 13));
			((Control)label10).set_TabIndex(90);
			((Control)label10).set_Text("North");
			((Control)groupBox1).set_BackColor(Color.LightGoldenrodYellow);
			((Control)groupBox1).get_Controls().Add((Control)(object)cmdSelectDirectory);
			((Control)groupBox1).get_Controls().Add((Control)(object)txtWorkingDirectory);
			((Control)groupBox1).set_Location(new Point(30, 19));
			((Control)groupBox1).set_Name("groupBox1");
			((Control)groupBox1).set_Size(new Size(584, 64));
			((Control)groupBox1).set_TabIndex(1);
			groupBox1.set_TabStop(false);
			((Control)groupBox1).set_Text("Set the directory holding the  .csv  files saved under Step 1");
			((Control)cmdSelectDirectory).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdSelectDirectory).set_Location(new Point(20, 19));
			((Control)cmdSelectDirectory).set_Name("cmdSelectDirectory");
			((Control)cmdSelectDirectory).set_Size(new Size(79, 42));
			((Control)cmdSelectDirectory).set_TabIndex(2);
			((Control)cmdSelectDirectory).set_Text("Select\r\ndirectory");
			((ButtonBase)cmdSelectDirectory).set_TextAlign(ContentAlignment.TopCenter);
			((ButtonBase)cmdSelectDirectory).set_UseVisualStyleBackColor(true);
			((Control)cmdSelectDirectory).add_Click((EventHandler)cmdSelectDirectory_Click);
			((Control)txtWorkingDirectory).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtWorkingDirectory).set_Location(new Point(132, 30));
			((Control)txtWorkingDirectory).set_Name("txtWorkingDirectory");
			((Control)txtWorkingDirectory).set_Size(new Size(427, 20));
			((Control)txtWorkingDirectory).set_TabIndex(1);
			((Control)txtWorkingDirectory).set_Text(Settings.Default.GaiaWorkingDir);
			((Control)tabPage3).set_BackColor(Color.AliceBlue);
			((Panel)tabPage3).set_BorderStyle((BorderStyle)2);
			((Control)tabPage3).get_Controls().Add((Control)(object)groupBox8);
			((Control)tabPage3).get_Controls().Add((Control)(object)label47);
			((Control)tabPage3).get_Controls().Add((Control)(object)groupBox7);
			((Control)tabPage3).get_Controls().Add((Control)(object)groupBox6);
			((Control)tabPage3).get_Controls().Add((Control)(object)groupBox5);
			tabPage3.set_Location(new Point(4, 25));
			((Control)tabPage3).set_Name("tabPage3");
			((Control)tabPage3).set_Size(new Size(773, 686));
			tabPage3.set_TabIndex(2);
			((Control)tabPage3).set_Text("Step 3 - Create catalogue && subsets");
			((Control)groupBox8).set_BackColor(Color.Lavender);
			((Control)groupBox8).get_Controls().Add((Control)(object)cmdBadHipStars);
			((Control)groupBox8).get_Controls().Add((Control)(object)label54);
			((Control)groupBox8).get_Controls().Add((Control)(object)label24);
			((Control)groupBox8).get_Controls().Add((Control)(object)cmdCreateHipparcosIndex);
			((Control)groupBox8).get_Controls().Add((Control)(object)cmdUCAC4Index);
			((Control)groupBox8).set_Location(new Point(317, 531));
			((Control)groupBox8).set_Name("groupBox8");
			((Control)groupBox8).set_Size(new Size(426, 138));
			((Control)groupBox8).set_TabIndex(103);
			groupBox8.set_TabStop(false);
			((Control)groupBox8).set_Text("Repair");
			((Control)cmdBadHipStars).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdBadHipStars).set_Location(new Point(348, 82));
			((Control)cmdBadHipStars).set_Name("cmdBadHipStars");
			((Control)cmdBadHipStars).set_Size(new Size(62, 44));
			((Control)cmdBadHipStars).set_TabIndex(53);
			((Control)cmdBadHipStars).set_Text("Bad Hip stars");
			((ButtonBase)cmdBadHipStars).set_UseVisualStyleBackColor(true);
			((Control)cmdBadHipStars).add_Click((EventHandler)cmdBadHipStars_Click);
			((Control)label54).set_AutoSize(true);
			((Control)label54).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label54).set_Location(new Point(156, 21));
			((Control)label54).set_Name("label54");
			((Control)label54).set_Size(new Size(179, 39));
			((Control)label54).set_TabIndex(52);
			((Control)label54).set_Text("For all Gaia files:\r\nCreate the index file need tro access\r\nany Gaia file using a UCAC4 number");
			((Control)label24).set_AutoSize(true);
			((Control)label24).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label24).set_Location(new Point(18, 27));
			((Control)label24).set_Name("label24");
			((Control)label24).set_Size(new Size(128, 26));
			((Control)label24).set_TabIndex(51);
			((Control)label24).set_Text("For the file selected in the\r\nlist of existing Gaia files");
			((Control)cmdCreateHipparcosIndex).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdCreateHipparcosIndex).set_Location(new Point(19, 78));
			((Control)cmdCreateHipparcosIndex).set_Name("cmdCreateHipparcosIndex");
			((Control)cmdCreateHipparcosIndex).set_Size(new Size(126, 48));
			((Control)cmdCreateHipparcosIndex).set_TabIndex(1);
			((Control)cmdCreateHipparcosIndex).set_Text("Create its index\r\nfile of Hipparcos stars");
			((ButtonBase)cmdCreateHipparcosIndex).set_UseVisualStyleBackColor(true);
			((Control)cmdCreateHipparcosIndex).add_Click((EventHandler)cmdCreateHipparcosIndex_Click);
			((Control)cmdUCAC4Index).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdUCAC4Index).set_Location(new Point(182, 78));
			((Control)cmdUCAC4Index).set_Name("cmdUCAC4Index");
			((Control)cmdUCAC4Index).set_Size(new Size(126, 48));
			((Control)cmdUCAC4Index).set_TabIndex(2);
			((Control)cmdUCAC4Index).set_Text("Create the UCAC4 \r\nindex file\r\n");
			((ButtonBase)cmdUCAC4Index).set_UseVisualStyleBackColor(true);
			((Control)cmdUCAC4Index).add_Click((EventHandler)cmdUCAC4Index_Click);
			((Control)label47).set_AutoSize(true);
			((Control)label47).set_ForeColor(Color.MidnightBlue);
			((Control)label47).set_Location(new Point(128, 5));
			((Control)label47).set_Name("label47");
			((Control)label47).set_Size(new Size(513, 17));
			((Control)label47).set_TabIndex(102);
			((Control)label47).set_Text("Files will be created/saved in the subdirectory   \\Resource Files\\Gaia\\");
			((Control)groupBox7).set_BackColor(Color.Azure);
			((Control)groupBox7).get_Controls().Add((Control)(object)lblTotalZoneStars);
			((Control)groupBox7).get_Controls().Add((Control)(object)label55);
			((Control)groupBox7).get_Controls().Add((Control)(object)lblTotalStars);
			((Control)groupBox7).get_Controls().Add((Control)(object)lblUCAC4BandPM_Added);
			((Control)groupBox7).get_Controls().Add((Control)(object)lblUCAC4ZonePM_Added);
			((Control)groupBox7).get_Controls().Add((Control)(object)lblUCAC4TotalPM_Added);
			((Control)groupBox7).get_Controls().Add((Control)(object)label49);
			((Control)groupBox7).get_Controls().Add((Control)(object)lblu4Files);
			((Control)groupBox7).get_Controls().Add((Control)(object)lblZoneNoProperMotion);
			((Control)groupBox7).get_Controls().Add((Control)(object)lblTotalNoProperMotion);
			((Control)groupBox7).get_Controls().Add((Control)(object)label42);
			((Control)groupBox7).get_Controls().Add((Control)(object)lblUCAC4BandDoubleMatchRemoved);
			((Control)groupBox7).get_Controls().Add((Control)(object)lblUCAC4ZoneDoubleMatchRemoved);
			((Control)groupBox7).get_Controls().Add((Control)(object)lblUCAC4TotalDoubleMatchRemoved);
			((Control)groupBox7).get_Controls().Add((Control)(object)label23);
			((Control)groupBox7).get_Controls().Add((Control)(object)lblUCAC4TotalNoMatch);
			((Control)groupBox7).get_Controls().Add((Control)(object)lblUCAC4TotalPoorMatch);
			((Control)groupBox7).get_Controls().Add((Control)(object)lblUCAC4TotalGoodMatch);
			((Control)groupBox7).get_Controls().Add((Control)(object)lblUCAC4BandNoMatch);
			((Control)groupBox7).get_Controls().Add((Control)(object)lblUCAC4ZoneNoMatch);
			((Control)groupBox7).get_Controls().Add((Control)(object)lblUCAC4BandPoorMatch);
			((Control)groupBox7).get_Controls().Add((Control)(object)lblUCAC4ZonePoorMatch);
			((Control)groupBox7).get_Controls().Add((Control)(object)label20);
			((Control)groupBox7).get_Controls().Add((Control)(object)label18);
			((Control)groupBox7).get_Controls().Add((Control)(object)label17);
			((Control)groupBox7).get_Controls().Add((Control)(object)label16);
			((Control)groupBox7).get_Controls().Add((Control)(object)label15);
			((Control)groupBox7).get_Controls().Add((Control)(object)label9);
			((Control)groupBox7).get_Controls().Add((Control)(object)label14);
			((Control)groupBox7).get_Controls().Add((Control)(object)label53);
			((Control)groupBox7).get_Controls().Add((Control)(object)label73);
			((Control)groupBox7).get_Controls().Add((Control)(object)label72);
			((Control)groupBox7).get_Controls().Add((Control)(object)lblTGASzoneaddedCount);
			((Control)groupBox7).get_Controls().Add((Control)(object)lblHIPzonematchedCount);
			((Control)groupBox7).get_Controls().Add((Control)(object)lblHIPzoneaddedCount);
			((Control)groupBox7).get_Controls().Add((Control)(object)lblTGASzoneCount);
			((Control)groupBox7).get_Controls().Add((Control)(object)lblHIPzoneCount);
			((Control)groupBox7).get_Controls().Add((Control)(object)lblGaiazoneCount);
			((Control)groupBox7).get_Controls().Add((Control)(object)lblTGASzonematchedCount);
			((Control)groupBox7).get_Controls().Add((Control)(object)lblTGAStotalAddedCount);
			((Control)groupBox7).get_Controls().Add((Control)(object)lblHIPtotalMatchedCount);
			((Control)groupBox7).get_Controls().Add((Control)(object)lblHIPtotalAddedCount);
			((Control)groupBox7).get_Controls().Add((Control)(object)lblUCAC4_Zone);
			((Control)groupBox7).get_Controls().Add((Control)(object)label59);
			((Control)groupBox7).get_Controls().Add((Control)(object)label61);
			((Control)groupBox7).get_Controls().Add((Control)(object)lblUCAC4BandGoodMatch);
			((Control)groupBox7).get_Controls().Add((Control)(object)label64);
			((Control)groupBox7).get_Controls().Add((Control)(object)lblTotalTGASreadCount);
			((Control)groupBox7).get_Controls().Add((Control)(object)label62);
			((Control)groupBox7).get_Controls().Add((Control)(object)lblTotalHIPReadCount);
			((Control)groupBox7).get_Controls().Add((Control)(object)label60);
			((Control)groupBox7).get_Controls().Add((Control)(object)lblTotalGaiaReadCount);
			((Control)groupBox7).get_Controls().Add((Control)(object)label58);
			((Control)groupBox7).get_Controls().Add((Control)(object)label57);
			((Control)groupBox7).get_Controls().Add((Control)(object)lblTGAStotalMatchedCount);
			((Control)groupBox7).get_Controls().Add((Control)(object)lblUCAC4ZoneGoodMatch);
			((Control)groupBox7).get_Controls().Add((Control)(object)lblUCAC4BandCount);
			((Control)groupBox7).get_Controls().Add((Control)(object)lblUCAC4ZoneCount);
			((Control)groupBox7).get_Controls().Add((Control)(object)label51);
			((Control)groupBox7).get_Controls().Add((Control)(object)lblProcess);
			((Control)groupBox7).get_Controls().Add((Control)(object)label52);
			((Control)groupBox7).get_Controls().Add((Control)(object)lblZone);
			((Control)groupBox7).get_Controls().Add((Control)(object)label21);
			((Control)groupBox7).set_Location(new Point(317, 36));
			((Control)groupBox7).set_Name("groupBox7");
			((Control)groupBox7).set_Size(new Size(426, 473));
			((Control)groupBox7).set_TabIndex(101);
			groupBox7.set_TabStop(false);
			((Control)groupBox7).set_Text("Processing status");
			((Control)lblTotalZoneStars).set_AutoSize(true);
			((Control)lblTotalZoneStars).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblTotalZoneStars).set_Location(new Point(197, 267));
			((Control)lblTotalZoneStars).set_Name("lblTotalZoneStars");
			((Control)lblTotalZoneStars).set_Size(new Size(14, 13));
			((Control)lblTotalZoneStars).set_TabIndex(62);
			((Control)lblTotalZoneStars).set_Text("#");
			((Control)label55).set_AutoSize(true);
			((Control)label55).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label55).set_Location(new Point(79, 267));
			((Control)label55).set_Name("label55");
			((Control)label55).set_Size(new Size(106, 13));
			((Control)label55).set_TabIndex(61);
			((Control)label55).set_Text("Total number of stars");
			((Control)lblTotalStars).set_AutoSize(true);
			((Control)lblTotalStars).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblTotalStars).set_Location(new Point(278, 267));
			((Control)lblTotalStars).set_Name("lblTotalStars");
			((Control)lblTotalStars).set_Size(new Size(14, 13));
			((Control)lblTotalStars).set_TabIndex(60);
			((Control)lblTotalStars).set_Text("#");
			((Control)lblUCAC4BandPM_Added).set_AutoSize(true);
			((Control)lblUCAC4BandPM_Added).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblUCAC4BandPM_Added).set_Location(new Point(197, 401));
			((Control)lblUCAC4BandPM_Added).set_Name("lblUCAC4BandPM_Added");
			((Control)lblUCAC4BandPM_Added).set_Size(new Size(14, 13));
			((Control)lblUCAC4BandPM_Added).set_TabIndex(59);
			((Control)lblUCAC4BandPM_Added).set_Text("#");
			((Control)lblUCAC4ZonePM_Added).set_AutoSize(true);
			((Control)lblUCAC4ZonePM_Added).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblUCAC4ZonePM_Added).set_Location(new Point(278, 401));
			((Control)lblUCAC4ZonePM_Added).set_Name("lblUCAC4ZonePM_Added");
			((Control)lblUCAC4ZonePM_Added).set_Size(new Size(14, 13));
			((Control)lblUCAC4ZonePM_Added).set_TabIndex(58);
			((Control)lblUCAC4ZonePM_Added).set_Text("#");
			((Control)lblUCAC4TotalPM_Added).set_AutoSize(true);
			((Control)lblUCAC4TotalPM_Added).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblUCAC4TotalPM_Added).set_Location(new Point(353, 401));
			((Control)lblUCAC4TotalPM_Added).set_Name("lblUCAC4TotalPM_Added");
			((Control)lblUCAC4TotalPM_Added).set_Size(new Size(14, 13));
			((Control)lblUCAC4TotalPM_Added).set_TabIndex(57);
			((Control)lblUCAC4TotalPM_Added).set_Text("#");
			((Control)label49).set_AutoSize(true);
			((Control)label49).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label49).set_Location(new Point(80, 401));
			((Control)label49).set_Name("label49");
			((Control)label49).set_Size(new Size(105, 13));
			((Control)label49).set_TabIndex(56);
			((Control)label49).set_Text("Proper motion added");
			((Control)lblu4Files).set_AutoSize(true);
			((Control)lblu4Files).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblu4Files).set_Location(new Point(58, 325));
			((Control)lblu4Files).set_Name("lblu4Files");
			((Control)lblu4Files).set_Size(new Size(83, 13));
			((Control)lblu4Files).set_TabIndex(55);
			((Control)lblu4Files).set_Text("files z000 - z001");
			((Control)lblZoneNoProperMotion).set_AutoSize(true);
			((Control)lblZoneNoProperMotion).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblZoneNoProperMotion).set_Location(new Point(197, 242));
			((Control)lblZoneNoProperMotion).set_Name("lblZoneNoProperMotion");
			((Control)lblZoneNoProperMotion).set_Size(new Size(14, 13));
			((Control)lblZoneNoProperMotion).set_TabIndex(54);
			((Control)lblZoneNoProperMotion).set_Text("#");
			((Control)lblTotalNoProperMotion).set_AutoSize(true);
			((Control)lblTotalNoProperMotion).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblTotalNoProperMotion).set_Location(new Point(278, 242));
			((Control)lblTotalNoProperMotion).set_Name("lblTotalNoProperMotion");
			((Control)lblTotalNoProperMotion).set_Size(new Size(14, 13));
			((Control)lblTotalNoProperMotion).set_TabIndex(53);
			((Control)lblTotalNoProperMotion).set_Text("#");
			((Control)label42).set_AutoSize(true);
			((Control)label42).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label42).set_Location(new Point(29, 242));
			((Control)label42).set_Name("label42");
			((Control)label42).set_Size(new Size(156, 13));
			((Control)label42).set_TabIndex(52);
			((Control)label42).set_Text("Has no proper motion from Gaia");
			((Control)lblUCAC4BandDoubleMatchRemoved).set_AutoSize(true);
			((Control)lblUCAC4BandDoubleMatchRemoved).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblUCAC4BandDoubleMatchRemoved).set_Location(new Point(197, 439));
			((Control)lblUCAC4BandDoubleMatchRemoved).set_Name("lblUCAC4BandDoubleMatchRemoved");
			((Control)lblUCAC4BandDoubleMatchRemoved).set_Size(new Size(14, 13));
			((Control)lblUCAC4BandDoubleMatchRemoved).set_TabIndex(51);
			((Control)lblUCAC4BandDoubleMatchRemoved).set_Text("#");
			((Control)lblUCAC4ZoneDoubleMatchRemoved).set_AutoSize(true);
			((Control)lblUCAC4ZoneDoubleMatchRemoved).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblUCAC4ZoneDoubleMatchRemoved).set_Location(new Point(278, 439));
			((Control)lblUCAC4ZoneDoubleMatchRemoved).set_Name("lblUCAC4ZoneDoubleMatchRemoved");
			((Control)lblUCAC4ZoneDoubleMatchRemoved).set_Size(new Size(14, 13));
			((Control)lblUCAC4ZoneDoubleMatchRemoved).set_TabIndex(50);
			((Control)lblUCAC4ZoneDoubleMatchRemoved).set_Text("#");
			((Control)lblUCAC4TotalDoubleMatchRemoved).set_AutoSize(true);
			((Control)lblUCAC4TotalDoubleMatchRemoved).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblUCAC4TotalDoubleMatchRemoved).set_Location(new Point(353, 439));
			((Control)lblUCAC4TotalDoubleMatchRemoved).set_Name("lblUCAC4TotalDoubleMatchRemoved");
			((Control)lblUCAC4TotalDoubleMatchRemoved).set_Size(new Size(14, 13));
			((Control)lblUCAC4TotalDoubleMatchRemoved).set_TabIndex(49);
			((Control)lblUCAC4TotalDoubleMatchRemoved).set_Text("#");
			((Control)label23).set_AutoSize(true);
			((Control)label23).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label23).set_Location(new Point(57, 439));
			((Control)label23).set_Name("label23");
			((Control)label23).set_Size(new Size(128, 13));
			((Control)label23).set_TabIndex(48);
			((Control)label23).set_Text("Double matches removed");
			((Control)lblUCAC4TotalNoMatch).set_AutoSize(true);
			((Control)lblUCAC4TotalNoMatch).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblUCAC4TotalNoMatch).set_Location(new Point(353, 420));
			((Control)lblUCAC4TotalNoMatch).set_Name("lblUCAC4TotalNoMatch");
			((Control)lblUCAC4TotalNoMatch).set_Size(new Size(14, 13));
			((Control)lblUCAC4TotalNoMatch).set_TabIndex(47);
			((Control)lblUCAC4TotalNoMatch).set_Text("#");
			((Control)lblUCAC4TotalPoorMatch).set_AutoSize(true);
			((Control)lblUCAC4TotalPoorMatch).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblUCAC4TotalPoorMatch).set_Location(new Point(353, 382));
			((Control)lblUCAC4TotalPoorMatch).set_Name("lblUCAC4TotalPoorMatch");
			((Control)lblUCAC4TotalPoorMatch).set_Size(new Size(14, 13));
			((Control)lblUCAC4TotalPoorMatch).set_TabIndex(46);
			((Control)lblUCAC4TotalPoorMatch).set_Text("#");
			((Control)lblUCAC4TotalGoodMatch).set_AutoSize(true);
			((Control)lblUCAC4TotalGoodMatch).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblUCAC4TotalGoodMatch).set_Location(new Point(353, 363));
			((Control)lblUCAC4TotalGoodMatch).set_Name("lblUCAC4TotalGoodMatch");
			((Control)lblUCAC4TotalGoodMatch).set_Size(new Size(14, 13));
			((Control)lblUCAC4TotalGoodMatch).set_TabIndex(45);
			((Control)lblUCAC4TotalGoodMatch).set_Text("#");
			((Control)lblUCAC4BandNoMatch).set_AutoSize(true);
			((Control)lblUCAC4BandNoMatch).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblUCAC4BandNoMatch).set_Location(new Point(197, 420));
			((Control)lblUCAC4BandNoMatch).set_Name("lblUCAC4BandNoMatch");
			((Control)lblUCAC4BandNoMatch).set_Size(new Size(14, 13));
			((Control)lblUCAC4BandNoMatch).set_TabIndex(44);
			((Control)lblUCAC4BandNoMatch).set_Text("#");
			((Control)lblUCAC4ZoneNoMatch).set_AutoSize(true);
			((Control)lblUCAC4ZoneNoMatch).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblUCAC4ZoneNoMatch).set_Location(new Point(278, 420));
			((Control)lblUCAC4ZoneNoMatch).set_Name("lblUCAC4ZoneNoMatch");
			((Control)lblUCAC4ZoneNoMatch).set_Size(new Size(14, 13));
			((Control)lblUCAC4ZoneNoMatch).set_TabIndex(43);
			((Control)lblUCAC4ZoneNoMatch).set_Text("#");
			((Control)lblUCAC4BandPoorMatch).set_AutoSize(true);
			((Control)lblUCAC4BandPoorMatch).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblUCAC4BandPoorMatch).set_Location(new Point(197, 382));
			((Control)lblUCAC4BandPoorMatch).set_Name("lblUCAC4BandPoorMatch");
			((Control)lblUCAC4BandPoorMatch).set_Size(new Size(14, 13));
			((Control)lblUCAC4BandPoorMatch).set_TabIndex(42);
			((Control)lblUCAC4BandPoorMatch).set_Text("#");
			((Control)lblUCAC4ZonePoorMatch).set_AutoSize(true);
			((Control)lblUCAC4ZonePoorMatch).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblUCAC4ZonePoorMatch).set_Location(new Point(278, 382));
			((Control)lblUCAC4ZonePoorMatch).set_Name("lblUCAC4ZonePoorMatch");
			((Control)lblUCAC4ZonePoorMatch).set_Size(new Size(14, 13));
			((Control)lblUCAC4ZonePoorMatch).set_TabIndex(41);
			((Control)lblUCAC4ZonePoorMatch).set_Text("#");
			((Control)label20).set_AutoSize(true);
			((Control)label20).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label20).set_Location(new Point(124, 382));
			((Control)label20).set_Name("label20");
			((Control)label20).set_Size(new Size(61, 13));
			((Control)label20).set_TabIndex(40);
			((Control)label20).set_Text("Poor match");
			((Control)label18).set_AutoSize(true);
			((Control)label18).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label18).set_Location(new Point(117, 420));
			((Control)label18).set_Name("label18");
			((Control)label18).set_Size(new Size(68, 13));
			((Control)label18).set_TabIndex(39);
			((Control)label18).set_Text("Not matched");
			((Control)label17).set_AutoSize(true);
			label17.set_BorderStyle((BorderStyle)1);
			((Control)label17).set_Location(new Point(55, 300));
			((Control)label17).set_Name("label17");
			((Control)label17).set_Size(new Size(130, 19));
			((Control)label17).set_TabIndex(38);
			((Control)label17).set_Text("UCAC4 matching");
			((Control)label16).set_AutoSize(true);
			((Control)label16).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label16).set_Location(new Point(162, 325));
			((Control)label16).set_Name("label16");
			((Control)label16).set_Size(new Size(23, 13));
			((Control)label16).set_TabIndex(37);
			((Control)label16).set_Text("File");
			((Control)label15).set_AutoSize(true);
			((Control)label15).set_Location(new Point(197, 302));
			((Control)label15).set_Name("label15");
			((Control)label15).set_Size(new Size(45, 17));
			((Control)label15).set_TabIndex(36);
			((Control)label15).set_Text("Band");
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Location(new Point(353, 302));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(45, 17));
			((Control)label9).set_TabIndex(35);
			((Control)label9).set_Text("Total");
			((Control)label14).set_AutoSize(true);
			((Control)label14).set_Location(new Point(278, 302));
			((Control)label14).set_Name("label14");
			((Control)label14).set_Size(new Size(45, 17));
			((Control)label14).set_TabIndex(34);
			((Control)label14).set_Text("Zone");
			((Control)label53).set_AutoSize(true);
			((Control)label53).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label53).set_Location(new Point(120, 363));
			((Control)label53).set_Name("label53");
			((Control)label53).set_Size(new Size(65, 13));
			((Control)label53).set_TabIndex(33);
			((Control)label53).set_Text("Good match");
			((Control)label73).set_AutoSize(true);
			((Control)label73).set_Location(new Point(278, 68));
			((Control)label73).set_Name("label73");
			((Control)label73).set_Size(new Size(45, 17));
			((Control)label73).set_TabIndex(32);
			((Control)label73).set_Text("Total");
			((Control)label72).set_AutoSize(true);
			((Control)label72).set_Location(new Point(197, 68));
			((Control)label72).set_Name("label72");
			((Control)label72).set_Size(new Size(45, 17));
			((Control)label72).set_TabIndex(31);
			((Control)label72).set_Text("Zone");
			((Control)lblTGASzoneaddedCount).set_AutoSize(true);
			((Control)lblTGASzoneaddedCount).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblTGASzoneaddedCount).set_Location(new Point(197, 173));
			((Control)lblTGASzoneaddedCount).set_Name("lblTGASzoneaddedCount");
			((Control)lblTGASzoneaddedCount).set_Size(new Size(14, 13));
			((Control)lblTGASzoneaddedCount).set_TabIndex(30);
			((Control)lblTGASzoneaddedCount).set_Text("#");
			((Control)lblHIPzonematchedCount).set_AutoSize(true);
			((Control)lblHIPzonematchedCount).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblHIPzonematchedCount).set_Location(new Point(197, 198));
			((Control)lblHIPzonematchedCount).set_Name("lblHIPzonematchedCount");
			((Control)lblHIPzonematchedCount).set_Size(new Size(14, 13));
			((Control)lblHIPzonematchedCount).set_TabIndex(29);
			((Control)lblHIPzonematchedCount).set_Text("#");
			((Control)lblHIPzoneaddedCount).set_AutoSize(true);
			((Control)lblHIPzoneaddedCount).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblHIPzoneaddedCount).set_Location(new Point(197, 217));
			((Control)lblHIPzoneaddedCount).set_Name("lblHIPzoneaddedCount");
			((Control)lblHIPzoneaddedCount).set_Size(new Size(14, 13));
			((Control)lblHIPzoneaddedCount).set_TabIndex(28);
			((Control)lblHIPzoneaddedCount).set_Text("#");
			((Control)lblTGASzoneCount).set_AutoSize(true);
			((Control)lblTGASzoneCount).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblTGASzoneCount).set_Location(new Point(197, 110));
			((Control)lblTGASzoneCount).set_Name("lblTGASzoneCount");
			((Control)lblTGASzoneCount).set_Size(new Size(14, 13));
			((Control)lblTGASzoneCount).set_TabIndex(27);
			((Control)lblTGASzoneCount).set_Text("#");
			((Control)lblHIPzoneCount).set_AutoSize(true);
			((Control)lblHIPzoneCount).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblHIPzoneCount).set_Location(new Point(197, 129));
			((Control)lblHIPzoneCount).set_Name("lblHIPzoneCount");
			((Control)lblHIPzoneCount).set_Size(new Size(14, 13));
			((Control)lblHIPzoneCount).set_TabIndex(26);
			((Control)lblHIPzoneCount).set_Text("#");
			((Control)lblGaiazoneCount).set_AutoSize(true);
			((Control)lblGaiazoneCount).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblGaiazoneCount).set_Location(new Point(197, 91));
			((Control)lblGaiazoneCount).set_Name("lblGaiazoneCount");
			((Control)lblGaiazoneCount).set_Size(new Size(14, 13));
			((Control)lblGaiazoneCount).set_TabIndex(25);
			((Control)lblGaiazoneCount).set_Text("#");
			((Control)lblTGASzonematchedCount).set_AutoSize(true);
			((Control)lblTGASzonematchedCount).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblTGASzonematchedCount).set_Location(new Point(197, 154));
			((Control)lblTGASzonematchedCount).set_Name("lblTGASzonematchedCount");
			((Control)lblTGASzonematchedCount).set_Size(new Size(14, 13));
			((Control)lblTGASzonematchedCount).set_TabIndex(24);
			((Control)lblTGASzonematchedCount).set_Text("#");
			((Control)lblTGAStotalAddedCount).set_AutoSize(true);
			((Control)lblTGAStotalAddedCount).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblTGAStotalAddedCount).set_Location(new Point(278, 173));
			((Control)lblTGAStotalAddedCount).set_Name("lblTGAStotalAddedCount");
			((Control)lblTGAStotalAddedCount).set_Size(new Size(14, 13));
			((Control)lblTGAStotalAddedCount).set_TabIndex(23);
			((Control)lblTGAStotalAddedCount).set_Text("#");
			((Control)lblHIPtotalMatchedCount).set_AutoSize(true);
			((Control)lblHIPtotalMatchedCount).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblHIPtotalMatchedCount).set_Location(new Point(278, 198));
			((Control)lblHIPtotalMatchedCount).set_Name("lblHIPtotalMatchedCount");
			((Control)lblHIPtotalMatchedCount).set_Size(new Size(14, 13));
			((Control)lblHIPtotalMatchedCount).set_TabIndex(22);
			((Control)lblHIPtotalMatchedCount).set_Text("#");
			((Control)lblHIPtotalAddedCount).set_AutoSize(true);
			((Control)lblHIPtotalAddedCount).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblHIPtotalAddedCount).set_Location(new Point(278, 217));
			((Control)lblHIPtotalAddedCount).set_Name("lblHIPtotalAddedCount");
			((Control)lblHIPtotalAddedCount).set_Size(new Size(14, 13));
			((Control)lblHIPtotalAddedCount).set_TabIndex(21);
			((Control)lblHIPtotalAddedCount).set_Text("#");
			((Control)lblUCAC4_Zone).set_AutoSize(true);
			((Control)lblUCAC4_Zone).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblUCAC4_Zone).set_Location(new Point(197, 325));
			((Control)lblUCAC4_Zone).set_Name("lblUCAC4_Zone");
			((Control)lblUCAC4_Zone).set_Size(new Size(30, 13));
			((Control)lblUCAC4_Zone).set_TabIndex(20);
			((Control)lblUCAC4_Zone).set_Text("z000");
			((Control)label59).set_AutoSize(true);
			((Control)label59).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label59).set_Location(new Point(73, 217));
			((Control)label59).set_Name("label59");
			((Control)label59).set_Size(new Size(112, 13));
			((Control)label59).set_TabIndex(19);
			((Control)label59).set_Text("Added from Hipparcos");
			((Control)label61).set_AutoSize(true);
			((Control)label61).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label61).set_Location(new Point(73, 198));
			((Control)label61).set_Name("label61");
			((Control)label61).set_Size(new Size(112, 13));
			((Control)label61).set_TabIndex(18);
			((Control)label61).set_Text("Matched to Hipparcos");
			((Control)lblUCAC4BandGoodMatch).set_AutoSize(true);
			((Control)lblUCAC4BandGoodMatch).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblUCAC4BandGoodMatch).set_Location(new Point(197, 363));
			((Control)lblUCAC4BandGoodMatch).set_Name("lblUCAC4BandGoodMatch");
			((Control)lblUCAC4BandGoodMatch).set_Size(new Size(14, 13));
			((Control)lblUCAC4BandGoodMatch).set_TabIndex(17);
			((Control)lblUCAC4BandGoodMatch).set_Text("#");
			((Control)label64).set_AutoSize(true);
			((Control)label64).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label64).set_Location(new Point(130, 344));
			((Control)label64).set_Name("label64");
			((Control)label64).set_Size(new Size(55, 13));
			((Control)label64).set_TabIndex(16);
			((Control)label64).set_Text("Stars read");
			((Control)lblTotalTGASreadCount).set_AutoSize(true);
			((Control)lblTotalTGASreadCount).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblTotalTGASreadCount).set_Location(new Point(278, 110));
			((Control)lblTotalTGASreadCount).set_Name("lblTotalTGASreadCount");
			((Control)lblTotalTGASreadCount).set_Size(new Size(14, 13));
			((Control)lblTotalTGASreadCount).set_TabIndex(15);
			((Control)lblTotalTGASreadCount).set_Text("#");
			((Control)label62).set_AutoSize(true);
			((Control)label62).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label62).set_Location(new Point(46, 129));
			((Control)label62).set_Name("label62");
			((Control)label62).set_Size(new Size(139, 13));
			((Control)label62).set_TabIndex(14);
			((Control)label62).set_Text("Hipparcos not in TychoGaia");
			((Control)lblTotalHIPReadCount).set_AutoSize(true);
			((Control)lblTotalHIPReadCount).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblTotalHIPReadCount).set_Location(new Point(278, 129));
			((Control)lblTotalHIPReadCount).set_Name("lblTotalHIPReadCount");
			((Control)lblTotalHIPReadCount).set_Size(new Size(14, 13));
			((Control)lblTotalHIPReadCount).set_TabIndex(13);
			((Control)lblTotalHIPReadCount).set_Text("#");
			((Control)label60).set_AutoSize(true);
			((Control)label60).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label60).set_Location(new Point(106, 91));
			((Control)label60).set_Name("label60");
			((Control)label60).set_Size(new Size(79, 13));
			((Control)label60).set_TabIndex(12);
			((Control)label60).set_Text("Stars from Gaia");
			((Control)lblTotalGaiaReadCount).set_AutoSize(true);
			((Control)lblTotalGaiaReadCount).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblTotalGaiaReadCount).set_Location(new Point(278, 91));
			((Control)lblTotalGaiaReadCount).set_Name("lblTotalGaiaReadCount");
			((Control)lblTotalGaiaReadCount).set_Size(new Size(14, 13));
			((Control)lblTotalGaiaReadCount).set_TabIndex(11);
			((Control)lblTotalGaiaReadCount).set_Text("#");
			((Control)label58).set_AutoSize(true);
			((Control)label58).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label58).set_Location(new Point(69, 173));
			((Control)label58).set_Name("label58");
			((Control)label58).set_Size(new Size(116, 13));
			((Control)label58).set_TabIndex(10);
			((Control)label58).set_Text("Added from TychoGaia");
			((Control)label57).set_AutoSize(true);
			((Control)label57).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label57).set_Location(new Point(69, 154));
			((Control)label57).set_Name("label57");
			((Control)label57).set_Size(new Size(116, 13));
			((Control)label57).set_TabIndex(9);
			((Control)label57).set_Text("Matched to TychoGaia");
			((Control)lblTGAStotalMatchedCount).set_AutoSize(true);
			((Control)lblTGAStotalMatchedCount).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblTGAStotalMatchedCount).set_Location(new Point(278, 154));
			((Control)lblTGAStotalMatchedCount).set_Name("lblTGAStotalMatchedCount");
			((Control)lblTGAStotalMatchedCount).set_Size(new Size(14, 13));
			((Control)lblTGAStotalMatchedCount).set_TabIndex(8);
			((Control)lblTGAStotalMatchedCount).set_Text("#");
			((Control)lblUCAC4ZoneGoodMatch).set_AutoSize(true);
			((Control)lblUCAC4ZoneGoodMatch).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblUCAC4ZoneGoodMatch).set_Location(new Point(278, 363));
			((Control)lblUCAC4ZoneGoodMatch).set_Name("lblUCAC4ZoneGoodMatch");
			((Control)lblUCAC4ZoneGoodMatch).set_Size(new Size(14, 13));
			((Control)lblUCAC4ZoneGoodMatch).set_TabIndex(7);
			((Control)lblUCAC4ZoneGoodMatch).set_Text("#");
			((Control)lblUCAC4BandCount).set_AutoSize(true);
			((Control)lblUCAC4BandCount).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblUCAC4BandCount).set_Location(new Point(197, 344));
			((Control)lblUCAC4BandCount).set_Name("lblUCAC4BandCount");
			((Control)lblUCAC4BandCount).set_Size(new Size(14, 13));
			((Control)lblUCAC4BandCount).set_TabIndex(6);
			((Control)lblUCAC4BandCount).set_Text("#");
			((Control)lblUCAC4ZoneCount).set_AutoSize(true);
			((Control)lblUCAC4ZoneCount).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblUCAC4ZoneCount).set_Location(new Point(278, 344));
			((Control)lblUCAC4ZoneCount).set_Name("lblUCAC4ZoneCount");
			((Control)lblUCAC4ZoneCount).set_Size(new Size(14, 13));
			((Control)lblUCAC4ZoneCount).set_TabIndex(5);
			((Control)lblUCAC4ZoneCount).set_Text("#");
			((Control)label51).set_AutoSize(true);
			((Control)label51).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label51).set_Location(new Point(101, 110));
			((Control)label51).set_Name("label51");
			((Control)label51).set_Size(new Size(84, 13));
			((Control)label51).set_TabIndex(4);
			((Control)label51).set_Text("TychoGaia stars");
			((Control)lblProcess).set_AutoSize(true);
			((Control)lblProcess).set_Location(new Point(88, 44));
			((Control)lblProcess).set_Name("lblProcess");
			((Control)lblProcess).set_Size(new Size(65, 17));
			((Control)lblProcess).set_TabIndex(3);
			((Control)lblProcess).set_Text("process");
			((Control)label52).set_AutoSize(true);
			((Control)label52).set_Location(new Point(17, 44));
			((Control)label52).set_Name("label52");
			((Control)label52).set_Size(new Size(71, 17));
			((Control)label52).set_TabIndex(2);
			((Control)label52).set_Text("Process:");
			((Control)lblZone).set_AutoSize(true);
			((Control)lblZone).set_Location(new Point(133, 25));
			((Control)lblZone).set_Name("lblZone");
			((Control)lblZone).set_Size(new Size(43, 17));
			((Control)lblZone).set_TabIndex(1);
			((Control)lblZone).set_Text("zone");
			((Control)label21).set_AutoSize(true);
			((Control)label21).set_Location(new Point(17, 25));
			((Control)label21).set_Name("label21");
			((Control)label21).set_Size(new Size(102, 17));
			((Control)label21).set_TabIndex(0);
			((Control)label21).set_Text("Current zone");
			((Control)groupBox6).set_BackColor(Color.LavenderBlush);
			((Control)groupBox6).get_Controls().Add((Control)(object)label50);
			((Control)groupBox6).get_Controls().Add((Control)(object)cmdCreateSubset);
			((Control)groupBox6).get_Controls().Add((Control)(object)updnLimitingMag);
			((Control)groupBox6).get_Controls().Add((Control)(object)label19);
			((Control)groupBox6).set_Location(new Point(23, 531));
			((Control)groupBox6).set_Name("groupBox6");
			((Control)groupBox6).set_Size(new Size(263, 138));
			((Control)groupBox6).set_TabIndex(100);
			groupBox6.set_TabStop(false);
			((Control)groupBox6).set_Text("Create a sub-set of selected file");
			((Control)label50).set_AutoSize(true);
			((Control)label50).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label50).set_Location(new Point(5, 23));
			((Control)label50).set_Name("label50");
			((Control)label50).set_Size(new Size(252, 17));
			((Control)label50).set_TabIndex(70);
			((Control)label50).set_Text("1. Select an existing file from above list");
			((Control)cmdCreateSubset).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdCreateSubset).set_Location(new Point(49, 78));
			((Control)cmdCreateSubset).set_Name("cmdCreateSubset");
			((Control)cmdCreateSubset).set_Size(new Size(164, 48));
			((Control)cmdCreateSubset).set_TabIndex(0);
			((Control)cmdCreateSubset).set_Text("Create a subset\r\n");
			((ButtonBase)cmdCreateSubset).set_UseVisualStyleBackColor(true);
			((Control)cmdCreateSubset).add_Click((EventHandler)cmdCreateSubset_Click);
			updnLimitingMag.set_DecimalPlaces(1);
			((Control)updnLimitingMag).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnLimitingMag.set_Increment(new decimal(new int[4] { 1, 0, 0, 65536 }));
			((Control)updnLimitingMag).set_Location(new Point(181, 49));
			updnLimitingMag.set_Maximum(new decimal(new int[4] { 20, 0, 0, 0 }));
			updnLimitingMag.set_Minimum(new decimal(new int[4] { 6, 0, 0, 0 }));
			((Control)updnLimitingMag).set_Name("updnLimitingMag");
			((Control)updnLimitingMag).set_Size(new Size(43, 20));
			((Control)updnLimitingMag).set_TabIndex(68);
			updnLimitingMag.set_Value(new decimal(new int[4] { 14, 0, 0, 0 }));
			((Control)label19).set_AutoSize(true);
			((Control)label19).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label19).set_Location(new Point(5, 48));
			((Control)label19).set_Name("label19");
			((Control)label19).set_Size(new Size(174, 17));
			((Control)label19).set_TabIndex(69);
			((Control)label19).set_Text("2. Set faint magnitude limit");
			label19.set_TextAlign(ContentAlignment.MiddleRight);
			((Control)groupBox5).set_BackColor(Color.Honeydew);
			((Control)groupBox5).get_Controls().Add((Control)(object)label30);
			((Control)groupBox5).get_Controls().Add((Control)(object)chkIgnore);
			((Control)groupBox5).get_Controls().Add((Control)(object)label25);
			((Control)groupBox5).get_Controls().Add((Control)(object)label2);
			((Control)groupBox5).get_Controls().Add((Control)(object)chkMatchToUCAC4);
			((Control)groupBox5).get_Controls().Add((Control)(object)groupBox4);
			((Control)groupBox5).get_Controls().Add((Control)(object)txtNewName);
			((Control)groupBox5).get_Controls().Add((Control)(object)label12);
			((Control)groupBox5).get_Controls().Add((Control)(object)label22);
			((Control)groupBox5).get_Controls().Add((Control)(object)cmdCreateDR3);
			((Control)groupBox5).get_Controls().Add((Control)(object)label34);
			((Control)groupBox5).set_Location(new Point(23, 36));
			((Control)groupBox5).set_Name("groupBox5");
			((Control)groupBox5).set_Size(new Size(263, 473));
			((Control)groupBox5).set_TabIndex(0);
			groupBox5.set_TabStop(false);
			((Control)groupBox5).set_Text("Create the catalogue file");
			((Control)label30).set_AutoSize(true);
			((Control)label30).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label30).set_ForeColor(Color.Maroon);
			((Control)label30).set_Location(new Point(8, 377));
			((Control)label30).set_Name("label30");
			((Control)label30).set_Size(new Size(246, 13));
			((Control)label30).set_TabIndex(100);
			((Control)label30).set_Text("Only check when bright star limit is fainter than 12.0");
			label30.set_TextAlign(ContentAlignment.MiddleLeft);
			((Control)chkIgnore).set_AutoSize(true);
			((Control)chkIgnore).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)chkIgnore).set_Location(new Point(21, 362));
			((Control)chkIgnore).set_Name("chkIgnore");
			((Control)chkIgnore).set_Size(new Size(213, 17));
			((Control)chkIgnore).set_TabIndex(99);
			((Control)chkIgnore).set_Text("Ignore TychoGaia and Hipparcos");
			((ButtonBase)chkIgnore).set_UseVisualStyleBackColor(true);
			((Control)label25).set_AutoSize(true);
			((Control)label25).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label25).set_Location(new Point(146, 330));
			((Control)label25).set_Name("label25");
			((Control)label25).set_Size(new Size(95, 24));
			((Control)label25).set_TabIndex(96);
			((Control)label25).set_Text("This is checked when \r\nUCAC4 is present");
			label25.set_TextAlign(ContentAlignment.MiddleLeft);
			((Control)chkMatchToUCAC4).set_AutoSize(true);
			chkMatchToUCAC4.set_Checked(true);
			chkMatchToUCAC4.set_CheckState((CheckState)1);
			((Control)chkMatchToUCAC4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)chkMatchToUCAC4).set_Location(new Point(21, 334));
			((Control)chkMatchToUCAC4).set_Name("chkMatchToUCAC4");
			((Control)chkMatchToUCAC4).set_Size(new Size(120, 17));
			((Control)chkMatchToUCAC4).set_TabIndex(1);
			((Control)chkMatchToUCAC4).set_Text("Match to UCAC4");
			((ButtonBase)chkMatchToUCAC4).set_UseVisualStyleBackColor(true);
			((Control)groupBox4).get_Controls().Add((Control)(object)label11);
			((Control)groupBox4).get_Controls().Add((Control)(object)lstGaiaFiles);
			((Control)groupBox4).set_Location(new Point(27, 127));
			((Control)groupBox4).set_Name("groupBox4");
			((Control)groupBox4).set_Size(new Size(208, 195));
			((Control)groupBox4).set_TabIndex(97);
			groupBox4.set_TabStop(false);
			((Control)groupBox4).set_Text("Existing Gaia files");
			((Control)label34).set_AutoSize(true);
			((Control)label34).set_Location(new Point(141, 48));
			((Control)label34).set_Name("label34");
			((Control)label34).set_Size(new Size(85, 17));
			((Control)label34).set_TabIndex(98);
			((Control)label34).set_Text("_EDR3.bin");
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(787, 792));
			((Control)this).get_Controls().Add((Control)(object)tabGaia);
			((Control)this).get_Controls().Add((Control)(object)label5);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_Name("CreateGaiaCatalog");
			((Control)this).set_Text("Create Gaia Catalog");
			((Form)this).add_Load((EventHandler)CreateGaiaCatalog_Load);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)tabGaia).ResumeLayout(false);
			((Control)tabPage1).ResumeLayout(false);
			((Control)tabPage1).PerformLayout();
			((Control)tabPage2).ResumeLayout(false);
			((Control)groupBox3).ResumeLayout(false);
			((Control)groupBox3).PerformLayout();
			((Control)panel1).ResumeLayout(false);
			((Control)panel1).PerformLayout();
			((Control)groupBox2).ResumeLayout(false);
			((Control)groupBox2).PerformLayout();
			((Control)panel3).ResumeLayout(false);
			((Control)panel3).PerformLayout();
			((Control)groupBox1).ResumeLayout(false);
			((Control)groupBox1).PerformLayout();
			((Control)tabPage3).ResumeLayout(false);
			((Control)tabPage3).PerformLayout();
			((Control)groupBox8).ResumeLayout(false);
			((Control)groupBox8).PerformLayout();
			((Control)groupBox7).ResumeLayout(false);
			((Control)groupBox7).PerformLayout();
			((Control)groupBox6).ResumeLayout(false);
			((Control)groupBox6).PerformLayout();
			((ISupportInitialize)updnLimitingMag).EndInit();
			((Control)groupBox5).ResumeLayout(false);
			((Control)groupBox5).PerformLayout();
			((Control)groupBox4).ResumeLayout(false);
			((Control)groupBox4).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
