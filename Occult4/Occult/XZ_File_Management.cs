using System;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Windows.Forms;
using Occult.Asteroid_Observations;
using Occult.Properties;
using Occult.Star_Catalogues;

namespace Occult
{
	public class XZ_File_Management : Form
	{
		private readonly string AppPath;

		private const double Radian = 180.0 / Math.PI;

		internal KObjects KObj;

		private double[] RA_Begin = new double[10] { 90.08, 106.13, 238.5, 328.5, 49.0, 122.3, 197.0, 278.0, 0.0, 262.0 };

		private double[] RA_End = new double[10] { 166.5, 181.4, 254.05, 344.0, 67.5, 136.5, 212.5, 297.0, 0.0, 278.5 };

		private double[] Dec_North = new double[10] { 29.99, 8.83, -15.27, -3.5, 27.0, 24.0, -4.0, -15.5, 0.0, -14.0 };

		private double[] Dec_South = new double[10] { 14.1, -6.01, -29.53, -19.2, 10.2, 9.5, -18.5, -31.2, 0.0, -29.2 };

		private IContainer components;

		private Button cmdCreateSubsets;

		private Button cmdReIndexDoubles;

		private ProgressBar pBarXZ;

		private Button cmdReIndexVariables;

		private Label label1;

		private Button cmdEditVariables;

		private Button cmdEditDoubles;

		private Label label2;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem helpToolStripMenuItem;

		private Button cmdOrbits;

		private Button cmdCreateNewVariableFile;

		private Label label3;

		private GroupBox grpKepler2;

		private Label label4;

		private Button cmdHistoricLunarInKepler2;

		private Label lblStatus;

		private Label lblFile;

		private Label lblTag;

		public GroupBox grpEdit;

		public GroupBox grpReIndex;

		public Button cmdCreateKepler2Cats;

		public Button cmdKepler2InAsteroidals;

		private Label label5;

		private Button cmdMatchToGaia_DR2;

		private ProgressBar pBarGaia;

		public XZ_File_Management()
		{
			InitializeComponent();
			AppPath = Utilities.AppPath;
		}

		private void XZ_File_Management_Load(object sender, EventArgs e)
		{
			if (((Control)this).get_Top() < -30000)
			{
				((Control)this).set_Top(0);
			}
			if ((double)((Control)this).get_Left() + (double)((Control)this).get_Width() / 1.2 < (double)Screen.GetWorkingArea((Control)(object)this).Left)
			{
				((Control)this).set_Left(Screen.GetWorkingArea((Control)(object)this).Left);
			}
			((Control)grpEdit).set_Enabled(Utilities.Administrator);
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
		}

		private void cmdCreateSubsets_Click(object sender, EventArgs e)
		{
			//IL_0307: Unknown result type (might be due to invalid IL or missing references)
			byte[] array = new byte[35];
			array[9] = 23;
			array[10] = 91;
			array[11] = 202;
			FileStream fileStream = new FileStream(AppPath + "\\Resource Files\\XZ80.dat", FileMode.Open, FileAccess.Read);
			BinaryReader readXZ = new BinaryReader(fileStream);
			FileStream fileStream2 = new FileStream(AppPath + "\\Resource Files\\XZ80Mag4.dat", FileMode.Create, FileAccess.Write);
			BinaryWriter binaryWriter = new BinaryWriter(fileStream2);
			FileStream fileStream3 = new FileStream(AppPath + "\\Resource Files\\XZ80Mag7.dat", FileMode.Create, FileAccess.Write);
			BinaryWriter binaryWriter2 = new BinaryWriter(fileStream3);
			FileStream fileStream4 = new FileStream(AppPath + "\\Resource Files\\XZ80Mag9.dat", FileMode.Create, FileAccess.Write);
			BinaryWriter binaryWriter3 = new BinaryWriter(fileStream4);
			FileStream fileStream5 = new FileStream(AppPath + "\\Resource Files\\ZC.dat", FileMode.Create, FileAccess.Write);
			BinaryWriter binaryWriter4 = new BinaryWriter(fileStream5);
			for (int i = 0; i <= 3539; i++)
			{
				binaryWriter4.Write(array);
			}
			new XZ80Q();
			int num = (int)fileStream.Length / 35;
			for (int j = 0; j < num; j++)
			{
				XZ80Q.ReadNextSeriatum(readXZ, out var mag, out var ZC);
				if (mag > 0f || j == 0)
				{
					if (mag <= 4f || j == 0)
					{
						XZ80Q.WriteNextSeriatum(binaryWriter);
					}
					if (mag <= 7f || j == 0)
					{
						XZ80Q.WriteNextSeriatum(binaryWriter2);
					}
					if (mag <= 9f || j == 0)
					{
						XZ80Q.WriteNextSeriatum(binaryWriter3);
					}
					if (ZC > 0 || j == 0)
					{
						binaryWriter4.Seek(ZC * 35, SeekOrigin.Begin);
						XZ80Q.WriteNextSeriatum(binaryWriter4);
					}
				}
			}
			fileStream.Close();
			int num2 = (int)fileStream2.Length / 35;
			string value = ("   1\r " + string.Format("{0,1:F0}", num2 - 1)).PadRight(25);
			fileStream2.Seek(0L, SeekOrigin.Begin);
			binaryWriter.Write(value);
			fileStream2.Close();
			CreateIndexFile("XZ80Mag4", num2);
			num2 = (int)fileStream3.Length / 35;
			value = ("   1\r " + string.Format("{0,1:F0}", num2 - 1)).PadRight(25);
			fileStream3.Seek(0L, SeekOrigin.Begin);
			binaryWriter2.Write(value);
			fileStream3.Close();
			CreateIndexFile("XZ80Mag7", num2);
			num2 = (int)fileStream4.Length / 35;
			value = ("   1\r " + string.Format("{0,1:F0}", num2 - 1)).PadRight(25);
			fileStream4.Seek(0L, SeekOrigin.Begin);
			binaryWriter3.Write(value);
			fileStream4.Close();
			CreateIndexFile("XZ80Mag9", num2);
			num2 = (int)fileStream5.Length / 35;
			value = ("   1\r " + string.Format("{0,1:F0}", num2 - 1)).PadRight(25);
			fileStream5.Seek(0L, SeekOrigin.Begin);
			binaryWriter4.Write(value);
			fileStream5.Close();
			CreateIndexFile("ZC", num2);
			MessageBox.Show("XZ subsets created", "Success", (MessageBoxButtons)0, (MessageBoxIcon)64);
		}

		private void CreateIndexFile(string FileName, int Max)
		{
			FileStream fileStream = new FileStream(AppPath + "\\Resource Files\\" + FileName + ".dat", FileMode.Open, FileAccess.Read);
			StreamWriter streamWriter = new StreamWriter(AppPath + "\\Resource Files\\" + FileName + ".inx");
			BinaryReader readXZ = new BinaryReader(fileStream);
			int i = 0;
			for (int j = 1; j <= Max; j++)
			{
				XZ80Q.ReadStarEntry(fileStream, readXZ, j);
				if (XZ80Q.Dec_rad > -1.0)
				{
					for (; XZ80Q.RA_rad * (180.0 / Math.PI) >= (double)i; i++)
					{
						streamWriter.WriteLine(string.Format("{0,7:F0}", j + 1));
					}
				}
			}
			do
			{
				streamWriter.WriteLine(string.Format("{0,7:F0}", Max));
				i++;
			}
			while (i < 361);
			fileStream.Close();
			streamWriter.Close();
		}

		private void ReIndexXZ80ForDoubles()
		{
			char[] array = new char[35];
			byte b = 0;
			char c = '\0';
			DoubleStarLine doubleStarLine = new DoubleStarLine();
			FileStream fileStream = new FileStream(AppPath + "\\Resource Files\\XZ80.dat", FileMode.Open, FileAccess.ReadWrite);
			BinaryReader binaryReader = new BinaryReader(fileStream);
			BinaryWriter binaryWriter = new BinaryWriter(fileStream);
			fileStream.Seek(0L, SeekOrigin.Begin);
			array = binaryReader.ReadChars(35);
			int i;
			for (i = 1; i < 20 && array[i].CompareTo('\r') != 0; i++)
			{
			}
			int num = int.Parse(new string(array, i + 1, 8));
			string[] DoubleCode = new string[244438];
			StreamReader streamReader = new StreamReader(AppPath + "\\Resource Files\\XZDoubles.dat");
			((Control)label1).set_Visible(true);
			Application.DoEvents();
			do
			{
				doubleStarLine.ReadLine(streamReader.ReadLine());
				doubleStarLine.PAandSep(2000.0, out var _, out var Sep);
				if (Sep < 0.0)
				{
					Sep = 0.0;
				}
				if (doubleStarLine.XZa > 0)
				{
					DoubleCode[doubleStarLine.XZa] = DoubleStarCode(doubleStarLine.XZa, doubleStarLine.XZb, Sep, ref DoubleCode);
				}
				if (doubleStarLine.XZb > 0)
				{
					DoubleCode[doubleStarLine.XZb] = DoubleStarCode(doubleStarLine.XZb, doubleStarLine.XZa, Sep, ref DoubleCode);
				}
			}
			while (!streamReader.EndOfStream);
			streamReader.Close();
			pBarXZ.set_Minimum(0);
			pBarXZ.set_Maximum(num);
			((Control)pBarXZ).set_Visible(true);
			((Control)label1).set_Text("Clearing old double codes");
			b = 32;
			for (i = 1; i <= num; i++)
			{
				fileStream.Seek(i * 35 + 23, SeekOrigin.Begin);
				binaryWriter.Write(b);
				if (i % 100 == 0)
				{
					pBarXZ.set_Value(i);
					Application.DoEvents();
				}
			}
			((Control)label1).set_Text("Adding new double codes");
			for (i = 1; i <= 244437; i++)
			{
				if (DoubleCode[i] != null)
				{
					c = Convert.ToChar(DoubleCode[i]);
					fileStream.Seek(XZ80Q.Get_XZ_RecordNumber(i) * 35 + 23, SeekOrigin.Begin);
					binaryWriter.Write(c);
					if (i % 100 == 0)
					{
						pBarXZ.set_Value(i);
						Application.DoEvents();
					}
				}
			}
			fileStream.Close();
			((Control)pBarXZ).set_Visible(false);
			((Control)label1).set_Visible(false);
		}

		private static string DoubleStarCode(int Star1, int Star2, double Sep, ref string[] DoubleCode)
		{
			string text = DoubleCode[Star1];
			if (text == null)
			{
				text = " ";
			}
			if (Star2 == 0)
			{
				if (text.CompareTo("A") < 0)
				{
					if (Sep < 1.0)
					{
						return "c";
					}
					if (Sep < 10.0)
					{
						return "d";
					}
					return "w";
				}
				return "S";
			}
			if (text.CompareTo("A") < 0)
			{
				if (Sep < 1.0)
				{
					return "C";
				}
				if (Sep < 10.0)
				{
					return "D";
				}
				return "W";
			}
			if ((text == "C") | (text == "D") | (text == "W") | (text == "M"))
			{
				return "M";
			}
			return "S";
		}

		private void ReIndexXZ80ForVariables()
		{
			byte b = 0;
			char c = '\0';
			char[] array = new char[35];
			FileStream fileStream = new FileStream(AppPath + "\\Resource Files\\XZ80.dat", FileMode.Open, FileAccess.ReadWrite);
			BinaryReader binaryReader = new BinaryReader(fileStream);
			BinaryWriter binaryWriter = new BinaryWriter(fileStream);
			fileStream.Seek(0L, SeekOrigin.Begin);
			array = binaryReader.ReadChars(35);
			int i;
			for (i = 1; i < 20 && array[i].CompareTo('\r') != 0; i++)
			{
			}
			int num = int.Parse(new string(array, i + 1, 8));
			pBarXZ.set_Minimum(0);
			pBarXZ.set_Maximum(num + 380);
			((Control)pBarXZ).set_Visible(true);
			((Control)label1).set_Visible(true);
			((Control)label1).set_Text("Clearing old variable codes");
			b = 32;
			for (i = 1; i <= num; i++)
			{
				fileStream.Seek(i * 35 + 34, SeekOrigin.Begin);
				binaryWriter.Write(b);
				if (i % 100 == 0)
				{
					pBarXZ.set_Value(i);
					Application.DoEvents();
				}
			}
			StreamReader streamReader = new StreamReader(AppPath + "\\Resource Files\\XZVariables.dat");
			((Control)label1).set_Text("Adding new variable codes");
			do
			{
				string text = streamReader.ReadLine();
				int num2 = int.Parse(text.Substring(0, 6));
				string text2 = text.Substring(37, 11);
				string text3 = "v";
				if ((text2.Substring(0, 1) == "E") & (!text2.Contains("ELL") & !text2.Contains("EW")))
				{
					text3 = "e";
				}
				if (text.Substring(7, 3) == "NSV")
				{
					text3 = "s";
				}
				if (text.Substring(54, 1) != "(")
				{
					if (!double.TryParse(text.Substring(48, 6), out var result))
					{
						result = 15.0;
					}
					if (!double.TryParse(text.Substring(55, 5), out var result2))
					{
						result2 = 15.0;
					}
					if (result2 - result > 0.5)
					{
						text3 = text3.ToUpper();
					}
				}
				c = Convert.ToChar(text3);
				fileStream.Seek(XZ80Q.Get_XZ_RecordNumber(num2) * 35 + 34, SeekOrigin.Begin);
				binaryWriter.Write(c);
				pBarXZ.set_Value(num2);
				Application.DoEvents();
			}
			while (!streamReader.EndOfStream);
			streamReader.Close();
			fileStream.Close();
			((Control)pBarXZ).set_Visible(false);
			((Control)label1).set_Visible(false);
		}

		private void cmdReIndexDoubles_Click(object sender, EventArgs e)
		{
			//IL_0013: Unknown result type (might be due to invalid IL or missing references)
			ReIndexXZ80ForDoubles();
			MessageBox.Show("The XZ80 catalogue has been updated with new double-star codes generated from   XZDoubles.dat\r\n\r\nThe catalogue subsets  Mag 3,  Mag 6,  Mag 9  and  ZC  need to be recreated.", "Reindexing done", (MessageBoxButtons)0, (MessageBoxIcon)64);
		}

		private void cmdReIndexVariables_Click(object sender, EventArgs e)
		{
			//IL_0013: Unknown result type (might be due to invalid IL or missing references)
			ReIndexXZ80ForVariables();
			MessageBox.Show("The XZ80 catalogue has been updated with new variable-star codes generated from   XZVariables.dat\r\n\r\nThe catalogue subsets  Mag 3,  Mag 6,  Mag 9  and  ZC  need to be recreated.", "Reindexing done", (MessageBoxButtons)0, (MessageBoxIcon)64);
		}

		private void cmdEditVariables_Click(object sender, EventArgs e)
		{
			EditXZ_Forms.Show_VariableEditor();
		}

		private void cmdEditDoubles_Click(object sender, EventArgs e)
		{
			EditXZ_Forms.Show_DoubleEditor();
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"XZ catalogue File management");
		}

		private void cmdOrbits_Click(object sender, EventArgs e)
		{
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			if (!Utilities.InternetIsAvailable())
			{
				MessageBox.Show("This functionality requires internet access to download the\r\n6th Orbit Catalogue from US Naval Observatory.\r\n\r\nYou are not connected to the internet", "No internet");
			}
			else
			{
				UpdateOrbits.UpdateXZOrbits();
			}
		}

		private void cmdCreateNewVariableFile_Click(object sender, EventArgs e)
		{
			Interferometric_Plus_WDS.CreateXZVariables();
		}

		private void cmdCreateKepler2Cats_Click(object sender, EventArgs e)
		{
			//IL_0063: Unknown result type (might be due to invalid IL or missing references)
			CreateSortedListOfK2Stars();
			if (Kepler2.K2.Count > 0)
			{
				((Control)lblStatus).set_Text("Writing full Kepler2 catalogue");
				Application.DoEvents();
				Kepler2.WriteFullKepler2Cat();
				((Control)lblStatus).set_Text("Matching to XZ catalogue");
				Application.DoEvents();
				Kepler2.Write_XZinKepler2();
				((Control)lblStatus).set_Text("Done");
			}
			else
			{
				MessageBox.Show("No Kepler2 files have been selected", "Kepler2 conversion cancelled", (MessageBoxButtons)0);
			}
		}

		internal void CreateSortedListOfK2Stars()
		{
			//IL_0016: Unknown result type (might be due to invalid IL or missing references)
			//IL_001d: Expected O, but got Unknown
			//IL_0047: Unknown result type (might be due to invalid IL or missing references)
			//IL_004d: Invalid comparison between Unknown and I4
			int num = 0;
			int num2 = 0;
			string text = " ";
			Kepler2.K2.Clear();
			OpenFileDialog val = new OpenFileDialog();
			try
			{
				((FileDialog)val).set_Title("Select all Kepler2 files to be processed");
				val.set_Multiselect(true);
				((FileDialog)val).set_Filter("CSV & TXT files (*.csv & *.txt)|*.csv;*.txt|All files (*.*)|*.*");
				((FileDialog)val).set_FilterIndex(1);
				if ((int)((CommonDialog)val).ShowDialog() != 1)
				{
					return;
				}
				((Control)lblStatus).set_Visible(true);
				((Control)lblStatus).set_Text("Merging K2 target files");
				Application.DoEvents();
				string[] fileNames = ((FileDialog)val).get_FileNames();
				foreach (string path in fileNames)
				{
					string text2 = Path.GetFileNameWithoutExtension(path) + "tttttt";
					int num3 = text2.IndexOf("aign") + 4;
					if (num3 >= text2.Length || num3 < 0)
					{
						text = " _";
					}
					num2 = text2.IndexOf("t", num3) - num3;
					if (num2 > 2)
					{
						num2 = 2;
					}
					text = text2.Substring(num3, num2).PadLeft(2);
					using StreamReader streamReader = new StreamReader(path);
					do
					{
						string[] array = streamReader.ReadLine()!.Split(new char[1] { ',' });
						if (array.Length > 3 && ((array[1].Trim() != "") & (array[2].Trim() != "")) && long.TryParse(array[0], out var result))
						{
							KObj = new KObjects();
							KObj.EPIC_ID = result;
							KObj.RA = double.Parse(array[1]);
							KObj.Dec = double.Parse(array[2]);
							KObj.Mag = double.Parse(array[3].Replace('"', ' ').Replace("\t", "").Replace(array[0], ""));
							num = array.Length - 1;
							if (array[num].Contains("_SC"))
							{
								KObj.Cadence = 1;
							}
							else
							{
								KObj.Cadence = 9;
							}
							KObj.Field = text;
							KObj.XZ = 0;
							Kepler2.K2.Add(KObj);
						}
					}
					while (!streamReader.EndOfStream);
				}
				KObjects.SortByMag = false;
				KObjects.SortByCadence = false;
				KObjects.SortByXZ = false;
				Kepler2.K2.Sort();
				string text3 = Kepler2.K2[Kepler2.K2.Count - 1].K2LineNoFieldNum;
				for (int num4 = Kepler2.K2.Count - 1; num4 >= 1; num4--)
				{
					string k2LineNoFieldNum = Kepler2.K2[num4 - 1].K2LineNoFieldNum;
					if (k2LineNoFieldNum == text3)
					{
						Kepler2.K2.Remove(Kepler2.K2[num4]);
					}
					text3 = k2LineNoFieldNum;
				}
			}
			finally
			{
				((IDisposable)val)?.Dispose();
			}
		}

		private void cmdHistoricLunarInKepler2_Click(object sender, EventArgs e)
		{
			CreateHistoricLunarsInKepler2();
		}

		public void CreateHistoricLunarsInKepler2()
		{
			//IL_000b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0011: Invalid comparison between Unknown and I4
			if ((int)MessageBox.Show("This routine creates two files in the Occult4/Observations subdirectory\r\n\r\n* Archive_in_Kepler2.txt - contains the full archive record\r\n\r\n* Archive_in_Kepler2.csv - contains an abridged record and identifier", "Create list of Lunar Occultations involving Kepler2 stars", (MessageBoxButtons)1) != 2)
			{
				Label obj = lblTag;
				bool visible;
				((Control)lblFile).set_Visible(visible = true);
				((Control)obj).set_Visible(visible);
				Application.DoEvents();
				if (File.Exists(AppPath + "\\Observations\\Archive_in_Kepler2.old"))
				{
					File.Delete(AppPath + "\\Observations\\Archive_in_Kepler2.old");
				}
				if (File.Exists(AppPath + "\\Observations\\Archive_in_Kepler2.txt"))
				{
					File.Move(AppPath + "\\Observations\\Archive_in_Kepler2.txt", AppPath + "\\Observations\\Archive_in_Kepler2.old");
				}
				if (File.Exists(AppPath + "\\Observations\\Archive_in_Kepler2.cs"))
				{
					File.Delete(AppPath + "\\Observations\\Archive_in_Kepler2.cs");
				}
				if (File.Exists(AppPath + "\\Observations\\Archive_in_Kepler2.csv"))
				{
					File.Move(AppPath + "\\Observations\\Archive_in_Kepler2.csv", AppPath + "\\Observations\\Archive_in_Kepler2.cs");
				}
				string[] files = Directory.GetFiles(AppPath + "\\Resource Files", "Archive Observations*");
				foreach (string text in files)
				{
					((Control)lblFile).set_Text(Path.GetFileName(text));
					Application.DoEvents();
					Kepler2.HistoricLunarsInKepler2(text);
				}
				Label obj2 = lblTag;
				((Control)lblFile).set_Visible(visible = false);
				((Control)obj2).set_Visible(visible);
			}
		}

		private void cmdKepler2InAsteroidals_Click(object sender, EventArgs e)
		{
			Asteroid_Observations_Reports.ListKepler2AsteroidEvents();
		}

		private void XZ_File_Management_FormClosing(object sender, FormClosingEventArgs e)
		{
			Kepler2.XZinK2.Clear();
		}

		private void cmdMatchToGaia_DR2_Click(object sender, EventArgs e)
		{
			//IL_01b1: Unknown result type (might be due to invalid IL or missing references)
			//IL_01b7: Invalid comparison between Unknown and I4
			string SourceFile = "";
			pBarGaia.set_Maximum(245000);
			((Control)pBarGaia).set_Visible(true);
			using (FileStream fileStream = new FileStream(Utilities.AppPath + "\\Resource Files\\XZ80_original.dat", FileMode.Open))
			{
				int num = (int)fileStream.Length / 35;
				using BinaryReader readXZ = new BinaryReader(fileStream);
				using FileStream fileStream2 = new FileStream(Utilities.AppPath + "\\Resource Files\\XZ80_DR2.dat", FileMode.Create);
				using BinaryWriter writeXZ = new BinaryWriter(fileStream2);
				for (int i = 0; i < num; i++)
				{
					if (i % 100 == 0)
					{
						pBarGaia.set_Value(i);
						Application.DoEvents();
					}
					XZ80Q.ReadStarEntry(fileStream, readXZ, i);
					double matchDistanceArcSec = 0.4;
					if (XZ80Q.DoubleFlag.Trim().Length > 0)
					{
						matchDistanceArcSec = 1.5;
					}
					if (Gaia.Get_GaiaStar_fromGaia(XZ80Q.RA_rad * (180.0 / Math.PI), XZ80Q.Dec_rad * (180.0 / Math.PI), StarCoords_used_as_ID: false, 0.0, matchDistanceArcSec, XZ80Q.Mv, FilterUsingStarMag: true, LimitUsingStarMag: false, out SourceFile))
					{
						XZ80Q.RA_rad = Gaia.RA_rad + (0.0 - Gaia.Epoch_2000) * Gaia.PMRA_rad;
						XZ80Q.PMRA_rad = Gaia.PMRA_rad;
						XZ80Q.Dec_rad = Gaia.Dec_rad + (0.0 - Gaia.Epoch_2000) * Gaia.PMDec_rad;
						XZ80Q.PMDec_rad = Gaia.PMDec_rad;
					}
					XZ80Q.WriteNext(fileStream2, writeXZ, i);
				}
			}
			((Control)pBarGaia).set_Visible(false);
			if ((int)MessageBox.Show("The XZ80 file has been successfully updated, under the file name XZ80_DR2.dat\r\n\r\nDo you want to rename the current XZ80.dat file to XZ80_old.dat.\r\nand replace it with the new file?", "Replace XZ80file", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152) == 6)
			{
				if (File.Exists(Utilities.AppPath + "\\Resource Files\\XZ80_old.dat"))
				{
					File.Delete(Utilities.AppPath + "\\Resource Files\\XZ80_old.dat");
				}
				File.Move(Utilities.AppPath + "\\Resource Files\\XZ80.dat", Utilities.AppPath + "\\Resource Files\\XZ80_old.dat");
				File.Move(Utilities.AppPath + "\\Resource Files\\XZ80_DR2.dat", Utilities.AppPath + "\\Resource Files\\XZ80.dat");
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
			//IL_018b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0233: Unknown result type (might be due to invalid IL or missing references)
			//IL_02c1: Unknown result type (might be due to invalid IL or missing references)
			//IL_0341: Unknown result type (might be due to invalid IL or missing references)
			//IL_03fb: Unknown result type (might be due to invalid IL or missing references)
			//IL_048f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0537: Unknown result type (might be due to invalid IL or missing references)
			//IL_0650: Unknown result type (might be due to invalid IL or missing references)
			//IL_0671: Unknown result type (might be due to invalid IL or missing references)
			//IL_06f8: Unknown result type (might be due to invalid IL or missing references)
			//IL_07a4: Unknown result type (might be due to invalid IL or missing references)
			//IL_084e: Unknown result type (might be due to invalid IL or missing references)
			//IL_08fa: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a4e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a6f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0af6: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c3a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c5b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0cf2: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d87: Unknown result type (might be due to invalid IL or missing references)
			//IL_0e42: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ee3: Unknown result type (might be due to invalid IL or missing references)
			//IL_0f84: Unknown result type (might be due to invalid IL or missing references)
			//IL_1016: Unknown result type (might be due to invalid IL or missing references)
			//IL_10bf: Unknown result type (might be due to invalid IL or missing references)
			//IL_116a: Unknown result type (might be due to invalid IL or missing references)
			//IL_11d5: Unknown result type (might be due to invalid IL or missing references)
			//IL_1267: Unknown result type (might be due to invalid IL or missing references)
			//IL_136f: Unknown result type (might be due to invalid IL or missing references)
			//IL_1379: Expected O, but got Unknown
			//IL_13af: Unknown result type (might be due to invalid IL or missing references)
			//IL_13d7: Unknown result type (might be due to invalid IL or missing references)
			//IL_13e1: Expected O, but got Unknown
			cmdCreateSubsets = new Button();
			cmdReIndexDoubles = new Button();
			pBarXZ = new ProgressBar();
			cmdReIndexVariables = new Button();
			label1 = new Label();
			cmdEditVariables = new Button();
			cmdEditDoubles = new Button();
			grpEdit = new GroupBox();
			cmdCreateNewVariableFile = new Button();
			cmdOrbits = new Button();
			label2 = new Label();
			menuStrip1 = new MenuStrip();
			helpToolStripMenuItem = new ToolStripMenuItem();
			grpReIndex = new GroupBox();
			label3 = new Label();
			grpKepler2 = new GroupBox();
			label5 = new Label();
			cmdKepler2InAsteroidals = new Button();
			lblTag = new Label();
			lblFile = new Label();
			lblStatus = new Label();
			cmdHistoricLunarInKepler2 = new Button();
			cmdCreateKepler2Cats = new Button();
			label4 = new Label();
			cmdMatchToGaia_DR2 = new Button();
			pBarGaia = new ProgressBar();
			((Control)grpEdit).SuspendLayout();
			((Control)menuStrip1).SuspendLayout();
			((Control)grpReIndex).SuspendLayout();
			((Control)grpKepler2).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)cmdCreateSubsets).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdCreateSubsets).set_Location(new Point(32, 227));
			((Control)cmdCreateSubsets).set_Margin(new Padding(2));
			((Control)cmdCreateSubsets).set_Name("cmdCreateSubsets");
			((Control)cmdCreateSubsets).set_Size(new Size(143, 34));
			((Control)cmdCreateSubsets).set_TabIndex(0);
			((Control)cmdCreateSubsets).set_Text("Create catalogue subsets\r\nZC, Mag 4, Mag 7, Mag 9");
			((ButtonBase)cmdCreateSubsets).set_UseVisualStyleBackColor(true);
			((Control)cmdCreateSubsets).add_Click((EventHandler)cmdCreateSubsets_Click);
			((Control)cmdReIndexDoubles).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdReIndexDoubles).set_Location(new Point(32, 93));
			((Control)cmdReIndexDoubles).set_Margin(new Padding(2));
			((Control)cmdReIndexDoubles).set_Name("cmdReIndexDoubles");
			((Control)cmdReIndexDoubles).set_Size(new Size(143, 39));
			((Control)cmdReIndexDoubles).set_TabIndex(1);
			((Control)cmdReIndexDoubles).set_Text("Re-Index doubles\r\n&& add Kepler2 flags");
			((ButtonBase)cmdReIndexDoubles).set_UseVisualStyleBackColor(true);
			((Control)cmdReIndexDoubles).add_Click((EventHandler)cmdReIndexDoubles_Click);
			((Control)pBarXZ).set_Location(new Point(51, 137));
			((Control)pBarXZ).set_Margin(new Padding(2));
			((Control)pBarXZ).set_Name("pBarXZ");
			((Control)pBarXZ).set_Size(new Size(104, 8));
			((Control)pBarXZ).set_TabIndex(2);
			((Control)pBarXZ).set_Visible(false);
			((Control)cmdReIndexVariables).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdReIndexVariables).set_Location(new Point(32, 180));
			((Control)cmdReIndexVariables).set_Margin(new Padding(2));
			((Control)cmdReIndexVariables).set_Name("cmdReIndexVariables");
			((Control)cmdReIndexVariables).set_Size(new Size(143, 30));
			((Control)cmdReIndexVariables).set_TabIndex(3);
			((Control)cmdReIndexVariables).set_Text("Re-Index variables");
			((ButtonBase)cmdReIndexVariables).set_UseVisualStyleBackColor(true);
			((Control)cmdReIndexVariables).add_Click((EventHandler)cmdReIndexVariables_Click);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(39, 147));
			((Control)label1).set_Margin(new Padding(2, 0, 2, 0));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(129, 13));
			((Control)label1).set_TabIndex(4);
			((Control)label1).set_Text("Clearing old double codes");
			((Control)label1).set_Visible(false);
			((Control)cmdEditVariables).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdEditVariables).set_Location(new Point(32, 257));
			((Control)cmdEditVariables).set_Margin(new Padding(2));
			((Control)cmdEditVariables).set_Name("cmdEditVariables");
			((Control)cmdEditVariables).set_Size(new Size(143, 30));
			((Control)cmdEditVariables).set_TabIndex(5);
			((Control)cmdEditVariables).set_Text("Edit  XZ Variables");
			((ButtonBase)cmdEditVariables).set_UseVisualStyleBackColor(true);
			((Control)cmdEditVariables).add_Click((EventHandler)cmdEditVariables_Click);
			((Control)cmdEditDoubles).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdEditDoubles).set_Location(new Point(32, 69));
			((Control)cmdEditDoubles).set_Margin(new Padding(2));
			((Control)cmdEditDoubles).set_Name("cmdEditDoubles");
			((Control)cmdEditDoubles).set_Size(new Size(143, 50));
			((Control)cmdEditDoubles).set_TabIndex(6);
			((Control)cmdEditDoubles).set_Text("Edit   XZ Doubles\r\n&&\r\nOccultation discoveries");
			((ButtonBase)cmdEditDoubles).set_UseVisualStyleBackColor(true);
			((Control)cmdEditDoubles).add_Click((EventHandler)cmdEditDoubles_Click);
			((Control)grpEdit).get_Controls().Add((Control)(object)cmdCreateNewVariableFile);
			((Control)grpEdit).get_Controls().Add((Control)(object)cmdOrbits);
			((Control)grpEdit).get_Controls().Add((Control)(object)label2);
			((Control)grpEdit).get_Controls().Add((Control)(object)cmdEditDoubles);
			((Control)grpEdit).get_Controls().Add((Control)(object)cmdEditVariables);
			((Control)grpEdit).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpEdit).set_Location(new Point(218, 28));
			((Control)grpEdit).set_Margin(new Padding(2));
			((Control)grpEdit).set_Name("grpEdit");
			((Control)grpEdit).set_Padding(new Padding(2));
			((Control)grpEdit).set_Size(new Size(206, 301));
			((Control)grpEdit).set_TabIndex(7);
			grpEdit.set_TabStop(false);
			((Control)grpEdit).set_Text("Edit Doubles && Variables");
			((Control)cmdCreateNewVariableFile).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdCreateNewVariableFile).set_Location(new Point(32, 203));
			((Control)cmdCreateNewVariableFile).set_Margin(new Padding(2));
			((Control)cmdCreateNewVariableFile).set_Name("cmdCreateNewVariableFile");
			((Control)cmdCreateNewVariableFile).set_Size(new Size(143, 37));
			((Control)cmdCreateNewVariableFile).set_TabIndex(9);
			((Control)cmdCreateNewVariableFile).set_Text("Create new file of variables");
			((ButtonBase)cmdCreateNewVariableFile).set_UseVisualStyleBackColor(true);
			((Control)cmdCreateNewVariableFile).add_Click((EventHandler)cmdCreateNewVariableFile_Click);
			((Control)cmdOrbits).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdOrbits).set_Location(new Point(32, 135));
			((Control)cmdOrbits).set_Margin(new Padding(2));
			((Control)cmdOrbits).set_Name("cmdOrbits");
			((Control)cmdOrbits).set_Size(new Size(143, 38));
			((Control)cmdOrbits).set_TabIndex(8);
			((Control)cmdOrbits).set_Text("Update  XZ Doubles\r\nwith latest orbits");
			((ButtonBase)cmdOrbits).set_UseVisualStyleBackColor(true);
			((Control)cmdOrbits).add_Click((EventHandler)cmdOrbits_Click);
			((Control)label2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label2).set_Location(new Point(2, 12));
			((Control)label2).set_Margin(new Padding(2, 0, 2, 0));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(188, 43));
			((Control)label2).set_TabIndex(7);
			((Control)label2).set_Text("These options are used for updating \r\n the files of double and variable stars.\r\nThey are NOT for general use.");
			((ToolStrip)menuStrip1).set_ImageScalingSize(new Size(20, 20));
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[1] { (ToolStripItem)helpToolStripMenuItem });
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Padding(new Padding(4, 2, 0, 2));
			((Control)menuStrip1).set_Size(new Size(636, 28));
			((Control)menuStrip1).set_TabIndex(8);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(64, 24));
			((ToolStripItem)helpToolStripMenuItem).set_Text("&Help");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((Control)grpReIndex).get_Controls().Add((Control)(object)label3);
			((Control)grpReIndex).get_Controls().Add((Control)(object)label1);
			((Control)grpReIndex).get_Controls().Add((Control)(object)cmdReIndexVariables);
			((Control)grpReIndex).get_Controls().Add((Control)(object)pBarXZ);
			((Control)grpReIndex).get_Controls().Add((Control)(object)cmdReIndexDoubles);
			((Control)grpReIndex).get_Controls().Add((Control)(object)cmdCreateSubsets);
			((Control)grpReIndex).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpReIndex).set_Location(new Point(9, 28));
			((Control)grpReIndex).set_Margin(new Padding(2));
			((Control)grpReIndex).set_Name("grpReIndex");
			((Control)grpReIndex).set_Padding(new Padding(2));
			((Control)grpReIndex).set_Size(new Size(206, 301));
			((Control)grpReIndex).set_TabIndex(9);
			grpReIndex.set_TabStop(false);
			((Control)grpReIndex).set_Text("Re-index XZ80; Create subsets");
			((Control)label3).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label3).set_Location(new Point(4, 12));
			((Control)label3).set_Margin(new Padding(2, 0, 2, 0));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(183, 67));
			((Control)label3).set_TabIndex(8);
			((Control)label3).set_Text("These functions are used to update\r\nthe XZ file, and to create XZ subset\r\nfiles, whenever the files of double\r\nand variable stars are updated.\r\nThey are NOT for general use.");
			((Control)grpKepler2).get_Controls().Add((Control)(object)label5);
			((Control)grpKepler2).get_Controls().Add((Control)(object)cmdKepler2InAsteroidals);
			((Control)grpKepler2).get_Controls().Add((Control)(object)lblTag);
			((Control)grpKepler2).get_Controls().Add((Control)(object)lblFile);
			((Control)grpKepler2).get_Controls().Add((Control)(object)lblStatus);
			((Control)grpKepler2).get_Controls().Add((Control)(object)cmdHistoricLunarInKepler2);
			((Control)grpKepler2).get_Controls().Add((Control)(object)cmdCreateKepler2Cats);
			((Control)grpKepler2).get_Controls().Add((Control)(object)label4);
			((Control)grpKepler2).set_Enabled(false);
			((Control)grpKepler2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpKepler2).set_Location(new Point(426, 28));
			((Control)grpKepler2).set_Margin(new Padding(2));
			((Control)grpKepler2).set_Name("grpKepler2");
			((Control)grpKepler2).set_Padding(new Padding(2));
			((Control)grpKepler2).set_Size(new Size(206, 301));
			((Control)grpKepler2).set_TabIndex(10);
			grpKepler2.set_TabStop(false);
			((Control)grpKepler2).set_Text("Process Kepler2 stars");
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label5).set_Location(new Point(32, 265));
			((Control)label5).set_Margin(new Padding(2, 0, 2, 0));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(165, 26));
			((Control)label5).set_TabIndex(15);
			((Control)label5).set_Text("The stars must first be flagged, by\r\nsorting the Observations file");
			((Control)label5).set_Visible(false);
			((Control)cmdKepler2InAsteroidals).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdKepler2InAsteroidals).set_Location(new Point(32, 223));
			((Control)cmdKepler2InAsteroidals).set_Margin(new Padding(2));
			((Control)cmdKepler2InAsteroidals).set_Name("cmdKepler2InAsteroidals");
			((Control)cmdKepler2InAsteroidals).set_Size(new Size(143, 37));
			((Control)cmdKepler2InAsteroidals).set_TabIndex(14);
			((Control)cmdKepler2InAsteroidals).set_Text("Asteroidal occultations -\r\nList events of K2 stars");
			((ButtonBase)cmdKepler2InAsteroidals).set_UseVisualStyleBackColor(true);
			((Control)cmdKepler2InAsteroidals).add_Click((EventHandler)cmdKepler2InAsteroidals_Click);
			((Control)lblTag).set_AutoSize(true);
			((Control)lblTag).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblTag).set_Location(new Point(32, 188));
			((Control)lblTag).set_Margin(new Padding(2, 0, 2, 0));
			((Control)lblTag).set_Name("lblTag");
			((Control)lblTag).set_Size(new Size(110, 13));
			((Control)lblTag).set_TabIndex(13);
			((Control)lblTag).set_Text("Reading archive file...");
			((Control)lblTag).set_Visible(false);
			((Control)lblFile).set_AutoSize(true);
			((Control)lblFile).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblFile).set_Location(new Point(32, 200));
			((Control)lblFile).set_Margin(new Padding(2, 0, 2, 0));
			((Control)lblFile).set_Name("lblFile");
			((Control)lblFile).set_Size(new Size(23, 13));
			((Control)lblFile).set_TabIndex(12);
			((Control)lblFile).set_Text("File");
			((Control)lblFile).set_Visible(false);
			((Control)lblStatus).set_AutoSize(true);
			((Control)lblStatus).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblStatus).set_Location(new Point(32, 133));
			((Control)lblStatus).set_Margin(new Padding(2, 0, 2, 0));
			((Control)lblStatus).set_Name("lblStatus");
			((Control)lblStatus).set_Size(new Size(90, 13));
			((Control)lblStatus).set_TabIndex(11);
			((Control)lblStatus).set_Text("Processing status");
			((Control)lblStatus).set_Visible(false);
			((Control)cmdHistoricLunarInKepler2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdHistoricLunarInKepler2).set_Location(new Point(32, 151));
			((Control)cmdHistoricLunarInKepler2).set_Margin(new Padding(2));
			((Control)cmdHistoricLunarInKepler2).set_Name("cmdHistoricLunarInKepler2");
			((Control)cmdHistoricLunarInKepler2).set_Size(new Size(143, 37));
			((Control)cmdHistoricLunarInKepler2).set_TabIndex(10);
			((Control)cmdHistoricLunarInKepler2).set_Text("Create file of observations\r\n- K2 stars in Lunar Archive");
			((ButtonBase)cmdHistoricLunarInKepler2).set_UseVisualStyleBackColor(true);
			((Control)cmdHistoricLunarInKepler2).add_Click((EventHandler)cmdHistoricLunarInKepler2_Click);
			((Control)cmdCreateKepler2Cats).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdCreateKepler2Cats).set_Location(new Point(32, 93));
			((Control)cmdCreateKepler2Cats).set_Margin(new Padding(2));
			((Control)cmdCreateKepler2Cats).set_Name("cmdCreateKepler2Cats");
			((Control)cmdCreateKepler2Cats).set_Size(new Size(143, 37));
			((Control)cmdCreateKepler2Cats).set_TabIndex(9);
			((Control)cmdCreateKepler2Cats).set_Text("Create Kepler2\r\ncatalogues");
			((ButtonBase)cmdCreateKepler2Cats).set_UseVisualStyleBackColor(true);
			((Control)cmdCreateKepler2Cats).add_Click((EventHandler)cmdCreateKepler2Cats_Click);
			((Control)label4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label4).set_Location(new Point(2, 12));
			((Control)label4).set_Margin(new Padding(2, 0, 2, 0));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(192, 68));
			((Control)label4).set_TabIndex(8);
			((Control)label4).set_Text("These options create a file of XZ\r\nstars that are in the Kepler2 program,\r\nand a file of video observations in the\r\nLunar Archive of those stars.\r\nThey are NOT for general use.");
			((Control)cmdMatchToGaia_DR2).set_Location(new Point(54, 339));
			((Control)cmdMatchToGaia_DR2).set_Margin(new Padding(2));
			((Control)cmdMatchToGaia_DR2).set_Name("cmdMatchToGaia_DR2");
			((Control)cmdMatchToGaia_DR2).set_Size(new Size(143, 41));
			((Control)cmdMatchToGaia_DR2).set_TabIndex(11);
			((Control)cmdMatchToGaia_DR2).set_Text("Update positions to GAIA DR2");
			((ButtonBase)cmdMatchToGaia_DR2).set_UseVisualStyleBackColor(true);
			((Control)cmdMatchToGaia_DR2).add_Click((EventHandler)cmdMatchToGaia_DR2_Click);
			((Control)pBarGaia).set_Location(new Point(209, 353));
			((Control)pBarGaia).set_Margin(new Padding(2));
			pBarGaia.set_Maximum(187000);
			((Control)pBarGaia).set_Name("pBarGaia");
			((Control)pBarGaia).set_Size(new Size(164, 11));
			((Control)pBarGaia).set_TabIndex(12);
			((Control)pBarGaia).set_Visible(false);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(636, 387));
			((Control)this).get_Controls().Add((Control)(object)pBarGaia);
			((Control)this).get_Controls().Add((Control)(object)cmdMatchToGaia_DR2);
			((Control)this).get_Controls().Add((Control)(object)grpKepler2);
			((Control)this).get_Controls().Add((Control)(object)grpReIndex);
			((Control)this).get_Controls().Add((Control)(object)grpEdit);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationStarsXZManagement", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_Location(Settings.Default.LocationStarsXZManagement);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Form)this).set_Margin(new Padding(2));
			((Control)this).set_Name("XZ_File_Management");
			((Control)this).set_Text("XZ80Q star catalogue  -  file management");
			((Form)this).add_FormClosing(new FormClosingEventHandler(XZ_File_Management_FormClosing));
			((Form)this).add_Load((EventHandler)XZ_File_Management_Load);
			((Control)grpEdit).ResumeLayout(false);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)grpReIndex).ResumeLayout(false);
			((Control)grpReIndex).PerformLayout();
			((Control)grpKepler2).ResumeLayout(false);
			((Control)grpKepler2).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
