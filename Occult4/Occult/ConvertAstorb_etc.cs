using System;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Net;
using System.Windows.Forms;
using Occult.Asteroids;
using Occult.Properties;

namespace Occult
{
	public class ConvertAstorb_etc : Form
	{
		private readonly string AppPath;

		private readonly string AlwaysReadFile;

		private readonly string RingFile;

		private static string RingsIn;

		internal static int BinaryRecNum;

		private static int AlwaysIndex;

		private static int CurrentRingNumber;

		private static int CurrentNumOfRings;

		private const double Radian = 180.0 / Math.PI;

		internal StreamReader AsteroidRings;

		private IContainer components;

		private GroupBox groupBox1;

		private RadioButton optMPCorb;

		private RadioButton optAstorb;

		private Button cmdConvert;

		private NumericUpDown updnMinDia;

		private CheckBox chkNumberedOnly;

		private Label label6;

		private CheckBox chkOppNum;

		private GroupBox groupBox4;

		private Label label7;

		private GroupBox groupBox5;

		private Button cmdDeleteAlways;

		private Button cmdAddAlways;

		private TextBox txtAddAlways;

		private ListBox lstAlways;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ProgressBar pBar;

		private TextBox txtUnNumbered;

		private Label lblInclude;

		private ToolTip toolTip1;

		private RadioButton optAstDyS;

		private ToolStripMenuItem exitToolStripMenuItem;

		private Label label1;

		private Label label3;

		private Label label4;

		private Label label8;

		private Panel panel1;

		private RadioButton optGaia;

		private Panel panel4;

		private Label label2;

		private Panel panel3;

		private Panel panel2;

		private Label label5;

		private Button cmdDownloadMPCORB;

		private Label label10;

		private Label lblDownloadDate_MPCORB;

		private Label lblDownloadDate_AstDys2;

		private Label label11;

		private Button cmdDownloadASTDYS2;

		private Label lblDownloadDate_ASTORB;

		private Label label13;

		private Button cmdDownloadASTORB;

		public ConvertAstorb_etc(int FileNumber)
		{
			InitializeComponent();
			switch (FileNumber)
			{
			case 0:
				optAstorb.set_Checked(true);
				break;
			case 2:
				optAstDyS.set_Checked(true);
				chkNumberedOnly.set_Checked(true);
				break;
			default:
				optMPCorb.set_Checked(true);
				break;
			}
			AppPath = Utilities.AppPath;
			AlwaysReadFile = "\\Resource Files\\AsteroidsAlwaysRead.dat";
			RingFile = "\\Resource Files\\AsteroidRings.dat";
			if (!File.Exists(Utilities.AppPath + "/Resource Files/AsteroidDiameters.bin"))
			{
				http.DownloadHTTP(Settings.Default.OccultServer, "asteroiddiameters.zip", Utilities.AppPath + "\\Resource Files\\", unzip: true, gunzip: false, ShowMessages: false);
			}
			StreamReader streamReader = new StreamReader(AppPath + AlwaysReadFile);
			while (!streamReader.EndOfStream)
			{
				string text = streamReader.ReadLine();
				lstAlways.get_Items().Add((object)text);
			}
			streamReader.Close();
			if (lstAlways.get_Items().get_Count() > 0)
			{
				((ListControl)lstAlways).set_SelectedIndex(0);
			}
		}

		private void cmdConvert_Click(object sender, EventArgs e)
		{
			if (BinaryAsteroids.BinElements.Count < 1)
			{
				BinaryAsteroids.Fill_AllAsteroids();
			}
			AlwaysIndex = 0;
			pBar.set_Value(0);
			((Control)pBar).set_Visible(true);
			Utilities.OpenAsteroidDiameterFileForReading();
			if (optAstorb.get_Checked())
			{
				ConvertAstorb();
			}
			if (optMPCorb.get_Checked())
			{
				ConvertMPCorb();
			}
			if (optAstDyS.get_Checked())
			{
				ConvertAstDyS();
			}
			if (optGaia.get_Checked())
			{
				ConvertGaia();
			}
			Utilities.CloseAsteroidDiameterFileForReading();
			((Control)pBar).set_Visible(false);
			Application.set_UseWaitCursor(false);
			Elements.UpdateLunarUserFile();
			((Form)this).Close();
		}

		private void ConvertAstorb()
		{
			//IL_0021: Unknown result type (might be due to invalid IL or missing references)
			//IL_0028: Expected O, but got Unknown
			//IL_009a: Unknown result type (might be due to invalid IL or missing references)
			//IL_00a0: Invalid comparison between Unknown and I4
			//IL_041e: Unknown result type (might be due to invalid IL or missing references)
			if (AsteroidClassList.AClass.Count < 1)
			{
				AsteroidClassList.Fill_AllAsteroids();
			}
			int num = 0;
			int i = 0;
			int count = AsteroidClassList.AClass.Count;
			OpenFileDialog val = new OpenFileDialog();
			double num2 = (double)updnMinDia.get_Value();
			string text = ((Control)txtUnNumbered).get_Text().Trim();
			((FileDialog)val).set_CheckPathExists(true);
			((FileDialog)val).set_FileName("Astorb.dat");
			((FileDialog)val).set_Title("Convert Astorb");
			((FileDialog)val).set_InitialDirectory(AppPath + "\\Downloaded Files\\");
			((FileDialog)val).set_Filter("Astorb (Astorb.dat)|Astorb.dat|All files (*.*)|*.*");
			((FileDialog)val).set_FilterIndex(1);
			if ((int)((CommonDialog)val).ShowDialog() != 1)
			{
				return;
			}
			DateTime lastWriteTime = new FileInfo(((FileDialog)val).get_FileName()).LastWriteTime;
			string orbitDate = lastWriteTime.Year + " " + Utilities.ShortMonths[lastWriteTime.Month] + " " + lastWriteTime.Day.ToString().PadLeft(2, '0');
			BinaryRecNum = 0;
			using (StreamReader streamReader = new StreamReader(((FileDialog)val).get_FileName(), detectEncodingFromByteOrderMarks: true))
			{
				pBar.set_Maximum((int)(streamReader.BaseStream.Length / 268));
				using (StreamWriter streamWriter = new StreamWriter(AppPath + "\\Resource Files\\AsteroidElements.csv"))
				{
					Application.set_UseWaitCursor(true);
					Application.DoEvents();
					AsteroidElements asteroidElements = new AsteroidElements();
					streamWriter.WriteLine(AsteroidElements.CSVHeader());
					do
					{
						asteroidElements = new AsteroidElements();
						if (!asteroidElements.ReadAstorbLine(ASTORB_accented_Correction(streamReader.ReadLine())))
						{
							continue;
						}
						if ((asteroidElements.IDNumber == 0) & chkNumberedOnly.get_Checked())
						{
							break;
						}
						bool flag = (text == asteroidElements.IDName.Trim()) & (text.Length > 4);
						if ((asteroidElements.IDNumber == 0) | (asteroidElements.IDNumber >= 550000))
						{
							asteroidElements.Diameter_Mean = Math.Pow(10.0, 3.52 - 0.2 * asteroidElements.H0);
							asteroidElements.DiameterUncertainty = asteroidElements.Diameter_Mean / 10.0;
							asteroidElements.Dia_Source = "";
							asteroidElements.Num_Rings = 0;
							asteroidElements.Num_Moons = 0;
						}
						else
						{
							Utilities.GetAsteroidDiameter(asteroidElements.IDNumber, asteroidElements.H0, out var MeanDiameter, out var DiameterUncertainty, out var DiaSource, out var _);
							asteroidElements.Diameter_Mean = MeanDiameter;
							asteroidElements.DiameterUncertainty = DiameterUncertainty;
							asteroidElements.Dia_Source = DiaSource;
							asteroidElements.Num_Rings = AsteroidRings_All.GetNumberOfRings(asteroidElements.IDNumber);
							asteroidElements.Num_Moons = GetMoons(asteroidElements.IDNumber, asteroidElements.IDName.Trim());
						}
						asteroidElements.OrbitDate = orbitDate;
						if (i < count)
						{
							for (; AsteroidClassList.AClass[i].Number < asteroidElements.IDNumber && i < count - 1; i++)
							{
							}
							if (AsteroidClassList.AClass[i].Number == asteroidElements.IDNumber)
							{
								asteroidElements.AsteroidClass = AsteroidClassList.AClass[i].AstClass;
							}
						}
						num++;
						if ((num % 100 == 0) & (num < pBar.get_Maximum()))
						{
							pBar.set_Value(num);
							Application.DoEvents();
						}
						if (((((num2 <= asteroidElements.Diameter_Mean) & (asteroidElements.PeakEphemUncert < 100.0)) | MustWrite(asteroidElements.IDNumber)) || flag) && ((asteroidElements.IDNumber > 0) | !chkOppNum.get_Checked() | (asteroidElements.Arc >= 1000.0)) && asteroidElements.H0 != 0.0)
						{
							streamWriter.WriteLine(asteroidElements.CSVString());
						}
					}
					while (!streamReader.EndOfStream);
				}
				((Control)pBar).set_Visible(false);
			}
			MessageBox.Show("ASTORB conversion complete", "ASTORB conversion complete", (MessageBoxButtons)0, (MessageBoxIcon)64);
			Elements.MainAsteroids.AstElements.Clear();
			Elements.UserAsteroids_Lunar.AstElements.Clear();
			AsteroidClassList.AClass.Clear();
		}

		private string ASTORB_accented_Correction(string Astorb)
		{
			if (Astorb.Substring(44, 1) == ".")
			{
				return Astorb;
			}
			int num = Astorb.LastIndexOf(".", 44);
			if (num < 0)
			{
				return Astorb;
			}
			string value = "".PadRight(44 - num);
			int startIndex = num - 18;
			return Astorb.Insert(startIndex, value);
		}

		private string MPCORB_accented_Correction(string MPCorb)
		{
			int num = MPCorb.LastIndexOf(" ", 194);
			if (num >= 193)
			{
				return MPCorb;
			}
			string value = "".PadRight(193 - num);
			return MPCorb.Insert(num, value);
		}

		private void ConvertMPCorb()
		{
			//IL_002b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0032: Expected O, but got Unknown
			//IL_00d3: Unknown result type (might be due to invalid IL or missing references)
			//IL_00d9: Invalid comparison between Unknown and I4
			//IL_04d3: Unknown result type (might be due to invalid IL or missing references)
			//IL_04e6: Unknown result type (might be due to invalid IL or missing references)
			string text = "The following lines in MPCORB could not be read correctly; the asteroid has not been added.\r\n\r\n";
			int num = 0;
			if (AsteroidClassList.AClass.Count < 1)
			{
				AsteroidClassList.Fill_AllAsteroids();
			}
			int num2 = 0;
			int i = 0;
			int count = AsteroidClassList.AClass.Count;
			OpenFileDialog val = new OpenFileDialog();
			double num3 = (double)updnMinDia.get_Value();
			string text2 = ((Control)txtUnNumbered).get_Text().Trim();
			string path = Utilities.AppPath + "\\DownLoaded Files\\astorb.dat";
			StreamReader streamReader = null;
			bool flag = false;
			bool flag2 = true;
			if (File.Exists(path))
			{
				streamReader = new StreamReader(path);
				flag = true;
			}
			((FileDialog)val).set_CheckPathExists(true);
			((FileDialog)val).set_FileName("MPCorb.dat");
			((FileDialog)val).set_Title("Convert MPCorb");
			((FileDialog)val).set_InitialDirectory(AppPath + "\\Downloaded Files\\");
			((FileDialog)val).set_Filter("MPCorb (MPCorb.dat)|MPCorb.dat|All files (*.*)|*.*");
			((FileDialog)val).set_FilterIndex(1);
			if ((int)((CommonDialog)val).ShowDialog() != 1)
			{
				return;
			}
			DisplayData displayData = new DisplayData();
			Application.set_UseWaitCursor(true);
			Application.DoEvents();
			DateTime lastWriteTime = new FileInfo(((FileDialog)val).get_FileName()).LastWriteTime;
			string orbitDate = lastWriteTime.Year + " " + Utilities.ShortMonths[lastWriteTime.Month] + " " + lastWriteTime.Day.ToString().PadLeft(2, '0');
			BinaryRecNum = 0;
			using (StreamReader streamReader2 = new StreamReader(((FileDialog)val).get_FileName(), detectEncodingFromByteOrderMarks: true))
			{
				pBar.set_Maximum((int)((streamReader2.BaseStream.Length - 2099) / 204));
				using (StreamWriter streamWriter = new StreamWriter(AppPath + "\\Resource Files\\AsteroidElements.csv"))
				{
					AsteroidElements asteroidElements = new AsteroidElements();
					streamWriter.WriteLine(AsteroidElements.CSVHeader());
					string text3;
					do
					{
						text3 = streamReader2.ReadLine()!.PadRight(6);
						if (text3.Substring(0, 5) == "Des'n")
						{
							text = text + text3 + "\r\n\r\n";
						}
					}
					while (text3.Substring(0, 5) != "-----");
					do
					{
						asteroidElements = new AsteroidElements();
						text3 = streamReader2.ReadLine();
						if (text3.Length == 0)
						{
							text3 = streamReader2.ReadLine();
						}
						text3 = MPCORB_accented_Correction(text3);
						if (!asteroidElements.ReadMPCORBLine(text3))
						{
							text = text + text3 + "\r\n";
							num++;
							continue;
						}
						if ((asteroidElements.IDNumber == 0) & chkNumberedOnly.get_Checked())
						{
							break;
						}
						bool flag3 = (text2 == asteroidElements.IDName.Trim()) & (text2.Length > 4);
						if (flag)
						{
							try
							{
								if (asteroidElements.IDNumber > 0 && flag2)
								{
									string text4 = ASTORB_accented_Correction(streamReader.ReadLine());
									if (text4.Substring(0, 7).Trim() == "")
									{
										flag2 = false;
									}
									else
									{
										asteroidElements.PeakEphemUncert = double.Parse(text4.Substring(216, 8));
									}
								}
							}
							catch
							{
								asteroidElements.PeakEphemUncert = 0.1;
							}
						}
						Utilities.GetAsteroidDiameter(asteroidElements.IDNumber, asteroidElements.H0, out var MeanDiameter, out var DiameterUncertainty, out var DiaSource, out var _);
						asteroidElements.Diameter_Mean = MeanDiameter;
						asteroidElements.DiameterUncertainty = DiameterUncertainty;
						asteroidElements.Dia_Source = DiaSource;
						asteroidElements.Num_Rings = AsteroidRings_All.GetNumberOfRings(asteroidElements.IDNumber);
						asteroidElements.Num_Moons = GetMoons(asteroidElements.IDNumber, asteroidElements.IDName.Trim());
						asteroidElements.OrbitDate = orbitDate;
						if (i < count)
						{
							for (; AsteroidClassList.AClass[i].Number < asteroidElements.IDNumber && i < count - 1; i++)
							{
							}
							if (AsteroidClassList.AClass[i].Number == asteroidElements.IDNumber)
							{
								asteroidElements.AsteroidClass = AsteroidClassList.AClass[i].AstClass;
							}
						}
						num2++;
						if ((num2 % 100 == 0) & (num2 < pBar.get_Maximum()))
						{
							pBar.set_Value(num2);
							Application.DoEvents();
						}
						if (((num3 <= MeanDiameter) | MustWrite(asteroidElements.IDNumber)) && (((asteroidElements.IDNumber > 0) | !chkOppNum.get_Checked() | (asteroidElements.Num_Opp >= 3.0)) || flag3) && asteroidElements.H0 != 0.0)
						{
							streamWriter.WriteLine(asteroidElements.CSVString());
						}
					}
					while (!streamReader2.EndOfStream);
				}
				((Control)pBar).set_Visible(false);
				if (num > 0)
				{
					((Control)displayData).set_Text("Errors when reading MPCORB");
					((Control)displayData).set_Width(900);
					((Control)displayData.txtBox).set_Text(text);
					((TextBoxBase)displayData.txtBox).Select(0, 0);
					((Form)displayData).ShowDialog();
				}
				MessageBox.Show("MPCORB conversion complete", "MPCORB conversion complete", (MessageBoxButtons)0, (MessageBoxIcon)64);
				Elements.MainAsteroids.AstElements.Clear();
				Elements.UserAsteroids_Lunar.AstElements.Clear();
				AsteroidClassList.AClass.Clear();
			}
			((Form)displayData).Close();
		}

		private void ConvertAstDyS()
		{
			//IL_0024: Unknown result type (might be due to invalid IL or missing references)
			//IL_002b: Expected O, but got Unknown
			//IL_014c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0152: Invalid comparison between Unknown and I4
			//IL_0508: Unknown result type (might be due to invalid IL or missing references)
			if (AsteroidClassList.AClass.Count < 1)
			{
				AsteroidClassList.Fill_AllAsteroids();
			}
			int num = 0;
			int i = 0;
			int count = AsteroidClassList.AClass.Count;
			int num2 = 0;
			OpenFileDialog val = new OpenFileDialog();
			double num3 = (double)updnMinDia.get_Value();
			((Control)txtUnNumbered).get_Text().Trim();
			bool flag = true;
			StreamReader streamReader = null;
			string text = Utilities.AppPath + "\\DownLoaded Files\\astorb.dat";
			string text2 = Utilities.AppPath + "\\DownLoaded Files\\MPCORB.DAT";
			_ = Utilities.AppPath + "\\DownLoaded Files\\AstDys2.dat";
			string text3 = "";
			int num4 = 0;
			int num5 = -1;
			if (File.Exists(text))
			{
				text3 = text;
				num4 = 7;
				num5 = 216;
			}
			else if (File.Exists(text2))
			{
				text3 = text2;
				num4 = 175;
				num5 = -1;
			}
			if (num4 > 0)
			{
				streamReader = new StreamReader(text3);
				if (num4 == 175)
				{
					string text4;
					do
					{
						text4 = streamReader.ReadLine()!.PadRight(6);
					}
					while (text4.Substring(0, 5) != "-----");
				}
			}
			((FileDialog)val).set_CheckPathExists(true);
			((FileDialog)val).set_FileName("AstDyS2.dat");
			((FileDialog)val).set_Title("Convert AstDyS-2");
			((FileDialog)val).set_InitialDirectory(AppPath + "\\Downloaded Files\\");
			((FileDialog)val).set_Filter("AstDyS-2 (AstDyS2.dat)|AstDyS2.dat|All files (*.*)|*.*");
			((FileDialog)val).set_FilterIndex(1);
			if ((int)((CommonDialog)val).ShowDialog() != 1)
			{
				return;
			}
			Application.set_UseWaitCursor(true);
			Application.DoEvents();
			DateTime lastWriteTime = new FileInfo(((FileDialog)val).get_FileName()).LastWriteTime;
			string orbitDate = lastWriteTime.Year + " " + Utilities.ShortMonths[lastWriteTime.Month] + " " + lastWriteTime.Day.ToString().PadLeft(2, '0');
			BinaryRecNum = 0;
			using (StreamReader streamReader2 = new StreamReader(((FileDialog)val).get_FileName(), detectEncodingFromByteOrderMarks: true))
			{
				pBar.set_Maximum((int)(streamReader2.BaseStream.Length / 194 + 10));
				using StreamWriter streamWriter = new StreamWriter(AppPath + "\\Resource Files\\AsteroidElements.csv");
				AsteroidElements asteroidElements = new AsteroidElements();
				streamWriter.WriteLine(AsteroidElements.CSVHeader());
				for (int j = 0; j < 6; j++)
				{
					string text5 = streamReader2.ReadLine();
				}
				do
				{
					asteroidElements = new AsteroidElements();
					string text5 = streamReader2.ReadLine();
					asteroidElements.ReadAstDySLines(text5);
					if (asteroidElements.IDNumber == num2)
					{
						continue;
					}
					num2 = asteroidElements.IDNumber;
					double.Parse(text5.Substring(12, 15));
					if (num4 > 0)
					{
						string text4 = streamReader.ReadLine();
						if (text3 == text)
						{
							text4 = ASTORB_accented_Correction(text4);
						}
						else if (text3 == text2)
						{
							text4 = MPCORB_accented_Correction(text4);
						}
						if (((num4 == 7) & (text4.Substring(0, 7).Trim() != "")) | ((num4 == 175) & (text4.Substring(166, 8).Trim() != "")))
						{
							asteroidElements.IDName = text4.Substring(num4, 17).Trim();
						}
						else
						{
							asteroidElements.IDName = "";
							flag = false;
						}
						try
						{
							if (num5 > 0 && flag)
							{
								asteroidElements.PeakEphemUncert = double.Parse(text4.Substring(num5, 8));
							}
						}
						catch
						{
						}
					}
					Utilities.GetAsteroidDiameter(asteroidElements.IDNumber, asteroidElements.H0, out var MeanDiameter, out var DiameterUncertainty, out var DiaSource, out var _);
					asteroidElements.Diameter_Mean = MeanDiameter;
					asteroidElements.DiameterUncertainty = DiameterUncertainty;
					asteroidElements.Dia_Source = DiaSource;
					asteroidElements.Num_Rings = AsteroidRings_All.GetNumberOfRings(asteroidElements.IDNumber);
					asteroidElements.Num_Moons = GetMoons(asteroidElements.IDNumber, asteroidElements.IDName.Trim());
					asteroidElements.OrbitDate = orbitDate;
					if (i < count)
					{
						for (; AsteroidClassList.AClass[i].Number < asteroidElements.IDNumber && i < count - 1; i++)
						{
						}
						if (AsteroidClassList.AClass[i].Number == asteroidElements.IDNumber)
						{
							asteroidElements.AsteroidClass = AsteroidClassList.AClass[i].AstClass;
						}
					}
					num++;
					if ((num % 100 == 0) & (num < pBar.get_Maximum()))
					{
						pBar.set_Value(num);
						Application.DoEvents();
					}
					if (((num3 <= MeanDiameter) | MustWrite(asteroidElements.IDNumber)) && ((asteroidElements.IDNumber > 0) | !chkOppNum.get_Checked() | (asteroidElements.Num_Opp >= 3.0)) && asteroidElements.H0 != 0.0)
					{
						streamWriter.WriteLine(asteroidElements.CSVString());
					}
				}
				while (!streamReader2.EndOfStream);
			}
			((Control)pBar).set_Visible(false);
			streamReader.Close();
			MessageBox.Show("AstDyS conversion complete", "AstDyS conversion complete", (MessageBoxButtons)0, (MessageBoxIcon)64);
			Elements.MainAsteroids.AstElements.Clear();
			Elements.UserAsteroids_Lunar.AstElements.Clear();
			AsteroidClassList.AClass.Clear();
		}

		private void ConvertGaia()
		{
			//IL_0021: Unknown result type (might be due to invalid IL or missing references)
			//IL_0028: Expected O, but got Unknown
			//IL_009a: Unknown result type (might be due to invalid IL or missing references)
			//IL_00a0: Invalid comparison between Unknown and I4
			//IL_0397: Unknown result type (might be due to invalid IL or missing references)
			if (AsteroidClassList.AClass.Count < 1)
			{
				AsteroidClassList.Fill_AllAsteroids();
			}
			int num = 0;
			int i = 0;
			int count = AsteroidClassList.AClass.Count;
			OpenFileDialog val = new OpenFileDialog();
			double num2 = (double)updnMinDia.get_Value();
			string text = ((Control)txtUnNumbered).get_Text().Trim();
			((FileDialog)val).set_CheckPathExists(true);
			((FileDialog)val).set_FileName("aux_sso_orbits.csv");
			((FileDialog)val).set_Title("Convert Gaia");
			((FileDialog)val).set_InitialDirectory(AppPath + "\\Downloaded Files\\");
			((FileDialog)val).set_Filter("Gaia (sso_orbits)|aux_sso_orbits.csv|All files (*.*)|*.*");
			((FileDialog)val).set_FilterIndex(1);
			if ((int)((CommonDialog)val).ShowDialog() != 1)
			{
				return;
			}
			DateTime lastWriteTime = new FileInfo(((FileDialog)val).get_FileName()).LastWriteTime;
			string orbitDate = lastWriteTime.Year + " " + Utilities.ShortMonths[lastWriteTime.Month] + " " + lastWriteTime.Day.ToString().PadLeft(2, '0');
			BinaryRecNum = 0;
			using (StreamReader streamReader = new StreamReader(((FileDialog)val).get_FileName(), detectEncodingFromByteOrderMarks: true))
			{
				pBar.set_Maximum((int)(streamReader.BaseStream.Length / 268));
				streamReader.ReadLine();
				using (StreamWriter streamWriter = new StreamWriter(AppPath + "\\Resource Files\\AsteroidElements.csv"))
				{
					Application.set_UseWaitCursor(true);
					Application.DoEvents();
					AsteroidElements asteroidElements = new AsteroidElements();
					streamWriter.WriteLine(AsteroidElements.CSVHeader());
					do
					{
						asteroidElements = new AsteroidElements();
						if (!asteroidElements.ReadGaiaLine(streamReader.ReadLine()))
						{
							continue;
						}
						if ((asteroidElements.IDNumber == 0) & chkNumberedOnly.get_Checked())
						{
							break;
						}
						bool flag = (text == asteroidElements.IDName.Trim()) & (text.Length > 4);
						Utilities.GetAsteroidDiameter(asteroidElements.IDNumber, asteroidElements.H0, out var MeanDiameter, out var DiameterUncertainty, out var DiaSource, out var _);
						asteroidElements.Diameter_Mean = MeanDiameter;
						asteroidElements.DiameterUncertainty = DiameterUncertainty;
						asteroidElements.Dia_Source = DiaSource;
						asteroidElements.Num_Rings = AsteroidRings_All.GetNumberOfRings(asteroidElements.IDNumber);
						asteroidElements.Num_Moons = GetMoons(asteroidElements.IDNumber, asteroidElements.IDName);
						asteroidElements.OrbitDate = orbitDate;
						if (i < count)
						{
							for (; AsteroidClassList.AClass[i].Number < asteroidElements.IDNumber && i < count - 1; i++)
							{
							}
							if (AsteroidClassList.AClass[i].Number == asteroidElements.IDNumber)
							{
								asteroidElements.AsteroidClass = AsteroidClassList.AClass[i].AstClass;
							}
						}
						num++;
						if ((num % 100 == 0) & (num < pBar.get_Maximum()))
						{
							pBar.set_Value(num);
							Application.DoEvents();
						}
						if (((((num2 <= asteroidElements.Diameter_Mean) & (asteroidElements.PeakEphemUncert < 100.0)) | MustWrite(asteroidElements.IDNumber)) || flag) && ((asteroidElements.IDNumber > 0) | !chkOppNum.get_Checked() | (asteroidElements.Arc >= 1000.0)) && asteroidElements.H0 != 0.0)
						{
							streamWriter.WriteLine(asteroidElements.CSVString());
						}
					}
					while (!streamReader.EndOfStream);
				}
				((Control)pBar).set_Visible(false);
			}
			MessageBox.Show("Gaia conversion complete", "Gaia conversion complete", (MessageBoxButtons)0, (MessageBoxIcon)64);
			Elements.MainAsteroids.AstElements.Clear();
			Elements.UserAsteroids_Lunar.AstElements.Clear();
			AsteroidClassList.AClass.Clear();
		}

		internal int GetMoons(int AsteroidNumber, string AsteroidID)
		{
			int num = 0;
			if (BinaryRecNum >= BinaryAsteroids.BinElements.Count)
			{
				return num;
			}
			if (AsteroidNumber < 1)
			{
				string text = AsteroidID.Substring(0, 6) + AsteroidID.Substring(7).Trim().PadLeft(3, '0') + AsteroidID.Substring(6, 1);
				while (BinaryAsteroids.BinElements[BinaryRecNum].IDAsteroidNumber > 0.0)
				{
					BinaryRecNum++;
				}
				while (text.CompareTo(BinaryAsteroids.BinElements[BinaryRecNum].IDAsteroidIdentifier_SearchOrdered()) > 0)
				{
					BinaryRecNum++;
					if (BinaryRecNum >= BinaryAsteroids.BinElements.Count)
					{
						return num;
					}
				}
				while (text == BinaryAsteroids.BinElements[BinaryRecNum].IDAsteroidIdentifier_SearchOrdered())
				{
					num++;
					BinaryRecNum++;
					if (BinaryRecNum >= BinaryAsteroids.BinElements.Count)
					{
						return num;
					}
				}
			}
			else
			{
				while (((double)AsteroidNumber > BinaryAsteroids.BinElements[BinaryRecNum].IDAsteroidNumber) & (BinaryAsteroids.BinElements[BinaryRecNum].IDAsteroidNumber > 0.0))
				{
					BinaryRecNum++;
				}
				while (((double)AsteroidNumber == BinaryAsteroids.BinElements[BinaryRecNum].IDAsteroidNumber) & (BinaryAsteroids.BinElements[BinaryRecNum].IDAsteroidNumber > 0.0))
				{
					num++;
					BinaryRecNum++;
				}
			}
			if (num > 10)
			{
				num = 9;
			}
			return num;
		}

		internal int GetRings(int AsteroidNumber)
		{
			if (AsteroidNumber < 1)
			{
				return 0;
			}
			while ((AsteroidNumber > CurrentRingNumber) & !AsteroidRings.EndOfStream)
			{
				RingsIn = AsteroidRings.ReadLine();
				if (RingsIn.Length < 25)
				{
					break;
				}
				CurrentRingNumber = int.Parse(RingsIn.Substring(0, 7));
				CurrentNumOfRings = int.Parse(RingsIn.Substring(23, 2));
			}
			if (AsteroidNumber == CurrentRingNumber)
			{
				return CurrentNumOfRings;
			}
			return 0;
		}

		private bool MustWrite(double AsteroidNumber)
		{
			if (lstAlways.get_Items().get_Count() < 1)
			{
				return false;
			}
			while ((AsteroidNumber > double.Parse(lstAlways.get_Items().get_Item(AlwaysIndex).ToString())) & (AlwaysIndex < lstAlways.get_Items().get_Count() - 1))
			{
				AlwaysIndex++;
			}
			if (AsteroidNumber == double.Parse(lstAlways.get_Items().get_Item(AlwaysIndex).ToString()))
			{
				return true;
			}
			return false;
		}

		private void cmdAddAlways_Click(object sender, EventArgs e)
		{
			//IL_0021: Unknown result type (might be due to invalid IL or missing references)
			if (!int.TryParse(((Control)txtAddAlways).get_Text(), out var _))
			{
				MessageBox.Show("Only numbered asteroids can be added to this list.\r\n\r\nTo add an un-numbered asteroid, use the setting in '3. Set selection criteria'  ", "Data input error", (MessageBoxButtons)0, (MessageBoxIcon)16);
				return;
			}
			lstAlways.get_Items().Add((object)((Control)txtAddAlways).get_Text().Trim().PadLeft(7, ' '));
			SaveAlways();
		}

		private void cmdDeleteAlways_Click(object sender, EventArgs e)
		{
			if (((ListControl)lstAlways).get_SelectedIndex() >= 0)
			{
				lstAlways.get_Items().RemoveAt(((ListControl)lstAlways).get_SelectedIndex());
				SaveAlways();
			}
		}

		private void lstAlways_SelectedIndexChanged(object sender, EventArgs e)
		{
			if (((ListControl)lstAlways).get_SelectedIndex() >= 0)
			{
				((Control)txtAddAlways).set_Text(lstAlways.get_Items().get_Item(((ListControl)lstAlways).get_SelectedIndex()).ToString());
			}
		}

		private void SaveAlways()
		{
			StreamWriter streamWriter = new StreamWriter(AppPath + AlwaysReadFile, append: false);
			for (int i = 0; i < lstAlways.get_Items().get_Count(); i++)
			{
				streamWriter.WriteLine(lstAlways.get_Items().get_Item(i).ToString());
			}
			streamWriter.Close();
		}

		private void chkNumberedOnly_CheckedChanged(object sender, EventArgs e)
		{
			CheckBox obj = chkOppNum;
			Label obj2 = lblInclude;
			bool flag;
			((Control)txtUnNumbered).set_Enabled(flag = !chkNumberedOnly.get_Checked());
			bool enabled;
			((Control)obj2).set_Enabled(enabled = flag);
			((Control)obj).set_Enabled(enabled);
		}

		private void optAstorb_Click(object sender, EventArgs e)
		{
			optAstorb.set_Checked(true);
			RadioButton obj = optAstDyS;
			bool @checked;
			optMPCorb.set_Checked(@checked = false);
			obj.set_Checked(@checked);
			((Control)chkOppNum).set_Enabled(!chkNumberedOnly.get_Checked());
		}

		private void optAstDyS_Click(object sender, EventArgs e)
		{
			optAstDyS.set_Checked(true);
			RadioButton obj = optMPCorb;
			bool @checked;
			optAstorb.set_Checked(@checked = false);
			obj.set_Checked(@checked);
		}

		private void optMPCorb_Click(object sender, EventArgs e)
		{
			optMPCorb.set_Checked(true);
			RadioButton obj = optAstDyS;
			bool @checked;
			optAstorb.set_Checked(@checked = false);
			obj.set_Checked(@checked);
			((Control)chkOppNum).set_Enabled(!chkNumberedOnly.get_Checked());
		}

		private void updnMinDia_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnMinDia).Select(0, 10);
		}

		private void chkOppNum_CheckedChanged(object sender, EventArgs e)
		{
			ThreeOpposnMessage();
		}

		private void ThreeOpposnMessage()
		{
			//IL_002c: Unknown result type (might be due to invalid IL or missing references)
			if (!chkNumberedOnly.get_Checked() & !chkOppNum.get_Checked())
			{
				MessageBox.Show("Orbits of asteroids observed at less than 3 oppositions or a 1000 day arc are very unreliable. Occultation predictions of such asteroids should be treated as being meaningless.", "Single-opposition asteroids", (MessageBoxButtons)0, (MessageBoxIcon)64);
			}
		}

		private void cmdDownloadMPCORB_Click(object sender, EventArgs e)
		{
			string mPCOrb_Server = Settings.Default.MPCOrb_Server;
			string mPCOrb_file = Settings.Default.MPCOrb_file;
			string finalDestination = Utilities.AppPath + "\\Downloaded Files\\";
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			http.DownloadHTTP(mPCOrb_Server, mPCOrb_file, finalDestination, unzip: false, gunzip: true, ShowMessages: false);
			((Control)this).set_Cursor(Cursors.get_Default());
			DisplayLastDownloadDate_MPCORB();
		}

		private void cmdDownloadASTDYS2_Click(object sender, EventArgs e)
		{
			//IL_008a: Unknown result type (might be due to invalid IL or missing references)
			string astDys2_Server = Settings.Default.AstDys2_Server;
			string astDys2_AllNumFile = Settings.Default.AstDys2_AllNumFile;
			string finalDestination = Utilities.AppPath + "\\DownLoaded Files\\AstDyS2.dat";
			try
			{
				HttpWebRequest obj = (HttpWebRequest)WebRequest.Create(astDys2_Server);
				obj.Method = "HEAD";
				_ = (HttpWebResponse)obj.GetResponse();
				((Control)this).set_Cursor(Cursors.get_WaitCursor());
				http.DownloadHTTP(astDys2_Server, astDys2_AllNumFile, finalDestination, unzip: false, gunzip: false, ShowMessages: true);
				((Control)this).set_Cursor(Cursors.get_Default());
			}
			catch (Exception ex)
			{
				MessageBox.Show("AstDys-2 server\r\n   " + astDys2_Server + "\r\n is not available\r\n\r\n" + ex.Message, "Failed download", (MessageBoxButtons)0, (MessageBoxIcon)16);
			}
			DisplayLastDownloadDate_AstDys2();
		}

		private void cmdDownloadASTORB_Click(object sender, EventArgs e)
		{
			string aSTORB_Server = Settings.Default.ASTORB_Server;
			string aSTORB_file = Settings.Default.ASTORB_file;
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			http.DownloadHTTP(aSTORB_Server, aSTORB_file, Utilities.AppPath + "\\DownLoaded Files\\", unzip: false, gunzip: true, ShowMessages: false);
			((Control)this).set_Cursor(Cursors.get_Default());
			DisplayLastDownloadDate_ASTORB();
		}

		private void DisplayLastDownloadDate_MPCORB()
		{
			string path = Utilities.AppPath + "\\DownLoaded Files\\MPCORB.DAT";
			string text;
			if (File.Exists(path))
			{
				DateTime lastWriteTime = File.GetLastWriteTime(path);
				text = lastWriteTime.Day.ToString().PadLeft(2) + " " + Utilities.ShortMonths[lastWriteTime.Month] + " " + lastWriteTime.Year;
			}
			else
			{
				text = "---- --- --";
			}
			((Control)lblDownloadDate_MPCORB).set_Text(text);
		}

		private void DisplayLastDownloadDate_ASTORB()
		{
			string path = Utilities.AppPath + "\\DownLoaded Files\\astorb.DAT";
			string text;
			if (File.Exists(path))
			{
				DateTime lastWriteTime = File.GetLastWriteTime(path);
				text = lastWriteTime.Day.ToString().PadLeft(2) + " " + Utilities.ShortMonths[lastWriteTime.Month] + " " + lastWriteTime.Year;
			}
			else
			{
				text = "---- --- --";
			}
			((Control)lblDownloadDate_ASTORB).set_Text(text);
		}

		private void DisplayLastDownloadDate_AstDys2()
		{
			string path = Utilities.AppPath + "\\DownLoaded Files\\AstDyS2.dat";
			string text;
			if (File.Exists(path))
			{
				DateTime lastWriteTime = File.GetLastWriteTime(path);
				text = lastWriteTime.Day.ToString().PadLeft(2) + " " + Utilities.ShortMonths[lastWriteTime.Month] + " " + lastWriteTime.Year;
			}
			else
			{
				text = "---- --- --";
			}
			((Control)lblDownloadDate_AstDys2).set_Text(text);
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Convert Astorb");
		}

		private void ConvertAstorb_etc_Load(object sender, EventArgs e)
		{
			DisplayLastDownloadDate_MPCORB();
			DisplayLastDownloadDate_ASTORB();
			DisplayLastDownloadDate_AstDys2();
			if (((Control)this).get_Top() < -30000)
			{
				((Control)this).set_Top(0);
			}
			if ((double)((Control)this).get_Left() + (double)((Control)this).get_Width() / 1.2 < (double)Screen.GetWorkingArea((Control)(object)this).Left)
			{
				((Control)this).set_Left(Screen.GetWorkingArea((Control)(object)this).Left);
			}
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			if (AsteroidClassList.AClass.Count < 1)
			{
				AsteroidClassList.Fill_AllAsteroids();
			}
			ThreeOpposnMessage();
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Component)this).Dispose();
		}

		protected override void Dispose(bool disposing)
		{
			if (AsteroidRings != null)
			{
				AsteroidRings.Dispose();
			}
			if (disposing && components != null)
			{
				components.Dispose();
			}
			((Form)this).Dispose(disposing);
		}

		private void InitializeComponent()
		{
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
			//IL_01cf: Unknown result type (might be due to invalid IL or missing references)
			//IL_01d9: Expected O, but got Unknown
			//IL_01da: Unknown result type (might be due to invalid IL or missing references)
			//IL_01e4: Expected O, but got Unknown
			//IL_01e5: Unknown result type (might be due to invalid IL or missing references)
			//IL_01ef: Expected O, but got Unknown
			//IL_01f0: Unknown result type (might be due to invalid IL or missing references)
			//IL_01fa: Expected O, but got Unknown
			//IL_02bf: Unknown result type (might be due to invalid IL or missing references)
			//IL_02e0: Unknown result type (might be due to invalid IL or missing references)
			//IL_0416: Unknown result type (might be due to invalid IL or missing references)
			//IL_04bb: Unknown result type (might be due to invalid IL or missing references)
			//IL_05fa: Unknown result type (might be due to invalid IL or missing references)
			//IL_069f: Unknown result type (might be due to invalid IL or missing references)
			//IL_07fa: Unknown result type (might be due to invalid IL or missing references)
			//IL_08ab: Unknown result type (might be due to invalid IL or missing references)
			//IL_096e: Unknown result type (might be due to invalid IL or missing references)
			//IL_09fa: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a83: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a8d: Expected O, but got Unknown
			//IL_0aab: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b3b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c53: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c74: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d17: Unknown result type (might be due to invalid IL or missing references)
			//IL_0da8: Unknown result type (might be due to invalid IL or missing references)
			//IL_0e11: Unknown result type (might be due to invalid IL or missing references)
			//IL_0e1b: Expected O, but got Unknown
			//IL_0e45: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ef4: Unknown result type (might be due to invalid IL or missing references)
			//IL_0fc7: Unknown result type (might be due to invalid IL or missing references)
			//IL_0fd1: Expected O, but got Unknown
			//IL_0fec: Unknown result type (might be due to invalid IL or missing references)
			//IL_1096: Unknown result type (might be due to invalid IL or missing references)
			//IL_1156: Unknown result type (might be due to invalid IL or missing references)
			//IL_1177: Unknown result type (might be due to invalid IL or missing references)
			//IL_11de: Unknown result type (might be due to invalid IL or missing references)
			//IL_1266: Unknown result type (might be due to invalid IL or missing references)
			//IL_12ee: Unknown result type (might be due to invalid IL or missing references)
			//IL_1379: Unknown result type (might be due to invalid IL or missing references)
			//IL_1441: Unknown result type (might be due to invalid IL or missing references)
			//IL_1555: Unknown result type (might be due to invalid IL or missing references)
			//IL_1637: Unknown result type (might be due to invalid IL or missing references)
			//IL_16cf: Unknown result type (might be due to invalid IL or missing references)
			//IL_1793: Unknown result type (might be due to invalid IL or missing references)
			//IL_1d81: Unknown result type (might be due to invalid IL or missing references)
			//IL_1d8b: Expected O, but got Unknown
			//IL_1dde: Unknown result type (might be due to invalid IL or missing references)
			components = new Container();
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(ConvertAstorb_etc));
			groupBox1 = new GroupBox();
			panel4 = new Panel();
			label2 = new Label();
			optAstorb = new RadioButton();
			panel3 = new Panel();
			label8 = new Label();
			optAstDyS = new RadioButton();
			panel2 = new Panel();
			label5 = new Label();
			optMPCorb = new RadioButton();
			optGaia = new RadioButton();
			cmdConvert = new Button();
			updnMinDia = new NumericUpDown();
			label6 = new Label();
			groupBox4 = new GroupBox();
			label1 = new Label();
			lblInclude = new Label();
			txtUnNumbered = new TextBox();
			chkOppNum = new CheckBox();
			chkNumberedOnly = new CheckBox();
			label7 = new Label();
			groupBox5 = new GroupBox();
			cmdDeleteAlways = new Button();
			cmdAddAlways = new Button();
			txtAddAlways = new TextBox();
			lstAlways = new ListBox();
			menuStrip1 = new MenuStrip();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			pBar = new ProgressBar();
			toolTip1 = new ToolTip(components);
			label3 = new Label();
			label4 = new Label();
			panel1 = new Panel();
			cmdDownloadMPCORB = new Button();
			label10 = new Label();
			lblDownloadDate_MPCORB = new Label();
			lblDownloadDate_AstDys2 = new Label();
			label11 = new Label();
			cmdDownloadASTDYS2 = new Button();
			lblDownloadDate_ASTORB = new Label();
			label13 = new Label();
			cmdDownloadASTORB = new Button();
			((Control)groupBox1).SuspendLayout();
			((Control)panel4).SuspendLayout();
			((Control)panel3).SuspendLayout();
			((Control)panel2).SuspendLayout();
			((ISupportInitialize)updnMinDia).BeginInit();
			((Control)groupBox4).SuspendLayout();
			((Control)groupBox5).SuspendLayout();
			((Control)menuStrip1).SuspendLayout();
			((Control)panel1).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)groupBox1).get_Controls().Add((Control)(object)panel4);
			((Control)groupBox1).get_Controls().Add((Control)(object)panel3);
			((Control)groupBox1).get_Controls().Add((Control)(object)panel2);
			((Control)groupBox1).set_Location(new Point(7, 82));
			((Control)groupBox1).set_Margin(new Padding(2));
			((Control)groupBox1).set_Name("groupBox1");
			((Control)groupBox1).set_Padding(new Padding(2));
			((Control)groupBox1).set_Size(new Size(107, 163));
			((Control)groupBox1).set_TabIndex(0);
			groupBox1.set_TabStop(false);
			((Control)groupBox1).set_Text("1. Select file");
			toolTip1.SetToolTip((Control)(object)groupBox1, componentResourceManager.GetString("groupBox1.ToolTip"));
			((Control)panel4).set_BackColor(Color.PaleTurquoise);
			panel4.set_BorderStyle((BorderStyle)2);
			((Control)panel4).get_Controls().Add((Control)(object)label2);
			((Control)panel4).get_Controls().Add((Control)(object)optAstorb);
			((Control)panel4).set_Location(new Point(4, 115));
			((Control)panel4).set_Name("panel4");
			((Control)panel4).set_Size(new Size(100, 43));
			((Control)panel4).set_TabIndex(6);
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label2).set_Location(new Point(3, 20));
			((Control)label2).set_Margin(new Padding(2, 0, 2, 0));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(91, 13));
			((Control)label2).set_TabIndex(4);
			((Control)label2).set_Text("see  NOTE  below");
			label2.set_TextAlign(ContentAlignment.MiddleCenter);
			optAstorb.set_AutoCheck(false);
			((Control)optAstorb).set_AutoSize(true);
			((Control)optAstorb).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optAstorb).set_Location(new Point(3, 2));
			((Control)optAstorb).set_Margin(new Padding(2));
			((Control)optAstorb).set_Name("optAstorb");
			((Control)optAstorb).set_Size(new Size(63, 17));
			((Control)optAstorb).set_TabIndex(2);
			((Control)optAstorb).set_Text("AstOrb");
			((ButtonBase)optAstorb).set_UseVisualStyleBackColor(true);
			((Control)optAstorb).add_Click((EventHandler)optAstorb_Click);
			((Control)panel3).set_BackColor(Color.PaleTurquoise);
			panel3.set_BorderStyle((BorderStyle)2);
			((Control)panel3).get_Controls().Add((Control)(object)label8);
			((Control)panel3).get_Controls().Add((Control)(object)optAstDyS);
			((Control)panel3).set_Location(new Point(4, 63));
			((Control)panel3).set_Name("panel3");
			((Control)panel3).set_Size(new Size(100, 49));
			((Control)panel3).set_TabIndex(5);
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label8).set_Location(new Point(13, 19));
			((Control)label8).set_Margin(new Padding(2, 0, 2, 0));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(71, 26));
			((Control)label8).set_TabIndex(3);
			((Control)label8).set_Text("Numbered\r\nasteroids only");
			label8.set_TextAlign(ContentAlignment.MiddleCenter);
			optAstDyS.set_AutoCheck(false);
			((Control)optAstDyS).set_AutoSize(true);
			((Control)optAstDyS).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optAstDyS).set_Location(new Point(3, 2));
			((Control)optAstDyS).set_Margin(new Padding(2));
			((Control)optAstDyS).set_Name("optAstDyS");
			((Control)optAstDyS).set_Size(new Size(77, 17));
			((Control)optAstDyS).set_TabIndex(1);
			optAstDyS.set_TabStop(true);
			((Control)optAstDyS).set_Text("AstDyS-2");
			((ButtonBase)optAstDyS).set_UseVisualStyleBackColor(true);
			((Control)optAstDyS).add_Click((EventHandler)optAstDyS_Click);
			((Control)panel2).set_BackColor(Color.LightYellow);
			panel2.set_BorderStyle((BorderStyle)2);
			((Control)panel2).get_Controls().Add((Control)(object)label5);
			((Control)panel2).get_Controls().Add((Control)(object)optMPCorb);
			((Control)panel2).set_Location(new Point(4, 16));
			((Control)panel2).set_Name("panel2");
			((Control)panel2).set_Size(new Size(100, 44));
			((Control)panel2).set_TabIndex(4);
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label5).set_ForeColor(Color.Green);
			((Control)label5).set_Location(new Point(-2, 23));
			((Control)label5).set_Margin(new Padding(2, 0, 2, 0));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(101, 13));
			((Control)label5).set_TabIndex(4);
			((Control)label5).set_Text("Preferred source");
			label5.set_TextAlign(ContentAlignment.MiddleCenter);
			optMPCorb.set_AutoCheck(false);
			((Control)optMPCorb).set_AutoSize(true);
			optMPCorb.set_Checked(true);
			((Control)optMPCorb).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optMPCorb).set_Location(new Point(3, 4));
			((Control)optMPCorb).set_Margin(new Padding(2));
			((Control)optMPCorb).set_Name("optMPCorb");
			((Control)optMPCorb).set_Size(new Size(77, 17));
			((Control)optMPCorb).set_TabIndex(0);
			optMPCorb.set_TabStop(true);
			((Control)optMPCorb).set_Text("MPCORB");
			((ButtonBase)optMPCorb).set_UseVisualStyleBackColor(true);
			((Control)optMPCorb).add_Click((EventHandler)optMPCorb_Click);
			((Control)optGaia).set_AutoSize(true);
			((Control)optGaia).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optGaia).set_Location(new Point(579, 266));
			((Control)optGaia).set_Margin(new Padding(2));
			((Control)optGaia).set_Name("optGaia");
			((Control)optGaia).set_Size(new Size(51, 17));
			((Control)optGaia).set_TabIndex(4);
			optGaia.set_TabStop(true);
			((Control)optGaia).set_Text("Gaia");
			((ButtonBase)optGaia).set_UseVisualStyleBackColor(true);
			((Control)optGaia).set_Visible(false);
			((Control)cmdConvert).set_Location(new Point(572, 110));
			((Control)cmdConvert).set_Margin(new Padding(2));
			((Control)cmdConvert).set_Name("cmdConvert");
			((Control)cmdConvert).set_Size(new Size(58, 28));
			((Control)cmdConvert).set_TabIndex(3);
			((Control)cmdConvert).set_Text("&Convert");
			((ButtonBase)cmdConvert).set_UseVisualStyleBackColor(true);
			((Control)cmdConvert).add_Click((EventHandler)cmdConvert_Click);
			((Control)updnMinDia).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "AsteroidSearchMinimumDiameter", true, (DataSourceUpdateMode)1));
			((Control)updnMinDia).set_Location(new Point(178, 15));
			((Control)updnMinDia).set_Margin(new Padding(2));
			((Control)updnMinDia).set_Name("updnMinDia");
			((Control)updnMinDia).set_Size(new Size(32, 20));
			((Control)updnMinDia).set_TabIndex(1);
			updnMinDia.set_Value(Settings.Default.AsteroidSearchMinimumDiameter);
			((Control)updnMinDia).add_Enter((EventHandler)updnMinDia_Enter);
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Location(new Point(14, 18));
			((Control)label6).set_Margin(new Padding(2, 0, 2, 0));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(160, 13));
			((Control)label6).set_TabIndex(0);
			((Control)label6).set_Text("Include minor planets larger than");
			((Control)groupBox4).get_Controls().Add((Control)(object)label1);
			((Control)groupBox4).get_Controls().Add((Control)(object)lblInclude);
			((Control)groupBox4).get_Controls().Add((Control)(object)txtUnNumbered);
			((Control)groupBox4).get_Controls().Add((Control)(object)chkOppNum);
			((Control)groupBox4).get_Controls().Add((Control)(object)label6);
			((Control)groupBox4).get_Controls().Add((Control)(object)chkNumberedOnly);
			((Control)groupBox4).get_Controls().Add((Control)(object)updnMinDia);
			((Control)groupBox4).get_Controls().Add((Control)(object)label7);
			((Control)groupBox4).set_Location(new Point(118, 82));
			((Control)groupBox4).set_Margin(new Padding(2));
			((Control)groupBox4).set_Name("groupBox4");
			((Control)groupBox4).set_Padding(new Padding(2));
			((Control)groupBox4).set_Size(new Size(238, 163));
			((Control)groupBox4).set_TabIndex(1);
			groupBox4.set_TabStop(false);
			((Control)groupBox4).set_Text("2. Set selection criteria");
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label1).set_ForeColor(Color.Maroon);
			((Control)label1).set_Location(new Point(20, 59));
			((Control)label1).set_Margin(new Padding(2, 0, 2, 0));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(212, 12));
			((Control)label1).set_TabIndex(7);
			((Control)label1).set_Text("Clear this check for RIO-TNO predictions");
			label1.set_TextAlign(ContentAlignment.TopRight);
			((Control)lblInclude).set_AutoSize(true);
			((Control)lblInclude).set_Enabled(false);
			((Control)lblInclude).set_Location(new Point(4, 130));
			((Control)lblInclude).set_Margin(new Padding(2, 0, 2, 0));
			((Control)lblInclude).set_Name("lblInclude");
			((Control)lblInclude).set_Size(new Size(151, 13));
			((Control)lblInclude).set_TabIndex(6);
			((Control)lblInclude).set_Text("Include following un-numbered");
			((Control)txtUnNumbered).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "AsteroidSearchUnnumbered", true, (DataSourceUpdateMode)1));
			((Control)txtUnNumbered).set_Enabled(false);
			((Control)txtUnNumbered).set_Location(new Point(161, 125));
			((Control)txtUnNumbered).set_Margin(new Padding(2));
			((Control)txtUnNumbered).set_Name("txtUnNumbered");
			((Control)txtUnNumbered).set_Size(new Size(71, 20));
			((Control)txtUnNumbered).set_TabIndex(5);
			((Control)txtUnNumbered).set_Text(Settings.Default.AsteroidSearchUnnumbered);
			((Control)chkOppNum).set_AutoSize(true);
			chkOppNum.set_CheckAlign(ContentAlignment.MiddleRight);
			chkOppNum.set_Checked(Settings.Default.AstorbLimitUnNumbered);
			chkOppNum.set_CheckState((CheckState)1);
			((Control)chkOppNum).set_Enabled(false);
			((Control)chkOppNum).set_Location(new Point(8, 80));
			((Control)chkOppNum).set_Margin(new Padding(2));
			((Control)chkOppNum).set_Name("chkOppNum");
			((Control)chkOppNum).set_Size(new Size(221, 30));
			((Control)chkOppNum).set_TabIndex(4);
			((Control)chkOppNum).set_Text("Limit UnNumbered to 3+ oppositions\r\n{MPCORB}, or >1000-day arc {ASTORB}");
			((ButtonBase)chkOppNum).set_TextAlign(ContentAlignment.MiddleRight);
			((ButtonBase)chkOppNum).set_UseVisualStyleBackColor(true);
			chkOppNum.add_CheckedChanged((EventHandler)chkOppNum_CheckedChanged);
			((Control)chkNumberedOnly).set_AutoSize(true);
			chkNumberedOnly.set_CheckAlign(ContentAlignment.MiddleRight);
			chkNumberedOnly.set_Checked(Settings.Default.AstorbNumberedAsteroidsOnly);
			chkNumberedOnly.set_CheckState((CheckState)1);
			((Control)chkNumberedOnly).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "AstorbNumberedAsteroidsOnly", true, (DataSourceUpdateMode)1));
			((Control)chkNumberedOnly).set_Location(new Point(64, 44));
			((Control)chkNumberedOnly).set_Margin(new Padding(2));
			((Control)chkNumberedOnly).set_Name("chkNumberedOnly");
			((Control)chkNumberedOnly).set_Size(new Size(162, 17));
			((Control)chkNumberedOnly).set_TabIndex(3);
			((Control)chkNumberedOnly).set_Text("Numbered minor planets only");
			((ButtonBase)chkNumberedOnly).set_TextAlign(ContentAlignment.MiddleRight);
			((ButtonBase)chkNumberedOnly).set_UseVisualStyleBackColor(true);
			chkNumberedOnly.add_CheckedChanged((EventHandler)chkNumberedOnly_CheckedChanged);
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Location(new Point(211, 18));
			((Control)label7).set_Margin(new Padding(2, 0, 2, 0));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(21, 13));
			((Control)label7).set_TabIndex(2);
			((Control)label7).set_Text("km");
			((Control)groupBox5).get_Controls().Add((Control)(object)cmdDeleteAlways);
			((Control)groupBox5).get_Controls().Add((Control)(object)cmdAddAlways);
			((Control)groupBox5).get_Controls().Add((Control)(object)txtAddAlways);
			((Control)groupBox5).get_Controls().Add((Control)(object)lstAlways);
			((Control)groupBox5).set_Location(new Point(364, 90));
			((Control)groupBox5).set_Margin(new Padding(2));
			((Control)groupBox5).set_Name("groupBox5");
			((Control)groupBox5).set_Padding(new Padding(2));
			((Control)groupBox5).set_Size(new Size(194, 216));
			((Control)groupBox5).set_TabIndex(2);
			groupBox5.set_TabStop(false);
			((Control)groupBox5).set_Text("3. Specify 'mandatory' minor planets");
			((Control)cmdDeleteAlways).set_Location(new Point(127, 82));
			((Control)cmdDeleteAlways).set_Margin(new Padding(2));
			((Control)cmdDeleteAlways).set_Name("cmdDeleteAlways");
			((Control)cmdDeleteAlways).set_Size(new Size(58, 37));
			((Control)cmdDeleteAlways).set_TabIndex(3);
			((Control)cmdDeleteAlways).set_Text("Delete selected");
			((ButtonBase)cmdDeleteAlways).set_UseVisualStyleBackColor(true);
			((Control)cmdDeleteAlways).add_Click((EventHandler)cmdDeleteAlways_Click);
			((Control)cmdAddAlways).set_Location(new Point(127, 47));
			((Control)cmdAddAlways).set_Margin(new Padding(2));
			((Control)cmdAddAlways).set_Name("cmdAddAlways");
			((Control)cmdAddAlways).set_Size(new Size(58, 22));
			((Control)cmdAddAlways).set_TabIndex(2);
			((Control)cmdAddAlways).set_Text("Add");
			((ButtonBase)cmdAddAlways).set_UseVisualStyleBackColor(true);
			((Control)cmdAddAlways).add_Click((EventHandler)cmdAddAlways_Click);
			((Control)txtAddAlways).set_Location(new Point(127, 18));
			((Control)txtAddAlways).set_Margin(new Padding(2));
			((Control)txtAddAlways).set_Name("txtAddAlways");
			((Control)txtAddAlways).set_Size(new Size(56, 20));
			((Control)txtAddAlways).set_TabIndex(1);
			((Control)lstAlways).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstAlways).set_FormattingEnabled(true);
			lstAlways.set_ItemHeight(14);
			((Control)lstAlways).set_Location(new Point(10, 14));
			((Control)lstAlways).set_Margin(new Padding(2));
			((Control)lstAlways).set_Name("lstAlways");
			((Control)lstAlways).set_Size(new Size(101, 186));
			lstAlways.set_Sorted(true);
			((Control)lstAlways).set_TabIndex(0);
			lstAlways.add_SelectedIndexChanged((EventHandler)lstAlways_SelectedIndexChanged);
			((ToolStrip)menuStrip1).set_ImageScalingSize(new Size(20, 20));
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Padding(new Padding(4, 2, 0, 2));
			((Control)menuStrip1).set_Size(new Size(654, 28));
			((Control)menuStrip1).set_TabIndex(5);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(76, 24));
			((ToolStripItem)helpToolStripMenuItem).set_Text("&Help    ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(58, 24));
			((ToolStripItem)exitToolStripMenuItem).set_Text("Exit");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((Control)pBar).set_Location(new Point(561, 142));
			((Control)pBar).set_Margin(new Padding(2));
			((Control)pBar).set_Name("pBar");
			((Control)pBar).set_Size(new Size(79, 8));
			((Control)pBar).set_TabIndex(6);
			((Control)pBar).set_Visible(false);
			toolTip1.set_AutomaticDelay(100);
			toolTip1.set_AutoPopDelay(20000);
			toolTip1.set_InitialDelay(100);
			toolTip1.set_ReshowDelay(20);
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_BackColor(Color.PaleTurquoise);
			((Control)label3).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label3).set_ForeColor(Color.Black);
			((Control)label3).set_Location(new Point(12, 3));
			((Control)label3).set_Margin(new Padding(2, 0, 2, 0));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(311, 13));
			((Control)label3).set_TabIndex(9);
			((Control)label3).set_Text("AstOrb file is used to add error values to MPCORB and AstDyS-2");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_BackColor(Color.PaleTurquoise);
			((Control)label4).set_ForeColor(Color.Black);
			((Control)label4).set_Location(new Point(12, 19));
			((Control)label4).set_Margin(new Padding(2, 0, 2, 0));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(285, 13));
			((Control)label4).set_TabIndex(10);
			((Control)label4).set_Text("MPCORB   or  AstOrb  are used to add names to  AstDyS-2");
			((Control)panel1).set_BackColor(Color.PaleTurquoise);
			panel1.set_BorderStyle((BorderStyle)2);
			((Control)panel1).get_Controls().Add((Control)(object)label4);
			((Control)panel1).get_Controls().Add((Control)(object)label3);
			((Control)panel1).set_ForeColor(Color.Black);
			((Control)panel1).set_Location(new Point(10, 248));
			((Control)panel1).set_Margin(new Padding(2));
			((Control)panel1).set_Name("panel1");
			((Control)panel1).set_Size(new Size(336, 42));
			((Control)panel1).set_TabIndex(12);
			((Control)cmdDownloadMPCORB).set_BackColor(Color.Lime);
			((Control)cmdDownloadMPCORB).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdDownloadMPCORB).set_Location(new Point(35, 32));
			((Control)cmdDownloadMPCORB).set_Name("cmdDownloadMPCORB");
			((Control)cmdDownloadMPCORB).set_Size(new Size(80, 45));
			((Control)cmdDownloadMPCORB).set_TabIndex(13);
			((Control)cmdDownloadMPCORB).set_Text("Download MPCORB");
			((ButtonBase)cmdDownloadMPCORB).set_UseVisualStyleBackColor(false);
			((Control)cmdDownloadMPCORB).add_Click((EventHandler)cmdDownloadMPCORB_Click);
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Location(new Point(116, 40));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(88, 13));
			((Control)label10).set_TabIndex(15);
			((Control)label10).set_Text("Last downloaded");
			((Control)lblDownloadDate_MPCORB).set_AutoSize(true);
			((Control)lblDownloadDate_MPCORB).set_Location(new Point(116, 57));
			((Control)lblDownloadDate_MPCORB).set_Name("lblDownloadDate_MPCORB");
			((Control)lblDownloadDate_MPCORB).set_Size(new Size(60, 13));
			((Control)lblDownloadDate_MPCORB).set_TabIndex(16);
			((Control)lblDownloadDate_MPCORB).set_Text("2020 Jan 1");
			((Control)lblDownloadDate_AstDys2).set_AutoSize(true);
			((Control)lblDownloadDate_AstDys2).set_Location(new Point(324, 57));
			((Control)lblDownloadDate_AstDys2).set_Name("lblDownloadDate_AstDys2");
			((Control)lblDownloadDate_AstDys2).set_Size(new Size(60, 13));
			((Control)lblDownloadDate_AstDys2).set_TabIndex(19);
			((Control)lblDownloadDate_AstDys2).set_Text("2020 Jan 1");
			((Control)label11).set_AutoSize(true);
			((Control)label11).set_Location(new Point(324, 40));
			((Control)label11).set_Name("label11");
			((Control)label11).set_Size(new Size(67, 13));
			((Control)label11).set_TabIndex(18);
			((Control)label11).set_Text("Last file date");
			((Control)cmdDownloadASTDYS2).set_BackColor(Color.Aquamarine);
			((Control)cmdDownloadASTDYS2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdDownloadASTDYS2).set_Location(new Point(243, 32));
			((Control)cmdDownloadASTDYS2).set_Name("cmdDownloadASTDYS2");
			((Control)cmdDownloadASTDYS2).set_Size(new Size(80, 45));
			((Control)cmdDownloadASTDYS2).set_TabIndex(17);
			((Control)cmdDownloadASTDYS2).set_Text("Download AstDys-2");
			((ButtonBase)cmdDownloadASTDYS2).set_UseVisualStyleBackColor(false);
			((Control)cmdDownloadASTDYS2).add_Click((EventHandler)cmdDownloadASTDYS2_Click);
			((Control)lblDownloadDate_ASTORB).set_AutoSize(true);
			((Control)lblDownloadDate_ASTORB).set_Location(new Point(532, 57));
			((Control)lblDownloadDate_ASTORB).set_Name("lblDownloadDate_ASTORB");
			((Control)lblDownloadDate_ASTORB).set_Size(new Size(60, 13));
			((Control)lblDownloadDate_ASTORB).set_TabIndex(22);
			((Control)lblDownloadDate_ASTORB).set_Text("2020 Jan 1");
			((Control)label13).set_AutoSize(true);
			((Control)label13).set_Location(new Point(532, 40));
			((Control)label13).set_Name("label13");
			((Control)label13).set_Size(new Size(88, 13));
			((Control)label13).set_TabIndex(21);
			((Control)label13).set_Text("Last downloaded");
			((Control)cmdDownloadASTORB).set_BackColor(Color.Aquamarine);
			((Control)cmdDownloadASTORB).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdDownloadASTORB).set_Location(new Point(451, 32));
			((Control)cmdDownloadASTORB).set_Name("cmdDownloadASTORB");
			((Control)cmdDownloadASTORB).set_Size(new Size(80, 45));
			((Control)cmdDownloadASTORB).set_TabIndex(20);
			((Control)cmdDownloadASTORB).set_Text("Download ASTORB");
			((ButtonBase)cmdDownloadASTORB).set_UseVisualStyleBackColor(false);
			((Control)cmdDownloadASTORB).add_Click((EventHandler)cmdDownloadASTORB_Click);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(654, 307));
			((Control)this).get_Controls().Add((Control)(object)lblDownloadDate_ASTORB);
			((Control)this).get_Controls().Add((Control)(object)label13);
			((Control)this).get_Controls().Add((Control)(object)cmdDownloadASTORB);
			((Control)this).get_Controls().Add((Control)(object)lblDownloadDate_AstDys2);
			((Control)this).get_Controls().Add((Control)(object)label11);
			((Control)this).get_Controls().Add((Control)(object)cmdDownloadASTDYS2);
			((Control)this).get_Controls().Add((Control)(object)lblDownloadDate_MPCORB);
			((Control)this).get_Controls().Add((Control)(object)label10);
			((Control)this).get_Controls().Add((Control)(object)cmdDownloadMPCORB);
			((Control)this).get_Controls().Add((Control)(object)optGaia);
			((Control)this).get_Controls().Add((Control)(object)panel1);
			((Control)this).get_Controls().Add((Control)(object)pBar);
			((Control)this).get_Controls().Add((Control)(object)groupBox5);
			((Control)this).get_Controls().Add((Control)(object)groupBox4);
			((Control)this).get_Controls().Add((Control)(object)cmdConvert);
			((Control)this).get_Controls().Add((Control)(object)groupBox1);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationAsterConvert", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_FormBorderStyle((FormBorderStyle)2);
			((Form)this).set_Icon((Icon)componentResourceManager.GetObject("$this.Icon"));
			((Form)this).set_Location(Settings.Default.LocationAsterConvert);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Form)this).set_Margin(new Padding(2));
			((Form)this).set_MaximizeBox(false);
			((Control)this).set_Name("ConvertAstorb_etc");
			((Control)this).set_Text("Convert  Astorb  and  MPCOrb  files");
			((Form)this).add_Load((EventHandler)ConvertAstorb_etc_Load);
			((Control)groupBox1).ResumeLayout(false);
			((Control)panel4).ResumeLayout(false);
			((Control)panel4).PerformLayout();
			((Control)panel3).ResumeLayout(false);
			((Control)panel3).PerformLayout();
			((Control)panel2).ResumeLayout(false);
			((Control)panel2).PerformLayout();
			((ISupportInitialize)updnMinDia).EndInit();
			((Control)groupBox4).ResumeLayout(false);
			((Control)groupBox4).PerformLayout();
			((Control)groupBox5).ResumeLayout(false);
			((Control)groupBox5).PerformLayout();
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)panel1).ResumeLayout(false);
			((Control)panel1).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
