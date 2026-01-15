using System;
using System.IO;
using System.Text;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult
{
	public class NOMAD
	{
		private const double Radian = 180.0 / Math.PI;

		private const double MilliSecInDeg = 3600000.0;

		private const double MilliSecInRadian = 648000000.0 / Math.PI;

		private const double Deg90 = Math.PI / 2.0;

		private static string NomadPath;

		private static string Path;

		private static string TNum;

		private static string DoubleFlag;

		private static bool NOMAD_in_use = false;

		private static bool invalidPosition = false;

		private static int ra2000;

		private static int spd2000;

		private static int sDev_RA;

		private static int sDev_Dec;

		private static int pmRA;

		private static int pmDec;

		private static int sDev_pmRA;

		private static int sDev_pmDec;

		private static int tRA;

		private static int tDec;

		private static int B;

		private static int V;

		private static int R;

		private static int J;

		private static int H;

		private static int K;

		private static int USNOB1;

		private static int mASS;

		private static int yB6;

		private static int UCAC2no;

		private static int tycho2;

		private static int flags;

		private static int CurrentZone;

		private static int NomadSeqNumber;

		private static int recordsInFile;

		private static float parallax = 0f;

		private static int[] Bzero = new int[1800];

		private static bool UsingFullNomad = true;

		private static StringBuilder N;

		private static FileStream Tycho2IndexFile;

		private static FileStream StarCatFileStream;

		private static FileStream IndexFileStream;

		private static FileStream HipParallaxFile;

		private static BinaryReader Tycho2Index;

		private static BinaryReader NomadFile;

		private static BinaryReader IndexFile;

		private static BinaryReader HipParallax;

		public static double RAdeg => (double)ra2000 / 3600000.0;

		public static double RA => (double)ra2000 / (648000000.0 / Math.PI);

		public static double SDev_RA => sDev_RA;

		public static double Dec_deg => (double)spd2000 / 3600000.0 - 90.0;

		public static double Dec => (double)spd2000 / (648000000.0 / Math.PI) - Math.PI / 2.0;

		public static double SDev_Dec => sDev_Dec;

		public static double PM_ra => (double)pmRA / (648000000.0 / Math.PI) / 10.0 / Math.Cos(Dec);

		public static double SDev_pmRA => (double)sDev_pmRA / 10.0;

		public static double PM_dec => (double)pmDec / (648000000.0 / Math.PI) / 10.0;

		public static double SDev_pmDec => (double)sDev_pmDec / 10.0;

		public static float Parallax => parallax;

		public static double Epoch => tRA / 1000 - 2000;

		public static double Mb => (double)B / 1000.0;

		public static double Mv => (double)V / 1000.0;

		public static double Mr => (double)R / 1000.0;

		public static int HipNumber
		{
			get
			{
				if ((flags & 0x1000000) > 0)
				{
					return tycho2 % 1000000;
				}
				return 0;
			}
		}

		public static bool InvalidPosition => invalidPosition;

		public static bool InvalidPM => (sDev_pmRA == 0) | (sDev_pmDec == 0);

		public static bool Tycho_from_UCAC => (flags & 0x10000) < 65536;

		public static int RecordsInFile => recordsInFile;

		public static string UCAC_ID
		{
			get
			{
				if (UCAC2no > 0)
				{
					return "2U " + UCAC2no.ToString().PadLeft(8);
				}
				return "".PadLeft(11);
			}
		}

		public static string Tycho2_ID
		{
			get
			{
				if (tycho2 == 0)
				{
					return "".PadLeft(13);
				}
				if ((flags & 0x1000000) > 0)
				{
					return "Hip " + (tycho2 % 1000000).ToString().PadRight(9);
				}
				Tycho2Index.BaseStream.Seek(4 * tycho2 - 4, SeekOrigin.Begin);
				TNum = Tycho2Index.ReadUInt32().ToString().PadLeft(10, '0');
				if ((flags & 0x10000) > 0)
				{
					return TNum.Substring(5, 4) + "-" + TNum.Substring(0, 5) + "-" + TNum.Substring(9, 1) + " ";
				}
				return TNum.Substring(5, 4) + "-" + TNum.Substring(0, 5) + "-" + TNum.Substring(9, 1) + "u";
			}
		}

		public static string USNO_B1_ID
		{
			get
			{
				if (USNOB1 > 0)
				{
					return ("1B " + CurrentZone + "-" + (USNOB1 - Bzero[CurrentZone])).PadRight(14);
				}
				return "".PadLeft(14);
			}
		}

		public static string MASS_SeqNo
		{
			get
			{
				if (mASS > 0)
				{
					return mASS.ToString().PadLeft(12);
				}
				return "".PadLeft(12);
			}
		}

		public static string YB6_SeqNo
		{
			get
			{
				if (yB6 > 0)
				{
					return yB6.ToString().PadLeft(12);
				}
				return "".PadLeft(12);
			}
		}

		public static string StarID
		{
			get
			{
				DoubleFlag = " ";
				if ((flags & 0x8000000) > 0)
				{
					DoubleFlag = "d";
				}
				if (tycho2 > 0)
				{
					if ((flags & 0x1000000) > 0)
					{
						return "HIP " + tycho2 % 1000000 + DoubleFlag;
					}
					Tycho2IndexFile.Seek(4 * tycho2 - 4, SeekOrigin.Begin);
					TNum = Tycho2Index.ReadUInt32().ToString().PadLeft(10, '0');
					if ((flags & 0x10000) > 0)
					{
						return "TYC " + TNum.Substring(5, 4) + "-" + TNum.Substring(0, 5) + "-" + TNum.Substring(9, 1) + " " + DoubleFlag;
					}
					return "TYC " + TNum.Substring(5, 4) + "-" + TNum.Substring(0, 5) + "-" + TNum.Substring(9, 1) + "u" + DoubleFlag;
				}
				if (UCAC2no > 0)
				{
					return "2U " + UCAC2no.ToString().PadLeft(8) + DoubleFlag;
				}
				if (USNOB1 > 0)
				{
					return "1B " + CurrentZone + "-" + (USNOB1 - Bzero[CurrentZone]) + DoubleFlag;
				}
				return "1N " + CurrentZone + "-" + NomadSeqNumber + DoubleFlag;
			}
		}

		public static string Nomad_ASCII_line
		{
			get
			{
				N = new StringBuilder();
				if (UsingFullNomad)
				{
					N.Append(("1N " + CurrentZone + "-" + NomadSeqNumber + DoubleFlag).PadRight(16));
				}
				else
				{
					N.Append(("1N " + CurrentZone + "- ...").PadRight(16));
				}
				if (B < 30000)
				{
					N.AppendFormat("{0,5:F2} ", (double)B / 1000.0);
				}
				else
				{
					N.Append("      ");
				}
				if (V < 30000)
				{
					N.AppendFormat("{0,5:F2} ", (double)V / 1000.0);
				}
				else
				{
					N.Append("      ");
				}
				if (R < 30000)
				{
					N.AppendFormat("{0,5:F2}    ", (double)R / 1000.0);
				}
				else
				{
					N.Append("         ");
				}
				N.Append(Utilities.DEGtoDMS((double)ra2000 / 3600000.0 / 15.0, 2, 4, MinutesOnly: false));
				N.AppendFormat(" {0,8:F5} ", (double)pmRA / 150000.0 / Math.Cos(Dec));
				N.AppendFormat("  {0,6:F3}", (double)sDev_RA / 1000.0);
				N.AppendFormat(" {0,7:F4}", (double)sDev_pmRA / 10000.0);
				N.AppendFormat(" {0,7:F2}    ", (double)tRA / 1000.0);
				N.Append(Utilities.DEGtoDMS((double)spd2000 / 3600000.0 - 90.0, 3, 3, MinutesOnly: false));
				N.AppendFormat(" {0,8:F4} ", (double)pmDec / 10000.0);
				N.AppendFormat("  {0,6:F3}", (double)sDev_Dec / 1000.0);
				N.AppendFormat(" {0,7:F4}", (double)sDev_pmDec / 10000.0);
				N.AppendFormat(" {0,7:F2}   ", (double)tDec / 1000.0);
				if (parallax > 0f)
				{
					N.AppendFormat("{0,7:F5}  ", parallax);
				}
				else
				{
					N.Append("         ");
				}
				N.Append(UCAC_ID + "  " + Tycho2_ID + "  " + USNO_B1_ID);
				if (invalidPosition)
				{
					N.Append("x");
				}
				if (InvalidPM)
				{
					N.Append("p");
				}
				return N.ToString();
			}
		}

		public static double ErrorRA_arcsecs(double T)
		{
			double num = (double)sDev_RA / 1000.0;
			double num2 = (double)sDev_pmRA / 10000.0 * (T - (double)tRA / 1000.0);
			return Math.Sqrt(num * num + num2 * num2);
		}

		public static double ErrorDec_arcsecs(double T)
		{
			double num = (double)sDev_Dec / 1000.0;
			double num2 = (double)sDev_pmDec / 10000.0 * (T - (double)tDec / 1000.0);
			return Math.Sqrt(num * num + num2 * num2);
		}

		public static bool InitialiseNOMAD()
		{
			//IL_0014: Unknown result type (might be due to invalid IL or missing references)
			if (NOMAD_in_use)
			{
				MessageBox.Show("The NOMAD catalogue is being used by another part of OCCULT\r\n\r\nIt can only be used by one part at a time.", "NOMAD usage", (MessageBoxButtons)0, (MessageBoxIcon)48);
				return false;
			}
			NomadPath = Settings.Default.NOMAD_Path;
			if (!NomadPath.ToLower().Contains("nomad") | !File.Exists(NomadPath + "\\090\\m0900.cat"))
			{
				NomadPath = Settings.Default.NOMAD_Short_path;
			}
			if (NomadPath.ToLower().Contains("nomad1"))
			{
				UsingFullNomad = true;
			}
			else
			{
				UsingFullNomad = false;
			}
			if (!File.Exists(NomadPath + "\\Tycho2_id.bin"))
			{
				return false;
			}
			Tycho2IndexFile = new FileStream(NomadPath + "\\Tycho2_id.bin", FileMode.Open, FileAccess.Read);
			Tycho2Index = new BinaryReader(Tycho2IndexFile);
			using (StreamReader streamReader = new StreamReader(NomadPath + "\\USNO_B1_base.txt"))
			{
				for (int i = 0; i < 1800; i++)
				{
					Bzero[i] = int.Parse(streamReader.ReadLine()!.Substring(4));
				}
			}
			HipParallaxFile = new FileStream(NomadPath + "\\Hip_Parallax.bin", FileMode.Open, FileAccess.Read);
			HipParallax = new BinaryReader(HipParallaxFile);
			NOMAD_in_use = true;
			return true;
		}

		public static void Open_Nomad_Catalogue_and_Index_Files(int DecZone)
		{
			CurrentZone = DecZone;
			NomadPath = Settings.Default.NOMAD_Path;
			if (!NomadPath.ToLower().Contains("nomad") | !File.Exists(NomadPath + "\\090\\m0900.cat"))
			{
				NomadPath = Settings.Default.NOMAD_Short_path;
			}
			if (NomadPath.ToLower().Contains("nomad1"))
			{
				UsingFullNomad = true;
			}
			else
			{
				UsingFullNomad = false;
			}
			Path = NomadPath + "\\" + ((int)Math.Floor((double)DecZone / 10.0)).ToString().PadLeft(3, '0') + "\\m" + DecZone.ToString().PadLeft(4, '0');
			StarCatFileStream = new FileStream(Path + ".cat", FileMode.Open, FileAccess.Read);
			NomadFile = new BinaryReader(StarCatFileStream);
			recordsInFile = (int)StarCatFileStream.Length / 88;
			IndexFileStream = new FileStream(Path + ".inx", FileMode.Open, FileAccess.Read);
			IndexFile = new BinaryReader(IndexFileStream);
		}

		public static int GetNomadIndexValue(int entry)
		{
			if (entry == -1)
			{
				return (int)StarCatFileStream.Length / 88;
			}
			IndexFileStream.Seek(4 * entry, SeekOrigin.Begin);
			return IndexFile.ReadInt32();
		}

		internal static void Read_NOMAD_entry(int Record)
		{
			if (Record < 0)
			{
				Record = 0;
			}
			NomadSeqNumber = Record + 1;
			StarCatFileStream.Seek(88 * Record, SeekOrigin.Begin);
			ra2000 = NomadFile.ReadInt32();
			spd2000 = NomadFile.ReadInt32();
			sDev_RA = NomadFile.ReadInt32();
			sDev_Dec = NomadFile.ReadInt32();
			pmRA = NomadFile.ReadInt32();
			pmDec = NomadFile.ReadInt32();
			sDev_pmRA = NomadFile.ReadInt32();
			sDev_pmDec = NomadFile.ReadInt32();
			tRA = NomadFile.ReadInt32();
			tDec = NomadFile.ReadInt32();
			B = NomadFile.ReadInt32();
			V = NomadFile.ReadInt32();
			R = NomadFile.ReadInt32();
			J = NomadFile.ReadInt32();
			H = NomadFile.ReadInt32();
			K = NomadFile.ReadInt32();
			if ((V == 30000) & (B < 30000) & (R < 30000))
			{
				V = (int)(((double)R + 0.54 * (double)B) / 1.54);
			}
			USNOB1 = NomadFile.ReadInt32();
			mASS = NomadFile.ReadInt32();
			yB6 = NomadFile.ReadInt32();
			UCAC2no = NomadFile.ReadInt32();
			tycho2 = NomadFile.ReadInt32();
			flags = NomadFile.ReadInt32();
			if ((flags & 0x1000000) > 0)
			{
				HipParallaxFile.Seek(4 * (tycho2 % 1000000), SeekOrigin.Begin);
				parallax = HipParallax.ReadSingle();
			}
			else
			{
				parallax = 0f;
			}
			invalidPosition = false;
			if (((tycho2 == 0) & (UCAC2no == 0)) && ((B > 16) | (V > 16) | (R > 16)) && (((flags & 0x1000) > 0) | ((flags & 0x2000) > 0)))
			{
				invalidPosition = true;
			}
			if (((flags & 0x2000000) > 0) | ((flags & 0x10000000) > 0))
			{
				invalidPosition = true;
			}
		}

		public static void Close_Nomad_Catalogue_and_Index_Files()
		{
			NomadFile.Close();
			IndexFile.Close();
		}

		public static void ReleaseNOMAD()
		{
			Tycho2Index.Close();
			NOMAD_in_use = false;
		}

		public static bool Hipparcos(int HipparcosNumber)
		{
			if (HipparcosNumber >= 120500)
			{
				return false;
			}
			FileStream fileStream = new FileStream(NomadPath + "\\Hip_Index.bin", FileMode.Open, FileAccess.Read);
			BinaryReader binaryReader = new BinaryReader(fileStream);
			fileStream.Seek(6 * HipparcosNumber, SeekOrigin.Begin);
			int decZone = binaryReader.ReadInt16();
			int record = binaryReader.ReadInt32();
			binaryReader.Close();
			Open_Nomad_Catalogue_and_Index_Files(decZone);
			Read_NOMAD_entry(record);
			Close_Nomad_Catalogue_and_Index_Files();
			return !invalidPosition & !InvalidPM;
		}

		public static bool Tycho2(int Region, int SeqNum, int Component)
		{
			int num = Region * 100000 + SeqNum * 10 + Component;
			FileStream fileStream = new FileStream(NomadPath + "\\Tycho2_LookUpTable.bin", FileMode.Open, FileAccess.Read);
			BinaryReader binaryReader = new BinaryReader(fileStream);
			int num2 = 0;
			int num3 = (int)fileStream.Length / 10 - 1;
			int decZone = 0;
			int record = 0;
			bool flag = false;
			do
			{
				int num4 = (num2 + num3) / 2;
				fileStream.Seek(10 * num4, SeekOrigin.Begin);
				int num5 = binaryReader.ReadInt32();
				if (num5 == num)
				{
					flag = true;
					decZone = binaryReader.ReadInt16();
					record = binaryReader.ReadInt32();
					break;
				}
				if (num5 < num)
				{
					num2 = num4 + 1;
				}
				else if (num5 > num)
				{
					num3 = num4 - 1;
				}
			}
			while (num3 >= num2);
			binaryReader.Close();
			if (flag)
			{
				Open_Nomad_Catalogue_and_Index_Files(decZone);
				Read_NOMAD_entry(record);
				Close_Nomad_Catalogue_and_Index_Files();
			}
			return flag & !invalidPosition & !InvalidPM;
		}

		public static bool UCAC2(int UCACnumber)
		{
			bool flag = false;
			if (UCACnumber > 48330571)
			{
				return flag;
			}
			FileStream fileStream = new FileStream(NomadPath + "\\u2index.da", FileMode.Open, FileAccess.Read);
			BinaryReader binaryReader = new BinaryReader(fileStream);
			int num = 0;
			int num2 = (int)fileStream.Length / 4;
			do
			{
				int num3 = (num + num2) / 2;
				fileStream.Seek(4 * num3, SeekOrigin.Begin);
				int num4 = binaryReader.ReadInt32();
				if (num4 == UCACnumber)
				{
					if (num == num2)
					{
						num = num3--;
					}
					if (num < 0)
					{
						num = 0;
						num2 = 1;
					}
					break;
				}
				if (num4 < UCACnumber)
				{
					num = num3;
				}
				else if (num4 > UCACnumber)
				{
					num2 = num3 - 1;
				}
			}
			while (num2 - num > 1);
			binaryReader.Close();
			int num5 = 5 * (int)Math.Floor((double)num / 240.0);
			int num6 = num5 + 4;
			int num7 = ((num % 240 != 0) ? ((num2 % 240 != 0) ? ((int)(7.5 * (double)(num % 240 + 1) - 0.1)) : 1792) : 0);
			int num8 = num7 + 12;
			if (num8 > 1800)
			{
				num8 = 1800;
			}
			for (int i = num5; i <= num6; i++)
			{
				Open_Nomad_Catalogue_and_Index_Files(i);
				int nomadIndexValue = GetNomadIndexValue(num7);
				int nomadIndexValue2 = GetNomadIndexValue(num8);
				for (int j = nomadIndexValue; j <= nomadIndexValue2; j++)
				{
					Read_NOMAD_entry(j);
					if (UCAC2no == UCACnumber)
					{
						flag = true;
						break;
					}
					if (UCAC2no > UCACnumber + 2)
					{
						break;
					}
				}
				Close_Nomad_Catalogue_and_Index_Files();
				if (flag)
				{
					break;
				}
			}
			return flag & !invalidPosition & !InvalidPM;
		}

		public static bool USNO_B1(int Zone, int NumberInZone, out bool GoodPosition)
		{
			Open_Nomad_Catalogue_and_Index_Files(Zone);
			int num = 0;
			int num2 = recordsInFile - 1;
			int num3 = NumberInZone + Bzero[Zone];
			do
			{
				int num4 = (num + num2) / 2;
				do
				{
					Read_NOMAD_entry(num4);
					if (USNOB1 == 0)
					{
						num4++;
					}
				}
				while ((USNOB1 == 0) & (num4 < recordsInFile));
				if (USNOB1 == num3)
				{
					break;
				}
				if (USNOB1 < num3)
				{
					num = num4 + 1;
				}
				else if (USNOB1 > num3)
				{
					num2 = num4 - 1;
				}
			}
			while (num2 - num > 10);
			if (USNOB1 != num3)
			{
				int num4 = num - 5;
				do
				{
					Read_NOMAD_entry(num4);
					if (USNOB1 == num3)
					{
						break;
					}
					num4++;
				}
				while (num4 < recordsInFile && num4 < num2 + 5);
			}
			Close_Nomad_Catalogue_and_Index_Files();
			GoodPosition = !invalidPosition & !InvalidPM;
			if (USNOB1 == num3)
			{
				return true;
			}
			return false;
		}

		internal static string NOMADnumFromCoords(double RAstar, double DecStar, double PMyears, out double M_b, out double M_v, out double M_r)
		{
			string result = "";
			double num = 4.84813681109536E-06;
			double num2 = num / Math.Cos(DecStar);
			double num3 = 1.0;
			M_b = (M_v = (M_r = 25.0));
			int decZone = (int)(900.0 + 1800.0 / Math.PI * DecStar);
			int num4 = (int)Math.Floor(RAstar * (180.0 / Math.PI) * 5.0);
			Open_Nomad_Catalogue_and_Index_Files(decZone);
			int nomadIndexValue = GetNomadIndexValue(num4);
			int num5 = GetNomadIndexValue(num4 + 1) - nomadIndexValue;
			for (int i = 0; i < num5; i++)
			{
				Read_NOMAD_entry(nomadIndexValue + i);
				num3 = 1.0;
				if ((PM_ra == 0.0) & (PM_dec == 0.0))
				{
					num3 = 2.0;
				}
				if ((Math.Abs(RA + PM_ra * PMyears - RAstar) < num3 * num2) & (Math.Abs(Dec + PM_dec * PMyears - DecStar) < num3 * num))
				{
					result = StarID;
					M_b = Mb;
					M_v = Mv;
					M_r = Mr;
					break;
				}
				if (RA - RAstar > 4E-05)
				{
					break;
				}
			}
			Close_Nomad_Catalogue_and_Index_Files();
			return result;
		}

		public static void CreateNomad_inx_Files()
		{
			//IL_0032: Unknown result type (might be due to invalid IL or missing references)
			//IL_0066: Unknown result type (might be due to invalid IL or missing references)
			//IL_006c: Invalid comparison between Unknown and I4
			NomadPath = Settings.Default.NOMAD_Path;
			if (!File.Exists(NomadPath + "\\090\\m0900.cat"))
			{
				MessageBox.Show("The NOMAD catalogue is not present", "No catalogue", (MessageBoxButtons)0, (MessageBoxIcon)48);
			}
			else
			{
				if (File.Exists(NomadPath + "\\090\\m0900.inx") && (int)MessageBox.Show("At least some index files exist. Do you want to continue?", "Some index files exist", (MessageBoxButtons)1, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 2)
				{
					return;
				}
				PBar pBar = new PBar();
				pBar.pBarFTP.set_Minimum(0);
				pBar.pBarFTP.set_Maximum(1800);
				pBar.pBarFTP.set_Value(0);
				((Form)pBar).set_StartPosition((FormStartPosition)1);
				((Form)pBar).set_TopMost(true);
				((Control)pBar).Show();
				for (int i = 0; i < 180; i++)
				{
					string text = NomadPath + "\\" + i.ToString().PadLeft(3, '0') + "\\m" + i.ToString().PadLeft(3, '0');
					for (int j = 0; j < 10; j++)
					{
						string text2 = text + j;
						StarCatFileStream = new FileStream(text2 + ".cat", FileMode.Open, FileAccess.Read);
						long num = StarCatFileStream.Length / 88;
						NomadFile = new BinaryReader(StarCatFileStream);
						BinaryWriter binaryWriter = new BinaryWriter(new FileStream(text2 + ".inx", FileMode.Create, FileAccess.Write));
						double num2 = 0.0;
						for (int k = 0; k < num; k++)
						{
							Read_NOMAD_entry(k);
							for (double rAdeg = RAdeg; rAdeg >= num2; num2 += 0.2)
							{
								binaryWriter.Write(k);
							}
						}
						for (; num2 <= 360.001; num2 += 0.2)
						{
							binaryWriter.Write((int)num - 1);
						}
						binaryWriter.Close();
						NomadFile.Close();
						ProgressBar pBarFTP = pBar.pBarFTP;
						int value = pBarFTP.get_Value();
						pBarFTP.set_Value(value + 1);
						((Control)pBar).set_Text("Zone " + pBar.pBarFTP.get_Value().ToString().PadLeft(4, '0'));
						pBar.Devents();
					}
				}
				((Form)pBar).Close();
			}
		}

		public static void Create_B1_Index()
		{
			InitialiseNOMAD();
			int num = 0;
			StreamWriter streamWriter = new StreamWriter("c:\\temp\\B1_.txt");
			for (int i = 0; i < 180; i++)
			{
				for (int j = 0; j < 10; j++)
				{
					Open_Nomad_Catalogue_and_Index_Files(10 * i + j);
					long num2 = StarCatFileStream.Length / 88;
					Read_NOMAD_entry(0);
					int num3 = USNOB1;
					Read_NOMAD_entry(1);
					int uSNOB = USNOB1;
					if (num3 == 0)
					{
						num3 = uSNOB;
					}
					Read_NOMAD_entry(2);
					int uSNOB2 = USNOB1;
					if (num3 == 0)
					{
						num3 = uSNOB2;
					}
					Read_NOMAD_entry(3);
					int uSNOB3 = USNOB1;
					if (num3 == 0)
					{
						num3 = uSNOB3;
					}
					Read_NOMAD_entry(4);
					int uSNOB4 = USNOB1;
					if (num3 == 0)
					{
						num3 = uSNOB4;
					}
					if (uSNOB < num3 && uSNOB > 0)
					{
						num3 = uSNOB;
					}
					if (uSNOB2 < num3 && uSNOB2 > 0)
					{
						num3 = uSNOB2;
					}
					if (uSNOB3 < num3 && uSNOB3 > 0)
					{
						num3 = uSNOB3;
					}
					if (uSNOB4 < num3 && uSNOB4 > 0)
					{
						num3 = uSNOB4;
					}
					if (num3 - num == 1)
					{
						streamWriter.WriteLine((10 * i + j).ToString().PadLeft(4, '0') + "  " + $"{num}");
					}
					else
					{
						streamWriter.WriteLine((10 * i + j).ToString().PadLeft(4, '0') + "  " + (num3 - 1));
					}
					Read_NOMAD_entry((int)(num2 - 5));
					num = USNOB1;
					Read_NOMAD_entry((int)(num2 - 4));
					uSNOB = USNOB1;
					if (num == 0)
					{
						num = uSNOB;
					}
					Read_NOMAD_entry((int)(num2 - 3));
					uSNOB2 = USNOB1;
					if (num == 0)
					{
						num = uSNOB2;
					}
					Read_NOMAD_entry((int)(num2 - 2));
					uSNOB3 = USNOB1;
					if (num == 0)
					{
						num = uSNOB3;
					}
					Read_NOMAD_entry((int)(num2 - 1));
					uSNOB4 = USNOB1;
					if (num == 0)
					{
						num = uSNOB4;
					}
					if (uSNOB > num)
					{
						num = uSNOB;
					}
					if (uSNOB2 > num)
					{
						num = uSNOB2;
					}
					if (uSNOB3 > num)
					{
						num = uSNOB3;
					}
					if (uSNOB4 > num)
					{
						num = uSNOB4;
					}
					Close_Nomad_Catalogue_and_Index_Files();
				}
			}
			streamWriter.Close();
		}
	}
}
