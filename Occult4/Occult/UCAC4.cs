using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Text;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult
{
	internal class UCAC4
	{
		private const double Radian = 180.0 / Math.PI;

		private const double MilliSecInDeg = 3600000.0;

		private const double MilliSecInRadian = 648000000.0 / Math.PI;

		private const double Deg90 = Math.PI / 2.0;

		private static string UCAC4Path;

		private static string Path;

		private const int U4RecordLength = 78;

		private static int ra2000;

		private static int Dec2000;

		private static int sDev_RA;

		private static int sDev_Dec;

		private static int pmRA;

		private static int pmDec;

		private static int sDev_pmRA;

		private static int sDev_pmDec;

		private static int tRA;

		private static int tDec;

		private static int milliMagU4Model;

		private static int milliMagU4Aperture;

		private static int U4MagError;

		private static int millimagAPAS_B;

		private static int millimagAPAS_V;

		private static int millimagAPAS_g;

		private static int millimagAPAS_r;

		private static byte doubleStarFlag;

		private static byte objectType;

		private static byte numImages;

		private static byte numImagesUsed;

		private static byte numCats;

		private static byte galaxyFlag;

		private static byte massExtendedFlag;

		private static int CurrentZone;

		private static int UCAC4SeqNumber;

		private static int mPOSNumber;

		private static int hipNumber;

		private static int catMatchFlag;

		private static int in_T2HipFK;

		private static int u2Zone;

		private static int u2SeqNumber;

		private static double parallax_mas = 0.0;

		private static double HipMagV = 0.0;

		private static double HipMagB = 0.0;

		private static string tycho2Num = "";

		private static StringBuilder N;

		private static FileStream StarCatFileStream;

		private static FileStream IndexFileStream;

		private static BinaryReader UCAC4File;

		internal static int[] hpmStars = new int[32]
		{
			1, 2, 200157, 200194, 200198, 200267, 200464, 200581, 200610, 200922,
			200925, 201089, 201320, 201347, 201781, 202050, 202089, 202127, 202235, 202464,
			13985153, 80118783, 82273908, 82389220, 82390542, 93157181, 106363470, 108259698, 110586703, 110589580,
			113038183, 124167517
		};

		private static FileStream[] Fstream = new FileStream[4];

		private static BinaryReader[] Fread = new BinaryReader[4];

		private static int[] LastRecordNum = new int[4];

		private static int UCACSthZone = 0;

		private static int UCACNthZone = 0;

		private static int NumZones = 0;

		public static int MPOSNumber => mPOSNumber;

		public static double RAdeg => (double)ra2000 / 3600000.0;

		public static double RA => (double)ra2000 / (648000000.0 / Math.PI);

		public static double Dec_deg => (double)Dec2000 / 3600000.0;

		public static double Dec => (double)Dec2000 / (648000000.0 / Math.PI);

		public static double SDevRA_mas => sDev_RA;

		public static double SDev_Dec_mas => sDev_Dec;

		public static double SDev_pmRA_masyr => (double)sDev_pmRA / 10.0;

		public static double SDev_pmDec_masyr => (double)sDev_pmDec / 10.0;

		public static double PM_ra => (double)pmRA / (648000000.0 / Math.PI) / Math.Cos(Dec) / 10.0;

		public static double PM_ra_deg => (double)pmRA / 3600000.0 / Math.Cos(Dec) / 10.0;

		public static double PM_dec => (double)pmDec / (648000000.0 / Math.PI) / 10.0;

		public static double PM_dec_deg => (double)pmDec / 3600000.0 / 10.0;

		public static double Parallax_mas => parallax_mas;

		public static bool DoubtfulObject => (objectType == 8) | (objectType == 9);

		public static double Mag
		{
			get
			{
				if (HipMagV != 0.0)
				{
					return HipMagV;
				}
				if ((millimagAPAS_V < 20000) & (Math.Abs(millimagAPAS_V - milliMagU4Model) < 1000))
				{
					return (double)millimagAPAS_V / 1000.0;
				}
				if (millimagAPAS_V < 20000)
				{
					return (double)millimagAPAS_V / 1000.0;
				}
				if (milliMagU4Model < 20000)
				{
					return (double)milliMagU4Model / 1000.0;
				}
				if (milliMagU4Aperture < 20000)
				{
					return (double)milliMagU4Aperture / 1000.0;
				}
				if (millimagAPAS_B < 20000)
				{
					return (double)millimagAPAS_B / 1000.0;
				}
				if (millimagAPAS_g < 20000)
				{
					return (double)millimagAPAS_g / 1000.0;
				}
				if (millimagAPAS_r < 20000)
				{
					return (double)millimagAPAS_r / 1000.0;
				}
				return (double)milliMagU4Model / 1000.0;
			}
		}

		public static double Mag_Brightest
		{
			get
			{
				double num = 20000.0;
				if (HipMagV != 0.0)
				{
					num = HipMagV * 1000.0;
				}
				if ((double)millimagAPAS_V < num)
				{
					num = millimagAPAS_V;
				}
				if ((double)milliMagU4Model < num)
				{
					num = milliMagU4Model;
				}
				if ((double)milliMagU4Aperture < num)
				{
					num = milliMagU4Aperture;
				}
				if ((double)millimagAPAS_B < num)
				{
					num = millimagAPAS_B;
				}
				if ((double)millimagAPAS_g < num)
				{
					num = millimagAPAS_g;
				}
				if ((double)millimagAPAS_r < num)
				{
					num = millimagAPAS_r;
				}
				return num / 1000.0;
			}
		}

		public static double MagB
		{
			get
			{
				if (HipMagB != 0.0)
				{
					return HipMagB;
				}
				if (millimagAPAS_B < 20000)
				{
					return (double)millimagAPAS_B / 1000.0;
				}
				return (double)milliMagU4Model / 1000.0;
			}
		}

		public static double MagV
		{
			get
			{
				if (HipMagV != 0.0)
				{
					return HipMagV;
				}
				if (millimagAPAS_V < 20000)
				{
					return (double)millimagAPAS_V / 1000.0;
				}
				return (double)milliMagU4Model / 1000.0;
			}
		}

		public static double MagR
		{
			get
			{
				if (millimagAPAS_r < 20000)
				{
					return (double)millimagAPAS_r / 1000.0;
				}
				return (double)milliMagU4Model / 1000.0;
			}
		}

		public static double MagAPAS_B => (double)millimagAPAS_B / 1000.0;

		public static double MagAPAS_V => (double)millimagAPAS_V / 1000.0;

		public static double MagAPAS_r => (double)millimagAPAS_r / 1000.0;

		public static int DoubleStarFlag => Convert.ToInt16(doubleStarFlag);

		public static int ObjectType => Convert.ToInt16(objectType);

		public static bool NonStellar => (galaxyFlag > 0) | (massExtendedFlag > 0);

		public static int NumCats => Convert.ToInt16(numCats);

		public static int CatMatchFlag => catMatchFlag;

		public static int U2Zone => u2Zone;

		public static int U2SequenceNumber => u2SeqNumber;

		public static string U2Number => U2Zone.ToString().PadLeft(3, '0') + "-" + U2SequenceNumber.ToString().PadLeft(6, '0');

		public static string UCACnumber
		{
			get
			{
				if (hipNumber > 0)
				{
					return "HIP " + hipNumber;
				}
				if (tycho2Num.Length > 2)
				{
					return "TYC " + tycho2Num;
				}
				return "UCAC4 " + CurrentZone.ToString().PadLeft(3, '0') + "-" + UCAC4SeqNumber.ToString().PadLeft(6, '0');
			}
		}

		public static int U4Zone => CurrentZone;

		public static int U4SeqNum => UCAC4SeqNumber;

		public static string Raw_UCACnumber => UCAC4SeqNumber.ToString().PadLeft(6, '0');

		public static string HipNum
		{
			get
			{
				if (hipNumber > 0)
				{
					return "HIP " + hipNumber;
				}
				return "";
			}
		}

		public static int HipNumber
		{
			get
			{
				if (hipNumber > 0)
				{
					return hipNumber;
				}
				return 0;
			}
		}

		public static string Tycho2Num
		{
			get
			{
				if (in_T2HipFK > 0)
				{
					return "Tyc " + tycho2Num;
				}
				return "";
			}
		}

		public static string UCAC4_ASCII_line
		{
			get
			{
				N = new StringBuilder();
				N.Append(("4UC" + CurrentZone.ToString().PadLeft(3, '0') + "-" + UCAC4SeqNumber.ToString().PadLeft(6, '0')).PadRight(16));
				if (Mag != 20.0)
				{
					N.AppendFormat("{0,5:F2} ", Mag);
				}
				else
				{
					N.Append("      ");
				}
				if (MagAPAS_B != 20.0)
				{
					N.AppendFormat("{0,5:F2} ", MagAPAS_B);
				}
				else
				{
					N.Append("      ");
				}
				if (MagAPAS_r != 20.0)
				{
					N.AppendFormat("{0,5:F2}", MagAPAS_r);
				}
				else
				{
					N.Append("     ");
				}
				switch ((int)doubleStarFlag / 10)
				{
				case 0:
					N.Append("   ");
					break;
				case 1:
					N.Append(" A ");
					break;
				case 2:
					N.Append(" B ");
					break;
				case 3:
					N.Append(" M ");
					break;
				default:
					N.Append(" ? ");
					break;
				}
				if (NonStellar)
				{
					N.Append("g ");
				}
				else
				{
					N.Append("  ");
				}
				if (numCats > 0)
				{
					N.AppendFormat("{0,2:F0} ", numCats);
				}
				else
				{
					N.Append("   ");
				}
				N.Append(Utilities.DEGtoDMS((double)ra2000 / 3600000.0 / 15.0, 2, 4, MinutesOnly: false));
				N.AppendFormat(" {0,8:F5} ", (double)pmRA / Math.Cos(Dec) / 150000.0);
				N.AppendFormat("  {0,6:F3}", (double)sDev_RA / 1000.0);
				N.AppendFormat(" {0,7:F4}", (double)sDev_pmRA / 10000.0);
				N.AppendFormat(" {0,7:F2}    ", (double)tRA / 100.0 + 1900.0);
				N.Append(Utilities.DEGtoDMS((double)Dec2000 / 3600000.0, 3, 3, MinutesOnly: false));
				N.AppendFormat(" {0,8:F4} ", (double)pmDec / 10000.0);
				N.AppendFormat("  {0,6:F3}", (double)sDev_Dec / 1000.0);
				N.AppendFormat(" {0,7:F4}", (double)sDev_pmDec / 10000.0);
				N.AppendFormat(" {0,7:F2}", (double)tDec / 100.0 + 1900.0);
				if (hipNumber > 0)
				{
					N.AppendFormat(" {0,7:F4}", Parallax_mas / 1000.0);
					N.AppendFormat(" {0,6:F0}", hipNumber);
				}
				else
				{
					N.Append("".PadRight(15));
				}
				if (tycho2Num.Length > 2)
				{
					N.Append(" " + tycho2Num);
				}
				else
				{
					N.Append("".PadRight(13));
				}
				N.Append("  " + mPOSNumber.ToString().PadLeft(9));
				if (U2Zone > 0)
				{
					N.Append(U2Number.ToString().PadLeft(10));
				}
				return N.ToString();
			}
		}

		public static double ErrorRA_arcsecs(double T)
		{
			double num = (double)sDev_RA / 1000.0;
			double num2 = (double)sDev_pmRA / 10000.0 * (T - (double)tRA / 100.0 - 1900.0);
			return Math.Sqrt(num * num + num2 * num2);
		}

		public static double ErrorDec_arcsecs(double T)
		{
			double num = (double)sDev_Dec / 1000.0;
			double num2 = (double)sDev_pmDec / 10000.0 * (T - (double)tDec / 100.0 - 1900.0);
			return Math.Sqrt(num * num + num2 * num2);
		}

		public static void InitialiseUCAC4()
		{
			UCAC4Path = Settings.Default.UCAC4_Path;
		}

		public static void Open_UCAC4_Catalogue(int DecZone)
		{
			if (DecZone < 901)
			{
				CurrentZone = DecZone;
				Path = UCAC4Path + "\\u4b\\z" + DecZone.ToString().PadLeft(3, '0');
				StarCatFileStream = new FileStream(Path, FileMode.Open, FileAccess.Read);
				UCAC4File = new BinaryReader(StarCatFileStream);
			}
		}

		public static void GetUCAC4Index(int DecZone, int RAstep1440, out int StartRecordNum)
		{
			using (IndexFileStream = new FileStream(UCAC4Path + "\\u4i\\u4index.unf", FileMode.Open, FileAccess.Read))
			{
				BinaryReader binaryReader = new BinaryReader(IndexFileStream);
				IndexFileStream.Seek(4 * (DecZone - 1) + 3600 * RAstep1440, SeekOrigin.Begin);
				StartRecordNum = binaryReader.ReadInt32();
			}
		}

		public static void GetUCAC4IndexAndBin(int DecZone, int RAstep1440, out int StartRecordNum, out int NumInBin)
		{
			using (IndexFileStream = new FileStream(UCAC4Path + "\\u4i\\u4index.unf", FileMode.Open, FileAccess.Read))
			{
				BinaryReader binaryReader = new BinaryReader(IndexFileStream);
				IndexFileStream.Seek(4 * (DecZone - 1) + 3600 * RAstep1440, SeekOrigin.Begin);
				StartRecordNum = binaryReader.ReadInt32();
				IndexFileStream.Seek(5184000 + 4 * (DecZone - 1) + 3600 * RAstep1440, SeekOrigin.Begin);
				NumInBin = binaryReader.ReadInt32();
			}
		}

		internal static bool Read_UCAC4_entry(int Record, bool UseHipparcosForParallax)
		{
			if (Record < 0)
			{
				return false;
			}
			UCAC4SeqNumber = Record + 1;
			try
			{
				StarCatFileStream.Seek(78 * Record, SeekOrigin.Begin);
				ra2000 = UCAC4File.ReadInt32();
				Dec2000 = UCAC4File.ReadInt32() - 324000000;
				milliMagU4Model = UCAC4File.ReadInt16();
				milliMagU4Aperture = UCAC4File.ReadInt16();
				U4MagError = UCAC4File.ReadByte();
				objectType = UCAC4File.ReadByte();
				doubleStarFlag = UCAC4File.ReadByte();
				sDev_RA = UCAC4File.ReadSByte() + 128;
				sDev_Dec = UCAC4File.ReadSByte() + 128;
				numImages = UCAC4File.ReadByte();
				numImagesUsed = UCAC4File.ReadByte();
				numCats = UCAC4File.ReadByte();
				tRA = UCAC4File.ReadInt16();
				tDec = UCAC4File.ReadInt16();
				pmRA = UCAC4File.ReadInt16();
				pmDec = UCAC4File.ReadInt16();
				sDev_pmRA = CorrectLargeUncertainties(UCAC4File.ReadSByte() + 128);
				sDev_pmDec = CorrectLargeUncertainties(UCAC4File.ReadSByte() + 128);
				StarCatFileStream.Seek(78 * Record + 46, SeekOrigin.Begin);
				millimagAPAS_B = UCAC4File.ReadInt16();
				millimagAPAS_V = UCAC4File.ReadInt16();
				millimagAPAS_g = UCAC4File.ReadInt16();
				millimagAPAS_r = UCAC4File.ReadInt16();
				StarCatFileStream.Seek(78 * Record + 62, SeekOrigin.Begin);
				catMatchFlag = UCAC4File.ReadInt32();
				in_T2HipFK = (catMatchFlag - catMatchFlag % 100000000) / 100000000;
				galaxyFlag = UCAC4File.ReadByte();
				massExtendedFlag = UCAC4File.ReadByte();
				mPOSNumber = UCAC4File.ReadInt32();
				u2Zone = UCAC4File.ReadInt16();
				u2SeqNumber = UCAC4File.ReadInt32();
				if ((pmRA == 32767) | (pmDec == 32767))
				{
					GetHPMvalues(mPOSNumber);
				}
				parallax_mas = 0.0;
				hipNumber = 0;
				HipMagV = 0.0;
				HipMagB = 0.0;
				if ((mPOSNumber > 200000) & (mPOSNumber < 321641))
				{
					GetParallax(mPOSNumber, UseHipparcosForParallax);
				}
				if (in_T2HipFK > 0)
				{
					Tycho2_from_U4(CurrentZone, UCAC4SeqNumber, out tycho2Num);
				}
				else
				{
					tycho2Num = "";
				}
			}
			catch
			{
				return false;
			}
			return true;
		}

		internal static int CorrectLargeUncertainties(int uncertainty)
		{
			if (uncertainty < 251)
			{
				return uncertainty;
			}
			return uncertainty switch
			{
				251 => 275, 
				252 => 325, 
				253 => 375, 
				254 => 425, 
				255 => 500, 
				_ => uncertainty, 
			};
		}

		internal static void GetHPMvalues(int MPOS)
		{
			int num = -1;
			_ = new char[20];
			for (int i = 0; i < hpmStars.GetUpperBound(0); i++)
			{
				if (MPOS == hpmStars[i])
				{
					num = i;
					break;
				}
			}
			if (num < 0)
			{
				return;
			}
			using FileStream fileStream = new FileStream(UCAC4Path + "\\u4i\\u4hpm.dat", FileMode.Open, FileAccess.Read);
			using BinaryReader binaryReader = new BinaryReader(fileStream);
			fileStream.Seek((long)num * 65L + 20, SeekOrigin.Begin);
			char[] value = binaryReader.ReadChars(16);
			pmRA = int.Parse(new string(value, 0, 8));
			pmDec = int.Parse(new string(value, 8, 8));
		}

		internal static void GetParallax(int MPos, bool UseHipparcos)
		{
			char[] array = new char[188];
			using FileStream fileStream = new FileStream(UCAC4Path + "\\u4i\\hipsupl.dat", FileMode.Open, FileAccess.Read);
			using BinaryReader binaryReader = new BinaryReader(fileStream);
			try
			{
				fileStream.Seek(189L * (long)(MPos - 200001), SeekOrigin.Begin);
				array = binaryReader.ReadChars(188);
				double num = double.Parse(new string(array, 33, 12)) * 15.0 * 3600.0 * 1000.0;
				if (num != 0.0)
				{
					hipNumber = int.Parse(new string(array, 17, 6));
					parallax_mas = double.Parse(new string(array, 60, 7));
					double num2 = double.Parse(new string(array, 145, 8));
					double num3 = double.Parse(new string(array, 169, 6));
					HipMagV = num2 - 0.2964 * num3 + 0.111 * num3 * num3 + 0.0157 * num3 * num3 * num3 + 0.0072;
					HipMagB = HipMagV + num3;
					if (UseHipparcos)
					{
						tRA = (tDec = 9125);
						double num4 = double.Parse(new string(array, 47, 12));
						double num5 = Math.Cos(num4 / (180.0 / Math.PI));
						pmRA = Convert.ToInt32(double.Parse(new string(array, 68, 8)) * 10.0);
						pmDec = Convert.ToInt32(double.Parse(new string(array, 77, 8)) * 10.0);
						ra2000 = Convert.ToInt32(num + 0.875 * (double)pmRA / num5);
						Dec2000 = Convert.ToInt32(num4 * 3600.0 * 1000.0 + 0.875 * (double)pmDec);
						sDev_RA = Convert.ToInt32(double.Parse(new string(array, 86, 6)));
						sDev_Dec = Convert.ToInt32(double.Parse(new string(array, 93, 6)));
						sDev_pmRA = Convert.ToInt32(double.Parse(new string(array, 107, 6)) * 10.0);
						sDev_pmDec = Convert.ToInt32(double.Parse(new string(array, 114, 6)) * 10.0);
					}
				}
			}
			catch
			{
			}
		}

		internal static bool Read_UCAC_Number(int DecZone, int UCACnum, bool UseHipparcos)
		{
			InitialiseUCAC4();
			Open_UCAC4_Catalogue(DecZone);
			bool result = Read_UCAC4_entry(UCACnum - 1, UseHipparcos);
			Close_UCAC4_Catalogue();
			return result;
		}

		public static void Close_UCAC4_Catalogue()
		{
			UCAC4File.Close();
		}

		public static void UCAC4_from_HipNum(int Hip, out int Zone, out int Rec1, out int Rec2, out int Rec3)
		{
			Zone = (Rec1 = (Rec2 = (Rec3 = 0)));
			int num = 0;
			int A = 0;
			int B = 0;
			int num2 = 0;
			int num3 = 1;
			InitialiseUCAC4();
			using FileStream fileStream = new FileStream(UCAC4Path + "\\u4i\\HIPtoU4.bin", FileMode.Open, FileAccess.Read);
			num3 = (int)fileStream.Length / 12;
			int num4 = num3 - 1;
			using BinaryReader binaryReader = new BinaryReader(fileStream);
			int num5;
			do
			{
				num5 = (num4 + num) / 2;
				fileStream.Seek(num5 * 12, SeekOrigin.Begin);
				int num6 = binaryReader.ReadInt32();
				if (num6 == Hip)
				{
					break;
				}
				if (num6 < Hip)
				{
					num = num5 + 1;
				}
				else
				{
					num4 = num5 - 1;
				}
			}
			while (num4 >= num);
			num5 -= 3;
			if (num5 < 0)
			{
				num5 = 0;
			}
			do
			{
				fileStream.Seek(num5 * 12, SeekOrigin.Begin);
				int num6 = binaryReader.ReadInt32();
				if (num6 > Hip)
				{
					break;
				}
				if (num6 == Hip)
				{
					if (Rec1 == 0)
					{
						Zone = binaryReader.ReadInt16();
						Rec1 = binaryReader.ReadInt32();
						A = binaryReader.ReadInt16();
					}
					else if (Rec2 == 0)
					{
						Zone = binaryReader.ReadInt16();
						Rec2 = binaryReader.ReadInt32();
						B = binaryReader.ReadInt16();
						if (B < A)
						{
							Utilities.Swap(ref Rec1, ref Rec2);
							Utilities.Swap(ref A, ref B);
						}
					}
					else if (Rec3 == 0)
					{
						Zone = binaryReader.ReadInt16();
						Rec3 = binaryReader.ReadInt32();
						num2 = binaryReader.ReadInt16();
						if (num2 < B)
						{
							Utilities.Swap(ref Rec2, ref Rec3);
							Utilities.Swap(ref B, ref num2);
						}
						if (B < A)
						{
							Utilities.Swap(ref Rec1, ref Rec2);
							Utilities.Swap(ref A, ref B);
						}
					}
				}
				num5++;
			}
			while (num5 < num3);
		}

		public static bool UCAC4_from_Tycho2(int Tyc1, int Tyc2, int Tyc3, out int U4Zone, out int U4Seq)
		{
			U4Zone = (U4Seq = 0);
			int num = 0;
			if (Tyc3 < 1 || Tyc3 > 3)
			{
				Tyc3 = 1;
			}
			long num2 = Tyc1 * 1000000 + Tyc2 * 10 + Tyc3;
			InitialiseUCAC4();
			using (FileStream fileStream = new FileStream(UCAC4Path + "\\u4i\\Tycho2toU4.bin", FileMode.Open, FileAccess.Read))
			{
				int num3 = (int)fileStream.Length / 9 - 1;
				using BinaryReader binaryReader = new BinaryReader(fileStream);
				do
				{
					int num4 = (num3 + num) / 2;
					fileStream.Seek(num4 * 9, SeekOrigin.Begin);
					long num5 = (long)binaryReader.ReadInt32() * 10L + binaryReader.ReadByte();
					if (num5 == num2)
					{
						int num6 = binaryReader.ReadInt32();
						U4Seq = num6 % 1000000;
						U4Zone = (num6 - U4Seq) / 1000000;
						return true;
					}
					if (num5 < num2)
					{
						num = num4 + 1;
					}
					else
					{
						num3 = num4 - 1;
					}
				}
				while (num3 >= num);
			}
			return false;
		}

		public static bool Tycho2_from_U4(int U4Zone, int U4Seq, out string TycNum)
		{
			TycNum = "";
			int num = 0;
			int num2 = U4Zone * 1000000 + U4Seq;
			InitialiseUCAC4();
			using (FileStream fileStream = new FileStream(UCAC4Path + "\\u4i\\U4toTycho2.bin", FileMode.Open, FileAccess.Read))
			{
				int num3 = (int)fileStream.Length / 9 - 1;
				using BinaryReader binaryReader = new BinaryReader(fileStream);
				do
				{
					int num4 = (num3 + num) / 2;
					fileStream.Seek(num4 * 9, SeekOrigin.Begin);
					int num5 = binaryReader.ReadInt32();
					if (num5 == num2)
					{
						TycNum = binaryReader.ReadInt32().ToString().PadLeft(9);
						int num6 = binaryReader.ReadByte();
						TycNum = TycNum.Substring(0, 4) + "-" + TycNum.Substring(4) + "-" + num6;
						return true;
					}
					if (num5 < num2)
					{
						num = num4 + 1;
					}
					else
					{
						num3 = num4 - 1;
					}
				}
				while (num3 >= num);
			}
			return false;
		}

		internal static void CreateHipXrefFile()
		{
			//IL_0026: Unknown result type (might be due to invalid IL or missing references)
			//IL_0059: Unknown result type (might be due to invalid IL or missing references)
			//IL_005f: Invalid comparison between Unknown and I4
			InitialiseUCAC4();
			if (!File.Exists(UCAC4Path + "\\u4i\\hipsupl.dat"))
			{
				MessageBox.Show("To create the cross-reference files, Occult needs to access the file 'hipsupl.dat'. That\r\nfile should be located in the u4i subdirectory from where you have stored the UCAC4 catalogue.\r\nHowever Occult cannot find that file\r\n\r\nPlease read the Help file topic on UCAC4, to make sure you have copied everything required.", "UCAC4 error", (MessageBoxButtons)0);
			}
			if (File.Exists(UCAC4Path + "\\u4i\\HIPtoU4.bin") && (int)MessageBox.Show("The HiptoU4 xRef file already exists. \r\n\r\nDo you want to re-create the file?", "File exists", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
			{
				return;
			}
			List<U4Hip> list = new List<U4Hip>();
			PBar pBar = new PBar();
			ProgressBar pBarFTP = pBar.pBarFTP;
			int minimum;
			pBar.pBarFTP.set_Value(minimum = 0);
			pBarFTP.set_Minimum(minimum);
			pBar.pBarFTP.set_Maximum(900);
			((Control)pBar).Show();
			((Control)pBar).set_Text("Reading UCAC4 files");
			Cursor.set_Current(Cursors.get_WaitCursor());
			for (int i = 1; i <= 900; i++)
			{
				pBar.pBarFTP.set_Value(i);
				Application.DoEvents();
				Open_UCAC4_Catalogue(i);
				int num = (int)StarCatFileStream.Length / 78;
				for (int j = 0; j < num; j++)
				{
					StarCatFileStream.Seek(78 * j + 68, SeekOrigin.Begin);
					int num2 = UCAC4File.ReadInt32();
					if (num2 >= 200000 && num2 < 400000)
					{
						U4Hip u4Hip = new U4Hip();
						u4Hip.Mpos = num2;
						u4Hip.Zone = i;
						u4Hip.Rec = j + 1;
						u4Hip.Hip = 0;
						StarCatFileStream.Seek(78 * j + 48, SeekOrigin.Begin);
						u4Hip.U4Mag = (double)UCAC4File.ReadInt16() / 1000.0;
						if (u4Hip.U4Mag == 20.0)
						{
							StarCatFileStream.Seek(78 * j + 8, SeekOrigin.Begin);
							u4Hip.U4Mag = (double)UCAC4File.ReadInt16() / 1000.0;
						}
						if (u4Hip.U4Mag == 20.0)
						{
							StarCatFileStream.Seek(78 * j + 10, SeekOrigin.Begin);
							u4Hip.U4Mag = (double)UCAC4File.ReadInt16() / 1000.0;
						}
						if (u4Hip.U4Mag == 20.0)
						{
							StarCatFileStream.Seek(78 * j + 46, SeekOrigin.Begin);
							u4Hip.U4Mag = (double)UCAC4File.ReadInt16() / 1000.0;
						}
						list.Add(u4Hip);
					}
				}
				Close_UCAC4_Catalogue();
				U4Hip.SortByHip = false;
				list.Sort();
			}
			_ = new char[188];
			using (FileStream fileStream = new FileStream(UCAC4Path + "\\u4i\\hipsupl.dat", FileMode.Open, FileAccess.Read))
			{
				using BinaryReader binaryReader = new BinaryReader(fileStream);
				ProgressBar pBarFTP2 = pBar.pBarFTP;
				pBar.pBarFTP.set_Value(minimum = 0);
				pBarFTP2.set_Minimum(minimum);
				pBar.pBarFTP.set_Maximum(list.Count);
				((Control)pBar).set_Text("Matching to Hip stars");
				for (int k = 0; k < list.Count; k++)
				{
					pBar.pBarFTP.set_Value(k);
					Application.DoEvents();
					fileStream.Seek(189L * (long)(list[k].Mpos - 200001), SeekOrigin.Begin);
					hipNumber = int.Parse(new string(binaryReader.ReadChars(188), 17, 6));
					list[k].Hip = hipNumber;
				}
			}
			U4Hip.SortByHip = true;
			U4Hip.IncludemPos = false;
			list.Sort();
			using (FileStream output = new FileStream(UCAC4Path + "\\u4i\\HIPtoU4.bin", FileMode.Create, FileAccess.Write))
			{
				using BinaryWriter binaryWriter = new BinaryWriter(output);
				ProgressBar pBarFTP3 = pBar.pBarFTP;
				pBar.pBarFTP.set_Value(minimum = 0);
				pBarFTP3.set_Minimum(minimum);
				pBar.pBarFTP.set_Maximum(list.Count);
				((Control)pBar).set_Text("Writing xRef file");
				for (int l = 0; l < list.Count; l++)
				{
					pBar.pBarFTP.set_Value(l);
					Application.DoEvents();
					binaryWriter.Write(list[l].Hip);
					binaryWriter.Write((short)list[l].Zone);
					binaryWriter.Write(list[l].Rec);
					binaryWriter.Write((short)(list[l].U4Mag * 10.0));
				}
			}
			((Form)pBar).Close();
			((Component)(object)pBar).Dispose();
			Cursor.set_Current(Cursors.get_Default());
		}

		internal static void Create_U4Tycho2_xref_files()
		{
			//IL_005e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0064: Invalid comparison between Unknown and I4
			InitialiseUCAC4();
			if (!File.Exists(UCAC4Path + "\\u4i\\u4xtycho") || ((File.Exists(UCAC4Path + "\\u4i\\Tycho2toU4.bin") & File.Exists(UCAC4Path + "\\u4i\\U4toTycho2")) && (int)MessageBox.Show("The Tycho xRef files already exists. \r\n\r\nDo you want to re-create these files?", "File exists", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7))
			{
				return;
			}
			List<U4Tyc2> list = new List<U4Tyc2>();
			int num = 0;
			PBar pBar = new PBar();
			ProgressBar pBarFTP = pBar.pBarFTP;
			int minimum;
			pBar.pBarFTP.set_Value(minimum = 0);
			pBarFTP.set_Minimum(minimum);
			pBar.pBarFTP.set_Maximum(2550000);
			((Control)pBar).Show();
			((Control)pBar).set_Text("Reading Tycho2/UCAC4 xref ASCII file");
			Cursor.set_Current(Cursors.get_WaitCursor());
			using (StreamReader streamReader = new StreamReader(UCAC4Path + "\\u4i\\u4xtycho"))
			{
				do
				{
					string text = streamReader.ReadLine();
					U4Tyc2 u4Tyc = new U4Tyc2();
					u4Tyc.Tycho2 = int.Parse(text.Substring(0, 4) + text.Substring(5, 5));
					u4Tyc.T2Component = byte.Parse(text.Substring(11, 1));
					u4Tyc.U4 = int.Parse(text.Substring(13, 9));
					list.Add(u4Tyc);
					num++;
					if (num % 100 == 0)
					{
						pBar.pBarFTP.set_Value(num);
						Application.DoEvents();
					}
				}
				while (!streamReader.EndOfStream);
			}
			using (FileStream output = new FileStream(UCAC4Path + "\\u4i\\U4toTycho2.bin", FileMode.Create, FileAccess.Write))
			{
				using BinaryWriter binaryWriter = new BinaryWriter(output);
				((Control)pBar).set_Text("Writing UCAC4 to Tycho2 xref file");
				for (int i = 0; i < list.Count; i++)
				{
					binaryWriter.Write(list[i].U4);
					binaryWriter.Write(list[i].Tycho2);
					binaryWriter.Write(list[i].T2Component);
					if (i % 100 == 0)
					{
						pBar.pBarFTP.set_Value(i);
						Application.DoEvents();
					}
				}
			}
			((Control)pBar).set_Text("Sorting file");
			Application.DoEvents();
			list.Sort();
			using (FileStream output2 = new FileStream(UCAC4Path + "\\u4i\\Tycho2toU4.bin", FileMode.Create, FileAccess.Write))
			{
				using BinaryWriter binaryWriter2 = new BinaryWriter(output2);
				((Control)pBar).set_Text("Writing Tycho2 to UCAC4 xref file");
				for (int j = 0; j < list.Count; j++)
				{
					binaryWriter2.Write(list[j].Tycho2);
					binaryWriter2.Write(list[j].T2Component);
					binaryWriter2.Write(list[j].U4);
					if (j % 100 == 0)
					{
						pBar.pBarFTP.set_Value(j);
						Application.DoEvents();
					}
				}
			}
			((Form)pBar).Close();
			((Component)(object)pBar).Dispose();
			Cursor.set_Current(Cursors.get_Default());
		}

		internal static void CheckUCAC4_Updated(bool CheckAll)
		{
			//IL_00b0: Unknown result type (might be due to invalid IL or missing references)
			//IL_00b6: Invalid comparison between Unknown and I4
			//IL_00bd: Unknown result type (might be due to invalid IL or missing references)
			//IL_00d0: Unknown result type (might be due to invalid IL or missing references)
			//IL_0104: Unknown result type (might be due to invalid IL or missing references)
			//IL_016c: Unknown result type (might be due to invalid IL or missing references)
			InitialiseUCAC4();
			if (!File.Exists(UCAC4Path + "\\u4b\\z900"))
			{
				return;
			}
			if (!CheckAll)
			{
				Open_UCAC4_Catalogue(449);
				StarCatFileStream.Seek(5002710L, SeekOrigin.Begin);
				byte num = UCAC4File.ReadByte();
				byte b = UCAC4File.ReadByte();
				byte b2 = UCAC4File.ReadByte();
				byte b3 = UCAC4File.ReadByte();
				Close_UCAC4_Catalogue();
				if (num == 214 && b == 247 && b2 == 111 && b3 == 240)
				{
					return;
				}
			}
			if (!File.Exists(UCAC4Path + "\\u4b\\CorrectUCAC4.exe"))
			{
				if ((int)MessageBox.Show("UCAC4 catalogue has not been corrected. \r\n\r\nDo you want to download the program to update the catalogue?", "Download correction file", (MessageBoxButtons)4, (MessageBoxIcon)32) == 7)
				{
					MessageBox.Show("UCAC4 catalogue has not been corrected");
					return;
				}
				if (!Utilities.InternetIsAvailable())
				{
					MessageBox.Show("Correction file cannot be downloaded as there is no Internet connection");
					return;
				}
				if (!http.DownloadHTTP(Settings.Default.OccultServer, "correctucac4.zip", UCAC4Path + "\\u4b\\", unzip: true, gunzip: false, ShowMessages: false))
				{
					MessageBox.Show("Download of correction file failed");
					return;
				}
			}
			Process process = new Process();
			process.StartInfo.CreateNoWindow = true;
			process.StartInfo.WindowStyle = ProcessWindowStyle.Normal;
			process.StartInfo.FileName = UCAC4Path + "\\u4b\\CorrectUCAC4.exe";
			process.StartInfo.WorkingDirectory = UCAC4Path + "\\u4b\\";
			process.Start();
			Application.DoEvents();
			MessageBox.Show("Wait until the update program has finished, then select that window and \r\nhit any key to close that window\r\n\r\nCorrection program written by Bill Gray (Project Pluto) based on Python script prepared by CDS\r\n");
		}

		internal static string UCAC4numFromCoords(double RAstar, double DecStar, double PMyears)
		{
			double Mb;
			double Mv;
			double Mr;
			return UCAC4numFromCoords(RAstar, DecStar, PMyears, out Mb, out Mv, out Mr);
		}

		internal static string UCAC4numFromCoords(double RAstar, double DecStar, double PMyears, out double Mb, out double Mv, out double Mr)
		{
			string result = "";
			Mb = (Mv = (Mr = 20.0));
			double num = 5.817764173314431E-06;
			double num2 = num / Math.Cos(DecStar);
			int decZone = (int)(451.0 + 900.0 / Math.PI * DecStar);
			int rAstep = (int)Math.Floor(RAstar * (180.0 / Math.PI) * 4.0);
			int StartRecordNum = 0;
			int NumInBin = 0;
			InitialiseUCAC4();
			Open_UCAC4_Catalogue(decZone);
			GetUCAC4IndexAndBin(decZone, rAstep, out StartRecordNum, out NumInBin);
			bool flag = false;
			for (int i = 1; i < 3; i++)
			{
				num2 *= (double)i;
				num *= (double)i;
				for (int j = 0; j < NumInBin; j++)
				{
					Read_UCAC4_entry(StartRecordNum + j, UseHipparcosForParallax: false);
					if ((Math.Abs(RA + PM_ra * PMyears - RAstar) < num2) & (Math.Abs(Dec + PM_dec * PMyears - DecStar) < num))
					{
						flag = true;
						result = UCACnumber;
						Mb = MagB;
						Mv = MagV;
						Mr = MagR;
						break;
					}
					if (RA - RAstar > 7E-05)
					{
						break;
					}
				}
				if (flag)
				{
					break;
				}
			}
			Close_UCAC4_Catalogue();
			return result;
		}

		internal static void OpenUCAC_Zones_ForGaia(int GaiaZone_SthLimit_SemiDeg, double GaiaRA_deg_FirstStar)
		{
			int num = 180 + GaiaZone_SthLimit_SemiDeg;
			int num2 = num + 1;
			Math.Floor(GaiaRA_deg_FirstStar * 4.0);
			UCACSthZone = (int)Math.Floor(2.5 * (double)num - 0.001) + 1;
			UCACNthZone = (int)Math.Floor(2.5 * (double)num2 + 0.001) + 1;
			if (UCACSthZone < 1)
			{
				UCACSthZone = 1;
			}
			if (UCACNthZone > 900)
			{
				UCACNthZone = 900;
			}
			NumZones = UCACNthZone - UCACSthZone + 1;
			for (int i = 0; i < NumZones; i++)
			{
				string path = UCAC4Path + "\\u4b\\z" + (UCACSthZone + i).ToString().PadLeft(3, '0');
				Fstream[i] = new FileStream(path, FileMode.Open, FileAccess.Read);
				Fread[i] = new BinaryReader(Fstream[i]);
				LastRecordNum[i] = 0;
			}
		}

		internal static void CloseUCAC_Zones_ForGaia()
		{
			int num = UCACNthZone - UCACSthZone + 1;
			for (int i = 0; i < num; i++)
			{
				if (Fstream[i] != null)
				{
					Fstream[i].Close();
				}
			}
		}

		internal static bool GetProperMotion(double GaiaRA_deg, double GaiaDec_deg, double GaiaMagG, double GaiaEpochRA, double GaiaEpochDec, double UncertRA_mas, double UncertDec_mas, out double PM_RAmas_fromUCAC4, out double PM_Decmas_fromUCAC4, out double UncertPMRA, out double UncertPMDec, out int Cat, out string CatNum)
		{
			double num = 0.0;
			double num2 = 0.0;
			double num3 = 0.0;
			double num4 = 0.0;
			double num5 = 0.0;
			double num6 = 0.0;
			double num7 = 0.0;
			double num8 = 0.0;
			double num9 = 0.0;
			int num10 = -1;
			PM_RAmas_fromUCAC4 = (PM_Decmas_fromUCAC4 = (UncertPMRA = (UncertPMDec = 0.0)));
			Cat = 0;
			CatNum = "";
			int num11 = (int)(451.0 + 5.0 * (GaiaDec_deg - 0.0003));
			if (num11 < 1)
			{
				num11 = 1;
			}
			num11 -= UCACSthZone;
			int num12 = (int)(451.0 + 5.0 * (GaiaDec_deg + 0.0003));
			if (num12 > 900)
			{
				num12 = 900;
			}
			num12 -= UCACSthZone;
			for (int i = num11; i <= num12; i++)
			{
				num10 = -1;
				while (Fstream[i].Position < Fstream[i].Length - 78)
				{
					Read_UCAC4_entry(LastRecordNum[i], i);
					if (GaiaRA_deg - RAdeg > 0.25)
					{
						GetUCAC4Index(UCACSthZone + i, (int)Math.Floor(GaiaRA_deg * 4.0), out LastRecordNum[i]);
						Read_UCAC4_entry(LastRecordNum[i], i);
					}
					num = GaiaEpochRA;
					num2 = GaiaEpochDec;
					num3 = (RAdeg + PM_ra_deg * num - GaiaRA_deg) * Math.Cos(Dec);
					if (num10 == -1 && num3 > -0.00015)
					{
						num10 = LastRecordNum[i];
					}
					if (Math.Abs(Mag - GaiaMagG) < 3.5 && ((num3 < 0.00015) & (Math.Abs(Dec_deg + PM_dec_deg * num2 - GaiaDec_deg) < 0.00015)))
					{
						num6 = (double)tRA / 100.0 - 100.0;
						num7 = (double)tDec / 100.0 - 100.0;
						num4 = RAdeg + PM_ra_deg * num6;
						num5 = Dec_deg + PM_dec_deg * num7;
						num8 = (GaiaRA_deg - num4) / (GaiaEpochRA - num6) * 3600000.0 * Math.Cos(Dec);
						num9 = (GaiaDec_deg - num5) / (GaiaEpochDec - num7) * 3600000.0;
						PM_RAmas_fromUCAC4 = num8;
						PM_Decmas_fromUCAC4 = num9;
						UncertPMRA = Math.Sqrt(UncertRA_mas * UncertRA_mas + (double)(sDev_RA * sDev_RA)) / (GaiaEpochRA - num6);
						UncertPMDec = Math.Sqrt(UncertDec_mas * UncertDec_mas + (double)(sDev_Dec * sDev_Dec)) / (GaiaEpochRA - num7);
						Cat = 4;
						CatNum = (UCACSthZone + i).ToString().PadLeft(3, '0') + "-" + Raw_UCACnumber;
						if (num10 >= 0)
						{
							LastRecordNum[i] = num10;
						}
						return true;
					}
					LastRecordNum[i]++;
					if (!(num3 < 0.003))
					{
						break;
					}
				}
				if (num10 >= 0)
				{
					LastRecordNum[i] = num10;
				}
			}
			return false;
		}

		internal static bool Read_UCAC4_entry(int Record, int FileRefNo)
		{
			if (Record < 0)
			{
				return false;
			}
			UCAC4SeqNumber = Record + 1;
			try
			{
				Fstream[FileRefNo].Seek(78 * Record, SeekOrigin.Begin);
				ra2000 = Fread[FileRefNo].ReadInt32();
				Dec2000 = Fread[FileRefNo].ReadInt32() - 324000000;
				milliMagU4Model = Fread[FileRefNo].ReadInt16();
				milliMagU4Aperture = Fread[FileRefNo].ReadInt16();
				U4MagError = Fread[FileRefNo].ReadByte();
				objectType = Fread[FileRefNo].ReadByte();
				doubleStarFlag = Fread[FileRefNo].ReadByte();
				sDev_RA = Fread[FileRefNo].ReadSByte() + 128;
				sDev_Dec = Fread[FileRefNo].ReadSByte() + 128;
				numImages = Fread[FileRefNo].ReadByte();
				numImagesUsed = Fread[FileRefNo].ReadByte();
				numCats = Fread[FileRefNo].ReadByte();
				tRA = Fread[FileRefNo].ReadInt16();
				tDec = Fread[FileRefNo].ReadInt16();
				pmRA = Fread[FileRefNo].ReadInt16();
				pmDec = Fread[FileRefNo].ReadInt16();
				sDev_pmRA = CorrectLargeUncertainties(Fread[FileRefNo].ReadSByte() + 128);
				sDev_pmDec = CorrectLargeUncertainties(Fread[FileRefNo].ReadSByte() + 128);
				Fstream[FileRefNo].Seek(78 * Record + 46, SeekOrigin.Begin);
				millimagAPAS_B = Fread[FileRefNo].ReadInt16();
				millimagAPAS_V = Fread[FileRefNo].ReadInt16();
				millimagAPAS_g = Fread[FileRefNo].ReadInt16();
				millimagAPAS_r = Fread[FileRefNo].ReadInt16();
				HipMagV = 0.0;
			}
			catch
			{
				return false;
			}
			return true;
		}
	}
}
