using System;
using System.IO;
using System.Text;

namespace Occult
{
	internal class Tycho2
	{
		private const double Radian = 180.0 / Math.PI;

		private const double MilliSecInDeg = 3600000.0;

		private static double rA;

		private static double dec;

		private static double parallax;

		private static double mv;

		private static double mp;

		private static double rV;

		private static double pmRA;

		private static double pmDec;

		private static int sDev_RA;

		private static int sDev_Dec;

		private static int sDev_pmRA;

		private static int sDev_pmDec;

		private static int tRA;

		private static int tDec;

		private static int catID;

		private static int catNumber;

		private static int recordLength;

		private static StringBuilder TG;

		public static double RA => rA / (180.0 / Math.PI);

		public static double RAdeg => rA;

		public static double Dec => dec / (180.0 / Math.PI);

		public static double Decdeg => dec;

		public static double PMRA => pmRA / 3600.0 / (180.0 / Math.PI);

		public static double PMRAdeg => pmRA / 3600.0;

		public static double PMDec => pmDec / 3600.0 / (180.0 / Math.PI);

		public static double PMDecdeg => pmDec / 3600.0;

		public static double Parallax_asec => parallax;

		public static int SDev_RA => sDev_RA;

		public static int SDev_Dec => sDev_Dec;

		public static int SDev_pmRA => sDev_pmRA;

		public static int SDev_pmDec => sDev_pmDec;

		public static int TRA => tRA / 100;

		public static int TDec => tDec / 100;

		public static double MagV => mv;

		public static double MagB => mp;

		public double RadialVelocityKmSec => rV;

		public static int StarNumber => catNumber;

		public static string StarID => CurrentStarID();

		public static bool Tycho_from_UCAC => (catID & 0x80) >= 128;

		public static int RecordLength
		{
			get
			{
				return recordLength;
			}
			set
			{
				recordLength = value;
			}
		}

		public static string Tycho2_ASCII_line
		{
			get
			{
				TG = new StringBuilder();
				TG.Append(CurrentStarID().PadRight(20));
				TG.AppendFormat("{0,5:F2}  ", mv);
				if (catID != 130)
				{
					TG.AppendFormat("{0,5:F2}    ", mp);
				}
				else
				{
					TG.Append("".PadRight(9));
				}
				TG.Append(Utilities.DEGtoDMS(rA / 15.0, 2, 4, MinutesOnly: false));
				TG.AppendFormat(" {0,8:F5} ", pmRA / 15.0);
				TG.AppendFormat(" {0,6:F3}", (double)SDev_RA / 1000.0);
				TG.AppendFormat(" {0,7:F4}", (double)SDev_pmRA / 10000.0);
				TG.AppendFormat(" {0,7:F2}    ", TRA + 2000);
				TG.Append(Utilities.DEGtoDMS(dec, 3, 3, MinutesOnly: false));
				TG.AppendFormat(" {0,8:F4} ", pmDec);
				TG.AppendFormat(" {0,6:F3}", (double)SDev_Dec / 1000.0);
				TG.AppendFormat(" {0,7:F4}", (double)SDev_pmDec / 10000.0);
				TG.AppendFormat(" {0,7:F2}", TDec + 2000);
				if (rV != 0.0)
				{
					TG.AppendFormat("  {0,6:F1}", rV);
				}
				else
				{
					TG.Append("".PadRight(8));
				}
				if ((catID == 2) | (parallax != 0.0))
				{
					TG.AppendFormat("  {0,7:F4}", parallax);
				}
				return TG.ToString();
			}
		}

		public static double ErrorRA_arcsecs(double T)
		{
			if (RecordLength == 27)
			{
				if (catID == 130)
				{
					return 0.03;
				}
				if (catID == 135)
				{
					return 0.03;
				}
				if (catID > 7)
				{
					return 0.09;
				}
				if (catID == 2)
				{
					return 0.01;
				}
				return 0.09;
			}
			double num = (double)SDev_RA / 1000.0;
			double num2 = (double)SDev_pmRA / 10000.0 * (T - 2000.0 - (double)TRA);
			return Math.Sqrt(num * num + num2 * num2);
		}

		public static double ErrorDec_arcsecs(double T)
		{
			if (RecordLength == 27)
			{
				if (catID == 130)
				{
					return 0.03;
				}
				if (catID == 135)
				{
					return 0.03;
				}
				if (catID > 7)
				{
					return 0.09;
				}
				if (catID == 2)
				{
					return 0.01;
				}
				return 0.09;
			}
			double num = (double)SDev_Dec / 1000.0;
			double num2 = (double)SDev_pmDec / 10000.0 * (T - 2000.0 - (double)TDec);
			return Math.Sqrt(num * num + num2 * num2);
		}

		internal static bool ReadNext(FileStream Tycho2File, BinaryReader Tycho2, int RecordNumber)
		{
			if (RecordNumber < 0)
			{
				return false;
			}
			int num = RecordNumber * RecordLength;
			if (num + RecordLength > Tycho2File.Length)
			{
				return false;
			}
			Tycho2File.Seek(num, SeekOrigin.Begin);
			if (RecordLength == 27)
			{
				rA = (double)Tycho2.ReadInt32() / 10000000.0 + 180.0;
				pmRA = Tycho2.ReadSingle();
				dec = (double)Tycho2.ReadInt32() / 10000000.0;
				pmDec = Tycho2.ReadSingle();
				parallax = (double)Tycho2.ReadInt16() / 10000.0;
				mv = (double)Tycho2.ReadInt16() / 100.0;
				mp = (double)Tycho2.ReadInt16() / 100.0;
				catID = Tycho2.ReadByte();
				catNumber = Tycho2.ReadInt32();
				sDev_RA = (sDev_Dec = (sDev_pmRA = (sDev_pmDec = 0)));
				rV = 0.0;
				tRA = (tDec = 0);
			}
			else
			{
				rA = (double)Tycho2.ReadInt32() / 3600000.0;
				dec = (double)Tycho2.ReadInt32() / 3600000.0;
				pmRA = (double)Tycho2.ReadInt32() / Math.Cos(dec / (180.0 / Math.PI)) / 10000.0;
				pmDec = (double)Tycho2.ReadInt32() / 10000.0;
				parallax = (double)Tycho2.ReadInt16() / 10000.0;
				mv = (double)Tycho2.ReadInt16() / 100.0;
				mp = (double)Tycho2.ReadInt16() / 100.0;
				rV = (double)Tycho2.ReadInt16() / 10.0;
				sDev_RA = Tycho2.ReadInt16();
				sDev_Dec = Tycho2.ReadInt16();
				sDev_pmRA = Tycho2.ReadInt16();
				sDev_pmDec = Tycho2.ReadInt16();
				tRA = Tycho2.ReadInt16();
				tDec = Tycho2.ReadInt16();
				catID = Tycho2.ReadByte();
				catNumber = Tycho2.ReadInt32();
			}
			return true;
		}

		private static string CurrentStarID()
		{
			string text = "PPM HIP TYC TAC CMC ACRS";
			if (catID == 130)
			{
				return "2UCAC " + catNumber.ToString().PadLeft(8, '0');
			}
			if (catID == 135)
			{
				uint num = (uint)catNumber;
				return "1B " + (num >> 21) + "-" + (num & 0x1FFFFFu);
			}
			if (catID > 7)
			{
				string text2 = "";
				if ((catID & 0x10) > 0)
				{
					text2 = "p";
				}
				if ((catID & 0x20) > 0)
				{
					text2 += "s";
				}
				if ((catID & 0x40) > 0)
				{
					text2 += "d";
				}
				if ((catID & 0x80) > 0)
				{
					text2 += "u";
				}
				string text3 = catNumber.ToString().PadLeft(9);
				return "TYC " + text3.Substring(0, 4) + "-" + text3.Substring(4, 5) + "-" + Convert.ToString((catID & 7) + 1) + text2;
			}
			if (catID == 7)
			{
				return "X   " + catNumber.ToString().PadLeft(6);
			}
			if ((catID < 3) | (catID > 4))
			{
				return text.Substring(4 * (catID - 1), 4) + catNumber;
			}
			int num2 = Math.Abs(catNumber);
			int num3 = (int)Math.Floor((double)num2 / 100000.0);
			int num4 = num2 - num3 * 100000;
			if (catID == 3)
			{
				return "TYC " + num3.ToString().PadLeft(4) + "-" + num4.ToString().PadLeft(5, '0');
			}
			string text4 = "TAC +";
			if (catNumber < 0)
			{
				text4 = "TAC -";
			}
			return text4 + num3.ToString().PadLeft(2, '0') + " " + num4.ToString().PadLeft(5, '0');
		}

		internal static bool GetHIPfromTycho2(int Hip)
		{
			bool result = false;
			if (Hip > 118322 || Hip < 1)
			{
				return result;
			}
			FileStream fileStream = new FileStream(Utilities.AppPath + "\\Resource Files\\Hipparcos Index.dat", FileMode.Open, FileAccess.Read);
			BinaryReader binaryReader = new BinaryReader(fileStream);
			fileStream.Seek(4 * (Hip - 1), SeekOrigin.Begin);
			int num = binaryReader.ReadInt32();
			binaryReader.Close();
			FileStream fileStream2 = new FileStream(Utilities.AppPath + "\\Resource Files\\Tycho2.bin", FileMode.Open, FileAccess.Read);
			if (fileStream2.Length > 99000000)
			{
				RecordLength = 41;
			}
			else
			{
				RecordLength = 27;
			}
			BinaryReader binaryReader2 = new BinaryReader(fileStream2);
			result = ReadNext(fileStream2, binaryReader2, num - 1);
			binaryReader2.Close();
			return result;
		}

		internal static bool GetTycho2fromTycho2(int Region, int SeqNum, int Component)
		{
			int num = Region * 100000 + SeqNum;
			bool flag = false;
			if (Region > 9537 || Region < 1)
			{
				return false;
			}
			FileStream fileStream = new FileStream(Utilities.AppPath + "\\Resource Files\\GSC Fields.dat", FileMode.Open, FileAccess.Read);
			BinaryReader binaryReader = new BinaryReader(fileStream);
			fileStream.Seek(16 * (Region - 1), SeekOrigin.Begin);
			double num2 = binaryReader.ReadSingle();
			double num3 = binaryReader.ReadSingle();
			double num4 = binaryReader.ReadSingle();
			double num5 = binaryReader.ReadSingle();
			binaryReader.Close();
			int num6 = (int)(num4 + 0.01);
			if (num6 < 0)
			{
				num6--;
			}
			if (num6 < -89)
			{
				num6 = -89;
			}
			int num7 = (int)(num5 - 0.01);
			if (num7 < 0)
			{
				num7--;
			}
			if (num7 < -89)
			{
				num7 = -89;
			}
			FileStream fileStream2 = new FileStream(Utilities.AppPath + "\\Resource Files\\Tycho2.inx", FileMode.Open, FileAccess.Read);
			BinaryReader binaryReader2 = new BinaryReader(fileStream2);
			FileStream fileStream3 = new FileStream(Utilities.AppPath + "\\Resource Files\\Tycho2.bin", FileMode.Open, FileAccess.Read);
			if (fileStream3.Length > 99000000)
			{
				RecordLength = 41;
			}
			else
			{
				RecordLength = 27;
			}
			BinaryReader binaryReader3 = new BinaryReader(fileStream3);
			for (int i = num7; i <= num6; i++)
			{
				fileStream2.Seek(4 * (361 * (89 - i) + (int)num2 - 1), SeekOrigin.Begin);
				int num8 = binaryReader2.ReadInt32();
				fileStream2.Seek(4 * (361 * (89 - i) + (int)num3 + 360 * Convert.ToInt32(num3 == 0.0) + 2), SeekOrigin.Begin);
				int num9 = binaryReader2.ReadInt32();
				for (int j = num8; j <= num9; j++)
				{
					ReadNext(fileStream3, binaryReader3, j);
					if (catID > 7 && catNumber == num && (catID & 7) + 1 == Component)
					{
						flag = true;
						break;
					}
				}
				if (flag)
				{
					break;
				}
			}
			binaryReader2.Close();
			binaryReader3.Close();
			return flag;
		}

		internal static bool GetUCACfromTycho2(int U2)
		{
			bool result = false;
			if (U2 > 48330571)
			{
				return result;
			}
			FileStream fileStream = new FileStream(Utilities.AppPath + "\\Resource Files\\UCAC2 Index.dat", FileMode.Open, FileAccess.Read);
			BinaryReader binaryReader = new BinaryReader(fileStream);
			int num = (int)fileStream.Length / 12;
			int num2 = 1;
			int num3 = 1;
			for (int i = 0; i < num; i++)
			{
				if (binaryReader.ReadInt32() >= U2)
				{
					break;
				}
				num2 = binaryReader.ReadInt32();
				num3 = binaryReader.ReadInt32();
			}
			binaryReader.Close();
			FileStream fileStream2 = new FileStream(Utilities.AppPath + "\\Resource Files\\Tycho2.bin", FileMode.Open, FileAccess.Read);
			if (fileStream2.Length > 99000000)
			{
				RecordLength = 41;
			}
			else
			{
				RecordLength = 27;
			}
			BinaryReader binaryReader2 = new BinaryReader(fileStream2);
			for (int j = num2; j <= num3; j++)
			{
				ReadNext(fileStream2, binaryReader2, j);
				if (catID == 130 && catNumber == U2)
				{
					result = true;
					break;
				}
			}
			binaryReader2.Close();
			return result;
		}
	}
}
