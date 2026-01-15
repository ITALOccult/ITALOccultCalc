using System;
using System.IO;
using System.Text;

namespace Occult
{
	public class XZ80Q
	{
		private const double Radian = 180.0 / Math.PI;

		public static string AppPath;

		private static double rA;

		private static double dec;

		private static double pmRA;

		private static double pmDec;

		private static double mv;

		private static double mp;

		private static double mr;

		private static double parallax_rad;

		private static string doubleFlag;

		private static string variableFlag;

		private static string spectrum;

		private static string positionSource = "";

		private static int xZ;

		private static int sAO;

		private static int zC;

		private static byte SpectrumCode;

		private static byte[] XZline = new byte[35];

		public static double RA_rad
		{
			get
			{
				return rA;
			}
			set
			{
				rA = value;
			}
		}

		public static double Dec_rad
		{
			get
			{
				return dec;
			}
			set
			{
				dec = value;
			}
		}

		public static double PMRA_rad
		{
			get
			{
				return pmRA;
			}
			set
			{
				pmRA = value;
			}
		}

		public static double PMDec_rad
		{
			get
			{
				return pmDec;
			}
			set
			{
				pmDec = value;
			}
		}

		public static double Mv => mv;

		public static double Mp => mp;

		public static double Mr => mr;

		public static double Parallax_Rad
		{
			get
			{
				return parallax_rad;
			}
			set
			{
				parallax_rad = value;
			}
		}

		public static int XZ => xZ;

		public static int SAO => sAO;

		public static int ZC => zC;

		public static string DoubleFlag => doubleFlag;

		public static string VariableFlag => variableFlag;

		public static string Spectrum => spectrum;

		public static string PositionSource
		{
			get
			{
				return positionSource;
			}
			set
			{
				positionSource = value;
			}
		}

		public static bool Get_XZ_Star(int XZ)
		{
			if (!Get_XZ_Record(XZ))
			{
				zC = (sAO = (xZ = 0));
				return false;
			}
			return true;
		}

		public static bool Get_ZC_Star(int ZC)
		{
			if (ZC < 1 || ZC > 3539)
			{
				return false;
			}
			FileStream fileStream = new FileStream(AppPath + "\\Resource Files\\zc-xz.dat", FileMode.Open, FileAccess.Read);
			BinaryReader binaryReader = new BinaryReader(fileStream);
			fileStream.Seek(3 * (ZC - 1), SeekOrigin.Begin);
			byte b = binaryReader.ReadByte();
			byte b2 = binaryReader.ReadByte();
			byte b3 = binaryReader.ReadByte();
			int num = 65536 * b + 256 * b2 + b3;
			fileStream.Close();
			if (!Get_XZ_Record(num))
			{
				return false;
			}
			return true;
		}

		public static bool Get_SAO_Star(int SAO)
		{
			int num = 0;
			long num2 = 0L;
			long num3 = 0L;
			long num4 = 0L;
			FileStream fileStream = new FileStream(AppPath + "\\Resource Files\\SAO_xz.dat", FileMode.Open, FileAccess.Read);
			BinaryReader binaryReader = new BinaryReader(fileStream);
			num2 = fileStream.Length / 6 - 1;
			do
			{
				num4 = (num2 + num3) / 2;
				fileStream.Seek(6 * num4, SeekOrigin.Begin);
				byte b = binaryReader.ReadByte();
				byte b2 = binaryReader.ReadByte();
				byte b3 = binaryReader.ReadByte();
				long num5 = 65536 * b + 256 * b2 + b3;
				if (num5 == SAO)
				{
					b = binaryReader.ReadByte();
					b2 = binaryReader.ReadByte();
					b3 = binaryReader.ReadByte();
					num = b + 256 * b2 + 65536 * b3;
					break;
				}
				if (num2 <= num3)
				{
					break;
				}
				if (num5 > SAO)
				{
					num2 = num4 - 1;
					if (num2 < 0)
					{
						num2 = 0L;
					}
				}
				else
				{
					num3 = num4 + 1;
				}
			}
			while (num2 >= 0);
			fileStream.Close();
			if (!Get_XZ_Record(num))
			{
				return false;
			}
			return true;
		}

		internal static string GetStarIDfromXZ(int XZ)
		{
			if (!Get_XZ_Star(XZ))
			{
				return "".PadRight(7);
			}
			if (zC > 0)
			{
				return string.Format("R{0,6:f0}", zC);
			}
			if (sAO > 0)
			{
				return string.Format("S{0,6:f0}", sAO);
			}
			return string.Format("X{0,6:f0}", xZ);
		}

		internal static bool Get_XZ_Record(int XZ)
		{
			if (XZ < 1 || XZ > 244437)
			{
				return false;
			}
			FileStream fileStream = new FileStream(AppPath + "\\Resource Files\\xz80index.dat", FileMode.Open, FileAccess.Read);
			BinaryReader binaryReader = new BinaryReader(fileStream);
			fileStream.Seek(4 * (XZ - 1), SeekOrigin.Begin);
			int recordNumber = binaryReader.ReadInt32();
			fileStream.Close();
			FileStream fileStream2 = new FileStream(AppPath + "\\Resource Files\\xz80.dat", FileMode.Open, FileAccess.Read);
			BinaryReader readXZ = new BinaryReader(fileStream2);
			if (!ReadStarEntry(fileStream2, readXZ, recordNumber))
			{
				fileStream2.Close();
				return false;
			}
			fileStream2.Close();
			return true;
		}

		internal static int Get_XZ_RecordNumber(int XZ)
		{
			if (XZ < 1 || XZ > 244437)
			{
				return 0;
			}
			FileStream fileStream = new FileStream(AppPath + "\\Resource Files\\xz80index.dat", FileMode.Open, FileAccess.Read);
			BinaryReader binaryReader = new BinaryReader(fileStream);
			fileStream.Seek(4 * (XZ - 1), SeekOrigin.Begin);
			int result = binaryReader.ReadInt32();
			fileStream.Close();
			return result;
		}

		internal static bool ReadStarEntry(FileStream XZFile, BinaryReader ReadXZ, int RecordNumber)
		{
			int num = RecordNumber * 35;
			if (num + 35 >= XZFile.Length)
			{
				return false;
			}
			XZFile.Seek(num, SeekOrigin.Begin);
			rA = (double)ReadXZ.ReadInt32() / 10000000.0 / (180.0 / Math.PI) + Math.PI;
			pmRA = (double)ReadXZ.ReadSingle() / (180.0 / Math.PI);
			dec = (double)ReadXZ.ReadInt32() / 10000000.0 / (180.0 / Math.PI);
			pmDec = (double)ReadXZ.ReadSingle() / (180.0 / Math.PI);
			mv = (double)ReadXZ.ReadInt16() / 100.0;
			mp = (double)ReadXZ.ReadInt16() / 100.0;
			mr = (double)ReadXZ.ReadInt16() / 100.0;
			parallax_rad = 0.0;
			SpectrumCode = ReadXZ.ReadByte();
			spectrum = "* WOBAFGKMNRS+".Substring((SpectrumCode & 0xF0) / 16, 1) + "* 0123456789ABC+".Substring(SpectrumCode & 0xF, 1);
			doubleFlag = Convert.ToString(ReadXZ.ReadChar());
			xZ = ReadXZ.ReadInt32();
			sAO = ReadXZ.ReadInt32();
			zC = ReadXZ.ReadInt16();
			variableFlag = Convert.ToString(ReadXZ.ReadChar());
			PositionSource = "XZ80Q";
			if ((mv == 0.0) & (mr != 0.0))
			{
				mv = mr;
			}
			if ((mv == 0.0) & (mp != 0.0))
			{
				mv = mp;
			}
			return true;
		}

		internal static void WriteNext(FileStream XZFile, BinaryWriter WriteXZ, int RecordNumber)
		{
			int num = RecordNumber * 35;
			XZFile.Seek(num, SeekOrigin.Begin);
			WriteXZ.Write(Convert.ToInt32((rA - Math.PI) * (180.0 / Math.PI) * 10000000.0));
			WriteXZ.Write(Convert.ToSingle(pmRA * (180.0 / Math.PI)));
			WriteXZ.Write(Convert.ToInt32(dec * (180.0 / Math.PI) * 10000000.0));
			WriteXZ.Write(Convert.ToSingle(pmDec * (180.0 / Math.PI)));
			WriteXZ.Write(Convert.ToInt16(mv * 100.0));
			WriteXZ.Write(Convert.ToInt16(mp * 100.0));
			WriteXZ.Write(Convert.ToInt16(mr * 100.0));
			WriteXZ.Write(SpectrumCode);
			WriteXZ.Write(Convert.ToChar(doubleFlag));
			WriteXZ.Write(xZ);
			WriteXZ.Write(sAO);
			WriteXZ.Write((short)zC);
			WriteXZ.Write(Convert.ToChar(variableFlag));
		}

		internal static void ReadNextSeriatum(BinaryReader ReadXZ, out float mag, out int ZC)
		{
			XZline = ReadXZ.ReadBytes(35);
			mag = (float)(XZline[16] + 256 * XZline[17]) / 100f;
			ZC = XZline[32] + 256 * XZline[33];
			_ = ZC;
			_ = 0;
		}

		internal static void WriteNextSeriatum(BinaryWriter WriteXZ)
		{
			WriteXZ.Write(XZline);
		}

		internal static void CreateDummyEntry(double RAin, double Decin, double Magin)
		{
			rA = RAin;
			pmRA = 0.0;
			dec = Decin;
			pmDec = 0.0;
			mv = (mp = (mr = Magin));
			spectrum = "  ";
			doubleFlag = " ";
			xZ = 0;
			sAO = 0;
			zC = 0;
			variableFlag = " ";
		}

		internal static bool GetGaiaData(string DoubleFlag)
		{
			double matchDistanceArcSec = 0.4;
			if (DoubleFlag.Trim().Length > 0)
			{
				matchDistanceArcSec = 1.5;
			}
			if (Gaia.Get_GaiaStar_fromGaia(RA_rad * (180.0 / Math.PI), Dec_rad * (180.0 / Math.PI), StarCoords_used_as_ID: false, 0.0, matchDistanceArcSec, Mv, FilterUsingStarMag: true, LimitUsingStarMag: true, out var _))
			{
				RA_rad = Gaia.RA_rad + (0.0 - Gaia.Epoch_2000) * Gaia.PMRA_rad;
				PMRA_rad = Gaia.PMRA_rad;
				Dec_rad = Gaia.Dec_rad + (0.0 - Gaia.Epoch_2000) * Gaia.PMDec_rad;
				PMDec_rad = Gaia.PMDec_rad;
				Parallax_Rad = Gaia.Parallax_rad;
				PositionSource = Gaia.StarSourceCat[Gaia.GaiaVersionOfStar];
				return true;
			}
			return false;
		}

		public new static string ToString()
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.AppendFormat("{0,6:F0}", xZ);
			stringBuilder.AppendFormat("  {0,5:F2}", mv);
			if (mp > 0.0)
			{
				stringBuilder.AppendFormat(" {0,5:F2}", mp);
			}
			else
			{
				stringBuilder.Append("".PadRight(6));
			}
			if (mr > 0.0)
			{
				stringBuilder.AppendFormat(" {0,5:F2} ", mr);
			}
			else
			{
				stringBuilder.Append("".PadRight(7));
			}
			stringBuilder.Append(variableFlag);
			stringBuilder.Append(doubleFlag);
			stringBuilder.AppendFormat(" {0,2}   ", spectrum);
			stringBuilder.Append(Utilities.DEGtoDMS(rA * (180.0 / Math.PI) / 15.0, 2, 4, MinutesOnly: false));
			stringBuilder.AppendFormat(" {0,9:F6}   ", pmRA * (180.0 / Math.PI) / 15.0 * 3600.0);
			stringBuilder.Append(Utilities.DEGtoDMS(dec * (180.0 / Math.PI), 3, 3, MinutesOnly: false));
			stringBuilder.AppendFormat(" {0,9:F5} ", pmDec * (180.0 / Math.PI) * 3600.0);
			if (zC > 0)
			{
				stringBuilder.AppendFormat(" {0,6:F0}", zC);
			}
			else
			{
				stringBuilder.Append("".PadRight(7));
			}
			if (sAO > 0)
			{
				stringBuilder.AppendFormat("   {0,6:F0}", sAO);
			}
			else
			{
				stringBuilder.Append("".PadRight(9));
			}
			return stringBuilder.ToString();
		}
	}
}
