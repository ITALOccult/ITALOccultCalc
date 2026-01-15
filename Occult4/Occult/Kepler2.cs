using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace Occult
{
	internal class Kepler2
	{
		internal static List<KObjects> K2 = new List<KObjects>();

		internal KObjects KObj;

		internal static List<K2_XZ> XZinK2 = new List<K2_XZ>();

		private static readonly string AppPath = Utilities.AppPath;

		private static string XZinKepler2File = Utilities.AppPath + "\\Resource Files\\XZinKepler2.dat";

		private static string Kepler2File = Utilities.AppPath + "\\Resource Files\\Kepler2.dat";

		internal static bool Kepler2DataExists = true;

		private const double Radian = 180.0 / Math.PI;

		internal static int NumKep2Stars = 0;

		private static double[] RA_Begin = new double[20]
		{
			90.08, 166.5, 238.5, 328.5, 50.7, 122.8, 197.3, 279.5, 8.4, 262.7,
			179.32, 252.46, 343.74, 64.37, 153.3, 226.1, 126.43, 195.9, 122.9, 339.5
		};

		private static double[] RA_End = new double[20]
		{
			106.2, 181.4, 254.05, 344.7, 67.4, 137.6, 212.25, 296.43, 24.22, 278.56,
			194.2, 268.72, 359.55, 81.06, 167.22, 241.12, 141.26, 209.9, 137.2, 355.2
		};

		private static double[] Dec_North = new double[20]
		{
			29.0, 8.83, -15.27, -3.27, 26.5, 23.73, -3.95, -15.57, 13.15, -15.5,
			3.41, -16.5, 2.8, 28.57, 14.2, -13.15, 25.5, -0.4, 23.6, 3.8
		};

		private static double[] Dec_South = new double[20]
		{
			14.1, -6.0, -29.53, -18.7, 10.7, 9.8, -18.6, -31.1, -2.66, -29.35,
			-11.45, -31.38, -13.0, 13.01, -0.52, -26.91, 11.49, -15.0, 9.9, -12.1
		};

		internal static bool StarInKepler2(double RA_deg, double Dec_deg)
		{
			return StarInKepler2(RA_deg, Dec_deg, 0);
		}

		internal static bool StarInKepler2(double RA_deg, double Dec_deg, int YearsFrom2015)
		{
			int RecNum;
			return StarInKepler2(RA_deg, Dec_deg, YearsFrom2015, out RecNum);
		}

		internal static bool StarInKepler2(double RA_deg, double Dec_deg, out int RecNum)
		{
			return StarInKepler2(RA_deg, Dec_deg, 0, out RecNum);
		}

		internal static bool StarInKepler2(double RA_deg, double Dec_deg, int YearsFrom2015, out int RecNum)
		{
			double num = (2.0 + Math.Abs((double)YearsFrom2015 / 10.0)) / 3600.0;
			RecNum = 0;
			if (!Kepler2DataExists | (NumKep2Stars < 1))
			{
				return false;
			}
			bool flag = false;
			RecNum = 0;
			for (int i = 0; i < RA_Begin.Length; i++)
			{
				if ((RA_deg > RA_Begin[i]) & (RA_deg < RA_End[i]) & (Dec_deg < Dec_North[i]) & (Dec_deg > Dec_South[i]))
				{
					flag = true;
					break;
				}
			}
			if (!flag)
			{
				return false;
			}
			double num2 = RA_deg - 0.0001;
			double num3 = RA_deg + 0.0001;
			int num4 = 0;
			int num5 = NumKep2Stars - 1;
			do
			{
				RecNum = (num4 + num5) / 2;
				if (num2 == K2[RecNum].RA)
				{
					break;
				}
				if (num2 > K2[RecNum].RA)
				{
					num4 = RecNum + 1;
				}
				else
				{
					num5 = RecNum - 1;
				}
			}
			while (num5 >= num4);
			RecNum -= 50;
			if (RecNum < 0)
			{
				RecNum = 0;
			}
			while (!(K2[RecNum].RA >= num3))
			{
				if ((Math.Abs(K2[RecNum].RA - RA_deg) < num) & (Math.Abs(K2[RecNum].Dec - Dec_deg) < num))
				{
					return true;
				}
				RecNum++;
				if (RecNum >= K2.Count)
				{
					break;
				}
			}
			return false;
		}

		internal static void WriteFullKepler2Cat()
		{
			if (File.Exists(AppPath + "\\Resource Files\\Kepler2.old"))
			{
				File.Delete(AppPath + "\\Resource Files\\Kepler2.old");
			}
			if (File.Exists(AppPath + "\\Resource Files\\Kepler2.dat"))
			{
				File.Move(AppPath + "\\Resource Files\\Kepler2.dat", AppPath + "\\Resource Files\\Kepler2.old");
			}
			using StreamWriter streamWriter = new StreamWriter(AppPath + "\\Resource Files\\Kepler2.dat");
			for (int i = 0; i < K2.Count; i++)
			{
				streamWriter.WriteLine(K2[i].K2Line);
			}
		}

		internal static void Write_XZinKepler2()
		{
			int num = 0;
			int num2 = 0;
			int num3 = 35;
			using (FileStream fileStream = new FileStream(AppPath + "\\Resource Files\\xz80.dat", FileMode.Open, FileAccess.Read))
			{
				using BinaryReader binaryReader = new BinaryReader(fileStream);
				int num4 = (int)fileStream.Length / num3;
				for (int i = 0; i < K2.Count; i++)
				{
					_ = 57000;
					double mag = K2[i].Mag;
					if (mag < 13.0)
					{
						double rA = K2[i].RA;
						double dec = K2[i].Dec;
						num = num2 - 30;
						if (num < 1)
						{
							num = 1;
						}
						do
						{
							fileStream.Seek(num * 35, SeekOrigin.Begin);
							double num5 = (double)binaryReader.ReadInt32() / 10000000.0 + 180.0;
							if (rA - num5 > 0.001)
							{
								num++;
								continue;
							}
							if (num5 - rA > 0.001)
							{
								break;
							}
							binaryReader.ReadSingle();
							double num6 = (double)binaryReader.ReadInt32() / 10000000.0;
							binaryReader.ReadSingle();
							double num7 = (double)binaryReader.ReadInt16() / 100.0;
							if ((Math.Abs(rA - num5) < 0.0005) & (Math.Abs(dec - num6) < 0.0005) & (Math.Abs(mag - num7) < 2.0))
							{
								fileStream.Seek(num * 35 + 24, SeekOrigin.Begin);
								K2[i].XZ = binaryReader.ReadInt32();
								break;
							}
							num++;
						}
						while (num < num4);
					}
					num2 = num;
				}
			}
			KObjects.SortByXZ = true;
			K2.Sort();
			if (File.Exists(AppPath + "\\Resource Files\\XZinKepler2.old"))
			{
				File.Delete(AppPath + "\\Resource Files\\XZinKepler2.old");
			}
			if (File.Exists(AppPath + "\\Resource Files\\XZinKepler2.dat"))
			{
				File.Move(AppPath + "\\Resource Files\\XZinKepler2.dat", AppPath + "\\Resource Files\\XZinKepler2.old");
			}
			using StreamWriter streamWriter = new StreamWriter(AppPath + "\\Resource Files\\XZinKepler2.dat");
			for (int j = 0; j < K2.Count; j++)
			{
				if (K2[j].XZ >= 1)
				{
					StringBuilder stringBuilder = new StringBuilder();
					stringBuilder.AppendFormat("{0,6:f0}", K2[j].XZ);
					stringBuilder.AppendFormat(" {0,1:f0}", K2[j].Cadence);
					stringBuilder.Append(K2[j].EPIC_ID.ToString().PadLeft(10));
					stringBuilder.AppendFormat("{0,3:f0}", K2[j].Field);
					streamWriter.WriteLine(stringBuilder.ToString());
				}
			}
		}

		internal static void ReIndex_XZ80_ForKepler2()
		{
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
			int.Parse(new string(array, i + 1, 8));
			using (StreamReader streamReader = new StreamReader(AppPath + "\\Resource Files\\XZinKepler2.dat"))
			{
				do
				{
					string text = streamReader.ReadLine();
					if (int.TryParse(text.Substring(0, 6), out var result) && int.TryParse(text.Substring(7, 1), out var result2))
					{
						int num = XZ80Q.Get_XZ_RecordNumber(result) * 35 + 23;
						fileStream.Seek(num, SeekOrigin.Begin);
						string text2 = binaryReader.ReadChar().ToString();
						char ch = (((text2 == " ") | (text2.ToUpper() == "K")) ? ((result2 >= 2) ? Convert.ToChar("k") : Convert.ToChar("K")) : ((result2 >= 2) ? Convert.ToChar("p") : Convert.ToChar("P")));
						fileStream.Seek(num, SeekOrigin.Begin);
						binaryWriter.Write(ch);
					}
				}
				while (!streamReader.EndOfStream);
			}
			fileStream.Close();
		}

		internal static bool GetKepler2InfoForXZ(int XZ, out long ID, out int Cadence)
		{
			string Field = "";
			return GetKepler2InfoForXZ(XZ, out ID, out Cadence, out Field);
		}

		internal static bool GetKepler2InfoForXZ(int XZ, out long ID, out int Cadence, out string Field)
		{
			if (XZinK2.Count < 1 && File.Exists(XZinKepler2File))
			{
				using StreamReader streamReader = new StreamReader(XZinKepler2File);
				do
				{
					K2_XZ k2_XZ = new K2_XZ();
					k2_XZ.Read_XZinKepler2_Line(streamReader.ReadLine());
					XZinK2.Add(k2_XZ);
				}
				while (!streamReader.EndOfStream);
			}
			int num = 0;
			int num2 = XZinK2.Count - 1;
			int num3 = 0;
			do
			{
				num3 = (num2 + num) / 2;
				if (XZinK2[num3].XZ == XZ)
				{
					Cadence = XZinK2[num3].Cadence;
					ID = XZinK2[num3].EPIC_ID;
					Field = XZinK2[num3].Field;
					return true;
				}
				if (XZinK2[num3].XZ > XZ)
				{
					num2 = num3 - 1;
				}
				else
				{
					num = num3 + 1;
				}
			}
			while (num2 >= num);
			ID = 0L;
			Cadence = 0;
			Field = "";
			return false;
		}

		internal static void HistoricLunarsInKepler2(string ArchiveFile)
		{
			int xZ = 0;
			long ID = 0L;
			int Cadence = 0;
			string Field = "";
			using StreamWriter streamWriter2 = new StreamWriter(AppPath + "\\Observations\\Archive_in_Kepler2.txt", append: true);
			using StreamWriter streamWriter = new StreamWriter(AppPath + "\\Observations\\Archive_in_Kepler2.csv", append: true);
			if (streamWriter.BaseStream.Length == 0L)
			{
				streamWriter.WriteLine("yyyymmddhhmmss.ss Star ID Ev ,field,Cad,   EPIC-ID, longitude  latitude  ,Observer");
			}
			using StreamReader streamReader = new StreamReader(ArchiveFile);
			do
			{
				string text = streamReader.ReadLine();
				if (!"GVMP".Contains(text.Substring(34, 1)))
				{
					continue;
				}
				string text2 = text.Substring(18, 1);
				if (!"RSX".Contains(text2))
				{
					continue;
				}
				int num = int.Parse(text.Substring(19, 6));
				switch (text2)
				{
				case "X":
					xZ = num;
					break;
				case "R":
					if (XZ80Q.Get_ZC_Star(num))
					{
						xZ = XZ80Q.XZ;
					}
					break;
				case "S":
					if (XZ80Q.Get_SAO_Star(num))
					{
						xZ = XZ80Q.XZ;
					}
					break;
				}
				if (GetKepler2InfoForXZ(xZ, out ID, out Cadence, out Field))
				{
					streamWriter2.WriteLine(text);
					StringBuilder stringBuilder = new StringBuilder();
					stringBuilder.Append(text.Substring(0, 29));
					stringBuilder.Append(", {" + Field.Trim().PadLeft(2) + "}");
					stringBuilder.Append(", " + Cadence.ToString().PadLeft(2));
					stringBuilder.Append("," + ID.ToString().PadLeft(10));
					stringBuilder.Append("," + text.Substring(81, 11) + "_" + text.Substring(92, 10));
					stringBuilder.Append("," + text.Substring(188, 25));
					streamWriter.WriteLine(stringBuilder.ToString());
				}
			}
			while (!streamReader.EndOfStream);
		}

		internal static void Initialise_Kepler2_ForAsteroids()
		{
			K2.Clear();
			NumKep2Stars = 0;
			Kepler2DataExists = File.Exists(AppPath + "\\Resource Files\\Kepler2.dat");
			if (!Kepler2DataExists)
			{
				return;
			}
			using StreamReader streamReader = new StreamReader(AppPath + "\\Resource Files\\Kepler2.dat");
			do
			{
				string text = streamReader.ReadLine();
				KObjects kObjects = new KObjects();
				kObjects.EPIC_ID = long.Parse(text.Substring(0, 9));
				kObjects.Field = text.Substring(9, 2);
				kObjects.RA = double.Parse(text.Substring(12, 10));
				kObjects.Dec = double.Parse(text.Substring(23, 10));
				kObjects.Mag = double.Parse(text.Substring(34, 4));
				kObjects.Cadence = int.Parse(text.Substring(39, 1));
				if (kObjects.Cadence == 9)
				{
					kObjects.Cadence = 30;
				}
				K2.Add(kObjects);
				NumKep2Stars++;
			}
			while (!streamReader.EndOfStream);
		}
	}
}
