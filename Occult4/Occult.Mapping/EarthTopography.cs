using System;
using System.IO;

namespace Occult.Mapping
{
	internal class EarthTopography
	{
		internal static string FileName = Utilities.AppPath + "\\Resource files\\Earth2014.SUR2014.1min.geod.bin";

		internal static FileStream EarthStream;

		internal static BinaryReader EarthBinaryReader;

		internal static long EarthPointer;

		internal static void OpenEarthFile()
		{
			EarthStream = new FileStream(FileName, FileMode.Open, FileAccess.Read);
			EarthBinaryReader = new BinaryReader(EarthStream);
		}

		internal static void CloseEarthFile()
		{
			try
			{
				EarthStream.Close();
			}
			catch
			{
			}
		}

		internal static int Elevation_Above_MSL(double Longitude_deg, double Latitude_deg)
		{
			if (Math.Abs(Latitude_deg) >= 90.0)
			{
				return 0;
			}
			if (Longitude_deg < -180.0)
			{
				Longitude_deg += 360.0;
			}
			double num = (Longitude_deg + 180.0) * 60.0;
			double num2 = (Latitude_deg + 90.0) * 60.0;
			int num3 = (int)num;
			double num4 = num - (double)num3 - 0.5;
			int num5 = (int)num2;
			double num6 = num2 - (double)num5 - 0.5;
			long num7 = 2 * (num5 * 21600 + num3);
			int num8 = PointHeight(num7);
			if (Math.Abs(Latitude_deg) >= 89.0)
			{
				return num8;
			}
			int num9;
			int num10;
			int num11;
			if (num4 > 0.0)
			{
				num9 = PointHeight(num7 + 2);
				if (num6 > 0.0)
				{
					num10 = PointHeight(num7 + 43200);
					num11 = PointHeight(num7 + 43200 + 2);
				}
				else
				{
					num10 = PointHeight(num7 - 43200);
					num11 = PointHeight(num7 - 43200 + 2);
				}
			}
			else
			{
				num9 = PointHeight(num7 - 2);
				if (num6 > 0.0)
				{
					num10 = PointHeight(num7 + 43200);
					num11 = PointHeight(num7 + 43200 - 2);
				}
				else
				{
					num10 = PointHeight(num7 - 43200);
					num11 = PointHeight(num7 - 43200 - 2);
				}
			}
			double num12 = (double)(num8 + num10) / 2.0;
			double num13 = (double)(num8 + num9) / 2.0;
			double num14 = (double)(num8 + num10 + num9 + num11) / 4.0;
			double num15 = Math.Abs(num4);
			double num16 = Math.Abs(num6);
			double num17;
			double num18 = (num17 = 0.0);
			double num19;
			double num20 = (num19 = 0.5);
			double num21 = num8;
			double num22 = num12;
			double num23 = num13;
			double num24 = num14;
			return (int)(((num20 - num15) * (num19 - num16) * num21 + (num15 - num18) * (num19 - num16) * num23 + (num20 - num15) * (num16 - num17) * num22 + (num15 - num18) * (num16 - num17) * num24) / ((num20 - num18) * (num19 - num17)));
		}

		private static int PointHeight(long EarthPointer)
		{
			EarthStream.Position = EarthPointer;
			byte[] array = EarthBinaryReader.ReadBytes(2);
			Array.Reverse((Array)array);
			int num = BitConverter.ToInt16(array, 0);
			if (num < 0)
			{
				num = 0;
			}
			return num;
		}
	}
}
