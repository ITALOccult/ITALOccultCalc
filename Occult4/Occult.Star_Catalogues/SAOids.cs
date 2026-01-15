using System;
using System.Collections.Generic;
using System.IO;

namespace Occult.Star_Catalogues
{
	internal class SAOids
	{
		private const double Radian = 180.0 / Math.PI;

		private static List<SAO> Stars = new List<SAO>();

		private static FileStream SAOindex;

		private static FileStream SAOfile;

		private static BinaryReader Index;

		private static BinaryReader File;

		internal static void InitialiseSAO()
		{
			SAOindex = new FileStream(Utilities.AppPath + "\\Resource files\\SAO.inx", FileMode.Open);
			Index = new BinaryReader(SAOindex);
			SAOfile = new FileStream(Utilities.AppPath + "\\Resource files\\SAO1950.bin", FileMode.Open);
			File = new BinaryReader(SAOfile);
		}

		internal static void CloseSAO()
		{
			try
			{
				Index.Close();
				SAOindex.Close();
				File.Close();
				SAOfile.Close();
			}
			catch
			{
			}
		}

		internal static int SAOnumber(double RA2000_deg, double PMra_arcsec, double Dec2000_Deg, double PMdec_arcsec)
		{
			int[] array = new int[4];
			if (SAOindex == null)
			{
				InitialiseSAO();
			}
			Stars.Clear();
			double RA = (RA2000_deg - 50.0 * PMra_arcsec / 3600.0) / (180.0 / Math.PI);
			double Dec = (Dec2000_Deg - 50.0 * PMdec_arcsec / 3600.0) / (180.0 / Math.PI);
			Utilities.PrecessFromJ2000(2433282.5, use2006values_Not1976: false, ref RA, ref Dec);
			double num = RA * (180.0 / Math.PI);
			double num2 = Dec * (180.0 / Math.PI);
			int num3 = (int)Math.Floor(num - 0.1);
			if (num3 < 0)
			{
				num3 += 360;
			}
			int num4 = (int)Math.Ceiling(num + 0.1);
			if (num4 > 360)
			{
				num4 -= 360;
			}
			int num5 = 8 - (int)Math.Floor(num2 / 10.0);
			SAOindex.Seek(4 * (361 * num5), SeekOrigin.Begin);
			array[0] = Index.ReadInt32();
			SAOindex.Seek(4 * (361 * num5 + num3), SeekOrigin.Begin);
			array[1] = Index.ReadInt32();
			SAOindex.Seek(4 * (361 * num5 + num4), SeekOrigin.Begin);
			array[2] = Index.ReadInt32();
			SAOindex.Seek(4 * (361 * num5 + 360), SeekOrigin.Begin);
			array[3] = Index.ReadInt32();
			int num6 = array[1];
			int num7 = array[2];
			double num8 = 0.0;
			if (num6 >= num7)
			{
				num6 = array[0];
				num7 = array[2];
				num8 = 360.0;
			}
			while (true)
			{
				SAOfile.Seek(9 * num6 - 9, SeekOrigin.Begin);
				float num9 = File.ReadSingle();
				float num10 = File.ReadSingle();
				float mag = (float)File.ReadSByte() / 10f;
				Utilities.Distance(RA, Dec, (double)num9 / (180.0 / Math.PI), (double)num10 / (180.0 / Math.PI), out var Distance, out var _);
				Distance *= 648000.0 / Math.PI;
				if (Distance < 60.0)
				{
					SAO sAO = new SAO();
					sAO.Number = num6;
					sAO.Separation = (float)Distance;
					sAO.Mag = mag;
					Stars.Add(sAO);
				}
				num6++;
				if (num6 > num7)
				{
					if (num8 == 0.0)
					{
						break;
					}
					num8 = 0.0;
					num6 = array[1];
					num7 = array[3];
					if (!(num8 > 0.0))
					{
						break;
					}
				}
			}
			if (Stars.Count == 0)
			{
				return 0;
			}
			Stars.Sort();
			if (Stars[0].Separation > 10f)
			{
				return 0;
			}
			return Stars[0].Number;
		}
	}
}
