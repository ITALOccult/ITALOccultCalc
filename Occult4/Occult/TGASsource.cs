using System;
using System.Text;

namespace Occult
{
	internal class TGASsource : IComparable
	{
		private const double EpochDiff = 1.0;

		private const double MilliSec_InRadian = 648000000.0 / Math.PI;

		private double ra_2016;

		private double dec_2016;

		private double mag;

		private double raCosDec_2016;

		private double cosDecFactor;

		private uint catNumber;

		private string tgasLine = "";

		private string catID = "";

		private string catNum = "";

		internal static bool SortByRA_NotHip = true;

		internal string TgasLine
		{
			set
			{
				tgasLine = value;
				string[] array = tgasLine.Split(new char[1] { ',' });
				dec_2016 = double.Parse(array[1]) + 1.0 * double.Parse(array[4]);
				cosDecFactor = Math.Cos(dec_2016 / (648000000.0 / Math.PI));
				ra_2016 = double.Parse(array[0]) + 1.0 * double.Parse(array[3]) / cosDecFactor;
				raCosDec_2016 = ra_2016 * cosDecFactor;
				mag = double.Parse(array[6]);
				if (array[15].Trim() != "")
				{
					catID = "2";
					catNum = array[15];
				}
				else
				{
					string[] array2 = array[16].Replace("\"", "").Trim().Split(new char[1] { '-' });
					if (array2[2] == "1")
					{
						catID = "3";
					}
					else if (array2[2] == "2")
					{
						catID = "4";
					}
					else if (array2[2] == "3")
					{
						catID = "5";
					}
					catNum = array2[0] + array2[1].PadLeft(5, '0');
				}
				catNumber = uint.Parse(catNum);
			}
		}

		internal double RA_2016 => ra_2016;

		internal double CosDecFactor => cosDecFactor;

		internal double RACosDec_2016 => raCosDec_2016;

		internal double Dec_2016 => dec_2016;

		internal double Mag => mag;

		internal string CatID => catID;

		internal string CatNum => catNum;

		internal uint CatNumber => catNumber;

		public int CompareTo(object other)
		{
			if (SortByRA_NotHip)
			{
				return RA_2016.CompareTo(((TGASsource)other).RA_2016);
			}
			if (CatID != ((TGASsource)other).CatID)
			{
				return CatID.CompareTo(((TGASsource)other).CatID);
			}
			return CatNumber.CompareTo(((TGASsource)other).CatNumber);
		}

		public override string ToString()
		{
			StringBuilder stringBuilder = new StringBuilder();
			string[] array = tgasLine.Split(new char[1] { ',' });
			stringBuilder.Append(array[0] + "," + array[1] + "," + array[2] + "," + array[3] + "," + array[4] + ",0,15.0,");
			stringBuilder.Append("30," + array[6] + ",30,");
			stringBuilder.Append(array[7] + "," + array[8] + "," + array[9] + "," + array[10] + "," + array[11] + ",0,");
			stringBuilder.Append(array[12] + "," + array[13] + ",0,");
			stringBuilder.Append("1,");
			stringBuilder.Append(array[14] + ",");
			stringBuilder.Append(catID + "," + catNum);
			return stringBuilder.ToString();
		}
	}
}
