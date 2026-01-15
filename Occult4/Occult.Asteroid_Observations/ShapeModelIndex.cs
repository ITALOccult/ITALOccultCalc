using System;
using System.Text;

namespace Occult.Asteroid_Observations
{
	internal class ShapeModelIndex : IComparable
	{
		private int asteroid_id;

		private int asteroid_number;

		private int dAMITModelCount;

		private int iSAMmodelCount;

		private int[] damit_id = new int[6];

		private int[] iSAM_id = new int[8];

		private string[] iSAM_Version = new string[8] { "", "", "", "", "", "", "", "" };

		internal int Asteroid_id
		{
			get
			{
				return asteroid_id;
			}
			set
			{
				asteroid_id = value;
			}
		}

		internal int Asteroid_number
		{
			get
			{
				return asteroid_number;
			}
			set
			{
				asteroid_number = value;
			}
		}

		internal int DAMITModelCount
		{
			get
			{
				return dAMITModelCount;
			}
			set
			{
				dAMITModelCount = value;
			}
		}

		internal int[] DAMIT_id
		{
			get
			{
				return damit_id;
			}
			set
			{
				damit_id = value;
			}
		}

		internal int ISAMmodelCount
		{
			get
			{
				return iSAMmodelCount;
			}
			set
			{
				iSAMmodelCount = value;
			}
		}

		internal int[] ISAM_id
		{
			get
			{
				return iSAM_id;
			}
			set
			{
				iSAM_id = value;
			}
		}

		internal string[] ISAM_Version
		{
			get
			{
				return iSAM_Version;
			}
			set
			{
				iSAM_Version = value;
			}
		}

		internal string Summary => Asteroid_number + "," + NumModels();

		public int CompareTo(object other)
		{
			return Asteroid_number.CompareTo(((ShapeModelIndex)other).Asteroid_number);
		}

		public override string ToString()
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.AppendFormat("{0,6:f0}", Asteroid_number);
			if (DAMITModelCount > 0)
			{
				stringBuilder.AppendFormat(DAMITModelCount.ToString().PadLeft(5));
			}
			else
			{
				stringBuilder.Append(".".PadLeft(5));
			}
			if (ISAMmodelCount > 0)
			{
				stringBuilder.AppendFormat(ISAMmodelCount.ToString().PadLeft(5));
			}
			else
			{
				stringBuilder.Append(".".PadLeft(5));
			}
			return stringBuilder.ToString();
		}

		private int NumModels()
		{
			int num = 0;
			for (int i = 0; i < 6; i++)
			{
				if (damit_id[i] > 0)
				{
					num++;
				}
			}
			return num;
		}
	}
}
