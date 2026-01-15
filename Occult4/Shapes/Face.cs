namespace Shapes
{
	internal class Face
	{
		public int f1;

		public int f2;

		public int f3;

		public int F1
		{
			get
			{
				return f1;
			}
			set
			{
				f1 = value;
			}
		}

		public int F2
		{
			get
			{
				return f2;
			}
			set
			{
				f2 = value;
			}
		}

		public int F3
		{
			get
			{
				return f3;
			}
			set
			{
				f3 = value;
			}
		}

		public void SetFaces(string Face1, string Face2, string Face3, bool Subtract1)
		{
			f1 = int.Parse(Face1);
			f2 = int.Parse(Face2);
			f3 = int.Parse(Face3);
			if (Subtract1)
			{
				f1--;
				f2--;
				f3--;
			}
		}

		public override string ToString()
		{
			return string.Format("{0,1:f0},{1,1:f0},{2,1:f0}", F1, F2, F3);
		}
	}
}
